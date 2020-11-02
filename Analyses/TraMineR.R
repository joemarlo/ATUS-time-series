library(tidyverse)
library(TraMineR)
library(fastcluster)
library(NbClust)
library(dendextend)
source("Plots/ggplot_settings.R")
set.seed(44)

# read in ATUS data
atus_raw <- read_tsv("Inputs/atus.tsv")

# read in the demographics data
demographics <- read_delim(file = "Inputs/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)


# filter to only include respondents who logged on weekdays and non holidays
IDs <- demographics %>% 
  filter(day_of_week %in% 2:6,
         holiday == 0) %>% 
  dplyr::select(ID, survey_weight)

# filter to just include weekends, pivot wider, and sample with weights
n_sample <- 10000
atus_sampled <- atus_raw %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  right_join(IDs, by = "ID") %>% 
  slice_sample(n = n_sample, weight_by = survey_weight) %>% 
  dplyr::select(-survey_weight)

# define alphabet as all unique states
alphabet <- atus_sampled[,-1] %>% unlist() %>% unique() %>% sort()
# states <- c("CHH")
labels <- c("Care_HH", "Care_NHH", "Cons_Pur", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Prsl_care", "Care_svcs", "Rel_spirit", 
            "Sleep", "Leisure", "Recreation", "Volunteer", "Work")

# create state sequence object
atus_seq <- seqdef(data = atus_sampled[, -1], 
                   alphabet = alphabet, 
                   # states = mvad.scodes,
                   id = atus_sampled$ID,
                   labels = labels,
                   xtstep = 1)

dev.off()
# plot first 10 sequences
seqiplot(atus_seq, with.legend = "right", border = NA)
# plot all sequences sorted 
seqIplot(atus_seq, sortv = "from.start", with.legend = "right")
png("Plots/state_distribution.png", width = 900, height = 450)
seqdplot(atus_seq, sortv = "from.start", with.legend = "right", border = 'white', main = "State distribution of activities", xlab = "Time")
dev.off()
# plot 10 most frequent sequences
seqfplot(atus_seq, with.legend = "right", border = NA)
# plot legend
seqlegend(atus_seq)

# summary statistics
dev.off()
par(mfrow = c(2, 2))
seqdplot(atus_seq, with.legend = FALSE, border = NA, main = "(a) state distribution")
seqHtplot(atus_seq, main = "(b) entropy index")
seqmsplot(atus_seq, with.legend = FALSE, border = NA, main = "(c) mean time")
seqmtplot(atus_seq, with.legend = FALSE, main = "(d) modal state seq.")

# compute optimal matching distances
dist_om_ham <- seqdist(atus_seq, method = "HAM")
dist_om_LCS <- seqdist(atus_seq, method = "OM", indel = 1, sm = seqsubm(atus_seq, method = "CONSTANT", cval = 2))
dist_om_ham_dyn <- seqdist(atus_seq, method = "DHD")
dist_om_TRATE <- seqdist(atus_seq, method = "OM", indel = 1, sm = "TRATE")

dist_names <- c("Hamming", "LCS", "Dynamic Hamming", "TRATE")

# cluster
dists <- list(dist_om_ham, dist_om_LCS, dist_om_ham_dyn, dist_om_TRATE)
clusters <- lapply(dists, function(dist){
  hclust(as.dist(dist), method = "ward.D2")
})
# clusterward1 <- hclust(as.dist(dist_om_ham), method = "ward.D2")
# dev.off()
# plot(clusterward1)

# get optimal cluster sizes by calculating silhouette width
s_widths <- lapply(dists, function(clus){
  NbClust(
    data = NULL,
    diss = as.dist(clus),
    distance = NULL,
    method = 'ward.D2',
    max.nc = 14,
    min.nc = 2,
    index = 'silhouette'
  )
})

# plot the sillhouette widhts
map_dfr(seq_along(s_widths), function(i){
  s_widths[[i]]$All.index %>% 
    enframe() %>% 
    mutate(name = as.numeric(name),
           method = dist_names[i])
}) %>% 
  mutate(method = factor(method, c("Hamming", "LCS", "Dynamic Hamming", "TRATE"))) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_line(color = 'grey30') +
  geom_area(alpha = 0.4) +
  geom_point(color = 'grey30') +
  scale_x_continuous(breaks = 2:14) +
  scale_y_continuous(limits = c(0, 0.3)) +
  facet_wrap(~method) +
  labs(title = "Silhouette width by edit distance method",
       subtitle = paste0("Weighted sample of ", 
                         scales::comma_format()(n_sample),
                         " respondents"),
       x = 'n clusters',
       y = 'Silhouette width')
save_plot(filename = paste0("Plots/TraMineR/silhouettes"), height = 6, width = 9)



# dendrograms -------------------------------------------------------------

ggdends <- map_dfr(seq_along(s_widths), function(i){
  hcl_ward <- clusters[[i]]
  hcl_k <- 4 #hclust_sw$Best.nc[['Number_clusters']]
  dend <- as.dendrogram(hcl_ward) %>% set("branches_k_color", k = hcl_k) %>% set("labels_colors")
  dend <- cut(dend, h = 50)$upper # cut off bottom of dendogram for computation performance
  ggd1 <- as.ggdend(dend)
  
  # set dashed line for non-cluster segments
  ggd1$segments$linetype <- 'solid'
  ggd1$segments$linetype[which(is.na(ggd1$segments$col))] <- 'dashed'
  
  # set connecting lines to grey
  ggd1$segments$col[is.na(ggd1$segments$col)] <- 'grey50'
  
  # add an identifier column
  ggd1$segments$method <- dist_names[i]

  return(ggd1$segments)
})

# plot the dendrograms
ggdends %>% 
  mutate(method = factor(method, c("Hamming", "LCS", "Dynamic Hamming", "TRATE"))) %>%
  ggplot() + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), 
               color = ggdends$col, linetype = ggdends$linetype,
               lwd = 0.6, alpha = 0.7) +
  facet_wrap(~method, scales = "free_y") +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Dendrograms by edit distance with Ward (D2) linkage",
       subtitle = paste0("Weighted sample of ", 
                         scales::comma_format()(n_sample),
                         " respondents"),
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none')
save_plot(filename = paste0("Plots/TraMineR/dendrograms"), height = 6, width = 10)


# sequence plots ----------------------------------------------------------


for (i in seq_along(clusters)){
  cluster_5 <- cutree(clusters[[i]], k = 4)
  cluster_5 <- factor(cluster_5, labels = paste("Cluster", 1:4))

  # dev.off()
  par(mar = c(3, 3, 3, 3))
  png(paste0("Plots/TraMineR/", dist_names[i], "_state_sequence.png"), width = 600, height = 450)
  seqIplot(atus_seq, group = cluster_5, sortv = "from.start", main = paste0(dist_names[i], " state sequences"), with.legend = FALSE)
  dev.off()
  
  par(mar = c(3, 3, 3, 3))
  png(paste0("Plots/TraMineR/", dist_names[i], "_state_distribution.png"), width = 900, height = 450)
  seqdplot(atus_seq, group = cluster_5, sortv = "from.start", border = 'white', main = paste0(dist_names[i], " state distributions"), with.legend = FALSE)
  dev.off()
  
  par(mar = c(3, 3, 3, 3))
  png(paste0("Plots/TraMineR/", dist_names[i], "_state_entropy.png"), width = 900, height = 450)
  seqHtplot(atus_seq, group = cluster_5, main = paste0(dist_names[i], " entropy index"), with.legend = FALSE)
  dev.off()
  
  par(mar = c(3, 3, 3, 3))
  png(paste0("Plots/TraMineR/", dist_names[i], "_state_modal.png"), width = 900, height = 450)
  seqmsplot(atus_seq, group = cluster_5, border = NA, main = paste0(dist_names[i], " mean time"))
  dev.off()
}

# get cluster membership by method
cluster_memberships <- map_dfc(seq_along(clusters), function(i){
  tmp <- tibble(cutree(clusters[[i]], k = 4))
  colnames(tmp) <- paste0(dist_names[i], "_cluster")
  tmp
}) %>%
  bind_cols(ID = atus_sampled$ID)

hamming_clusters <- cluster_memberships %>% select(`Dynamic Hamming_cluster`, ID)
colnames(hamming_clusters) <- c("cluster", "ID")
hamming_clusters$cluster <- case_when(
  hamming_clusters$cluster == 1 ~ "Uncategorized",
  hamming_clusters$cluster == 2 ~ "Night workers",
  hamming_clusters$cluster == 3 ~ "Students",
  hamming_clusters$cluster == 4 ~ "Day workers"
)


# modeling ----------------------------------------------------------------

master_df <- hamming_clusters %>% 
  left_join(demographics, by = "ID") %>% 
  mutate(alone_transformed = sqrt(TRTALONE),
         year = year - min(year))

# alone time distributions
master_df %>% 
  ggplot(aes(x = TRTALONE)) +
  geom_density() +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~cluster) +
  labs(title = "Distribution of alone time by cluster membership",
       x = "Daily minutes alone",
       y = NULL)
save_plot(filename = paste0("Plots/TraMineR/alone_time_distribution"), height = 5, width = 8)

# transformed alone time distributions
master_df %>% 
  ggplot(aes(x = alone_transformed)) +
  geom_density() +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~cluster) +
  labs(title = "Distribution of alone time by cluster membership - square root transformed",
       x = "Daily minutes alone",
       y = NULL)
save_plot(filename = paste0("Plots/TraMineR/alone_time_distribution_sqrt"), height = 5, width = 8)

# linear models
master_df %>% 
  group_by(cluster) %>% 
  nest() %>% 
  mutate(#model = map(data, function(df) MASS::glm.nb(alone_minutes ~ year, data = df)),
    model = map(data, function(df) glm(alone_transformed ~ year, data = df)),
    tidied = map(model, broom::tidy),
    confint = map(model, function(model){
      cf <- confint(model)
      tibble(lower = cf[2,1], upper = cf[2,2])
    })) %>% 
  unnest(c(tidied, confint)) %>% 
  dplyr::select(-data) %>% 
  ungroup() %>% 
  filter(term == 'year') %>% 
  ggplot(aes(x = estimate, y = cluster, xmin = lower, xmax = upper)) +
  geom_point() +
  geom_linerange() + 
  # facet_grid(~cluster, scales = 'free_x') +
  labs(title = "Linear regression estimates and 95% confidence interval",
       subtitle = "Four models fitted individually by cluster membership",
       x = "\nAnnual change in time spent alone",
       y = NULL)
save_plot("Plots/TraMineR/lm_results", height = 4, width = 6.5)

# linear models
library(lme4)
mlm <- lme4::lmer(alone_transformed ~ year + (year | cluster), data = master_df,
                  verbose = TRUE,
                  control = lme4::lmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e12)))
summary(mlm)
anova(mlm)


# demographics ------------------------------------------------------------

# age densities
master_df %>% 
  dplyr::select(age, cluster) %>%
  ggplot(aes(x = age)) +
  geom_density() +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~ cluster, scales = 'free_y', nrow = 2) +
  labs(title = "Distribution of age by cluster method",
       x = "Age",
       y = NULL)
save_plot("Plots/TraMineR/age_distribution", height = 5, width = 8)

# sex
master_df %>% 
  dplyr::select(age, cluster, sex) %>%
  group_by(cluster) %>% 
  summarize(Male = mean(sex == 1),
            Female = mean(sex == 2),
            .groups = 'drop') %>% 
  pivot_longer(cols = c("Male", "Female")) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_col(position = 'dodge', color = 'white') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 0.6, by = 0.2)) +
  facet_wrap(~ cluster, nrow = 2) +
  labs(title = "Sex split by cluster",
       x = NULL,
       y = NULL)
save_plot("Plots/TraMineR/sex_distribution", height = 5, width = 8)

# density of turbulence by cluster
# seqST(atus_seq) %>% 
#   cbind(cluster_5) %>% 
#   as.data.frame() %>% 
#   ggplot(aes(x = Turbulence)) +
#   geom_density() +
#   facet_wrap(~cluster_5)
