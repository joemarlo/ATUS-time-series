library(tidyverse)
library(dendextend)
library(NbClust)
library(stringdist)
source("Plots/ggplot_settings.R")
options(mc.cores = parallel::detectCores())
set.seed(44)
options(scipen = 999)


# read in the sample ------------------------------------------------------

atus_string_samp <- read_csv("Analyses/Data/sample_10k.csv",
                             col_types = cols(
                               ID = col_double(),
                               string = col_character()
                             ))

# set sample size
n_sample <- nrow(atus_string_samp)


# distance ----------------------------------------------------------------

# compute the string distances
distance_method <- "lv" # "lv" "hamming" "osa" "lcs"
distance_method_pretty <- case_when(distance_method == 'hamming' ~ "Hamming",
                                    distance_method == 'osa' ~ "OSA",
                                    distance_method == 'lcs' ~ 'LCS',
                                    distance_method == 'lv' ~ 'Levenshtein')
dist_matrix <- stringdistmatrix(a = atus_string_samp$string, method = distance_method)

# there may be infinite distances between two strings so need to top top code them
hist(dist_matrix)
range(dist_matrix)
any(is.infinite(dist_matrix))
dist_matrix[is.infinite(dist_matrix)] <- 100000


# DBSCAN ------------------------------------------------------------------

# db_cluster <- dbscan::dbscan(x = dist_matrix, eps = 10, minPts = 100)


# optimal clusters --------------------------------------------------------

# get optimal cluster sizes by calculating silhouette width
hclust_sw <- NbClust(
  data = NULL,
  diss = dist_matrix,
  distance = NULL,
  method = 'ward.D2',
  max.nc = 20,
  min.nc = 2,
  index = 'silhouette'
)

# plot the results
hclust_sw$All.index %>% 
  enframe() %>% 
  mutate(name = as.numeric(name)) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_line(color = 'grey30') +
  geom_area(alpha = 0.4) +
  geom_point(color = 'grey30') +
  scale_x_continuous(breaks = 2:20) +
  labs(title = paste0(distance_method_pretty, " edit distance silhouette width"),
       subtitle = paste0("Weighted sample of ", 
                         scales::comma_format()(n_sample),
                         " respondents"),
       x = 'n clusters',
       y = 'Silhouette width')
save_plot(filename = paste0("Analyses/", distance_method_pretty, "/Plots/", distance_method_pretty, "_silhouette"))
# ggsave(filename = paste0("Analyses/", distance_method_pretty, "/Plots/", distance_method_pretty, "_silhouette.svg"),
#        device = "svg",
#        height = 5,
#        width = 7)


# re-cluster using a larger sample ----------------------------------------

# the optimum amount of clusters appears to be four
# now resample from the original dataset with a large sample size and 
  # recompute the distance matrix and clustering

# increase sample size
atus_string_samp <- read_csv("Analyses/Data/sample_25k.csv",
                             col_types = cols(
                               ID = col_double(),
                               string = col_character()
                             ))

# set sample size
n_sample <- nrow(atus_string_samp)

# compute the string distances
dist_matrix <- stringdistmatrix(a = atus_string_samp$string, method = distance_method)

# there may be infinite distances between two strings so need to top top code them
# hist(dist_matrix)
range(dist_matrix)
any(is.infinite(dist_matrix))
dist_matrix[is.infinite(dist_matrix)] <- 100000

# ward (D2) linkage hier clustering
hcl_ward <- hclust(d = dist_matrix, method = 'ward.D2')
# plot(hcl_ward)


# color the dendrogram and get the cluster membership ---------------------

# trimmed, colored branches tree
hcl_k <- hclust_sw$Best.nc[['Number_clusters']]
dend <- as.dendrogram(hcl_ward) %>% set("branches_k_color", k = hcl_k) %>% set("labels_colors")
dend <- cut(dend, h = 50)$upper # cut off bottom of dendogram for computation performance
ggd1 <- as.ggdend(dend)

# get labels from model
groupings <- cutree(hcl_ward, hcl_k)

# fix label switching problem
# get approx cluster size by counting line segments by color (proxy for cluster)
label_match <- ggd1$segments %>% 
  filter(col != '<NA>') %>% 
  group_by(col) %>% 
  # filter(xend <= quantile(xend, 0.99),
  #        xend >= quantile(xend, 0.01)) %>% 
  summarize(x_min = min(xend),
            x_max = max(xend),
            .groups = 'drop') %>% 
  arrange(x_min) %>% 
  mutate(Approx_cluster_size = x_max - x_min,
         Cluster = 1:hcl_k) %>% 
  arrange(desc(Approx_cluster_size)) %>% 
  mutate(Actual_cluster = names(rev(sort(table(groupings))))) %>% 
  select(Dend_cluster = Cluster,
         Actual_cluster,
         col)

# replace groupings with similar vector but with switched labels
correct_clusters <- label_match$Dend_cluster[match(groupings, label_match$Actual_cluster)]
rm(groupings)

# match brewer palette colors to cluster assignment
custom_colors <- colorspace::darken(RColorBrewer::brewer.pal(hcl_k, 'Spectral'), amount = 0.2)
label_match <- label_match %>% 
  arrange(Dend_cluster) %>% 
  mutate(brew_palette = custom_colors)

# replace colors with brewer colors
ggd1$segments$col <- label_match$brew_palette[match(ggd1$segments$col, label_match$col)]

# set dashed line for non-cluster segements
ggd1$segments$linetype <- 'solid'
ggd1$segments$linetype[which(is.na(ggd1$segments$col))] <- 'dashed'

# set connecting lines to grey
ggd1$segments$col[is.na(ggd1$segments$col)] <- 'grey50'

# set labels for below the plot
# hamming, lv
text_labels <- tribble(
  ~label, ~x, ~y,
  'Cluster 1', 4500, -70,
  '2', 11900, -70,
  '3', 13100, -70,
  'Cluster 4', 19000, -70
)
# OSA
# text_labels <- tribble(
#   ~label, ~x, ~y,
#   'Cluster 1', 1000, -70,
#   'Cluster 2', 7000, -70,
#   'Cluster 3', 17000, -70,
#   'Cluster 4', 24000, -70
# )
# 
# # LCS
# text_labels <- tribble(
#   ~label, ~x, ~y,
#   'Cluster 1', 4500, -90,
#   'Cluster 2', 13250, -90,
#   'Cluster 3', 19000, -90
# )

# plot the dendrogram
ggplot(ggd1$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = ggd1$segments$col,
               linetype = ggd1$segments$linetype, lwd = 0.6, alpha = 0.7) +
  geom_text(data = text_labels, aes(label = label, x = x, y = y), family = 'Helvetica') +
  coord_cartesian(ylim = c(100, 2500), clip = 'off') +
  # coord_cartesian(ylim = c(100, 4500), clip = 'off') +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  labs(title =paste0(distance_method_pretty, ' edit distance with Ward (D2) linkage'),
       subtitle = paste0("Weighted sample of ", 
                         scales::comma_format()(n_sample),
                         " respondents"),
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none')
save_plot(filename = paste0("Analyses/", distance_method_pretty, "/Plots/", distance_method_pretty, "_dendrogram"))


# examine the cluster sequences -------------------------------------------

# add cluster membership to df
atus_string_samp$cluster <- correct_clusters

# get the string to activity descriptions mapping
string_table <- read_csv("Analyses/Data/string_table.csv",
                         col_types = cols(
                           description = col_character(),
                           string = col_character()
                         ))

# convert df back to long with a row per activity
atus_samp <- atus_string_samp %>%
  separate(string, as.character(1:48), sep = 1:48) %>% 
  pivot_longer(cols = 2:49) %>%
  rename(string = value,
         period = name) %>% 
  left_join(string_table, by = 'string') %>% 
  na.omit() %>% 
  mutate(period = as.numeric(period),
         ID = as.factor(ID))

# need to shift time four hours and fix labels
labels <- as.character(seq(2, 24, by = 2))
labels <- c(labels[2:12], labels[1:2])
labels[1] <- "4am"

# sequence plots by cluster
atus_samp %>%
  mutate(cluster = if_else(cluster == 1, "Cluster 1", as.character(cluster)),
         cluster = factor(cluster, levels = c("Cluster 1", as.character(2:hcl_k)))) %>%
  group_by(ID) %>% 
  mutate(entropy = DescTools::Entropy(table(string))) %>%
  ungroup() %>% 
  ggplot(aes(x = period, y = reorder(ID, entropy), fill = description)) +
  geom_tile() +
  scale_y_discrete(labels = NULL) +
  scale_x_continuous(labels = labels,
                     breaks = seq(0, 48, by = 4)) +
  facet_wrap(~cluster, ncol = 2, scales = 'free_y') +
  labs(title = paste0(distance_method_pretty, ' edit distance and Ward D2 clustering'),
       subtitle = paste0("Each row represents an individual respondent's day"),
       x = "Time of day", 
       y = "Individuals") +
  theme(legend.position = 'right')
save_plot(filename = paste0("Analyses/", distance_method_pretty, "/Plots/", distance_method_pretty, "_sequence_plots"),
          height = 5,
          width = 8,
          dpi = 700)

# plots of proportion of individuals participating in a given activity for a given time period
atus_samp %>% 
  group_by(cluster, period, description) %>% 
  tally() %>%
  mutate(proportion = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(cluster = if_else(cluster == 1, "Cluster 1", as.character(cluster)),
         cluster = factor(cluster, levels = c("Cluster 1", as.character(2:hcl_k)))) %>% 
  ggplot(aes(x = period, y = proportion, fill = description)) +
  geom_area(position = 'stack', color = 'white', size = 0.3) +
  scale_x_continuous(labels = labels,
                     breaks = seq(0, 48, by = 4)) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~cluster, ncol = 2) +
  labs(title = paste0(distance_method_pretty, ' edit distance and Ward D2 clustering'),
       subtitle = "Observed proportion of activities by cluster",
       x = "Time of day",
       y = "Proportion") +
  theme(legend.position = 'right',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())
save_plot(filename = paste0("Analyses/", distance_method_pretty, "/Plots/", distance_method_pretty, "_proportion_plots"),
          height = 5,
          width = 8,
          dpi = 700)


# write out cluster membership --------------------------------------------

atus_string_samp %>% 
  select(ID, cluster) %>% 
  rename_with(function(x) paste0(distance_method, "_cluster"), .cols = cluster) %>% 
  write_csv(path = paste0("Analyses/", distance_method_pretty, "/", distance_method_pretty, "_clusters.csv"))

save(hclust_sw, dist_matrix, hcl_ward,
     file = paste0("Analyses/", distance_method_pretty, "/", distance_method_pretty, "_cluster_data.Rdata"))

