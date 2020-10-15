library(tidyverse)
library(TraMineR)
library(fastcluster)
library(NbClust)
source("Plots/ggplot_settings.R")
set.seed(44)
# data(mvad)
# seqstatl(mvad[, 17:86])
# 
# 
# mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school", 
#                    "training")
# mvad.labels <- c("employment", "further education", "higher education", 
#                  "joblessness", "school", "training")
# mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
# mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphabet, states = mvad.scodes, 
#                    labels = mvad.labels, xtstep = 6)
# 
# dev.off()
# par(mfrow = c(2, 2))
# 
# # plot first 10 sequences
# seqiplot(mvad.seq, with.legend = FALSE, border = NA)
# # plot all sequences sorted 
# seqIplot(mvad.seq, sortv = "from.start", with.legend = FALSE)
# # plot t10 most frequent
# seqfplot(mvad.seq, with.legend = FALSE, border = NA)
# # plot legend
# seqlegend(mvad.seq)



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
n_sample <- 5000
atus_sampled <- atus_raw %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  right_join(IDs, by = "ID") %>% 
  slice_sample(n = n_sample, weight_by = survey_weight) %>% 
  dplyr::select(-survey_weight)

# define alphabet as all unique states
alphabet <- atus_sampled[,-1] %>% unlist() %>% unique() %>% sort()
# states <- c("CHH")
labels <- c("Care_HH", "Care_NHH", "Cons_Purch", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Pers_care", "Pro_Pers_Care", "Rel_spirit", 
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
seqdplot(atus_seq, sortv = "from.start", with.legend = "right", border = NA)
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
dist_om1 <- seqdist(atus_seq, method = "OM", indel = 1, sm = "TRATE")

# cluster
clusterward1 <- hclust(as.dist(dist_om1), method = "ward.D2")
dev.off()
plot(clusterward1)

# get optimal cluster sizes by calculating silhouette width
hclust_sw <- NbClust(
  data = NULL,
  diss = as.dist(dist_om1),
  distance = NULL,
  method = 'ward.D2',
  max.nc = 20,
  min.nc = 2,
  index = 'silhouette'
)
hclust_sw$All.index %>% 
  enframe() %>% 
  mutate(name = as.numeric(name)) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_line(color = 'grey30') +
  geom_area(alpha = 0.4) +
  geom_point(color = 'grey30') +
  scale_x_continuous(breaks = 2:20) +
  scale_y_continuous(limits = c(0, 0.3)) +
  labs(title = "TraMineR",
       subtitle = paste0("Weighted sample of ", 
                         scales::comma_format()(n_sample),
                         " respondents"),
       x = 'n clusters',
       y = 'Silhouette width')

cluster_5 <- cutree(clusterward1, k = 5)
cluster_5 <- factor(cluster_5, labels = paste("Cluster", 1:5))

seqIplot(atus_seq, group = cluster_5, sortv = "from.start")
seqdplot(atus_seq, group = cluster_5, sortv = "from.start", border = NA)
seqHtplot(atus_seq, group = cluster_5, main = "(b) entropy index")
seqmsplot(atus_seq, group = cluster_5, border = NA, main = "(c) mean time")
seqmtplot(atus_seq, group = cluster_5, main = "(d) modal state seq.")

# density of turbulence by cluster
seqST(atus_seq) %>% 
  cbind(cluster_5) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Turbulence)) +
  geom_density() +
  facet_wrap(~cluster_5)
