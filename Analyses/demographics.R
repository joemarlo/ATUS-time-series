library(tidyverse)
source("Plots/ggplot_settings.R")
options(mc.cores = parallel::detectCores())
set.seed(44)
options(scipen = 999)


# read in the demographics data
demographics <- read_delim(file = "Inputs/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)

# read in the cluster membership
hamming_clusters <- read_csv(file = "Analyses/Hamming/Hamming_clusters.csv", 
                             col_types = cols(hamming_cluster = col_integer()))
lcs_clusters <- read_csv(file = "Analyses/LCS/LCS_clusters.csv", 
                         col_types = cols(lcs_cluster = col_integer()))
levenshtein_clusters <- read_csv(file = "Analyses/Levenshtein/Levenshtein_clusters.csv", 
                                 col_types = cols(lv_cluster = col_integer()))
osa_clusters <- read_csv(file = "Analyses/OSA/OSA_clusters.csv", 
                         col_types = cols(osa_cluster = col_integer()))

# join all the clusters dfs together and remove duplicates (due to weighted sampling)
clusters_df <- Reduce(
  x = list(hamming_clusters, lcs_clusters, levenshtein_clusters, osa_clusters),
  f = function(x, y) left_join(x, y, by = 'ID')
)
rm(hamming_clusters, lcs_clusters, levenshtein_clusters, osa_clusters)


# create dataframe of variables of interest; factorized the cluster variables
final_df <- clusters_df %>% 
  left_join(demographics, by = "ID") %>% 
  dplyr::select(ID, contains("cluster"), age, sex, state, alone_minutes = TRTALONE, year) %>% 
  mutate(across(contains("cluster"), ~ {
    x <- if_else(.x == 1, "Cluster 1", as.character(.x))
    x <- factor(x, levels = c("Cluster 1", as.character(2:4))) 
  }))


# cluster descriptions based on proportion plots
cluster_descriptions <- tribble(~method, ~cluster, ~description,
                                'hamming', 1, '9-5 workers',
                                'hamming', 2, 'Night workers',
                                'hamming', 3, 'Students',
                                'hamming', 4, 'Uncategorized',
                                'lcs', 1, '9-5 workers',
                                'lcs', 2, 'Students',
                                'lcs', 3, 'Uncategorized',
                                'lv', 1, '9-5 workers',
                                'lv', 2, 'Night workers',
                                'lv', 3, 'Students',
                                'lv', 4, 'Uncategorized',
                                'osa', 1, 'Students',
                                'osa', 2, 'Uncategorized',
                                'osa', 3, '9-5 workers',
                                'osa', 4, 'Night workers')

# distribution of alone time by cluster
clusters_df %>% 
  left_join(demographics, by = "ID") %>% 
  select(ID, contains("cluster"), age, sex, state, race, alone_minutes = TRTALONE, year, student = TESCHFT) %>% 
  mutate(student = recode(student,
                          '-1' = "No",
                          '1' = 'Full time',
                          '2' = 'Part time')) %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method)) %>% 
  left_join(cluster_descriptions) %>% 
  filter(alone_minutes != -1) %>% 
  ggplot(aes(x = alone_minutes, color = method, fill = method)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(labels = scales::comma_format()) +
  facet_wrap(~ description, scales = 'free_y', nrow = 2) +
  labs(title = "Distribution of alone time by cluster and edit distance",
       x = "Daily minutes alone",
       y = NULL) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot("Plots/alone_time_densities", height = 6)
