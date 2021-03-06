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

# cluster descriptions based on proportion plots
cluster_descriptions <- tribble(~method, ~cluster, ~description,
                                'hamming', 1, 'Day workers',
                                'hamming', 2, 'Night workers',
                                'hamming', 3, 'Students',
                                'hamming', 4, 'Uncategorized',
                                'lcs', 1, 'Day workers',
                                'lcs', 2, 'Students',
                                'lcs', 3, 'Uncategorized',
                                'lv', 1, 'Day workers',
                                'lv', 2, 'Night workers',
                                'lv', 3, 'Students',
                                'lv', 4, 'Uncategorized',
                                'osa', 1, 'Students',
                                'osa', 2, 'Uncategorized',
                                'osa', 3, 'Day workers',
                                'osa', 4, 'Night workers')

# create dataframe of variables of interest; factorized the cluster variables
final_df <- clusters_df %>% 
  left_join(demographics, by = "ID") %>% 
  dplyr::select(ID, contains("cluster"), age, sex, state, alone_minutes = TRTALONE, year) %>% 
  # mutate(across(contains("cluster"), ~ {
  #   x <- if_else(.x == 1, "Cluster 1", as.character(.x))
  #   x <- factor(x, levels = c("Cluster 1", as.character(2:4))) 
  # }))
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method)) %>% 
  left_join(cluster_descriptions) 

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
  # group_by(method, description) %>% 
  # summarize(mean = mean(alone_minutes),
  #           var = var(alone_minutes)) %>% mutate(ratio = var / mean)
  ggplot(aes(x = alone_minutes, color = method, fill = method)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~ description, scales = 'free_y', nrow = 2) +
  labs(title = "Distribution of alone time by cluster and edit distance",
       x = "Daily minutes alone",
       y = NULL)
save_plot("Plots/alone_time_densities", height = 5, width = 7)


# age densities
final_df %>% 
  select(age, method, description) %>%
  ggplot(aes(x = age, color = method, fill = method)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~ description, scales = 'free_y', nrow = 2) +
  labs(title = "Distribution of age by cluster and edit distance method",
       x = "Age",
       y = NULL)
save_plot("Plots/age_densities", height = 5, width = 7)

# sex difference
final_df %>% 
  select(sex, method, description) %>%
  group_by(method, description) %>% 
  summarize(Male = mean(sex == 1),
            Female = mean(sex == 2),
            .groups = 'drop') %>% 
  pivot_longer(cols = c("Male", "Female")) %>% 
  ggplot(aes(x = name, y = value, color = method, fill = method)) +
  geom_col(position = 'dodge', color = 'white') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 0.6, by = 0.2)) +
  facet_wrap(~ description, nrow = 2) +
  labs(title = "Sex split by cluster and edit distance method",
       x = NULL,
       y = NULL)
save_plot("Plots/sex_bars", height = 5, width = 7)

# map
cluster_props <- final_df %>% 
  filter(method == 'hamming') %>% 
  group_by(description) %>% 
  summarize(national_prop = n() / nrow(.),
            .groups = 'drop')
final_df %>% 
  filter(method == 'hamming') %>% 
  group_by(state) %>% 
  mutate(state_n = n()) %>% 
  group_by(state, description) %>% 
  summarize(state_prop = n() / state_n,
            .groups = 'drop') %>% 
  distinct() %>% 
  left_join(cluster_props, by = 'description') %>% 
  mutate(ratio = state_prop / national_prop) %>% 
  group_by(state) %>% 
  filter(ratio == max(ratio)) %>% 
  na.omit() %>% 
  fuzzyjoin::stringdist_left_join(map_data("state"), by = c(state = 'region')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = description), color = 'white') +
  coord_map(projection = "albers", lat0 = 38, lat1 = 45,
            xlim = c(-120, -75)) +
  labs(title = 'Most over represented cluster by state',
       subtitle = 'I.e. most frequent cluster compared to national average') +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = 'right')
save_plot("Plots/clusters_by_state")
