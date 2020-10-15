library(MASS)
library(tidyverse)
library(lme4)
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
hamming_clusters <- read_csv(file = "Analyses/Hamming/Hamming_5_clusters.csv", 
                             col_types = cols(hamming_cluster = col_integer()))
levenshtein_clusters <- read_csv(file = "Analyses/Levenshtein/Levenshtein_5_clusters.csv", 
                                 col_types = cols(lv_cluster = col_integer()))


# join all the clusters dfs together and remove duplicates (due to weighted sampling)
clusters_df <- left_join(hamming_clusters, levenshtein_clusters, by = 'ID')
rm(hamming_clusters, levenshtein_clusters)

# create dataframe of variables of interest; factorized the cluster variables
final_df <- clusters_df %>% 
  left_join(demographics, by = "ID") %>% 
  dplyr::select(ID, contains("cluster"), age, sex, state, alone_minutes = TRTALONE, year) %>% 
  mutate(across(contains("cluster"), ~ {
    x <- if_else(.x == 1, "Cluster 1", as.character(.x))
    x <- factor(x, levels = c("Cluster 1", as.character(2:5))) 
  }))

# cluster descriptions based on proportion plots
cluster_descriptions <- tribble(~method, ~cluster, ~description,
                                'hamming', 1, 'Day workers',
                                'hamming', 2, 'Day workers - late',
                                'hamming', 3, 'Students',
                                'hamming', 4, 'Night workers',
                                'hamming', 5, 'Uncategorized',
                                'lv', 1, 'Students',
                                'lv', 2, 'Householders',
                                'lv', 3, "Leisure'ers",
                                'lv', 4, 'Day workers',
                                'lv', 5, 'Night workers')

final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>% 
  mutate(alone_minutes = sqrt(alone_minutes)) %>% 
  ggplot(aes(x = alone_minutes, color = method)) +
  geom_density() +
  facet_wrap(~description)



# fit linear models -------------------------------------------------------

# fit linear mlms for all clustering methods
mlm_models <- final_df %>% 
  mutate(year = year - min(year)) %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>%
  dplyr::select(alone_minutes, year, method, cluster = description, age, sex) %>% 
  mutate(alone_minutes = sqrt(alone_minutes)) %>% 
  group_by(method) %>% 
  nest() %>% 
  mutate(model = map(data, function(df){
    lme4::lmer(alone_minutes ~ year + (year | cluster), 
               data = df,
               control = lmerControl(optimizer = "bobyqa", 
                                     optCtrl = list(maxfun = 2e5)))
  }))

# calculate confidence interval and plot
mlm_models %>% 
  mutate(tidied = map(model, function(model){
    # extract the standard error of the random errors
    standard_errors <- arm::se.ranef(model)$cluster[,'year']
    
    # calculate the confidence interval
    coef(model)$cluster %>% 
      as.data.frame() %>%
      rownames_to_column() %>%
      mutate(lower = year - (1.96 * standard_errors),
             upper = year + (1.96 * standard_errors))
  })) %>% 
  unnest(tidied) %>% 
  dplyr::select(method, description = rowname, estimate = year, lower, upper) %>% 
  ungroup() %>% 
  mutate(description = factor(description, 
                              levels = c('Day workers', 'Day workers - late', 'Night workers', 'Students', "Leisure'ers", 'Householders', 'Uncategorized'))) %>%
  ggplot(aes(x = estimate, y = method, color = description, xmin = lower, xmax = upper)) +
  geom_point() +
  geom_linerange() +
  # scale_x_continuous(limits = c(-0.025, 0.015)) +
  facet_grid(description~., scales = 'free_x') +
  labs(title = "Estimates from linear MLMs", # and 95% confidence interval",
       subtitle = "Four MLM models fitted individually by edit distance method ",
       x = "\nAnnual change in sqrt(time spent alone)",
       y = NULL) +
  theme(legend.position = 'none',
        strip.text.y = element_text(size = 7))

