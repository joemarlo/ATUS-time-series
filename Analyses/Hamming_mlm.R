library(tidyverse)
library(MASS)
library(lme4)
source("Plots/ggplot_settings.R")
options(mc.cores = parallel::detectCores())
set.seed(44)
options(scipen = 999)

# read in the cluster membership
hamming_clusters <- read_csv(file = "Analyses/Hamming_clusters.csv", 
                             col_types = cols(cluster = col_integer()))

n_clusters <- length(unique(hamming_clusters$cluster)) 

# read in the demographics data
demographics <- read_delim(file = "Data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)

# create dataframe of variables of interest
final_df <- hamming_clusters %>% 
  left_join(demographics, by = "ID") %>% 
  select(ID, cluster, age, sex, state, alone_minutes = TRTALONE, year) %>% 
  mutate(cluster = as.factor(cluster))

# check sample size per cluster per year
final_df %>% 
  group_by(cluster, year) %>% 
  tally() %>% 
  mutate(cluster = if_else(cluster == 1, "Cluster 1", as.character(cluster)),
         cluster = factor(cluster, levels = c("Cluster 1", as.character(2:n_clusters)))) %>% 
  ggplot(aes(x = year, y = n, color = cluster)) +
  geom_line() +
  labs(title = "Sample size per cluster per year",
       x = "Year",
       y = "n")

# plot the mean alone time per cluster
final_df %>%
  group_by(year, cluster) %>% 
  summarize(mean_alone_time = mean(alone_minutes),
            .groups = 'drop') %>% 
  mutate(cluster = if_else(cluster == 1, "Cluster 1", as.character(cluster)),
         cluster = factor(cluster, levels = c("Cluster 1", as.character(2:n_clusters)))) %>% 
  ggplot(aes(x = year, y = mean_alone_time, color = cluster, group = cluster)) +
  geom_line() +
  geom_smooth()
  labs(title = "Data shows differing patterns in mean time spent alone across clusters",
       subtitle = paste0("Sample of ", 
                         scales::comma_format()(nrow(final_df)),
                         " respondents"),
       caption = "American Time Use Survey 2003-2018",
       x = "Year",
       y = "Mean minutes per day")

# plot a negative binomial model on respondent level data
final_df %>% 
  mutate(cluster = if_else(cluster == 1, "Cluster 1", as.character(cluster)),
         cluster = factor(cluster, levels = c("Cluster 1", as.character(2:n_clusters)))) %>% 
  ggplot(aes(x = year, y = alone_minutes, color = cluster, group = cluster)) +
  geom_jitter(alpha = 0.01) +
  geom_smooth(method = 'glm.nb') +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Negative binomial models show differing patterns across clusters",
       subtitle = paste0("Sample of ", 
                         scales::comma_format()(nrow(final_df)),
                         " respondents"),
       caption = "American Time Use Survey 2003-2018",
       x = "Year",
       y = "Minutes alone per day")
ggsave(filename = "Plots/hamming_nb.png",
       device = "png",
       height = 5,
       width = 7)


# fit the mlms ------------------------------------------------------------

# scale the year variable
final_df$year <- final_df$year - min(final_df$year)

# fit poisson and negative binomial
summary(glm(alone_minutes ~ year + cluster, data = final_df, family = "poisson"))
summary(MASS::glm.nb(alone_minutes ~ year + cluster, data = final_df))

# recode min minutes are difference over 2003 ???
# how would this be done given there's not universal baseline

# cluster as random-effect intercept and Year as both fixed and random effect slope
mlm_poisson <- lme4::glmer(alone_minutes ~ year + (year | cluster), data = final_df, family = "poisson")
summary(mlm_poisson)
anova(mlm_poisson)

mlm_nb <- lme4::glmer.nb(alone_minutes ~ year + (year | cluster), data = final_df, verbose = TRUE)
summary(mlm_nb)
anova(mlm_nb)

mlm_linear <- lme4::lmer(alone_minutes ~ year + (year | cluster), data = final_df)
summary(mlm_linear)
anova(mlm_linear)

# all three models seem to agree that the difference among the clusters in nominal


# plot the results --------------------------------------------------------

# extract the fixed-effect slope
year_slope <- fixef(mlm_poisson)['year']

# extract the random-effect slopes
cluster_slope <- ranef(mlm_poisson)$cluster

# create a new column for the slope
cluster_slope$slope <- cluster_slope$year + year_slope

# use the row names to create a cluster name column
cluster_slope$cluster <- rownames(cluster_slope)

# create an ordered cluster-level factor based upon slope values
cluster_slope$cluster_ordered <- factor(cluster_slope$cluster,
                                   levels = cluster_slope$cluster[order(cluster_slope$slope)])

# plot the slopes
cluster_slope %>% 
  ggplot(aes(x = cluster_ordered, y = slope)) +
  geom_point() +
  coord_flip() +
  labs(title = "Time alone has been slowly increasing except in cluster 2: students",
       subtitle = "Poisson MLM with cluster as random intercept and Year as fixed and random slope",
       caption = "Data from American Time Use Survey 2003-2018",
       x = "Cluster membership",
       y = "Yearly change in daily minutes spent alone")
ggsave(filename = "Plots/hamming_poisson_effects.png",
       device = "png",
       height = 5,
       width = 7)
