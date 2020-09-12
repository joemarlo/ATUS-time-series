library(MASS)
library(tidyverse)
library(lme4)
source("Plots/ggplot_settings.R")
options(mc.cores = parallel::detectCores())
set.seed(44)
options(scipen = 999)

select <- function(...){
  # MASS also has a select() function so this prioritizes dplyr's select()
  dplyr::select(...)
}

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

# check sample size per method per cluster per year
final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  group_by(method, cluster, year) %>% 
  tally() %>% 
  ggplot(aes(x = year, y = n, color = cluster)) +
  geom_line() +
  facet_wrap(~method) +
  labs(title = "Sample size per cluster per year",
       x = "Year",
       y = "n")

# plot the mean alone time per cluster and method
# final_df %>%
#   pivot_longer(cols = contains('cluster'),
#                names_to = "method", values_to = "cluster") %>% 
#   group_by(method, year, cluster) %>% 
#   summarize(mean_alone_time = mean(alone_minutes),
#             .groups = 'drop') %>% 
#   ggplot(aes(x = year, y = mean_alone_time, color = cluster, group = cluster)) +
#   geom_line(alpha = 0.5) +
#   geom_smooth() +
#   facet_wrap(~method) +
#   labs(title = "Data shows differing patterns in mean time spent alone across clusters",
#        subtitle = paste0("Sample of ", 
#                        scales::comma_format()(nrow(final_df)),
#                        " respondents"),
#      caption = "American Time Use Survey 2003-2018",
#      x = "Year",
#      y = "Mean minutes per day")

# plot a negative binomial model on respondent level data
final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  ggplot(aes(x = year, y = alone_minutes, color = cluster, group = cluster)) +
  geom_jitter(alpha = 0.01) +
  geom_smooth(method = 'glm.nb') +
  facet_wrap(~method) +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_cartesian(ylim = c(200, 450)) +
  labs(title = "Negative binomial models show differing patterns across clusters",
       subtitle = paste0("Weighted sample of ", 
                         scales::comma_format()(nrow(final_df)),
                         " respondents"),
       caption = "American Time Use Survey 2003-2018",
       x = "Year",
       y = "Minutes alone per day")
ggsave(filename = "Plots/negative_binomial.png",
       device = "png",
       height = 7,
       width = 7)

# fit the previously plotted negative binomial models individually and examine the coeficients
nb_models <- final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  group_by(method, cluster) %>% 
  nest() %>% 
  mutate(model = map(data, function(df) MASS::glm.nb(alone_minutes ~ year, data = df)),
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied) %>% 
  select(-model, -data) %>% 
  ungroup()

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
        

# plot the estimate and std err per each model
nb_models %>% 
  filter(term == 'year') %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  # group = paste0(method, "-", cluster)) %>%
  left_join(cluster_descriptions) %>% 
  mutate(description = factor(description, 
                              levels = c('9-5 workers', 'Night workers', 'Students', 'Uncategorized'))) %>% 
  select(method, description, estimate, std.error) %>% 
  pivot_longer(cols = -c('method', 'description')) %>% 
  ggplot(aes(x = value, y = method, color = description)) +
  geom_point() +
  facet_grid(description~name, scales = 'free_x') +
  labs(title = "Negative binomial estimates for `year`",
       subtitle = "Models fitted individually by edit distance method and cluster membership",
       x = "\nValue {Annual change in minutes spent alone}",
       y = NULL) +
  theme(legend.position = 'none',
        strip.text.y = element_text(size = 7))
save_plot("Plots/negative_binomial_estimates", height = 5.5)

# these coefficients are interpreted as roughly a range of +1min to -3min per year depending on the cluster
# e.g. log(300) - log(299) = 0.003 which equals to exp(0.003) - 1 = 0.003004505 or 0.3% increase annually

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
  ggplot(aes(x = alone_minutes)) +
  geom_density() +
  facet_grid(description ~ method, scales = 'free_y')


# check the agreement among the clustering methods ------------------------

# percent of respondents that have full cluster agreement
clusters_df %>%
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>%
  mutate(method = sub(pattern = "*_.*", "", method)) %>%
  left_join(cluster_descriptions) %>%
  group_by(ID) %>%
  summarize(n_memberships = length(table(description))) %>%
  ggplot(aes(x = n_memberships)) +
  geom_bar(aes(y = ..count.. / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Percent of respondents who have X cluster memberships",
       subtitle = "1 = total agreement across the clustering methods",
       x = "Number of unique cluster memberships",
       y = "Percent of respondents")
save_plot("Plots/cluster_agreement", height = 6)
  
# overlap of cluster membership
clusters_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method)) %>% 
  left_join(cluster_descriptions) %>%
  arrange(ID, method, description) %>% 
  group_by(ID) %>% 
  mutate(entropy = DescTools::Entropy(table(description)),
         most_common = names(rev(sort(table(description))))[[1]]) %>% 
  ungroup() %>% 
  arrange(most_common, entropy) %>% 
  mutate(order = row_number()) %>% 
  ggplot(aes(x = method, y = reorder(as.factor(ID), order), fill = description)) +
  geom_tile() +
  scale_y_discrete(labels = NULL) +
  labs(title = "Cluster agreement across methods",
       subtitle = "Each row represents a respondent\nThe color represents the cluster they were assigned to under that clustering method",
       x = 'Clustering method',
       y = 'Respondents') +
  theme(legend.position = 'top')
save_plot("Plots/cluster_overlap", height = 6, dpi = 600)


# fit the mlms ------------------------------------------------------------

# split the data by cluster into individual dataframes
final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  # mutate(method = sub(pattern = "*_.*", "", method)) %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>%
  select(ID, age, sex, state, alone_minutes, year, method, cluster = description) %>% 
  group_by(method) %>% 
  group_split() %>% 
  set_names(c("hamming_df", "lcs_df", "lv_df", "osa_df")) %>% 
  list2env(envir = .GlobalEnv)

# set which clustering method we're modeling
method_df <- hamming_df %>% dplyr::select(-method)

# scale the year variable
method_df$year <- method_df$year - min(method_df$year)

# there's an issue here with duplicate observations
# alone_time is not continuous, its discrete
# this ideally would be TRUE
# method_df %>% 
#   select(alone_minutes, cluster, year) %>% 
#   n_distinct() == nrow(method_df)
# method_df %>% 
#   ggplot(aes(x = alone_minutes)) +
#   geom_histogram(binwidth = 1)
# jitter the data to overcome singularities?
# method_df$alone_minutes <- pmax(0, jitter(method_df$alone_minutes))

# or is there a problem because its zero-inflated ?
# hist(method_df$alone_minutes)
# mean(method_df$alone_minutes == 0)
# zip_model <- pscl::zeroinfl(formula = alone_minutes ~ year + cluster | age,
#                       data = method_df)
# summary(zip_model)

# fit poisson and negative binomial
summary(glm(alone_minutes ~ year + cluster, data = method_df, family = "poisson"))
summary(MASS::glm.nb(alone_minutes ~ year + cluster, data = method_df))

# recode min minutes are difference over 2003 ???
# how would this be done given there's not universal baseline

# cluster as random-effect intercept and Year as both fixed and random effect slope
mlm_poisson <- lme4::glmer(alone_minutes ~ year + (year | cluster), data = method_df, family = "poisson")
summary(mlm_poisson)
anova(mlm_poisson)

mlm_nb <- lme4::glmer.nb(alone_minutes ~ year + (year | cluster), data = method_df, verbose = TRUE)
summary(mlm_nb)
anova(mlm_nb)
getME(mlm_nb, 'theta')
getME(mlm_nb, 'lower')

# restart
ss <- getME(mlm_nb, c("theta","fixef"))
m2 <- update(mlm_nb, start = ss, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e8)))
#https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

mlm_linear <- lme4::lmer(alone_minutes ~ year + (year | cluster), data = method_df)
summary(mlm_linear)
anova(mlm_linear)


# fit neg binom -----------------------------------------------------------

mlm_nb <- lme4::glmer.nb(alone_minutes ~ year + (year | cluster), data = method_df, verbose = TRUE)
summary(mlm_nb)
anova(mlm_nb)

broom.mixed::tidy(mlm_nb)
# broom.mixed::augment(mlm_nb)
broom.mixed::glance(mlm_nb)


# plot the results --------------------------------------------------------

# extract the fixed-effect slope
year_slope <- fixef(mlm_nb)['year']

# extract the random-effect slopes
cluster_slope <- ranef(mlm_nb)$cluster

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
  labs(title = "MLM negative binomial estimates for `year`",
       # subtitle = "Cluster as random intercept and Year as fixed and random slope",
       # caption = "Data from American Time Use Survey 2003-2018",
       x = NULL,
       y = "\nAnnual change in minutes spent alone")
save_plot("Plots/hamming_negbin_mlm_effects", height = 3)
