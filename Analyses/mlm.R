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


# plots -------------------------------------------------------------------

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

# plot the distribution by cluster and method
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
ggsave("Plots/sqrt_transform.png",
       device = "png",
       height = 5,
       width = 7)

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
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>% 
  ggplot(aes(x = year, y = alone_minutes)) +
  # geom_jitter(alpha = 0.01) +
  # geom_boxplot(aes(group = year)) +
  geom_smooth(method = 'glm.nb') +
  facet_grid(description ~ method) +
  scale_y_continuous(labels = scales::comma_format()) +
  # coord_cartesian(ylim = c(200, 450)) +
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

# fit the previously plotted negative binomial models individually and examine the coefficients
nb_models <- final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  group_by(method, cluster) %>% 
  nest() %>% 
  mutate(#model = map(data, function(df) MASS::glm.nb(alone_minutes ~ year, data = df)),
    model = map(data, function(df) glm(alone_minutes ~ year, data = df, family = quasipoisson(link = 'log'))),
    tidied = map(model, broom::tidy),
    confint = map(model, function(model){
      cf <- confint(model)
      tibble(lower = cf[2,1], upper = cf[2,2])
    })) %>% 
  unnest(c(tidied, confint)) %>% 
  dplyr::select(-data) %>% 
  ungroup()

# plot the estimate and std err per each model
nb_models %>% 
  filter(term == 'year') %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>% 
  mutate(description = factor(description, 
                              levels = c('Day workers', 'Night workers', 'Students', 'Uncategorized'))) %>%
  dplyr::select(method, description, estimate, lower, upper) %>% 
  # pivot_longer(cols = estimate) %>% 
  ggplot(aes(x = estimate, y = method, xmin = lower, xmax = upper, color = description)) +
  geom_point() +
  geom_linerange() + 
  facet_grid(description~., scales = 'free_x') +
  labs(title = "Quasi-poisson estimates and 95% confidence interval",
       subtitle = "15 models fitted individually by edit distance method and cluster membership",
       x = "\nAnnual change in time spent alone",
       y = NULL) +
  theme(legend.position = 'none',
        strip.text.y = element_text(size = 7))
save_plot("Plots/qp_estimates", height = 5.5, width = 6.5)

# these coefficients are interpreted as roughly a range of +1min to -3min per year depending on the cluster
# e.g. log(300) - log(299) = 0.003 which equals to exp(0.003) - 1 = 0.003004505 or 0.3% increase annually


# check the agreement among the clustering methods ------------------------

# percent of respondents that have full cluster agreement
clusters_df %>%
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>%
  mutate(method = sub(pattern = "*_.*", "", method)) %>%
  left_join(cluster_descriptions) %>%
  group_by(ID) %>%
  summarize(n_memberships = length(unique(description))) %>%
  ggplot(aes(x = n_memberships)) +
  geom_bar(aes(y = ..count.. / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Cluster agreement across methods",
       subtitle = "1 = total agreement across edit distance methods",
       x = "Count of unique cluster memberships",
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
  labs(title = "Cluster agreement across methods by respondent",
       subtitle = "Each row represents a respondent\nThe color represents the cluster they were assigned to under that method",
       x = 'Edit distance method',
       y = 'Respondents') +
  theme(legend.position = 'top')
save_plot("Plots/cluster_overlap", height = 6, dpi = 600)


# prep the data -----------------------------------------------------------

# split the data by cluster into individual dataframes
final_df %>% 
  mutate(year = year - min(year)) %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  # mutate(method = sub(pattern = "*_.*", "", method)) %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>%
  dplyr::select(ID, age, sex, state, alone_minutes, year, method, cluster = description) %>% 
  group_by(method) %>% 
  group_split() %>% 
  set_names(c("hamming_df", "lcs_df", "lv_df", "osa_df")) %>% 
  list2env(envir = .GlobalEnv)

# set which clustering method we're modeling
method_df <- hamming_df %>% dplyr::select(-method)

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


# fit the mlms ------------------------------------------------------------

# fit poisson and negative binomial
summary(glm(alone_minutes ~ year + cluster, data = method_df, family = "poisson"))
summary(glm(alone_minutes ~ year + cluster, data = method_df, family = quasipoisson(link = 'log')))
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
m2 <- update(mlm_nb, start = ss, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e8)))
#https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

mlm_linear <- lme4::lmer(alone_minutes ~ year + (year | cluster), data = method_df)
summary(mlm_linear)
anova(mlm_linear)


# fit neg binom -----------------------------------------------------------

mlm_nb <- lme4::glmer.nb(alone_minutes ~ year + (year | cluster), data = method_df, verbose = TRUE)
summary(mlm_nb)
anova(mlm_nb)
plot(mlm_nb)

broom.mixed::tidy(mlm_nb)
# broom.mixed::augment(mlm_nb)
broom.mixed::glance(mlm_nb)
coef(mlm_nb)

mlm_pois <- glmer(alone_minutes ~ year + (year | cluster), 
                  data = method_df, family=poisson(link="log"))
performance::check_overdispersion(mlm_pois)

# gelman hill 2006 pg115
y <- method_df$alone_minutes
n <- nrow(method_df) 
k <- 1
yhat <- predict(mlm_pois, type = "response")
z <- (y - yhat) / sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2) / (n - k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n - k), "\n")

# fit quasi poisson -------------------------------------------------------

mlm_qp <- MASS::glmmPQL(fixed = alone_minutes ~ year, random = ~ year | cluster, 
                        data = method_df, family = quasipoisson(link = 'log'))
summary(mlm_qp)
broom.mixed::tidy(mlm_qp)
broom.mixed::augment(mlm_qp)
broom.mixed::glance(mlm_qp)
coef(mlm_qp)

lme4::ranef(mlm_qp, condVar=T)
nlme::intervals(mlm_qp)
plot(ranef(mlm_qp))
plot(mlm_qp)
plot(density(residuals(mlm_qp)))
qqnorm(residuals(mlm_qp))
qqline(residuals(mlm_qp))
qqnorm(ranef(mlm_qp)[, 1])
qqline(ranef(mlm_qp)[, 1])
qqnorm(ranef(mlm_qp)[, 2])
qqline(ranef(mlm_qp)[, 2])

# ploting random effects --------------------------------------------------

# extract the var-cov matrices
# cov_var_matrices <- attr(ranef(mlm_nb, condVar = TRUE)[[1]], "postVar")

# extract the standard error of the random errors
# this is done through the conditional variance-covariance matrices 
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#confidence-intervals-on-conditional-meansblupsrandom-effects
standard_errors <- arm::se.ranef(mlm_nb)$cluster[,'year']
# broom.mixed::tidy(mlm_nb, effects = "ran_vals")


# plot the BLUPs and their SE
coef(mlm_nb)$cluster %>%  
  as.data.frame() %>% 
  rownames_to_column() %>% 
  # I believe this addition assumes independence of the conditional variance and fixed effect sampling variance
  mutate(lower = year - (1.96 * standard_errors),
         upper = year + (1.96 * standard_errors)) %>% 
  ggplot(aes(x = year, y = reorder(rowname, 4:1), 
             xmin = lower, xmax = upper, color = rowname)) +
  geom_point() +
  geom_linerange() +
  scale_x_continuous(limits = c(-0.025, 0.015)) +
  scale_y_discrete(position = "right") +
  labs(title = "Negative binomial estimates and 95% confidence interval",
       subtitle = "Multilevel model fitted on the Hamming clusters",
       x = "\nAnnual change in time spent alone",
       y = NULL) +
  theme(legend.position = 'none')
save_plot("Plots/hamming_negbin_mlm_effects", height = 3, width = 6.5)

# repeat for all clustering methods
mlm_models <- final_df %>% 
  mutate(year = year - min(year)) %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>%
  select(alone_minutes, year, method, cluster = description, age, sex) %>% 
  group_by(method) %>% 
  rownames_to_column('id') %>% # for overdispersion parameter
  nest() %>% 
  mutate(model = map(data, function(df){
    # lme4::glmer.nb(alone_minutes ~ year + (year | cluster), 
    #                data = df, verbose = TRUE, 
    #                # initCtrl = list(theta = 1.122),
    #                control = glmerControl(optCtrl = list(maxfun = 1e5)))
    # control = glmerControl(optimizer = "Nelder_Mead",
    #                        optCtrl = list(maxfun = 1e6)))
    # theta parameter pulled from first fitting model to the hamming group
    # this fixes convergence errors for the other models
    
    # lme4::glmer(alone_minutes ~ year + (year | cluster), data = df, family = "poisson")
    # lme4::glmer(alone_minutes ~ year + id + (year | cluster), data = df, family = 'poisson')
    MASS::glmmPQL(fixed = alone_minutes ~ year, random = ~ year | cluster,
                  data = df, family = quasipoisson(link = 'log'))
    # http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion
  }))


# dat <- final_df %>% 
#   mutate(year = year - min(year)) %>% 
#   pivot_longer(cols = contains('cluster'),
#                names_to = "method", values_to = "cluster") %>% 
#   mutate(method = sub(pattern = "*_.*", "", method),
#          cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
#   left_join(cluster_descriptions) %>%
#   select(alone_minutes, year, method, cluster = description) %>% 
#   filter(method == 'lcs')
# 
# tmp_lcs  <- lme4::glmer.nb(alone_minutes ~ year + (year | cluster), data = dat, verbose = TRUE)  
# 
# summary(tmp_lcs)
# anova(tmp_lcs)
# getME(tmp_lcs, 'theta')
# getME(tmp_lcs, 'lower')
# 
# # restart
# ss <- getME(nb_mlm_models$model[[1]], c("theta","fixef"))
# m2 <- update(tmp_lcs, start = ss, control = glmerControl(optimizer = "Nelder_Mead", tolPwrss = 1e-8,
#                                                          optCtrl = list()))

# calculate confidence interval and plot
mlm_models %>% 
  mutate(tidied = map(model, function(model){
    # extract the standard error of the random errors
    # standard_errors <- arm::se.ranef(model)$cluster[,'year']       
    
    # calculate the confidence interval
    # coef(model)$cluster %>%
    coef(model) %>% 
      as.data.frame() %>%
      rownames_to_column()# %>%
    # mutate(lower = year - (1.96 * standard_errors),
    #        upper = year + (1.96 * standard_errors))
  })) %>% 
  unnest(tidied) %>% 
  select(method, description = rowname, estimate = year) %>% #, lower, upper) %>% 
  ungroup() %>% 
  mutate(description = factor(description, 
                              levels = c('Day workers', 'Night workers', 'Students', 'Uncategorized'))) %>%
  ggplot(aes(x = estimate, y = method, color = description)) + #, xmin = lower, xmax = upper)) +
  geom_point() +
  # geom_linerange() + 
  scale_x_continuous(limits = c(-0.025, 0.015)) +
  facet_grid(description~., scales = 'free_x') +
  labs(title = "Quasi-Poisson estimates", # and 95% confidence interval",
       subtitle = "Four MLM models fitted individually by edit distance method ",
       x = "\nAnnual change in time spent alone",
       y = NULL) +
  theme(legend.position = 'none',
        strip.text.y = element_text(size = 7))
save_plot("Plots/qp_mlm_effects_all_methods", height = 5.5, width = 6.5)


#  plot the results: old method -------------------------------------------

# extract the fixed-effect slope
year_slope <- fixef(mlm_nb)['year']

# extract the random-effect slopes (this is the conditional mode)
cluster_slope <- ranef(mlm_nb)$cluster

# create a new column for the slope
cluster_slope$slope <- cluster_slope$year + year_slope

# use the row names to create a cluster name column
cluster_slope$cluster <- rownames(cluster_slope)

# plot the slopes
cluster_slope %>% 
  ggplot(aes(x = reorder(cluster, slope), y = slope, color = cluster)) +
  geom_point() +
  coord_flip() +
  labs(title = "MLM negative binomial estimates for `year`",
       # subtitle = "Cluster as random intercept and Year as fixed and random slope",
       # caption = "Data from American Time Use Survey 2003-2018",
       x = NULL,
       y = "\nAnnual change in time spent alone") +
  theme(legend.position = 'none')
# save_plot("Plots/hamming_negbin_mlm_effects", height = 3)



# bayesian ----------------------------------------------------------------

# https://mc-stan.org/users/documentation/case-studies/tutorial_rstanarm.html
library(rstanarm)

# stratified sample
# samp <- method_df %>% 
#   group_by(cluster) %>% 
#   # slice_sample(n = 1000) %>% 
#   ungroup()

# fit bayesian poisson model via MCMC with default priors
# takes >5 hours on the 25k observations
mlm_nb_bayes <- rstanarm::stan_glmer(formula = alone_minutes ~ year + (year | cluster),
                                     family = rstanarm::neg_binomial_2, data = method_df, 
                                     adapt_delta = 0.9, iter = 3000, seed = 44)
save(mlm_nb_bayes, file = "Analyses/bayesian_mlm_hamming.RData")
summary(mlm_nb_bayes, digits = 4)
prior_summary(mlm_nb_bayes)

# check convergence and effective sample size
plot(mlm_nb_bayes, "rhat")
plot(mlm_nb_bayes, "ess")

coef(mlm_nb_bayes)
ranef(mlm_nb_bayes)

# fixed plus random
# fixef(mlm_nb_bayes)['year'] + ranef(mlm_nb)$cluster$year

# pull credible interval of the effects
estimates <- summary(mlm_nb_bayes, 
                     regex_pars = c("year", "b\\[\\year *"),
                     probs = c(0.025, 0.50, 0.975),
                     digits = 4) %>% 
  as.data.frame() %>% 
  .[,c('mean', '2.5%', '97.5%')]

# add the slopes together and plot
# this assumes independence b/t the intercept and slope
(rbind(estimates['year',],
       estimates['year',],
       estimates['year',],
       estimates['year',]) +
    estimates[2:5,]) %>%
  as_tibble() %>% 
  mutate(description = factor(c('Day workers', 'Night workers', 'Students', 'Uncategorized'), 
                              levels = c('Day workers', 'Night workers', 'Students', 'Uncategorized'))) %>%
  ggplot(aes(x = mean, y = reorder(description, 4:1), color = description,
             xmin = `2.5%`, xmax = `97.5%`)) +
  geom_point() +
  geom_linerange() +
  scale_y_discrete(position = "right") +
  # scale_x_continuous(limits = c(-0.025, 0.015)) +
  # facet_grid(description~., scales = 'free_x') +
  labs(title = "Bayesian negative binomial estimates and 95% credible interval",
       subtitle = 'MLM models fitted on clusters determined by Hamming edit distance',
       x = "\nAnnual change in time spent alone",
       y = NULL) +
  theme(legend.position = 'none',
        strip.text.y = element_text(size = 7))
save_plot("Plots/bayes_nb_mlm_effects_hamming", height = 3, width = 6.5)




# fit linear mlm on transformed data --------------------------------------

# hypothesis test of poisson
gf_tests <- final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>%
  select(alone_minutes, year, method, cluster = description, age, sex) %>% 
  group_by(method, cluster) %>% 
  summarize(fit = list(vcd::goodfit(alone_minutes, type = "nbinomial")))

# plot(gf_tests$fit[[1]])
# summary(gf_tests$fit[[1]])

# all p vals == 2 so indicating not a neg binomial for any of the clusters
lapply(gf_tests$fit, summary)

# test sqrt transform for normality
# all reject the null of normality
final_df %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>% 
  mutate(alone_minutes = sqrt(alone_minutes)) %>% 
  group_by(method, description) %>%
  summarize(p_val = shapiro.test(sample(alone_minutes, 500))$p.val)

# fit linear mlms for all clustering methods
mlm_models <- final_df %>% 
  mutate(year = year - min(year)) %>% 
  pivot_longer(cols = contains('cluster'),
               names_to = "method", values_to = "cluster") %>% 
  mutate(method = sub(pattern = "*_.*", "", method),
         cluster = as.numeric(sub(pattern = ".+[a-z| ]", '', cluster))) %>% 
  left_join(cluster_descriptions) %>%
  select(alone_minutes, year, method, cluster = description, age, sex) %>% 
  mutate(alone_minutes = sqrt(alone_minutes)) %>% 
  group_by(method) %>% 
  nest() %>% 
  mutate(model = map(data, function(df){
    lme4::lmer(alone_minutes ~ year + (year | cluster), 
               data = df,
               control = lmerControl(optimizer = "bobyqa", 
                                     optCtrl = list(maxfun = 2e5)))
  }))

# test logLik
logLik(mlm_models$model[[1]])
mlm_models$model[[1]] %>% residuals() %>% density() %>% plot()
mlm_qp %>% residuals() %>% density() %>% plot()
logLik(mlm_qp)
logLik(mlm_nb)

y <- mlm_models$data[[1]]$alone_minutes^2

plot(y, predict(mlm_models$model[[1]])^2)
plot(y, predict(mlm_qp))

# residuals of niether are normally distributed
plot(density(predict(mlm_models$model[[1]])^2 - y))
plot(density(predict(mlm_qp) - y))

# mean of residuals
mean(predict(mlm_models$model[[1]])^2 - y)
mean(predict(mlm_qp) - y)


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
  select(method, description = rowname, estimate = year, lower, upper) %>% 
  ungroup() %>% 
  mutate(description = factor(description, 
                              levels = c('Day workers', 'Night workers', 'Students', 'Uncategorized'))) %>%
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
save_plot("Plots/linear_mlms", height = 5.5, width = 6.5)
