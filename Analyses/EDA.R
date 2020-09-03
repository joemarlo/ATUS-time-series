library(tidyverse)
source("Plots/ggplot_settings.R")

# read in the demographics data
demographics_df <- read_delim(file = "Data/demographic.tsv",
                              delim = "\t",
                              escape_double = FALSE,
                              trim_ws = TRUE)


# EDA to justify alone time changing --------------------------------------

plot(density(demographics_df$TRTALONE))

# average time spent alone over time
demographics_df %>%
  group_by(year) %>% 
  summarize("Alone time excluding work" = mean(TRTALONE),
            "All alone time" = mean(TRTALONE_WK)) %>% 
  mutate(`All alone time` = ifelse(year <= 2009, NA, `All alone time`)) %>% 
  pivot_longer(cols = c("Alone time excluding work", "All alone time")) %>% 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line() +
  labs(title = "Time spent alone has been increasing steadily",
       caption = "American Time Use Survey 2003-2018",
       x = "Year",
       y = "Mean minutes per day")
ggsave(filename = "Plots/mean_alone_time.svg",
       device = "svg",
       height = 5,
       width = 7)

# weighted mean minutes
demographics_df %>%
  group_by(year) %>% 
  summarize("Alone time excluding work" = sum(survey_weight * TRTALONE) / sum(survey_weight),
            "All alone time" = sum(survey_weight * TRTALONE_WK) / sum(survey_weight)) %>% 
  mutate(`All alone time` = ifelse(year <= 2009, NA, `All alone time`)) %>% 
  pivot_longer(cols = c("Alone time excluding work", "All alone time")) %>% 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line() +
  labs(title = "Time spent alone has been increasing steadily",
       caption = "American Time Use Survey 2003-2018",
       x = "Year",
       y = "Mean minutes per day")



# glmer -------------------------------------------------------------------

# plot a poisson fit on year means
demographics_df %>% 
  group_by(year) %>% 
  summarize(alone_time = mean(TRTALONE),
            .groups = 'drop') %>% 
  ggplot(aes(x = year, y = alone_time)) +
  geom_line() +
  geom_smooth(method = 'glm',
              method.args = c("poisson"))

# plot a poisson fit on respondent level data
demographics_df %>% 
  ggplot(aes(x = year, y = TRTALONE)) +
  geom_jitter(alpha = 0.01) +
  geom_smooth(method = 'glm',
              method.args = c("poisson"))

# fit a poisson model
glm_model <- glm(TRTALONE ~ year + weekday, data = demographics_df %>% mutate(weekday = day_of_week %in% 2:6), family = "poisson")
summary(glm_model)

# fit a negative binomial model
nb_model <- MASS::glm.nb(TRTALONE ~ year + weekday, data = demographics_df %>% mutate(weekday = day_of_week %in% 2:6))
summary(nb_model)


# Add a Poisson trend line for each cluster
# ggplot(aes(x = Year, y = TotalIncidents, group = Cluster)) +
#   geom_line() +
#   geom_smooth(method = 'glm', 
#               method.args = c("poisson"), 
#               se = FALSE)
# 
# glm(Time spent alone ~ Year + Cluster + weekday, data = hate, family = "poisson")

# Cluster as random-effect intercept and Year as both fixed and random effect slope
# need to scale Year first
# Year_scaled <- Year - min(Year)
# lmer4::glmer(Time spent alone ~ Year + weekday + (Year | Cluster), data = hate, family = "poisson")

# what other variables? Age?



# displaying results of model ---------------------------------------------

# glmer_out is the model

# # Extract out the fixed-effect slope for Year2
# Year2_slope <- fixef(glmer_out)['Year2']
# 
# # Extract out the random-effect slopes for county
# county_slope <- ranef(glmer_out)$County
# 
# # Create a new column for the slope
# county_slope$slope <- county_slope$Year2 + Year2_slope
# 
# # Use the row names to create a county name column
# county_slope$county <- rownames(county_slope)
# 
# # Create an ordered county-level factor based upon slope values
# county_slope$county_plot <- factor(county_slope$county, 
#                                    levels = county_slope$county[order(county_slope$slope)])
# 
# # Now plot the results using ggplot2
# ggplot(data = county_slope, aes(x = county_plot, y = slope)) + 
#   geom_point() +
#   coord_flip() + 
#   theme_bw() + 
#   ylab("Change in hate crimes per year")  +
#   xlab("County")




