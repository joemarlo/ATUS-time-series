library(tidyverse)
source("Plots/ggplot_settings.R")

# read in the demographics data
demographics_df <- read_delim(file = "Inputs/demographic.tsv",
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


# alone time by state -----------------------------------------------------

demographics_df %>% 
  group_by(state) %>% 
  summarize("Alone time excluding work" = sum(survey_weight * TRTALONE) / sum(survey_weight),
            # "All alone time" = sum(survey_weight * TRTALONE_WK) / sum(survey_weight),
            .groups = 'drop') %>% 
  na.omit() %>% 
  fuzzyjoin::stringdist_left_join(map_data("state"), by = c(state = 'region')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Alone time excluding work`), color = 'white') +
  coord_map(projection = "albers", lat0 = 38, lat1 = 45,
            xlim = c(-120, -75)) +
  scale_fill_gradient(low = '#b5e6ce', high = '#030f09',
                      name = 'Minutes') +
  labs(title = 'Mean daily alone time',
       subtitle = 'Excludes alone time associated with working or sleeping',
       caption = '2003-2018 American Time Use Survey') +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(hjust = 0),
        legend.position = 'right')
ggsave(filename = "Plots/alone_time_by_state.png",
       device = "png",
       height = 7,
       width = 9)


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

