library(tidyverse)
set.seed(44)

# read in activities
atus <- read_delim(file = "Inputs/atus.tsv", 
                   delim = "\t", 
                   escape_double = FALSE, 
                   col_types = cols(ID = col_double(), 
                                    period = col_number()), 
                   trim_ws = TRUE)

# read in the demographics data
demographics <- read_delim(file = "Inputs/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)


# filtering ---------------------------------------------------------------

# filter to only include respondents who logged on weekdays and non holidays
IDs <- demographics %>% 
  filter(day_of_week %in% 2:6,
         holiday == 0) %>% 
  pull(ID)

# filter the atus df to include only these respondents
atus_filtered <- atus[atus$ID %in% IDs,]
rm(atus, IDs)


# string conversion -------------------------------------------------------

# create table that matches description to letter
string_table <- distinct(atus_filtered, description) %>% 
  mutate(string = LETTERS[1:(nrow(.))])

# write out table for future use
write_csv(string_table, "Analyses/Data/string_table.csv")

# turn each ID's periods into a single string representing the 48 30 minute
# periods of their day
atus_string <- atus_filtered %>% 
  left_join(string_table, by = 'description') %>% 
  group_by(ID) %>% 
  summarize(string = paste0(string, collapse = ""),
            .groups = 'drop')


# sampling ----------------------------------------------------------------

# data needs to be sampled b/c 200k respondents creates too large a distance
# matrix for 48gb of memory

# sample nrows from the dataset using the survey weights
n_sample <- 10000
weights <- demographics %>% right_join(atus_string, by = "ID") %>% pull(survey_weight)
atus_string_samp <- sample_n(atus_string, size = n_sample, weight = weights, replace = FALSE)

# write out sample
write_csv(atus_string_samp, "Analyses/Data/sample_10k.csv")

# increase sample size
n_sample <- 25000
atus_string_samp <- sample_n(atus_string, size = n_sample, weight = weights, replace = FALSE)

# write out sample
write_csv(atus_string_samp, "Analyses/Data/sample_25k.csv")
