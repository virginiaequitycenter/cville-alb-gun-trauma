# Exploring School Climate Data

library(janitor)
library(magrittr)
library(tidyverse)

school_scrubber <- . %>%
  mutate(school_level = gsub(".* ", "", school_name) %>% 
           gsub("Elem$", "Elementary", .) %>% factor(levels = c("Elementary", "Middle", "High")),
         school_level = case_when(
           (school_name == "Community Lab School" | 
              school_name == "Albemarle County Community Public Charter" |
              school_name == "Walker Upper Elementary") ~ "Middle", 
           TRUE ~ school_level),
         division = case_when(
           grepl("Charlottesville", division_name) ~ "CCS",
           TRUE ~ "ACPS"),
         school_short = word(school_name , 1  , -2),
         school_short = case_when(
           school_short == "Albemarle County Community Public" ~ "ACCP",
           school_short == "Albemarle" ~ "AHS",
           school_short == "Benjamin F. Yancey" ~ "Yancey",
           school_short == "Jack Jouett" ~ "Journey",
           school_short == "Jackson P. Burley" ~ "Burley",
           school_short == "Joseph T. Henley" ~ "Henley",
           school_short == "Leslie H. Walton" ~ "Walton",
           school_short == "Mary Carr Greer" ~ "Greer",
           school_short == "Monticello" ~ "MHS",
           school_short == "Mortimer Y. Sutherland" ~ "Lakeside",
           school_short == "Paul H. Cale" ~ "Mountain View",
           school_short == "Virginia L. Murray" ~ "Va Murray",
           school_short == "Western Albemarle" ~ "WAHS",
           school_short == "Charlottesville" ~ "CHS",
           school_short == "Walker Upper" ~ "Walker",
           school_short == "Murray" ~ "Community Lab",
           TRUE ~ school_short))

# Student populations ----
# total number of students per school per year
totals <- read_csv("school_climates/fall_membership_statistics.csv") %>%
  clean_names() %>%
  select(-division_number, division = division_name)

# total grades 7-12 (for dropout stats)
total_7to12 <- totals %>%
  select(school_year, school_name, gr_7:gr_12) %>%
  gather(key = "key", value = "value", -school_year, -school_name) %>%
  group_by(school_year, school_name) %>%
  summarise(total_7to12 = sum(value, na.rm = TRUE)) %>%
  left_join(distinct(select(totals, school_name)))

# Dropout rates ----
# total number of students that dropped out by year by school gr. 7-12
dropout <- read_csv("school_climates/annual_dropout_statistics.csv") %>%
  clean_names() 

# join with student totals
dropout <- dropout %>%
  left_join(total_7to12, by = c("school_year" = "school_year", "school_name" = "school_name"))

# clean school names
dropout <- dropout %>%
  school_scrubber()

# TODO: Calculate ratios


# Short term suspensions ----
# per https://law.lis.virginia.gov/vacodefull/title22.1/chapter14/article3/: 
# "Short-term suspension" means any disciplinary action whereby a student is not 
# permitted to attend school for a period not to exceed 10 school days.

sts <- read_csv("school_climates/short_term_suspensions.csv", skip = 2) %>%
  clean_names() %>%
  replace(is.na(.), 0) %>%
  rename(school_name = division, division = school, school_year = year, 
         percent_sts_school = percent_of_short_term_suspensions) %>%
  mutate(school_year = gsub(" ", "", school_year)) %>%
  filter(division %in% c("Albemarle County Public Schools",
                        "Charlottesville City Public Schools"))
# get sts totals
sts_totals <- sts %>%
  group_by(school_year, school_name) %>%
  summarise(sts_totals = sum(number_suspended_short_term))

# join with student pops
sts_totals <- sts_totals %>%
  left_join(select(totals, school_year, school_name, total_count), 
            by = c("school_year" = "school_year", "school_name" = "school_name"))

# clean names

sts_totals <- sts_totals %>%
  school_scrubber()

# visualize 

# long term suspensions ----
# per https://law.lis.virginia.gov/vacodefull/title22.1/chapter14/article3/: 
# "Long-term suspension" means any disciplinary action whereby a student is not 
# permitted to attend school for 11 to 45 school days.
lts <- read_csv("school_climates/long_term_suspensions.csv", skip = 2) %>%
  clean_names() %>%
  rename(school_name = division, division = school,
         percent_lts_school = percent_of_long_term_suspensions) %>%
  filter(division %in% c("Albemarle County Public Schools",
                         "Charlottesville City Public Schools"))

# expulsions
expel <- read_csv("school_climates/expulsions.csv", skip = 2) %>%
  clean_names() %>%
  rename(school_name = division, division = school, 
         percent_expelled_school = percent_of_expelled) %>%
  filter(division %in% c("Albemarle County Public Schools",
                         "Charlottesville City Public Schools"))





