
# Setup ----

library(ggrepel)
library(janitor)
library(reactable)
library(tidyverse)

# Downloaded from https://schoolquality.virginia.gov/download-data
cville <- read_csv("data/absenteeism/cville.csv", skip = 2) %>%
  janitor::clean_names()

alb <- read_csv("data/absenteeism/albemarle.csv", skip = 2) %>%
  janitor::clean_names()

chronic <- bind_rows(cville, alb) %>%
  mutate_at(c(6:9), ~as.numeric(.) %>% replace_na(0)) %>%
  mutate(school_level = gsub(".* ", "", school) %>% 
           gsub("Elem$", "Elementary", .) %>%
           factor(levels = c("Elementary", "Middle", "High")))

# Table ----
chronic23 <- chronic %>%
  filter(subgroup == "All Students",
         year == "2022 - 2023") %>%
  select(year, division, school, count_above_10, percent_above_10) %>%
  mutate(division = case_when(
    division == "Charlottesville City Public Schools" ~ "CCS",
    TRUE ~ "ACS")) %>%
  rename("Number of Chronically Absent Students" = count_above_10,
         "Percent of Student Population" = percent_above_10)

write_csv(chronic23, "data/absenteeism/chronic23.csv")

# Visualizations ----
all_students <- chronic %>% 
  filter(subgroup == "All Students") %>%
  select(-level)

ccs_average <- filter(all_students, grepl("Charlottesville", division)) %>%
  group_by(year) %>%
  summarise(percent_above_10 = mean(percent_above_10))

acs_average <- filter(all_students, grepl("Albemarle", division)) %>%
  group_by(year) %>%
  summarise(percent_above_10 = mean(percent_above_10))

# Facet - ugly
ggplot(all_students, aes(x = year, y = percent_above_10, group = school, color = school_level)) +
  geom_smooth() +
  geom_point() +
  geom_text(aes(label = paste0(round(percent_above_10), "%"), hjust = 1, vjust = 1.5)) +
  facet_grid( ~ division)

# CCS of interest
ccs <- all_students %>%
  filter(school %in% c("Johnson Elementary", "Venable Elementary", 
                       "Walker Upper Elementary", "Buford Middle", 
                       "Charlottesville High")) %>%
  mutate(short = case_when(
    school == "Buford Middle" ~ "Buford",
    school == "Walker Upper Elementary" ~ "Walker",
    school == "Charlottesville High" ~ "CHS",
    school == "Johnson Elementary" ~ "Johnson",
    school == "Venable Elementary" ~ "Venable"
  ))

ccs_labels <- ccs %>%
  filter(year == "2022 - 2023")


ggplot(ccs, aes(x = year, y = percent_above_10, group = school)) +
  geom_smooth(data = ccs_average, aes(color = "District Average", group = 1), size = 2) +
  geom_smooth() +
  geom_point() +
  geom_text(aes(label = paste0(round(percent_above_10), "%"), hjust = 1, vjust = 1.5)) +
  geom_label(data = ccs_labels, aes(label = short), hjust = -.1) +
  scale_color_manual(values = "grey") +
  labs(color = NULL,
       y = "Rate of Chronic Absenteeism",
       x = "School Year",
       title = "Charlottesville Schools of Interest")


# Demographics 

all_avg <- all_students %>%
  group_by(year, division) %>%
  summarise(percent_above_10 = mean(percent_above_10))

demo_means <- chronic %>% 
  group_by(division, subgroup, year) %>%
  summarise(percent_above_10 = mean(percent_above_10))

demo_labels <- demo_means %>%
  filter(year == "2022 - 2023") %>%
  mutate(short = case_when(
    subgroup == "Economically Disadvantaged" ~ "Economically\nDisadvantaged",
    subgroup == "Students with Disabilities" ~ "Students with\nDisabilities",
    TRUE ~ subgroup))

ggplot(demo_means, aes(x = year, y = percent_above_10, group = subgroup)) +
  geom_smooth() +
  geom_point() +
  geom_text(aes(label = paste0(round(percent_above_10), "%"), hjust = 1, vjust = 1.5), size = 3)  +
  geom_label(data = demo_labels, aes(label = short), hjust = -.1, size = 2.5) +
  facet_wrap(~division) +
  labs(title = "Student Demographics",
       x = "School Year",
       y = "Rate of Chronic Absenteeism")



  
