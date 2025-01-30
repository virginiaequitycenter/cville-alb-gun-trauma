# Script to pull and prep gun violence data from the Virginia Department of Health 
# via the Virginia Open Data Portal
# Homepage: https://data.virginia.gov/

# Libraries 
library(httr)
library(janitor)
library(jsonlite)
library(tidyverse)

# Injuries by County ----
# This dataset includes the number and rate of emergency department (ED) visits 
# for firearm injury (FAI) in Charlottesville and Albemarle by year. 
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-fai-by-citycounty

# Access via CKAN API:
injury_county_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=f79c0a7d-6b41-4b9e-936b-dfc1cfc197ec&q=charlottesville"
injury_county_page <- GET(injury_county_url)
#status_code(injury_county_page)
injury_county_list <- fromJSON(injury_county_url)
injury_county <- injury_county_list$result$records

injury_county <- injury_county %>%
  clean_names() %>%
  select(-id, -rank) %>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric)

write_csv(injury_county, "data/vdh_injury_county.csv")

# Injuries by District ----
# This dataset includes the number and rate of emergency department (ED) visits 
# for firearm injury (FAI) in the Blue Ridge Health District by year. 
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-fai-by-district

# Access via CKAN API:
injury_district_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=e7f3f1b8-c036-4061-a499-ec7aee58951b&q=blue"
injury_district_page <- GET(injury_district_url)
#status_code(injury_district_page)
injury_district_list <- fromJSON(injury_district_url)
injury_district <- injury_district_list$result$records

injury_district <- injury_district %>%
  clean_names() %>%
  select(-id, -patient_health_region, -rank) %>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric)

write_csv(injury_district, "data/vdh_injury_district.csv")

# Injuries by Demographic ----
# This dataset includes the number and rate of emergency department (ED) visits 
# for firearm injury (FAI) in Virginia by year, race/ethnicity, age group, and sex.
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-fai-by-demographics

# Access via CKAN API:
injury_demo_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=03f9bbce-abc3-49fe-9034-4b9316c6cf24"
injury_demo_page <- GET(injury_district_url)
#status_code(injury_demo_page)
injury_demo_list <- fromJSON(injury_demo_url)
injury_demo <- injury_demo_list$result$records

injury_demo <- injury_demo %>%
  clean_names() %>%
  select(-id) %>%
  mutate(locality = "Virginia") %>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric)

write_csv(injury_demo, "data/vdh_injury_demographics.csv")

# Deaths by Intent by District ----
# This dataset includes the number and rate of firearm-related deaths in the
# Blue Ridge Health District (Charlottesville, Albemarle, Fluvanna, Nelson, Greene, 
# and Louisa) by intent between 2018-2022. 
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-intent

# Access via CKAN API:
deaths_intent_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=b09626e9-3fbd-4ff5-bcba-763cf0659cc1&q=blue"
deaths_intent_page <- GET(deaths_intent_url)
#status_code(deaths_intent_page)
deaths_intent_list <- fromJSON(deaths_intent_url)
deaths_intent <- deaths_intent_list$result$records

deaths_intent <- deaths_intent %>%
  clean_names() %>%
  select(-id, -rank) %>%
  mutate(years = "2018-2022") %>%
  mutate_at(vars(contains("deaths")), as.numeric)

write_csv(deaths_intent, "data/vdh_deaths_intent.csv")

# Deaths by Race by District ----
# This dataset includes the number and rate of firearm-related deaths in the
# Blue Ridge Health District (Charlottesville, Albemarle, Fluvanna, Nelson, Greene, 
# and Louisa) by race between 2018-2022.
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-race

# Access via CKAN API:
deaths_race_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=b410bb43-346f-42c2-8b6b-1a25b9269a86&q=blue"
deaths_race_page <- GET(deaths_race_url)
#status_code(deaths_race_page)
deaths_race_list <- fromJSON(deaths_race_url)
deaths_race <- deaths_race_list$result$records

deaths_race <- deaths_race %>%
  clean_names() %>%
  select(-id, -rank) %>%
  mutate(years = "2018-2022") %>%
  mutate_at(vars(contains("deaths")), as.numeric)

write_csv(deaths_race, "data/vdh_deaths_race.csv")  

# Deaths by Age by District ----
# This dataset includes the number and rate of firearm-related deaths in the
# Blue Ridge Health District (Charlottesville, Albemarle, Fluvanna, Nelson, Greene, 
# and Louisa) by age between 2018-2022.
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-age

# Access via CKAN API: 
deaths_age_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=48fd049f-2dd3-4232-acc2-aeec14a9c087&q=blue"
deaths_age_page <- GET(deaths_age_url)  
#status_code(deaths_age_page)
deaths_age_list <- fromJSON(deaths_age_url)
deaths_age <- deaths_age_list$result$records

deaths_age <- deaths_age %>%
  clean_names() %>%
  select(-id, -rank) %>%
  mutate(years = "2018-2022") %>%
  mutate_at(vars(contains("deaths")), as.numeric) %>%
  arrange(age_group)

write_csv(deaths_age, "data/vdh_deaths_age.csv")

# Deaths by Sex by District ----
# This dataset includes the number and rate of firearm-related deaths in the
# Blue Ridge Health District (Charlottesville, Albemarle, Fluvanna, Nelson, Greene, 
# and Louisa) by sex between 2018-2022.
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-sex

# Access via CKAN API: 
deaths_sex_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=8638ad22-167e-477d-96c7-17de6e9ccb78&q=blue"
deaths_sex_page <- GET(deaths_sex_url)
#status_code(deaths_sex_page)
deaths_sex_list <- fromJSON(deaths_sex_url)
deaths_sex <- deaths_sex_list$result$records

deaths_sex <- deaths_sex %>%
  clean_names() %>%
  select(-id, -rank) %>%
  mutate(years = "2018-2022") %>%
  mutate_at(vars(contains("deaths")), as.numeric)

write_csv(deaths_sex, "data/vdh_deaths_sex.csv")
