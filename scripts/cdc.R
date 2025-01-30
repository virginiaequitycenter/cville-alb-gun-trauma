# Script to pull and prep gun violence mortality counts and rates from the Centers 
# for Disease Control WONDER (Wide-ranging ONline Data for Epidemiologic Research) portal 
# Homepage: https://wonder.cdc.gov/
# Database: Underlying Cause of Death, 2018-2023, Single Race 
# Database URL: https://wonder.cdc.gov/ucd-icd10-expanded.html

# Note: this data cannot be programmatically downloaded via API, so each query must
# be constructed using their "build-a-table" feature and then downloaded as a CSV. 
# The criteria for constructing each dataset is outlined below. 

# Libraries
library(janitor)
library(tidyverse)

# Deaths by Intent ----
# Query criteria:
# - Database: Underlying Cause of Death, 2018-2023, Single Race Results
# - Group by: County, Injury Intent
# - Localities: 
#   - Albemarle County, Charlottesville City (local) 
#   - Albemarle, Charlottesville, Fluvanna, Nelson, Greene, Louisa (Blue Ridge Health District)
#   - Virginia 
# - ICD-10 Codes: U01.4 (Terrorism involving firearms); W32 (Handgun discharge); 
# W33 (Rifle, shotgun and larger firearm discharge); W34 (Discharge from other 
# and unspecified firearms); X72 (Intentional self-harm by handgun discharge); 
# X73 (Intentional self-harm by rifle, shotgun and larger firearm discharge); 
# X74 (Intentional self-harm by other and unspecified firearm discharge); 
# X93 (Assault by handgun discharge); X94 (Assault by rifle, shotgun and larger 
# firearm discharge); X95 (Assault by other and unspecified firearm discharge); 
# Y22 (Handgun discharge, undetermined intent); Y23 (Rifle, shotgun and larger 
# firearm discharge, undetermined intent); Y24 (Other and unspecified firearm 
# discharge, undetermined intent); Y35.0 (Legal intervention involving firearm discharge); 
# Y36.4 (War operations involving firearm discharge and other forms of conventional warfare)
# Show Zero Values:	True
# Show Suppressed:	True

# Albemarle & Charlottesville
deaths_intent_local <- read_tsv("data/raw/CDC - All Deaths by County, 2018-2023.txt") %>%
  clean_names() %>%
  select(-contains("code"), -notes) %>%
  rename(locality = county, pop_6yr = population) %>%
  mutate(date_range = "2018-2023",
         deaths = case_when(
           deaths == "Suppressed" ~ "<10",
           TRUE ~ deaths
         ))

deaths_intent_local <- deaths_intent_local[c(1:10),]

# Blue Ridge Health District
deaths_intent_district <- read_tsv("data/raw/CDC - All Deaths by District, 2018-2023.txt") %>%
  clean_names() %>%
  select(-contains("code"), -notes) %>%
  rename(locality = state, pop_6yr = population) %>%
  mutate(date_range = "2018-2023",
         locality = "Blue Ridge Health District",
         deaths = case_when(
           deaths == "Suppressed" ~ "<10",
           TRUE ~ deaths
         ))

deaths_intent_district <- deaths_intent_district[c(1:5),]

# Virginia
deaths_intent_va <- read_tsv("data/raw/CDC - All Deaths by State, 2018-2023.txt") %>%
  clean_names() %>%
  select(-contains("code"), -notes) %>%
  rename(locality = state, pop_6yr = population) %>%
  mutate(date_range = "2018-2023")

deaths_intent_va <- deaths_intent_va[c(1:5),]

# Combine 
deaths_intent <- rbind(deaths_intent_local, deaths_intent_district, deaths_intent_va)
write_csv(deaths_intent, "data/cdc_deaths_intent.csv")

# Suicide by Age & Gender ----
# URL: https://wonder.cdc.gov/ucd-icd10-expanded.html
# Query criteria:
# - Database: Underlying Cause of Death, 2018-2023, Single Race Results
# - Group By: State, Ten-Year Age Groups, Gender
# - Localities: 
#   - Albemarle County, Fluvanna County, Greene County, Louisa County, Nelson County, Charlottesville City (BRHD)
#   - Virginia
# - ICD-10 Codes:	X72 (Intentional self-harm by handgun discharge); X73 (Intentional 
# self-harm by rifle, shotgun and larger firearm discharge); X74 (Intentional self-harm 
# by other and unspecified firearm discharge)
# Show Zero Values:	True
# Show Suppressed:	True

# Note: due to suppression, the information is not available for the local (Charlottesville and Albemarle) 
# grouping. The smallest grouping with minimal suppression is the Blue Ridge Health District.

# Blue Ridge Health District 
# Note: 6 year population estimates not available for this grouping -- use census info 
suicide_age_brhd <- read_tsv("data/raw/CDC - Suicides by Age Gender District.txt") %>%
  clean_names() %>%
  select(-contains("code"), -notes, -state) %>%
  mutate(locality = "Blue Ridge Health District",
         date_range = "2018-2023",
         deaths = case_when(
           deaths == "Suppressed" ~ "<10",
           TRUE ~ deaths
         ))

suicide_age_brhd <- suicide_age_brhd[c(1:24),]

# Virginia
suicide_age_va <- read_tsv("data/raw/CDC - Suicides by Age Gender State.txt") %>%
  clean_names() %>%
  select(-contains("code"), -notes, -state) %>%
  rename(pop_6yr = population) %>%
  mutate(locality = "Virginia",
         date_range = "2018-2023",
         deaths = case_when(
           deaths == "Suppressed" ~ "<10",
           TRUE ~ deaths
         ))

suicide_age_va <- suicide_age_va[c(1:24),]

suicide_age <- bind_rows(suicide_age_brhd, suicide_age_va)

write.csv(suicide_age, "data/cdc_suicide_age.csv")

# Suicide by Race/Eth & Gender ----
# URL: https://wonder.cdc.gov/ucd-icd10-expanded.html
# Query criteria:
# - Database: Underlying Cause of Death, 2018-2023, Single Race Results
# - Group By: State; Single Race 6; Hispanic Origin; Gender
# - Localities: 
#   - Albemarle County, Fluvanna County, Greene County, Louisa County, Nelson County, Charlottesville City (BRHD)
#   - Virginia
# - ICD-10 Codes:	X72 (Intentional self-harm by handgun discharge); X73 (Intentional 
# self-harm by rifle, shotgun and larger firearm discharge); X74 (Intentional self-harm 
# by other and unspecified firearm discharge)
# - Show Zero Values:	True
# - Show Suppressed:	True

# Note: due to suppression, the information is not available for the local (Charlottesville and Albemarle) 
# grouping. The smallest grouping with minimal suppression is the Blue Ridge Health District.

# Blue Ridge Health District
suicide_race_brhd <- read_tsv("data/raw/CDC - Suicides by Race Eth Gender District.txt") %>%
  clean_names() %>%
  select(-contains("code"), -notes, -state) %>%
  rename(pop_6yr = population, race = single_race_6) %>%
  mutate(locality = "Blue Ridge Health District",
         date_range = "2018-2023",
         deaths = case_when(
           deaths == "Suppressed" ~ "<10",
           TRUE ~ deaths
         ))

suicide_race_brhd <- suicide_race_brhd[1:42,]

# Virginia
suicide_race_va <- read_tsv("data/raw/CDC - Suicides by Race Eth Gender Va.txt") %>%
  clean_names() %>%
  select(-contains("code"), -notes, -state) %>%
  rename(pop_6yr = population, race = single_race_6) %>%
  mutate(locality = "Virginia",
         date_range = "2018-2023",
         deaths = case_when(
           deaths == "Suppressed" ~ "<10",
           TRUE ~ deaths
         ))

suicide_race_va <- suicide_race_va[1:42,]

suicide_race <- rbind(suicide_race_va, suicide_race_brhd)

write_csv(suicide_race, "data/cdc_suicide_race.csv")
