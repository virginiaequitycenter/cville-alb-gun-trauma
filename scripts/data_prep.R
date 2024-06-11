# Setup ----
library(ggmap)
library(janitor)
library(jsonlite)
library(lubridate)
library(magrittr)
library(tidycensus)
library(tidyverse)

# Gun Violence Archive ----

# This data can be accessed by downloading the raw CSV directly from the GVA website: https://www.gunviolencearchive.org/

gva_incidents <- read_csv("data/raw/gva_incidents.csv") %>%
  clean_names()

write_csv(gva_incidents, "data/gva_incidents.csv")

gva_participants <- read_csv("data/raw/gva_participants.csv") %>%
  clean_names() %>%
  select(-participant_name) %>%
  mutate(age = as.numeric(participant_age_group)) 

write_csv(gva_participants, "data/gva_participants.csv")

# Cville Open Data Portal ----

# *Crime Data ----

# This data can be accessed by the ODP REST API and then saved as a CSV, or downloaded directly from the ODP website: https://opendata.charlottesville.org/
# Date range: 2019-04-20 through 2024-04-12

# There are 3 incident types that directly pertain to gun violence: "Shots Fired/Illegal Hunting", "Robbery - Armed", "Weapons Violations"

odp_where_clause <- "%28Offense%20LIKE%20'%shot%'%29OR%28Offense%20LIKE%20'%armed%'%29OR%28Offense%20LIKE%20'%weapon%'%29"
odp_api <- glue::glue("https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_2/MapServer/6/query?where={odp_where_clause}&outFields=*&outSR=4326&f=json")
odp_response <- fromJSON(odp_api)

odp_crimes <- odp_response$features$attributes %>% #~500 obs
  clean_names() %>%
  select(-hour_reported) %>%
  mutate(block_number = ifelse(block_number == "", NA, block_number),
         date_reported = date_reported %>% gsub('000$', '', .) %>% as.numeric() %>% as.POSIXct())

# Geocode 
odp_crimes %<>% mutate(address = paste(block_number, street_name, "Charlottesville VA"))
lon_lat <- geocode(odp_crimes$address)
sum(is.na(lon_lat$lon)) #0 
odp_data <- bind_cols(odp_crimes, lon_lat)

write_csv(odp_crimes, "data/odp_crimes.csv")

# *Arrest Data ----

odp_arrests <- read_csv("data/raw/arrests.csv") %>%
  clean_names()

# De-Identify
odp_arrests <- odp_arrests %>%
  select(-name_suffix, -arrest_id) %>%
  filter(str_detect(statute_description, "FIREARM|GUN|SHOOT")) %>%
  mutate(arrest_datetime = ymd_hms(arrest_datetime))

ids <- odp_arrests %>% 
  group_by(first_name, last_name) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(id = 1:n())

odp_arrests <- left_join(odp_arrests, ids, by = c("first_name", "last_name"))

odp_arrests %<>% 
  select(-first_name, -last_name, -middle_name, -race, -n)

# Geocode 
odp_arrests %<>% mutate(address = paste(house_number, street, "Charlottesville VA"))
# lon_lat_arrests <- geocode(odp_arrests$address)
sum(is.na(lon_lat_arrests$lon)) #0 
odp_arrests <- bind_cols(odp_arrests, lon_lat_arrests)

odp_arrests <- read_csv("data/odp_arrests.csv")

# Organize into violent vs. nonviolent gun-related arrests - this takes a bit of assumption
odp_arrests <- odp_arrests %>%
  mutate(grp = case_when(str_detect(statute_description, "POINTING|RECKLESS|SHOOT|DISCHARGE|ASSAULT") ~ "violent",
                         TRUE ~ "nonviolent"))

write_csv(odp_arrests, "data/odp_arrests.csv")

# VA Open Data Portal ----

# This data can be accessed by downloading the raw CSV directly from the VA portal website: https://data.virginia.gov/

# Firearm injuries by county 
fai_county <- read_csv("data/raw/vdh-pud-fai-by-citycounty.csv") %>%
  clean_names()

fai_county %<>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric)

write_csv(fai_county, "data/fai_county.csv")

# Blue Ridge Health District - this is the smallest regional dataset that includes ages 
fai_age <- read_csv("data/raw/vdh-pud-firearm-deaths-by-district-age.csv") %>%
  clean_names() 

fai_age %<>%
  filter(str_detect(health_district, "Blue Ridge")) 

write_csv(fai_age, "data/fai_age.csv")

# FBI Data Explorer ----

# This data can be accessed using the FBI Crime Data Explorer build-a-table website: https://va.beyond2020.com/

# *Incidents ----
ucr <- read_csv("data/raw/ucr_firearm.csv") %>% 
  select(-matches("rate"))

pops <- ucr %>% 
  select(year, district, est_pop_district) %>% 
  distinct()

ucr <- ucr %>%
  group_by(year, type, district) %>%
  mutate(n_total = sum(n_juvenile, n_adult, n_unknown, n_missing, na.rm = TRUE))

write_csv(ucr, "data/ucr_firearm.csv")
write_csv(pops, "data/ucr_pops.csv")

# *Theft ----

# This data is surprisingly already tidy and properly formatted so we don't have to do anything :)

nibrs_theft <- read_csv("data/raw/nibrs_theft.csv")
write_csv(nibrs_theft, "data/nibrs_theft.csv")

# ATF ----

# This data can be downloaded directly from the ATF site: https://www.atf.gov/firearms/listing-federal-firearms-licensees

# *Dealers ----
atf <- readxl::read_excel("data/raw/0424-ffl-list-virginia.xlsx") %>%
  clean_names() %>% 
  mutate_at(vars(matches('lic_')), ~ as.numeric(.))

local_dealers <- atf %>%
  filter(lic_cnty == 003 | lic_cnty == 540) %>%
  rename(license_type = lic_type) %>%
  mutate(across(where(is.character), str_to_title),
         business_name = replace_na(business_name, "No Business Name Provided"),
         business_type = case_when(str_detect(business_name, "No Business Name Provided") ~ "No Business Name Provided",
                         TRUE ~ "Business"))

# Geocode 
local_dealers %<>% mutate(address = paste(premise_street, premise_city, premise_state))
lon_lat_dealers <- geocode(local_dealers$address)
sum(is.na(lon_lat_dealers$lon)) #0 
local_dealers <- bind_cols(local_dealers, lon_lat_dealers)

local_dealers %<>% select(license_type, license_name, business_name, premise_street, premise_city, lon, lat, business_type)

write_csv(local_dealers, "data/atf_dealers.csv")
