# Setup ----
library(ggmap)
library(janitor)
library(jsonlite)
library(lubridate)
library(magrittr)
library(rvest)
library(tidyverse)

# Gun Violence Archive ----

# This data can be accessed by downloading the raw CSV directly from the GVA website 

gva_incidents <- read_csv("data/raw/gva_incidents.csv") %>%
  clean_names()

write_csv(gva_incidents, "data/gva_incidents.csv")

gva_participants <- read_csv("data/raw/gva_participants.csv") %>%
  clean_names() %>%
  select(-participant_name) %>%
  mutate(age = as.numeric(participant_age_group)) 

write_csv(gva_participants, "data/gva_participants.csv")

# TODO: age groups 

# Cville Open Data Portal ----

# *Crime Data ----
# This data can be accessed by the ODP REST API and then saved as a CSV

# There are 3 incident types that directly pertain to gun violence: "Shots Fired/Illegal Hunting", "Robbery - Armed", "Weapons Violations"

odp_where_clause <- "%28Offense%20LIKE%20'%shot%'%29OR%28Offense%20LIKE%20'%armed%'%29OR%28Offense%20LIKE%20'%weapon%'%29"
odp_api <- glue::glue("https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_2/MapServer/6/query?where={odp_where_clause}&outFields=*&outSR=4326&f=json")
odp_response <- fromJSON(odp_api)

odp_data <- odp_response$features$attributes %>% #~500 obs
  clean_names() %>%
  select(-hour_reported) %>%
  mutate(block_number = ifelse(block_number == "", NA, block_number),
         date_reported = date_reported %>% gsub('000$', '', .) %>% as.numeric() %>% as.POSIXct())

# Geocode 
odp_data %<>% mutate(address = paste(block_number, street_name, "Charlottesville VA"))
lon_lat <- geocode(odp_data$address)
sum(is.na(lon_lat$lon)) #0 
odp_data <- bind_cols(odp_data, lon_lat)

write_csv(odp_data, "data/odp_data.csv")

# *Arrest Data ----
# TODO: API Firearm, gun, shoot  

arrests <- read_csv("data/raw/arrests.csv") %>%
  clean_names() 

# De-Identify
arrests %<>%
  select(-name_suffix, -arrest_id) %>%
  filter(str_detect(statute_description, "FIREARM|GUN|SHOOT")) %>%
  mutate(arrest_datetime = ymd_hms(arrest_datetime))

ids <- arrests %>% 
  group_by(first_name, last_name) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(id = 1:n())

arrests <- left_join(arrests, ids, by = c("first_name", "last_name"))

arrests %<>% 
  select(-first_name, -last_name, -middle_name, -race, -n)

# Geocode 
arrests %<>% mutate(address = paste(house_number, street, "Charlottesville VA"))
lon_lat_arrests <- geocode(arrests$address)
sum(is.na(lon_lat_arrests$lon)) #0 
arrests <- bind_cols(arrests, lon_lat_arrests)

write_csv(arrests, "data/arrests.csv")

# VA Open Data Portal ----

# This data can be accessed by downloading the raw CSV directly from the VA portal website 

# Counties 
fai_county <- read_csv("data/raw/vdh-pud-fai-by-citycounty.csv") %>%
  clean_names()

fai_county %<>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric)

write_csv(fai_county, "data/fai_county.csv")

# Blue Ridge Health District
fai_age <- read_csv("data/raw/vdh-pud-firearm-deaths-by-district-age.csv") %>%
  clean_names() 

fai_age %<>%
  filter(str_detect(health_district, "Blue Ridge")) 

write_csv(fai_age, "data/fai_age.csv")


