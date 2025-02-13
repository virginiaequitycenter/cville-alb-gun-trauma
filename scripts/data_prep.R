# Script to pull and prep Charlottesville and Albemarle gun violence data 
# Last updated: 11/20/2024

# Setup ----
library(janitor)
library(jsonlite)
library(lubridate)
library(magrittr)
library(readxl)
library(sf)
library(stringr)
library(tidycensus)
library(tidyverse)

# Cville Open Data Portal ----
# This data can be accessed by the ODP REST API and then saved as a CSV, or downloaded directly from the ODP website: https://opendata.charlottesville.org/

# *Crime Data ----
# Download url: https://opendata.charlottesville.org/datasets/charlottesville::crime-data/about
# There are 3 incident types that directly pertain to gun violence: "Shots Fired/Illegal Hunting", "Robbery - Armed", "Weapons Violations"

# Access via API:
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

# Search and download directly from ODP website:
# Download url: https://opendata.charlottesville.org/datasets/crime-data/explore?filters=eyJPZmZlbnNlIjpbIlNob3RzIEZpcmVkL0lsbGVnYWwgSHVudGluZyIsIlJvYmJlcnkgLSBBcm1lZCIsIldlYXBvbnMgVmlvbGF0aW9ucyJdfQ%3D%3D
# Filter criteria:
# - Offense = "Shots Fired/Illegal Hunting", "Robbery - Armed", and "Weapons Violations"
# Repeat cleaning process and save (lines 56-67)

# *Arrest Data ----
# This dataset can also be accessed by the ODP REST API or downloaded directly from the website: https://opendata.charlottesville.org/
# About dataset: https://opendata.charlottesville.org/datasets/charlottesville::arrests/about

# Access via API:
arrests_clause <- "%28StatuteDescription%20LIKE%20'%firearm%'%29OR%28StatuteDescription%20LIKE%20'%gun%'%29OR%28StatuteDescription%20LIKE%20'%weapon%'%29OR%28StatuteDescription%20LIKE%20'%shoot%'%29"
arrests_api <- glue::glue("https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_2/MapServer/22/query?where={arrests_clause}outFields=*&outSR=4326&f=json")
arrests_response <- fromJSON(arrests_api)

# Search and download directly from ODP website:
# Download url: https://opendata.charlottesville.org/datasets/charlottesville::arrests/explore
# Filter criteria:
# - Statute description includes FIREARM, GUN, WEAPON, or SHOOT 

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



# Police ----

# This data includes an anonymized collection of all gun violence incidents recorded by the Charlottesville 
# Police Department (CPD), the Albemarle County Police Department (ACPD), and the UVA Police Department (UPD). 

# The data was collected from their SQL database by the ACPD crime analyst and shared with the Equity Center in June 2024
# Block numbers are used instead of specific addresses to preserve anonymity while also providing geographic information

# All incidents of gun violence from 2019 - 2024
all_gv <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD_BLOCK ADDRESS.xlsx") %>%
  clean_names()

all_gv <- all_gv %>%
  mutate(locality = case_when(
    agency == "ACPD" ~ "Albemarle County",
    TRUE ~ "Charlottesville"))

# Filter to only include verified shots fired (Shots Fired, 1, 2, 3), murder (09A), Active Shooter, and aggravated assaults (13A)
gv <- all_gv %>%
  filter(str_detect(crime_code, "Shots|09A|13A|Active Shooter"),
         !str_detect(verified, "^UN")) %>%
  mutate(description = case_when(
    crime_code == "13A" ~ "Aggravated Assault",
    crime_code == "09A" ~ "Murder",
    crime_code == "Active Shooter 1" ~ "Active Shooter",
    TRUE ~ "Shots Fired"))

# Geocode
gv <- gv %>%
  mutate(address = paste(block_address, locality))
#lon_lat <- geocode(gv$address)
sum(is.na(lon_lat$lon)) #0 
gv <- bind_cols(gv, lon_lat)

write_csv(gv, "data/regional_gv.csv")

# Participant demographics - perhaps for another analysis 
# case_dems <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD.xlsx", sheet = "Demographic Data for Cases") %>%
#   clean_names()

