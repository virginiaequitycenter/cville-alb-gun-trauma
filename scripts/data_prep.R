# Script to pull and prep Cville and Albemarle gun violence data 
# Last updated: 8/19/2024

# Setup ----
library(ggmap)
library(janitor)
library(jsonlite)
library(lubridate)
library(magrittr)
library(readxl)
library(sf)
library(stringr)
library(tidycensus)
library(tidyverse)

# Gun Violence Archive ----
# This data can be accessed by downloading the raw CSVs from their Search Database page: https://www.gunviolencearchive.org/query

# *Incidents ----
# Download url: https://www.gunviolencearchive.org/query/3d0414ea-ee7e-49fa-8bcb-c97a46761f96
# Search criteria:
# - Location is in Virginia
# - City = Charlottesville

gva_incidents <- read_csv("data/raw/gva_incidents.csv") %>%
  clean_names()

write_csv(gva_incidents, "data/gva_incidents.csv")

# *Participants ----
# Download url: https://www.gunviolencearchive.org/query/45701b8a-ca83-4eb8-b605-2dd2c49e3b42
# Search criteria:
# - Location is in Virginia
# - City = Charlottesville

gva_participants <- read_csv("data/raw/gva_participants.csv") %>%
  clean_names() %>%
  select(-participant_name) %>%
  mutate(age = as.numeric(participant_age_group)) 

write_csv(gva_participants, "data/gva_participants.csv")

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

# VA Open Data Portal ----
# This data can be accessed by downloading the raw CSVs directly from the VA portal website: https://data.virginia.gov/

# *Injuries by county ---- 
# Download url: https://data.virginia.gov/dataset/vdh-pud-fai-by-citycounty

fai_county <- read_csv("data/raw/vdh-pud-fai-by-citycounty.csv") %>%
  clean_names()

fai_county %<>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric)

write_csv(fai_county, "data/fai_county.csv")

# *Injuries by intent ---- 
# Download url: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-intent

fai_intent <- read_csv("data/raw/vdh-pud-firearm-deaths-by-district-intent.csv") %>%
  clean_names() %>%
  filter(str_detect(patient_health_district, "Blue Ridge"))

write_csv(fai_intent, "data/fai_intent.csv")

# *Deaths by age and district ----
# Download url: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-age
# Blue Ridge Health District is the smallest regional dataset that includes ages 

fai_age <- read_csv("data/raw/vdh-pud-firearm-deaths-by-district-age.csv") %>%
  clean_names() 

fai_age %<>%
  filter(str_detect(health_district, "Blue Ridge")) 

write_csv(fai_age, "data/fai_age.csv")

# FBI Data Explorer ----
# This data can be accessed by downloading the raw CSVs from the Virginia NIBRS build-a-table website: https://va.beyond2020.com/

# *Crimes ----
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Crime by jurisdiction > All Crime Types by City or County
# Filter criteria:
# - Members = N Crimes, Estimated Population 
# - Type of Weapon/Force Involved = Firearm (no subtypes) 
# - Jurisdiction by geography = Virginia (no subtypes), VSP Division 3 > Albemarle County and Charlottesville City (no subtypes)
# - Incident dates = 2016-2023 (no subtypes)
# - Offense type = All (no subtypes)
# - Offender age = Under 18, 18 and over, Unknown, and Missing 
# Table construction: 
# - Rows: Incident Date, Offense Type, Jurisdiction by Geography
# - Columns: Measures, Type of Weapon/Force Involved, Offender Ages, Estimated Population 
# - Slicers: None
# - Order of Measures: Number of Crimes, Est Pop

# Read
ucr2 <- read_csv("data/raw/All Crime Types By City or County.csv", skip = 5) %>%
  clean_names()

# Clean 
ucr2 <- ucr2 %>%
  select(x1:under_18_8) %>%
  rename(year = x1, type = x2, district = offender_age, n_juvenile = under_18_4,
         n_adult = x18_and_over_5, n_unknown = unknown_6, n_missing = missing_7,
         est_pop_district = under_18_8)

ucr2 <- ucr2[-1,]

ucr2 <- ucr2 %>% 
  fill(year, type) %>%
  mutate(
    district = case_when(
      district == "Albemarle County" ~ "albemarle",
      district == "Charlottesville City" ~ "charlottesville",
      TRUE ~ "virginia"),
    type = case_when(
      type == "All Offense Types" ~ "crime_all"
    ))

# Calculate total N crimes for region by year:
ucr2 <- ucr2 %>%
  group_by(year, district) %>%
  mutate(n_total = sum(n_juvenile, n_adult, n_unknown, n_missing, na.rm = TRUE))

# # Tidy 
# ucr2 <- ucr2 %>%
#   pivot_longer(crime_n_juvenile:incident_n_missing, names_to = "type") %>%
#   separate_wider_delim(cols = type, delim = "_n_", names = c("type", "group")) %>%
#   unite(col = type, c("type", "crime"), sep = "_") %>%
#   pivot_wider(names_from = group, values_from = value) %>%
#   rename_with(~paste0("n_", .), grep("juvenile|adult|unknown|missing", names(.))) 

pops2 <- ucr2 %>% 
  select(year, district, est_pop_district) %>% 
  distinct()

write_csv(ucr2, "data/ucr2.csv")
write_csv(pops2, "data/pops2.csv")

# *Theft ----
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Property Crime Incidents by Offense Type
# Filter criteria:
# - Measures = N Incidents, Estimated Population, Avg. Property Value, Property Value
# - Offense Type = Theft from Motor Vehicles
# - Jurisdiction = Virginia, Charlottesville, and Albemarle 
# - Incident Dates = 2016-2023
# - Property Description = Firearms
# Table construction: 
# - Rows: Incident Date, Offense Type, Jurisdiction by Geography, Property Description
# - Columns: Measures 
# - Slicers: None

theft <- read_csv("data/raw/Property Crime Incidents by Offense Type.csv", skip = 4) %>%
  clean_names()

theft <- theft %>%
  select(x1:property_value) %>%
  rename(type = x1, year = x2, property = x3, district = measures, n_stolen = number_of_incidents, 
         est_pop_district = estimated_population, avg_value = average_property_value)

theft <- theft[-1,]

theft <- theft %>% 
  fill(year, type, property) 
  
write_csv(theft, "data/nibrs_theft.csv")

# ATF ----
# This data can be downloaded directly from the ATF site: https://www.atf.gov/firearms/listing-federal-firearms-licensees
# Filter criteria:
# Year: 2024
# Month: April
# State: Virginia 

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
#lon_lat_dealers <- geocode(local_dealers$address)
sum(is.na(lon_lat_dealers$lon)) #0 
local_dealers <- bind_cols(local_dealers, lon_lat_dealers)

local_dealers %<>% select(license_type, license_name, business_name, premise_street, premise_city, lon, lat, business_type)

write_csv(local_dealers, "data/atf_dealers.csv")

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

# Participant demographics 
# case_dems <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD.xlsx", sheet = "Demographic Data for Cases") %>%
#   clean_names()

# Census ----
county_codes <- data.frame(
  code = c(540, 003), 
  name = c("Albemarle County", "Charlottesville City"))

region <- county_codes$code

tract_names <- read_csv("data/tract_names.csv")

# Variables to explore:
vars <- c("B01003_001",    # total population
          "B01001_003",    # male pop <5
          "B01001_004",    # male pop 5-9
          "B01001_005",    # male pop 10-14
          "B01001_006",    # male pop 15-17
          "B01001_007",    # male pop 18-19
          "B01001_008",    # male pop 20
          "B01001_009",    # male pop 21
          "B01001_010",    # male pop 22-24
          "B01001_027",    # female pop <5
          "B01001_028",    # female pop 5-9
          "B01001_029",    # female pop 10-14
          "B01001_030",    # female pop 15-17
          "B01001_031",    # female pop 18-19
          "B01001_032",    # female pop 20
          "B01001_033",    # female pop 21
          "B01001_034",    # female pop 22-24
          "S1701_C03_001", # poverty rate
          "S1701_C03_002", # child poverty rate 
          "S2301_C04_001", # Percent unemployment (Population 16 and over)
          "B20002_001",    # median earnings 12m age 16+
          "B20004_001",    # median earnings 12m age 25+
          "B20004_002",    # median earnings 12m age 25+ < high school
          "B20004_003",    # median earnings 12m age 25+ high school grad
          "B20004_004",    # median earnings 12m age 25+ some college or associates
          "B20004_005",    # median earnings 12m age 25+ bachelor's 
          "B20004_006")    # median earnings 12m age 25+ graduate or professional degree 

# 2018-2022 5-year ACS
dat <- get_acs(geography = "tract",
               variables = vars,
               state = "VA",
               county = region,
               survey = "acs5",
               year = 2022,
               geometry = TRUE,
               output = "wide")

# Clean names and derive some variables 
dat <- dat %>%
  rename(pop_est = B01003_001E,
         poverty_est = S1701_C03_001E,
         cpov_est = S1701_C03_002E,
         unemployment_rate = S2301_C04_001E,
         med_earn_16 = B20002_001E,
         med_earn_25 = B20004_001E,
         med_earn_nohs = B20004_002E,
         med_earn_hs = B20004_003E,
         med_earn_col = B20004_004E,
         med_earn_bd = B20004_005E,
         med_earn_gd = B20004_006E) %>%
  group_by(GEOID) %>%
  mutate(m_under18 = sum(B01001_003E, B01001_004E, B01001_005E, B01001_006E),
         m_under25 = sum(m_under18, B01001_007E, B01001_008E, B01001_009E, B01001_010E),
         f_under18 = sum(B01001_027E, B01001_028E, B01001_029E, B01001_030E),
         f_under25 = sum(f_under18, B01001_031E, B01001_032E, B01001_033E, B01001_034E),
         total_under18 = sum(f_under18, m_under18),
         percent_under18 = (total_under18 / pop_est) * 100,
         total_under25 = sum(f_under25, m_under25),
         percent_under25 = (total_under25 / pop_est) * 100) %>%
  ungroup() %>%
  select(-starts_with("B"),
         -ends_with("M")) %>%
  separate_wider_delim(NAME, delim = "; ", names = c("tract", "locality", "state")) %>%
  left_join(tract_names, by = join_by(tract == tract_id)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

write_rds(dat, "data/census.RDS")



# TODO:  HDI 
