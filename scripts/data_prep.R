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

# Gun Violence Archive ----
# This data can be accessed by downloading the raw CSVs from their Search Database page: https://www.gunviolencearchive.org/query

# *Incidents ----
# Download url: https://www.gunviolencearchive.org/query/3d0414ea-ee7e-49fa-8bcb-c97a46761f96
# Search criteria:
# - Location is in Virginia
# - City = Charlottesville
# - Date = up to end of 2024

gva_incidents <- read_csv("data/raw/gva_incidents_24.csv") %>%
  clean_names() %>%
  mutate(incident_date = mdy(incident_date))

write_csv(gva_incidents, "data/gva_incidents.csv")

# *Participants ----
# Download url: https://www.gunviolencearchive.org/query/45701b8a-ca83-4eb8-b605-2dd2c49e3b42
# Search criteria:
# - Location is in Virginia
# - City = Charlottesville
# - Date = up to end of 2024

gva_participants <- read_csv("data/raw/gva_participants_24.csv") %>%
  clean_names() %>%
  select(-participant_name, -operations) 

write_csv(gva_participants, "data/gva_participants.csv")

# *Officer-involved ----
# Download url: https://www.gunviolencearchive.org/query/e0767e7f-d79b-41ff-aebc-2e918c2e6266
# Search criteria: 
# - Location is in Virginia
# - City = Charlottesville
# - Incident characteristic = Officer-involved

# This data is downloaded from the Gun Violence Archive and then manually augmented by reviewing  
# associated news articles for additional information such as age, race, and if the victim had a gun. 

gva_officer <- read_csv("data/raw/gva_officer.csv") %>%
  clean_names()%>%
  select(incident_date, "gender" = participant_gender, "victim_name" = participant_name) %>%
  mutate(incident_date = mdy(incident_date), gender = str_to_title(gender)) %>%
  filter(!gender == "N/A") # remove officers (gender not provided if officer)

# Info sourced from news articles:
victim_age <- c(44, 43, 60, 32, 45, 27, 29, 42, 25, 25) 
race <- c("White", "Black", "White", "N/A", "White", "Black", "Black", "Black", "Black", "White")
outcome <- c("Death", "Death", "Injury", "Injury", "Death", "Injury", "Injury", "Death", "Injury", "Injury")
weapon <- c("Firearm")

gva_officer <- cbind(gva_officer, outcome, victim_age, race, weapon)

gva_officer <- gva_officer %>% 
  select(-victim_name)

write_csv(gva_officer, "data/gva_officer.csv")

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
# This data can be accessed by downloading the raw CSVs directly from the VA open data portal website: https://data.virginia.gov/

# *Injuries by county ---- 
# Download url: https://data.virginia.gov/dataset/vdh-pud-fai-by-citycounty

vdh_injuries <- read_csv("data/raw/vdh-pud-fai-by-citycounty.csv") %>%
  clean_names()

vdh_injuries %<>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric)

write_csv(vdh_injuries, "data/vdh_injuries.csv")

# *Deaths by intent ---- 
# Download url: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-intent

vdh_intent <- read_csv("data/raw/vdh-pud-firearm-deaths-by-district-intent.csv") %>%
  clean_names() %>%
  filter(str_detect(patient_health_district, "Blue Ridge"))

write_csv(vdh_intent, "data/vdh_intent.csv")

# *Deaths by age and district ----
# Download url: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-age
# Blue Ridge Health District is the smallest regional dataset that includes ages 

vdh_age <- read_csv("data/raw/vdh-pud-firearm-deaths-by-district-age.csv") %>%
  clean_names() 

vdh_age %<>%
  filter(str_detect(health_district, "Blue Ridge")) 

write_csv(vdh_age, "data/vdh_age.csv")

# NIBRS ----
# This data can be accessed by downloading the raw CSVs from the Virginia NIBRS build-a-table website: https://va.beyond2020.com/

# *Crimes ----
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Crime by jurisdiction > All Crime Types by City or County
# Filter criteria:
# - Members = N Crimes, Estimated Population 
# - Type of Weapon/Force Involved = Firearm 
# - Jurisdiction by geography = Virginia, Albemarle County, Charlottesville City (VSP Division 3)
# - Incident dates = 2016-2023 
# - Offense type = All 
# - Offender age = Under 18, 18 and over, Unknown, and Missing 
# Table construction: 
# - Rows: Incident Date, Offense Type, Jurisdiction by Geography
# - Columns: Measures, Type of Weapon/Force Involved, Offender Ages, Estimated Population 
# - Slicers: None
# - Order of Measures: Number of Crimes, Est Pop

nibrs_crime <- read_csv("data/raw/All Crime Types By City or County.csv", skip = 5) %>%
  clean_names()

nibrs_crime <- nibrs_crime %>%
  select(x1:under_18_8) %>%
  rename(year = x1, type = x2, district = offender_age, n_juvenile = under_18_4,
         n_adult = x18_and_over_5, n_unknown = unknown_6, n_missing = missing_7,
         est_pop_district = under_18_8)

nibrs_crime <- nibrs_crime[-1,]

nibrs_crime <- nibrs_crime %>% 
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
nibrs_crime <- nibrs_crime %>%
  group_by(year, district) %>%
  mutate(n_total = sum(n_juvenile, n_adult, n_unknown, n_missing, na.rm = TRUE))

write_csv(nibrs_crime, "data/nibrs_crime.csv")

# *Theft ----
# This data can be accessed by downloading the raw CSVs from the Virginia NIBRS build-a-table website
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Property Crime Incidents by Offense Type
# Filter criteria:
# - Measures = N Incidents, Estimated Population, Avg. Property Value, Property Value
# - Offense Type = Theft from Motor Vehicles
# - Jurisdiction = Virginia, Charlottesville, and Albemarle (VSP Division 3)
# - Incident Dates = 2016-2023
# - Property Description = Firearms
# Table construction: 
# - Rows: Incident Date, Offense Type, Jurisdiction by Geography, Property Description
# - Columns: Measures 
# - Slicers: None

nibrs_theft <- read_csv("data/raw/Property Crime Incidents by Offense Type.csv", skip = 4) %>%
  clean_names()

nibrs_theft <- nibrs_theft %>%
  select(x1:property_value) %>%
  rename(type = x1, year = x2, property = x3, district = measures, n_stolen = number_of_incidents, 
         est_pop_district = estimated_population, avg_value = average_property_value)

nibrs_theft <- nibrs_theft[-1,]

nibrs_theft <- nibrs_theft %>% 
  fill(year, type, property) 
  
write_csv(nibrs_theft, "data/nibrs_theft.csv")

# *Domestic (Interpersonal) Violence ----
# This data can be accessed by downloading the raw CSVs from the Virginia NIBRS build-a-table website
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Domestic Violence by City or County
# Filter criteria:
# - Measures = Number of Victims, Victim to Offender Relationship (Intimate, Family, Acquaintance), Victim Gender, Offender Gender
# - Offense Type = Crimes Against Person
# - Jurisdiction = Charlottesville and Albemarle (located in VSP Division 3)
# - Type of Weapon/Force Involved = Firearm
# - Incident Dates = 2016-2023
# Table construction: 
# - Rows: Type of Weapon/Force Involved, Incident Date, Jurisdiction by Geography, Victim Gender, Offender Gender
# - Columns: Victim to Offender Relationship, Offense
# - Slicers: None

interpersonal <- read_csv("data/raw/Domestic Violence by City or County.csv", skip = 4) %>%
  clean_names()

interpersonal <- interpersonal[-1,-c(1, 9)]

interpersonal <- interpersonal %>%
  rename(yr = x2, location = x3, victim_gender = x4, offender_gender = victim_to_offender_relationship)

interpersonal <- interpersonal[-1,]

interpersonal <- interpersonal %>%
  fill(yr, location, victim_gender, offender_gender)

write_csv(interpersonal, "data/nibrs_interpersonal.csv")


# ATF ----
# This data can be downloaded directly from the ATF site: https://www.atf.gov/firearms/listing-federal-firearms-licensees
# Filter criteria:
# - Year: 2024
# - Month: April
# - State: Virginia 

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

# Participant demographics - perhaps for another analysis 
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

# *Race ----
race_vars <- c(pop_est = "B01001_001",
               white = "B01001A_001",
               black = "B01001B_001",
               aian = "B01001C_001",
               asian = "B01001D_001",
               nhpi = "B01001E_001",
               other = "B01001F_001",
               multi = "B01001G_001",
               hispanic = "B01001I_001")

race_county <- get_acs(geography = "county",
                       variables = race_vars,
                       state = "VA",
                       county = region,
                       survey = "acs5",
                       year = 2022,
                       geometry = FALSE,
                       output = "wide")

# Quick function for calculating percentages:
calculate_pcts <- . %>%
  mutate(pct_black = (black/pop_est) * 100,
         pct_white = (white/pop_est) * 100,
         pct_hispanic = (hispanic/pop_est) * 100)

race_county <- race_county %>%
  mutate(region = word(NAME, 1)) %>%
  select(-ends_with("M"), -GEOID, -NAME)

names(race_county) <- str_remove_all(names(race_county), "E")

race_county <- race_county %>%
  calculate_pcts()

write_csv(race_county, "data/race_county.csv")

# Base Maps ----
# By exporting the Cville and Albemarle base ggmap objects, users don't have to register with the ggmap API to plot and knit on their own 

cville_map <- get_map(c(left = -78.53, bottom = 38.00, right = -78.45, top = 38.07), 
                      maptype = "roadmap", color = "bw")
save(cville_map, file = "data/cville_map.RData")

alb_map <- get_map(c(left = -79, bottom = 37, right = -78, top = 39),
                   maptype = "roadmap", color = "bw")
save(alb_map, file = "data/alb_map.RData")
