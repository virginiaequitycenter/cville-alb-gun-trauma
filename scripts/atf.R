# Script to pull and prep firearm licensing data from the the Bureau of Alcohol, 
# Tobacco, Firearms, and Explosives (ATF) database
# Homepage: https://www.atf.gov/firearms

# Note: this data cannot be programmatically downloaded via API, so each query must
# be constructed using their "build-a-table" feature and then downloaded as a CSV. 
# The criteria for constructing each dataset is outlined below. 

# Libraries
library(ggmap)
library(janitor)
library(tidyverse)

# VA Federal Firearms Licenses (FFLs) ----
# Download URL: https://www.atf.gov/firearms/listing-federal-firearms-licensees
# Filter criteria:
# - Year: 2025
# - Month: January
# - State: Virginia 

va_ffl <- read.delim("data/raw/0125-ffl-list-virginia.txt") %>%
  clean_names() %>% 
  select(type = lic_type, county_code = lic_cnty, license_name, business_name, 
         premise_street, premise_city, premise_zip_code) %>%
  mutate_if(is.character, list(~na_if(.,""))) %>%
  mutate(across(where(is.character), str_to_title),
         business_name = replace_na(business_name, "No Business Name Provided"),
         business_type = case_when(
           business_name == "No Business Name Provided" ~ "Residential",
           TRUE ~ "Business"))

write_csv(va_ffl, "data/atf_va_licenses.csv")

# Local FFLs ----
local_ffl <- va_ffl %>%
  filter(county_code == 3 | county_code == 540)

# Geocode:
local_ffl <- local_ffl %>%
  mutate(address = paste(premise_street, premise_city, "VA"))

lonlat_local_ffl <- geocode(local_ffl$address)

sum(is.na(lonlat_local_ffl$lon)) #0 

local_ffl <- bind_cols(local_ffl, lonlat_local_ffl)

write_csv(local_ffl, "data/atf_local_licenses.csv")
