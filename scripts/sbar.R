# Script to pull and prep firearm-related incidents at VA public schools during the
# 2023-2024 school year using the Student Behavior and Administrative Response reporting 
# Homepage: https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education

# Libraries ----
library(here)
library(httr)
library(janitor)
library(readxl)
library(sf)
library(tidycensus)
library(tidyverse)

# Download data ----
# Data pulled from https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education

# Create vector of urls:
urls <- c(
  "https://www.doe.virginia.gov/home/showpublisheddocument/57664/638629474066500000", # 2023-2024
  "https://www.doe.virginia.gov/home/showpublisheddocument/50647/638345340433400000", # 2022-2023
  "https://www.doe.virginia.gov/home/showpublisheddocument/50645/638345340429030000") # 2021-2022


# Create vector of destination file names:
dest <- paste0("data/raw/sbar_", c(2022:2024), ".xlsx")

if (!dir.exists(here("data/raw"))) {
  dir.create(here("data/raw"))
}

# Use headers to masquerade as a browser by manually supplying your user-agent,
# otherwise you'll get a Error 403: Forbidden. You'll need to do this every time you
# update one of your browsers. 

# To get your user agent: 
# 1. Open url above (in Chrome): https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education
# 2. Right click anywhere on the page and select INSPECT
# 3. Navigate to NETWORK tab 
# 4. Resubmit the api request by selecting one of the school download links as an example 
# 5. Click on the request (it will start with image.aspx?...)
# 6. Scroll down to REQUEST HEADERS
# 7. Copy the text after USER-AGENT and paste it into field below

headers = c(
  'user-agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36'
)

custom_dl_func = function(file, dest) {
  res <- GET(url = file, add_headers(.headers = headers))
  bin <- content(res, "raw")
  writeBin(bin, dest)
}

walk2(urls, dest, custom_dl_func)

# Read ----
files <- list.files("data/raw", pattern = "^sbar_20", full.names = TRUE)
data <- map_dfr(files, ~read_excel(.x, sheet = "Events by Behavior"))

# Filter to only incidents related to firearms 
# Behaviors of interest: 
# "Assault with Firearm or Weapon", "Illegal Possession of Rifle or Shotgun", 
# "Illegal Possession of Handgun", "Illegal Possession of Any Other Projectile Weapon",
# "Illegal Possession of Other Firearms:..."

sbar <- data %>%
  clean_names()

sbar <- sbar %>%
  filter(grepl("Firearm or Weapon|Rifle|Handgun|Other Projectile Weapon|Other Firearms", behavior)) %>%
  group_by(division_name, school_year) %>%
  summarize(total_incidents = sum(number_of_events))

# regions <- tibble(
#   region = c(1, 2, 3, 4, 5, 6, 7, 8),
#   region_name = c("Central Virginia", "Tidewater", "Northern Neck", "Northern Virginia",
#                   "Valley", "Western Virginia", "Southwest", "Southside"))

# Join with school enrollment numbers to calculate rates ----
# ACS school enrollment 
raw <- get_acs(geography = "county",
               variables = c("B01003_001", "B14001_005", "B14001_006", "B14001_007"),
               state = "VA",
               survey = "acs5",
               year = 2023,
               geometry = TRUE,
               output = "wide")

acs <- raw %>%
  mutate(county = gsub('.{10}$', "", NAME)) %>%
  select(-ends_with("M"), -NAME) %>%
  rename(pop_est = B01003_001E,
         grade1to4 = B14001_005E,
         grade5to8 = B14001_006E,
         grade9to12 = B14001_007E) %>%
  group_by(county) %>%
  mutate(enrolled = sum(grade1to4, grade5to8, grade9to12)) %>%
  ungroup() %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

sbar_firearms <- acs %>%
  left_join(sbar, by = join_by(county == division_name)) %>%
  mutate(incident_rate = (total_incidents / enrolled))

# Quickly visualize to confirm
plot(sbar_firearms["incident_rate"])
plot(sbar_firearms["total_incidents"])

sbar_firearms <- sbar_firearms %>%
  drop_na(incident_rate) %>%
  select(district = county, school_year, total_pop = pop_est, enrolled, total_incidents, 
         incident_rate, geometry)

# Save ----
saveRDS(sbar_firarms, "data/sbar_firearms.RDS")