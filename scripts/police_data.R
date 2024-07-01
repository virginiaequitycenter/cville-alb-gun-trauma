
library(ggmap)
library(janitor)
library(readxl)
library(stringr)
library(tidyverse)

# Participant demographics 
# case_dems <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD.xlsx", sheet = "Demographic Data for Cases") %>%
#   clean_names()

# All incidents of gun violence from 2019 - 2024
all_gv <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD_BLOCK ADDRESS.xlsx") %>%
  clean_names()

all_gv <- all_gv %>%
  mutate(locality = case_when(
    agency == "ACPD" ~ "Albemarle County",
    TRUE ~ "Charlottesville")
  )

# Filter to only include verified shots fired (Shots Fired, 1, 2, 3), murder (09A), Active Shooter, and aggravated assaults (13A)
gv <- all_gv %>%
  filter(str_detect(crime_code, "Shots|09A|13A|Active Shooter"),
         !str_detect(verified, "^UN"))

# Geocode
gv <- gv %>%
  mutate(address = paste(block_address, locality))
lon_lat <- geocode(gv$address)
sum(is.na(lon_lat$lon)) #0 
gv <- bind_cols(gv, lon_lat)

# Geographic spread

source("scripts/base_maps.R")

ggmap(alb_map) +
  geom_count(data = shots_fired, aes(x = lon, y = lat, color = after_stat(n)))

ggmap(alb_map) +
  stat_bin2d(data = shots_fired, aes(x = lon, y = lat), bins = 50) +
  scale_fill_viridis_c(limits = c(1, 5))

ggmap(cville_map) +
  stat_bin2d(data = shots_fired, aes(x = lon, y = lat), bins = 40) +
  scale_fill_viridis_c(limits = c(1, 15))

ggmap(cville_map) +
  geom_count(data = shots_fired, aes(x = lon, y = lat, color = after_stat(n)))






