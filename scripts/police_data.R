
library(magrittr)
library(janitor)
library(readxl)
library(tidyverse)

# Participant demographics 
case_dems <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD.xlsx", sheet = "Demographic Data for Cases") %>%
  clean_names()

# All incidents of gun violence from 2019 - 2024
all_gv <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD.xlsx", sheet = "ALL GV INCIDENTS") %>%
  clean_names()

shots_fired <- all_gv %>%
  filter(crime_code == "Shots Fired",
         case_disp_verified == "VERIFIED")

shots_fired %<>% mutate(address = paste(street, "Charlottesville VA"))
lon_lat <- geocode(shots_fired$address)
sum(is.na(lon_lat$lon)) #0 
shots_fired <- bind_cols(shots_fired, lon_lat)

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






