# Script to pull and prep population and geography information for Charlottesville and Albemarle 
# based on the 5-year American Community Survey administered by the US Census Bureau
# Homepage: https://www.census.gov/programs-surveys/acs/data.html

# Libraries
library(janitor)
library(sf)
library(tidycensus)
library(tidyverse)

# Charlottesville & Albemarle region
county_codes <- data.frame(
  code = c(540, 003), 
  name = c("Albemarle County", "Charlottesville City"))

region <- county_codes$code

# Local census tract names 
tract_names <- read_csv("data/acs_tract_names.csv")

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


# 2019-2023 5-year ACS by Tract -----
dat <- get_acs(geography = "tract",
               variables = vars,
               state = "VA",
               county = region,
               survey = "acs5",
               year = 2023,
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

write_rds(dat, "data/acs_tract_info.RDS")

# Race by County----
race_vars <- c(pop_est = "B01001_001",
               white = "B01001A_001",
               black = "B01001B_001",
               aian = "B01001C_001",
               asian = "B01001D_001",
               nhpi = "B01001E_001",
               other = "B01001F_001",
               multi = "B01001G_001",
               hispanic = "B01001I_001",
               white_male = "B01001A_002",
               black_male = "B01001B_002")

# 2019 - 2023 5 year ACS 
race_county <- get_acs(geography = "county",
                       variables = race_vars,
                       state = "VA",
                       county = region,
                       survey = "acs5",
                       year = 2023,
                       geometry = FALSE,
                       output = "wide")

# Quick function for calculating percentages:
calculate_pcts <- . %>%
  mutate(pct_black = (black/pop_est) * 100,
         pct_white = (white/pop_est) * 100,
         pct_hispanic = (hispanic/pop_est) * 100,
         pct_white_male = (white_male/pop_est) * 100,
         pct_black_male = (black_male/pop_est) * 100)

race_county <- race_county %>%
  mutate(region = word(NAME, 1)) %>%
  select(-ends_with("M"), -GEOID, -NAME)

names(race_county) <- str_remove_all(names(race_county), "E")

race_county <- race_county %>%
  calculate_pcts()

write_csv(race_county, "data/acs_race_county.csv")

# Race by Blue Ridge Health District ----

brhd_codes <- data.frame(
  code = c(540, 003, 125, 065, 079, 109), 
  name = c("Albemarle", "Charlottesville", "Nelson", "Fluvanna", "Greene", "Louisa"))

brhd <- brhd_codes$code

# 2019 - 2023 5 year ACS 
race_brhd <- get_acs(geography = "county",
                       variables = race_vars,
                       state = "VA",
                       county = brhd,
                       survey = "acs5",
                       year = 2023,
                       geometry = FALSE,
                       output = "wide")

race_brhd <- race_brhd %>%
  mutate(region = word(NAME, 1)) %>%
  select(-ends_with("M"), -GEOID, -NAME)

names(race_brhd) <- str_remove_all(names(race_brhd), "E")

race_brhd <- enframe(colSums(Filter(is.numeric, race_brhd)))

race_brhd <- race_brhd %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(locality = "Blue Ridge Health District") %>%
  calculate_pcts()

write_csv(race_brhd, "data/acs_race_brhd.csv")

# Race by Virginia ----

race_va <- get_acs(geography = "state",
                     variables = race_vars,
                     state = "VA",
                     survey = "acs5",
                     year = 2023,
                     geometry = FALSE,
                     output = "wide")

race_va <- race_va %>%
  mutate(region = word(NAME, 1)) %>%
  select(-ends_with("M"), -GEOID, -NAME)

names(race_va) <- str_remove_all(names(race_va), "E")

race_va <- race_va %>%
  calculate_pcts()

write_csv(race_va, "data/acs_race_va.csv")
