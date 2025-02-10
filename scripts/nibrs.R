# Script to pull and prep gun-related crimes and rates from the the Virginia Uniform 
# Crime Reporting - Incident Based Reporting portal
# Homepage: https://vsp.virginia.gov/sections-units-bureaus/bass/criminal-justice-information-services/uniform-crime-reporting/
# Portal: https://va.beyond2020.com/

# Note: this data cannot be programmatically downloaded via API, so each query must
# be constructed using their "build-a-table" feature and then downloaded as a CSV. 
# The criteria for constructing each dataset is outlined below. 

# Libraries 
library(janitor)
library(tidyverse)
  
# Jurisdiction populations (for rate calculations)
file_pth <- "data/raw/Population by Division.csv"
col_names <- read.table(file_pth, sep = ';', skip = 4, nrow = 1)
col_names2 <- read.table(file_pth, sep = ';', skip = 5, nrow = 1)
col_names <- c(col_names2, col_names[-1]) %>% unlist() %>% unname() %>% na.omit()
nibrs_pops <- read.table(file_pth, sep = ';', skip = 6) %>% 
  set_names(col_names) %>%
  clean_names()

nibrs_pops <- nibrs_pops %>%
  pivot_longer(cols = c(charlottesville_city, albemarle_county, virginia),
               names_to = "locality",
               values_to = "est_pop") %>%
  filter(incident_date >= 2016) %>%
  mutate(est_pop = as.numeric(gsub(",", "", est_pop)),
         locality = str_to_title(gsub("_", " ", locality, fixed = TRUE)))

# Firearm Crimes by Age ----
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Crime by Jurisdiction > All Crime Types by City or County
# Filter criteria:
# - Jurisdiction by geography = Virginia, Albemarle County, Charlottesville City
# - Type of Weapon/Force Involved = Firearm 
# - Incident dates = 2016-2023
# - Offender age = Under 18, 18 and over, Unknown, and Missing 
# Table construction: 
# - Rows: Jurisdiction by Geography, Incident Date
# - Columns: Type of Weapon/Force Involved, Offender Age

firearm_age <- read_csv("data/raw/All Crime Types By City or County - Firearm by Age.csv", 
                        skip = 5)

firearm_age <- firearm_age[-1, -7] %>%
  clean_names() %>%
  rename(locality = x1, yr = offender_age) %>%
  fill(locality) %>%
  mutate(total = rowSums(across(c(under_18:missing)), na.rm = T),
         yr = as.integer(yr))

# Calculate rates
firearm_age <- firearm_age %>%
  left_join(nibrs_pops, by = join_by(yr == incident_date, locality == locality))

firearm_age <- firearm_age %>%
  rowwise() %>%
  mutate(youth_rate_100k = (under_18 / est_pop) * 1e5,
         adult_rate_100k = (x18_and_over / est_pop) * 1e5)

write_csv(firearm_age, "data/nibrs_firearm_age.csv")

# Theft from Cars----
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

# Domestic Violence - Firearm ----
# This data can be accessed by downloading the raw CSVs from the Virginia NIBRS build-a-table website
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Crime by Jurisdiction > Domestic Violence by City or County
# Filter criteria:
# - Measures = Number of Victims, Victim to Offender Relationship (Intimate, Family, Acquaintance), Victim Gender, Offender Gender
# - Offense Type = Crimes Against Person
# - Jurisdiction = Virginia, Charlottesville, and Albemarle
# - Type of Weapon/Force Involved = Firearm
# - Incident Dates = 2016-2023
# Table construction: 
# - Rows: Type of Weapon/Force Involved, Incident Date, Jurisdiction by Geography, Victim Gender, Offender Gender
# - Columns: Victim to Offender Relationship, Offense
# - Slicers: None

domestic_firearm <- read_csv("data/raw/Domestic Violence by City or County - Firearm.csv", skip = 4) %>%
  clean_names()

domestic_firearm <- domestic_firearm[-1,-c(1, 9)]

domestic_firearm <- domestic_firearm %>%
  rename(locality = x2, yr = x3, victim_gender = x4, offender_gender = victim_to_offender_relationship)

domestic_firearm <- domestic_firearm[-1,]

domestic_firearm <- domestic_firearm %>%
  fill(locality, yr, victim_gender, offender_gender) %>%
  mutate_at(vars(intimate, family, acquaintance), as.numeric) %>%
  rename(intimate_firearm = intimate, family_firearm = family, acquaintance_firearm = acquaintance)

write_csv(domestic_firearm, "data/nibrs_domestic_firearm.csv")

# Domestic Violence - All ----
# This data can be accessed by downloading the raw CSVs from the Virginia NIBRS build-a-table website
# Download url: https://va.beyond2020.com/va_public/Browse/browsetables.aspx
# Table: Crime by Jurisdiction > Domestic Violence by City or County
# Filter criteria:
# - Measures = Number of Victims, Victim to Offender Relationship (Intimate, Family, Acquaintance), Victim Gender, Offender Gender
# - Offense Type = Crimes Against Person
# - Jurisdiction = Virginia, Charlottesville, and Albemarle
# - Incident Dates = 2016-2023
# Table construction: 
# - Rows: Jurisdiction by Geography, Incident Date, Victim Gender, Offender Gender
# - Columns: Victim to Offender Relationship, Offense Type
# - Slicers: Measures

domestic_type <- read_csv("data/raw/Domestic Violence by City or County - Type.csv", skip = 4) %>%
  clean_names()

domestic_type <- domestic_type[-1, -19] %>%
  rename(locality = x1, yr = x2, weapon = x3, relationship = offense_type) %>%
  fill(locality, yr, weapon)

ipv <- domestic_type %>%
  filter(relationship == "Intimate") %>%
  rowwise() %>%
  mutate(homicide = sum(murder_and_nonnegligent_manslaughter, negligent_manslaughter, justifiable_homicide, na.rm = TRUE),
         sex_crimes = sum(all_rape, criminal_sexual_contact, incest, statutory_rape, human_trafficking_commercial_sex_acts, na.rm = TRUE),
         assault = sum(aggravated_assault, simple_assault, na.rm = TRUE)) %>%
  select(locality, yr, weapon, relationship, all_offense_types, homicide, sex_crimes, assault, intimidation, human_trafficking_involuntary_servitude)

write_csv(ipv, "data/nibrs_ipv.csv")

# Use of Force ----

force <- read_csv("data/raw/Use of Force Incidents by Location.csv", skip = 5) %>%
  clean_names() %>%
  rename(yr = x1, locality = incident_category, all = all_incident_categories, 
         death = the_death_of_a_person_due_to_law_enforcement_use_of_force,
         serious_injury = the_serious_injury_of_a_person_due_to_law_enforcement_use_of_force,
         firearm_nodeathorinjury = the_discharge_of_a_firearm_by_law_enforcement_at_a_person_not_resulting_in_death_or_serious_injuries) %>%
  fill(yr)

force <- force[-1, -c(7:8)]

force <- force %>%
  filter(locality == "Virginia") %>%
  rowwise() %>%
  mutate(pct_death = round((death/all) * 100, 1),
         pct_serious_injury = round((serious_injury/all) * 100, 1),
         pct_firearm_noharm = round((firearm_nodeathorinjury/all) * 100, 1))

write_csv(force, "data/nibrs_force.csv")


#TODO: 
# Hate Crimes - Firearm
