# Script to pull and prep gun-related crimes and rates from the the Virginia Uniform 
# Crime Reporting - Incident Based Reporting portal, which is based off of the National
# Incidident-Based Reporting System (NIBRS)
# Homepage: https://vsp.virginia.gov/sections-units-bureaus/bass/criminal-justice-information-services/uniform-crime-reporting/
# Portal: https://va.beyond2020.com/

# Note: this data cannot be programmatically downloaded via API, so each query must
# be constructed using their "build-a-table" feature and then downloaded as a CSV. 
# The criteria for constructing each dataset is outlined below. 


# Crimes ----
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

# Domestic (Interpersonal) Violence ----
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

interpersonal <- read_csv("data/raw/Domestic Violence by City or County.csv", skip = 4) %>%
  clean_names()

interpersonal <- interpersonal[-1,-c(1, 9)]

interpersonal <- interpersonal %>%
  rename(locality = x2, yr = x3, victim_gender = x4, offender_gender = victim_to_offender_relationship)

interpersonal <- interpersonal[-1,]

interpersonal <- interpersonal %>%
  fill(locality, yr, victim_gender, offender_gender) %>%
  mutate_at(vars(intimate, family, acquaintance), as.numeric)

write_csv(interpersonal, "data/nibrs_interpersonal.csv")

