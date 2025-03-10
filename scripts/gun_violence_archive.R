# Script to pull and prep data from the the Gun Violence Archive
# Homepage: https://www.gunviolencearchive.org/

# Note: this data cannot be programmatically downloaded via API, so each query must
# be constructed using their "build-a-table" feature and then downloaded as a CSV. 
# The criteria for constructing each dataset is outlined below. 

# Incidents in Charlottesville ----
# Download url: https://www.gunviolencearchive.org/query/3d0414ea-ee7e-49fa-8bcb-c97a46761f96
# Search criteria:
# - Location is in Virginia
# - City = Charlottesville
# - Date = up to end of 2024

gva_incidents <- read_csv("data/raw/gva_incidents_24.csv") %>%
  clean_names() %>%
  mutate(incident_date = mdy(incident_date))

write_csv(gva_incidents, "data/gva_incidents.csv")

# Participants in Charlottesville ----
# Download url: https://www.gunviolencearchive.org/query/45701b8a-ca83-4eb8-b605-2dd2c49e3b42
# Search criteria:
# - Location is in Virginia
# - City = Charlottesville
# - Date = up to end of 2024

gva_participants <- read_csv("data/raw/gva_participants_24.csv") %>%
  clean_names() %>%
  select(-participant_name, -operations) 

write_csv(gva_participants, "data/gva_participants.csv")

# Officer-involved shootings in Charlottesville ----
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

# All Incidents in Virginia ----
# It looks like it only returns 2000 results per search
# so search by time: 2024-2025, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015
# Search criteria:
# - Location is in Virginia AND
# - Date is Year [search by year]
files <- list.files(path="download/incidents/", pattern="csv",
                    full.names = TRUE)

gva_incidents <- map_dfr(files, ~ read_csv(.x) %>%
                           clean_names() %>%
                           mutate(date = mdy(incident_date)) %>% 
                           select(-operations))

write_csv(gva_incidents, "data/gva_incidents_virginia.csv")
# gva_incidents <- read_csv("data/gva_incidents_virginia.csv")
# gva_incidents %>% mutate(year = year(date)) %>% count(year)

# All Participants in Virginia ----
# It looks like it only returns 2000 results per search
# search by time: 2025, 2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015 
# Search criteria:
# - Location is in Virginia
# - Date is Year [search by year]
files <- list.files(path="download/participants/", pattern="csv",
                    full.names = TRUE)

gva_participants <- map_dfr(files, ~ read_csv(.x) %>%
                              clean_names() %>%
                              mutate(date = mdy(incident_date)) %>% 
                              select(-operations, -participant_name))

write_csv(gva_participants, "data/gva_participants_virginia.csv")
# gva_participants <- read_csv("data/gva_participants_virginia.csv")
# gva_participants %>% mutate(year = year(date)) %>% count(year)


# All Officer-involved in Virginia ----
# Download url: https://www.gunviolencearchive.org/query/cb5e0dbc-e97e-41a8-be3b-edf0b2dddd76
# Search criteria: 
# - Location is in Virginia
# - Incident characteristic = Officer-involved

gva_officer <- read_csv("download/gva_officerinvolved_participants.csv") %>%
  clean_names() %>%
  select(-operations, -participant_name) %>%
  mutate(date = mdy(incident_date))

write_csv(gva_officer, "data/gva_officerinvolved_virginia.csv")
