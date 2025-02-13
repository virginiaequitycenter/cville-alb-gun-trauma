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

# TODO: pending API access
# All Incidents in Virginia ----
