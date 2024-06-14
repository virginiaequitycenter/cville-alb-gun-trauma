
library(janitor)
library(tidyverse)

brfss <- read_csv("data/raw/Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present__20240611.csv") %>%
  clean_names()


# Questions of interest:
# - "Ever told you that you have a form of depression?" 
# - "What is your age?" 
# - "Do you have serious difficulty concentrating, remembering, or making decisions?"
# - "How is your general health?"
# - "Days when mental health status not good (variable calculated from one or more BRFSS questions)"
# - "Health Status (variable calculated from one or more BRFSS questions)"
# - "Adults who are limited in any activities because of physical, mental, or emotional problems"

# Topics of interest:
# - "Depression"
# - "Overall Health"
# - "Fair or Poor Health"


# Classes of interest:
# - "Chronic Health Indicators" 
# - "Days of Poor Health"
# - "Health Status"




