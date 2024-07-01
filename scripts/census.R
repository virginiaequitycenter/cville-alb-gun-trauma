# Comparing regional equity variables to gun violence outcomes 

# Setup ----
library(ggmap)
library(janitor)
library(leaflet)
library(lubridate)
library(sf)
library(readxl)
library(stringr)
library(tidycensus)
library(tidyverse)

# *Gun violence data prep ----
# All incidents of gun violence from 2019 - 2024
all_gv <- read_excel("data/raw/Regional GV Data - 2019-2024 YTD_BLOCK ADDRESS.xlsx") %>%
  clean_names()

all_gv <- all_gv %>%
  mutate(locality = case_when(
    agency == "ACPD" ~ "Albemarle County",
    TRUE ~ "Charlottesville"))

# Filter to only include verified shots fired (Shots Fired, 1, 2, 3), murder (09A), Active Shooter, and aggravated assaults (13A)
gv <- all_gv %>%
  filter(str_detect(crime_code, "Shots|09A|13A|Active Shooter"),
         !str_detect(verified, "^UN"))

gv <- gv %>% 
  mutate(description = case_when(
    crime_code == "09A" ~ "Homicide",
    crime_code == "13A" ~ "Aggravated Assault",
    crime_code == "Active Shooter" ~ "Active Shooter",
    TRUE ~ "Shots Fired"
  ))


# Geocode
gv <- gv %>%
  mutate(address = paste(block_address, locality))
lon_lat <- geocode(gv$address)
sum(is.na(lon_lat$lon)) #0 
gv <- bind_cols(gv, lon_lat)

#write_csv(gv, "data/gun_violence.csv")

# Filter to only post-covid incidents 

gv_2021 <- gv %>%
  filter(reported_date > "2021-01-01")

# *Census data prep ----
county_codes <- data.frame(code = c(540, 003), 
                           name = c("Albemarle County", "Charlottesville City"))
region <- county_codes$code

tract_names <- read_csv("data/tract_names.csv")

# Initial ACS variables to explore:
vars <- c("B01003_001",    # population
          "S1701_C03_001", # poverty rate
          "S1701_C03_002", # child poverty rate 
          "S2301_C04_001", # Percent unemployment (Population 16 and over)
          "B20002_001E",   # median earnings 12m age 16+
          "B20004_001E",   # median earnings 12m age 25+
          "B20004_002E",   # median earnings 12m age 25+ < high school
          "B20004_003E",   # median earnings 12m age 25+ high school grad
          "B20004_004E",   # median earnings 12m age 25+ some college or associates
          "B20004_005E",   # median earnings 12m age 25+ bachelor's 
          "B20004_006E")   # median earnings 12m age 25+ graduate or professional degree 

# TODO:  HDI 

# 2018-2022 5-year ACS
dat <- get_acs(geography = "tract",
               variables = vars,
               state = "VA",
               county = region,
               survey = "acs5",
               year = 2022,
               geometry = TRUE,
               output = "wide")

dat <- dat %>%
  select(-ends_with("M")) %>%
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
  separate_wider_delim(NAME, delim = "; ", names = c("tract", "locality", "state")) %>%
  left_join(tract_names, by = join_by(tract == tract_id)) %>%
  st_as_sf()
  
#write_csv(dat, "data/tmp_census.csv")

# Poverty & Childhood poverty ----
pal_pov <- colorNumeric(palette = "viridis",
                        domain = NULL,
                        reverse = TRUE)

dat %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(group = "Overall Poverty",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_pov(poverty_est),
              fillOpacity = 0.5,
              popup = paste0("Poverty Rate: ", dat$poverty_est, "%", "<br>",
                             "Population: ", dat$pop_est, "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addPolygons(group = "Child Poverty",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_pov(cpov_est),
              fillOpacity = 0.5,
              popup = paste0("Child Poverty Rate: ", dat$cpov_est, "%", "<br>",
                             "Population: ", dat$pop_est, "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addLegend("bottomright",
            pal = pal_pov,
            values = ~ poverty_est, 
            title = "Estimated Poverty Rates",
            labFormat = labelFormat(suffix = "%"), 
            opacity = 1) %>%
  addLayersControl(baseGroups = c("Overall Poverty", "Child Poverty"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMarkers(data = gv_2021, 
             lng = gv_2021$lon,
             lat = gv_2021$lat,
             popup = paste0("Description: ", gv$description, "<br>",
                            "Date: ", gv_2021$reported_date),
             clusterOptions = markerClusterOptions(
               showCoverageOnHover = FALSE,
               iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount();  
    if (childCount < 100) {  
      c = 'rgba(211,211,211);'
    } else if (childCount < 1000) {  
      c = 'rgba(211,211,211);'  
    } else { 
      c = 'rgb(211,211,211);'  
    }    
    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

  }")
             ))


# Median Income by Education ----
pal_earn <- colorNumeric(palette = "viridis",
                        domain = c(15000:130000),
                        reverse = FALSE)

dat %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(group = "All Education Levels",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_earn(med_earn_25),
              fillOpacity = 0.5,
              popup = paste0("Median Earnings: ", scales::dollar(dat$med_earn_25), "<br>",
                             "Poverty Rate: ", dat$poverty_est, "%", "<br>",
                             "Population: ", dat$pop_est, "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addPolygons(group = "High School Degree or Equivalent",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_earn(med_earn_hs),
              fillOpacity = 0.5,
              popup = paste0("Median Earnings: ", scales::dollar(dat$med_earn_hs), "<br>",
                             "Poverty Rate: ", dat$poverty_est, "%", "<br>",
                             "Population: ", dat$pop_est, "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addPolygons(group = "Bachelors Degree or Equivalent",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_earn(med_earn_bd),
              fillOpacity = 0.5,
              popup = paste0("Median Earnings: ", scales::dollar(dat$med_earn_bd), "<br>",
                             "Poverty Rate: ", dat$poverty_est, "%", "<br>",
                             "Population: ", dat$pop_est, "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addLegend("bottomright",
            pal = pal_earn,
            values = c(15000:130000), 
            title = "Median Earnings",
            labFormat = labelFormat(prefix = "$"), 
            opacity = 1) %>%
  addLayersControl(baseGroups = c("All Education Levels",
                                  "High School Degree",
                                  "Bachelors Degree"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMarkers(data = gv, 
             lng = gv$lon,
             lat = gv$lat,
             popup = paste0("Description: ", gv$description, "<br>",
                            "Date: ", gv$reported_date),
             clusterOptions = markerClusterOptions(
               showCoverageOnHover = FALSE,
               iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount();  
    if (childCount < 100) {  
      c = 'rgba(211,211,211);'
    } else if (childCount < 1000) {  
      c = 'rgba(211,211,211);'  
    } else { 
      c = 'rgb(211,211,211);'  
    }    
    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

  }")
             ))

# Unemployment Rates ----
# Reuse pal_pov palette since it has free scales 

dat %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(group = "Unemployment Rate",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_pov(unemployment_rate),
              fillOpacity = 0.5,
              popup = paste0("Unemployment Rate: ", dat$unemployment_rate, "%", "<br>",
                             "Population: ", dat$pop_est, "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addLegend("bottomright",
            pal = pal_pov,
            values = ~ unemployment_rate, 
            title = "Unemployment Rates",
            labFormat = labelFormat(suffix = "%"), 
            opacity = 1) %>%
  addMarkers(data = gv_2021, 
             lng = gv_2021$lon,
             lat = gv_2021$lat,
             popup = paste0("Description: ", gv$description, "<br>",
                            "Date: ", gv_2021$reported_date),
             clusterOptions = markerClusterOptions(
               showCoverageOnHover = FALSE,
               iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount();  
    if (childCount < 100) {  
      c = 'rgba(211,211,211);'
    } else if (childCount < 1000) {  
      c = 'rgba(211,211,211);'  
    } else { 
      c = 'rgb(211,211,211);'  
    }    
    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

  }")
             ))

  
