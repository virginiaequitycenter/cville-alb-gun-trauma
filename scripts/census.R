# Script to pull and save Census data for analysis 
# Sam Toet
# Last updated 7/11/2024

# Setup ----
library(janitor)
library(lubridate)
library(sf)
library(tidycensus)
library(tidyverse)

county_codes <- data.frame(
  code = c(540, 003), 
  name = c("Albemarle County", "Charlottesville City"))

region <- county_codes$code

tract_names <- read_csv("data/tract_names.csv")

# TODO:  HDI 

# Census ----
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

# 2018-2022 5-year ACS
dat <- get_acs(geography = "tract",
               variables = vars,
               state = "VA",
               county = region,
               survey = "acs5",
               year = 2022,
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
  
write_rds(dat, "data/census.RDS")


# Explore visualizations ----
library(leaflet)

# Total incidents over the years
gv <- read_csv("data/regional_gv.csv")

gv_pts <- gv %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

gv_sf_summary <- dat %>% 
  mutate(counts = lengths(st_intersects(., gv_poly))) %>%
  group_by(tract) %>%
  mutate(incidents_pop = (counts / pop_est) * 100,
         incidents_18 = (counts / total_under18 * 100),
         incidents_25 = (counts / total_under25) * 100)

  
pal <- colorNumeric(palette = "viridis", 
                    domain = NULL, 
                    reverse = TRUE)

gv_sf_summary %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal(counts),
              fillOpacity = 0.5,
              popup = paste0("N Incidents: ", gv_sf_summary$counts, "<br>",
                             "Total Population: ", gv_sf_summary$pop_est, "<br>",
                             "Tract: ", gv_sf_summary$tract_name, ", ", gv_sf_summary$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ counts, 
            title = "Number of Incidents 2019-2024", 
            opacity = 1)
  

# Population age summaries 

pal_pop <- colorNumeric(palette = "viridis", 
                        domain = c(0:89), 
                        reverse = TRUE)

dat %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(group = "Under 18",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_pop(percent_under18),
              fillOpacity = 0.5,
              popup = paste0("People Under 18: ", dat$total_under18, "<br>",
                             "Total Population: ", dat$pop_est, "<br>",
                             "Percent of Pop: ", round(dat$percent_under18), "%", "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addPolygons(group = "Under 25",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal_pop(percent_under25),
              fillOpacity = 0.5,
              popup = paste0("People Under 18: ", dat$total_under25, "<br>",
                             "Total Population: ", dat$pop_est, "<br>",
                             "Percent of Pop: ", round(dat$percent_under25), "%", "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addLegend("bottomright",
            pal = pal_pop,
            values = ~ percent_under25, 
            title = paste0("Percent of", "<br>", "Total Population"),
            labFormat = labelFormat(suffix = "%"), 
            opacity = 1) %>%
  addLayersControl(baseGroups = c("Under 18","Under 25"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMarkers(data = gv_past_year, 
             lng = gv_past_year$lon,
             lat = gv_past_year$lat,
             popup = paste0("Description: ", gv_past_year$description, "<br>",
                            "Date: ", gv_past_year$reported_date),
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


