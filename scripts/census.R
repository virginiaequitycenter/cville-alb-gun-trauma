# Comparing regional equity variables to gun violence outcomes 

# Setup ----
library(ggmap)
library(leaflet)
library(sf)
library(tidycensus)
library(tidyverse)

# Census prep
county_codes <- data.frame(code = c(540, 003), 
                           name = c("Albemarle County", "Charlottesville City"))
region <- county_codes$code

tract_names <- read_csv("data/tract_names.csv")

# Initial ACS variables to explore:
vars <- c("B01003_001",    # population
          "S1701_C03_001", # poverty rate
          "S1701_C03_002") # child poverty rate 

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
         cpov_est = S1701_C03_002E) %>%
  separate_wider_delim(NAME, delim = "; ", names = c("tract", "locality", "state")) %>%
  left_join(tract_names, by = join_by(tract == tract_id)) %>%
  st_as_sf()
  
#write_csv(dat, "data/acs_pov.csv")

# Albemarle  ----

alb_map <- get_map(c(left = -79, bottom = 37, right = -78, top = 39),
                   maptype = "roadmap", color = "bw")
  
# Poverty 
ggmap(alb_map) +
  geom_sf(data = dat, 
          aes(fill = poverty_est, 
              geometry = geometry), 
          inherit.aes = FALSE, alpha = .8) +
  scale_fill_viridis_c(direction = -1)
  

# Child poverty 
ggmap(alb_map) +
  geom_sf(data = dat, 
          aes(fill = cpov_est, 
              geometry = geometry), 
          inherit.aes = FALSE, alpha = .8) +
  scale_fill_viridis_c(direction = -1)

# Cville  ----
cville_map <- get_map(c(left = -78.54, bottom = 37.97, right = -78.43, top = 38.1),
                      maptype = "roadmap", color = "bw")

cville <- dat %>%
  filter(locality == "Charlottesville city")

# Poverty 
ggmap(cville_map) +
  geom_sf(data = cville, 
          aes(fill = poverty_est, 
              geometry = geometry), 
          inherit.aes = FALSE, alpha = .8) +
  scale_fill_viridis_c(direction = -1)


# Child poverty 
ggmap(cville_map) +
  geom_sf(data = cville, 
          aes(fill = cpov_est, 
              geometry = geometry), 
          inherit.aes = FALSE, alpha = .8) +
  scale_fill_viridis_c(direction = -1)


# Faceting ----
alb_long <- dat %>%
  pivot_longer(cols = c(poverty_est, cpov_est))

facet_names <- c(
  cpov_est = "Childhood Poverty",
  poverty_est = "Overall Poverty")

ggmap(alb_map) +
  geom_sf(data = alb_long, 
          aes(fill = value, 
              geometry = geometry), 
          inherit.aes = FALSE, alpha = .8) +
  facet_wrap( ~ name, labeller = as_labeller(facet_names)) +
  scale_fill_viridis_c(direction = -1, 
                       name = "Rate", 
                       labels=function(x) paste0(x,"%")) +
  labs(title = "Poverty Rates in Albemarle County") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

  
cville_long <- cville %>%
  pivot_longer(cols = c(poverty_est, cpov_est))

ggmap(cville_map) +
  geom_sf(data = cville_long, 
          aes(fill = value, 
              geometry = geometry), 
          inherit.aes = FALSE, alpha = .8) +
  facet_wrap( ~ name, labeller = as_labeller(facet_names)) +
  scale_fill_viridis_c(direction = -1, 
                       name = "Rate", 
                       labels=function(x) paste0(x,"%")) +
  labs(title = "Poverty Rates in Charlottesville City") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Leaflet ----

pal <- colorNumeric(palette = "viridis",
                    domain = NULL,
                    reverse = TRUE)

# Poverty
dat %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(group = "Overall Poverty",
              stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black", 
              fillColor = ~ pal(poverty_est),
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
              fillColor = ~ pal(cpov_est),
              fillOpacity = 0.5,
              popup = paste0("Child Poverty Rate: ", dat$cpov_est, "%", "<br>",
                             "Population: ", dat$pop_est, "<br>",
                             "Tract: ", dat$tract_name, ", ", dat$locality),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE
              )) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ poverty_est, 
            title = "Estimated Poverty Rates",
            labFormat = labelFormat(suffix = "%"), 
            opacity = 1) %>%
  addLayersControl(baseGroups = c("Overall Poverty", "Child Poverty"),
                   options = layersControlOptions(collapsed = FALSE))


 

  


