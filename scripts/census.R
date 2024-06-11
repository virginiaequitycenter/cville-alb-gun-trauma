# Comparing regional equity variables to gun violence outcomes 

library(ggmap)
library(sf)
library(tidycensus)
library(tidyverse)

county_codes <- data.frame(code = c(540, 003), name = c("Albemarle County", "Charlottesville City"))
region <- county_codes$code

# Initial variables to explore:
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
               geometry = "TRUE",
               output = "wide")

dat <- dat %>%
  select(-ends_with("M")) %>%
  rename(pop_est = B01003_001E,
         poverty_est = S1701_C03_001E,
         cpov_est = S1701_C03_002E) %>%
  separate_wider_delim(NAME, delim = "; ", names = c("tract", "locality", "state"))


# Albemarle and Cville ----

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

# Cville zoom ----
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
