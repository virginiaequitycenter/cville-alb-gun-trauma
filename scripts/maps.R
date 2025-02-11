# Script to pull and prep base street maps for Charlottesville and Albemarle 
# By exporting these ggmap objects, users don't have to register with the ggmap API to plot and knit on their own 

library(ggmap)
library(tidyverse)

# Charlottesville
cville_map <- get_map(c(left = -78.53, bottom = 38.00, right = -78.45, top = 38.07), 
                      maptype = "roadmap", color = "bw")
save(cville_map, file = "data/cville_map.RData")

# Albemarle 
alb_map <- get_map(c(left = -79, bottom = 37, right = -78, top = 39),
                   maptype = "roadmap", color = "bw")
save(alb_map, file = "data/alb_map.RData")
