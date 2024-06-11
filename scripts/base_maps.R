library(ggmap)

cville_map <- get_map(c(left = -78.54, bottom = 37.97, right = -78.43, top = 38.1),
                      maptype = "roadmap", color = "bw")

alb_map <- get_map(c(left = -79, bottom = 37, right = -78, top = 39),
                   maptype = "roadmap", color = "bw")
