library(janitor)
library(lubridate)
library(tidyverse)

# Format images for print media 

# N Victims Over Time 2024 ----

gva <- read_csv("data/raw/gva_incidents_24.csv") %>%
  clean_names() %>%
  mutate(incident_date = mdy(incident_date)) %>%
  select(-operations)

gva$month_year <- floor_date(gva$incident_date, unit = "month")

gva %>%
  filter(!grepl(2025, incident_date)) %>%
  group_by(month_year) %>%
  summarise(total_injured = sum(victims_injured + suspects_injured),
            total_killed = sum(victims_killed + suspects_killed)) %>%
  ungroup() %>%
  pivot_longer(matches("total")) %>%
  filter(!value == 0) %>% 
  ggplot(aes(x = month_year, y = value, color = name)) +
  geom_col()

  
gva %>%
  filter(between(incident_date, mdy("12-31-2017"), mdy("12-31-2024"))) %>%
  group_by(month_year) %>%
  summarise(total_injured = sum(victims_injured + suspects_injured),
            total_killed = sum(victims_killed + suspects_killed)) %>%
  ungroup() %>%
  pivot_longer(matches("total")) %>% 
  ggplot(aes(x = month_year, y = value, fill = name)) +
  geom_col(width = 25) +
  labs(x = "",
       y = "Number of Victims") +
  scale_y_continuous(breaks = 1:9) +
  scale_fill_manual(labels = c("Injured", "Killed"),
                      values = c("#F8BE3D", "#007BAB"),
                      guide = guide_legend(title = "Victim Status")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10),
        axis.text.x = element_text(angle = 25))


# Aug 22 - March 23
gva_narrow <- gva %>%
  filter(between(incident_date, mdy("8-1-2022"), mdy("3-31-2023")))
  


gva %>%
  filter(between(incident_date, mdy("12-31-2017"), mdy("12-31-2024"))) %>%
  ggplot(aes(x = month_year)) +
  geom_dotplot(method = "histodot")










# Childhood poverty ----

# Get census shape data
dat <- readRDS("data/census.RDS")

# Get police-provided gun violence data
gv <- read_csv("data/regional_gv.csv")

# Convert lat/lon coords to sf points
gv_pts <- gv %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Count incidents per tract
gv_sf_summary <- dat %>% 
  mutate(counts = lengths(st_intersects(., gv_pts))) %>%
  group_by(tract) %>%
  mutate(incidents_pop = (counts / pop_est),
         percent_incidents = (counts / 887) * 100,
         locality = str_to_title(locality)) 

scatter_text2 <- gv_sf_summary %>%
  filter(cpov_est > 31)

gv_sf_summary %>%
  filter(tract_name != "JPA - Fontaine") %>%
  ggplot(aes(x = cpov_est, y = percent_incidents, size = pop_est, color = locality)) +
  geom_point(alpha = 0.8) +
  scale_size(range = c(.1, 12)) +
  geom_label_repel(data = scatter_text2, 
                   aes(x = cpov_est, y = percent_incidents, label = tract_name), size = 3, 
                   min.segment.length = unit(0, 'lines'),
                   inherit.aes = FALSE) +
  scale_color_manual(values = c("#007BAB", "#F8BE3D"), 
                     name = "Region") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(labels = function(x) paste0(round(x), "%")) +
  labs(x = "Percentage of Children Under 18 Living in Poverty",
       y = "Percentage of Total Gun Violence Incidents",
       title = "Childhood Poverty Rates Compared to Incidents of Gun Violence",
       subtitle = "Each circle represents a different census tract",
       caption = "Years: 2018 - 2024",
       size = "Estimated Population") +
  guides(size = "none") +
  theme_minimal()

# Theft ----

nibrs_theft <- read_csv("data/nibrs_theft.csv") %>%
  filter(district != "Virginia")

ggplot(nibrs_theft, aes(year, n_stolen, colour = district)) +
  geom_line(linewidth = 1.5) +
  labs(x = "",
       y = "Number of Firearms Stolen",
       title = "Theft of Firearms from Vehicles") +
  scale_color_manual(values = c("#007BAB", "#F8BE3D"), 
                     name = "Region") +
  geom_label(data = nibrs_theft, 
             aes(label = n_stolen), 
             show.legend = F, 
             alpha = 0.75,
             fontface = "bold") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7),
                     guide = guide_axis(angle = 35)) +
  theme_bw() +
  theme(legend.position = "top")


library(extrafont)
loadfonts()
  