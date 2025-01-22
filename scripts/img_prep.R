library(ggrepel)
library(janitor)
library(lubridate)
library(sf)
#library(svglite)
library(tidyverse)

# Format images for print media and slide decks

# N Victims Over Time by Month ----
# Data: Gun Violence Archive 
# Date range: Jan 2018 - Dec 2024

gva <- read_csv("data/raw/gva_incidents_24.csv") %>%
  clean_names() %>%
  mutate(incident_date = mdy(incident_date)) %>%
  select(-operations)

gva$month_year <- floor_date(gva$incident_date, unit = "month")

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


# N Incidents Over Time by Month ----
# Data: Local Police Reports 
# Date Range: Jan 1 2019 - May 18 2024

gv <- read_csv("data/regional_gv.csv") %>%
  mutate(reported_date = ymd(as.Date(reported_date)))

gv$month_year <- floor_date(gv$reported_date, unit = "month")

gv %>%
  group_by(month_year) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x = month_year, y = count)) +
  geom_line()

# Combine

gva_plt <- gva %>%
  filter(between(incident_date, mdy("12-31-2017"), mdy("12-31-2024"))) %>%
  group_by(month_year) %>%
  summarise(total_injured = sum(victims_injured + suspects_injured),
            total_killed = sum(victims_killed + suspects_killed)) %>%
  ungroup() %>%
  pivot_longer(matches("total"))

gv_plt <- gv %>%
  group_by(month_year) %>% 
  summarise(count=n())

# Slide version 
ggplot() +
  geom_col(data = gva_plt, aes(x = month_year, y = value, fill = name)) +
  stat_smooth(data = gv_plt, aes(x = month_year, y = count), se = FALSE, span = .2, color = "grey") +
  labs(x = "",
       y = "Number of Victims") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18)) +
  scale_fill_manual(labels = c("Injured", "Killed"),
                    values = c("#F8BE3D", "#007BAB"),
                    guide = guide_legend(title = "Victim Status")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10),
        axis.text.x = element_text(angle = 25))

# One-pager version
ggplot() +
  geom_col(data = gva_plt, aes(x = month_year, y = value, fill = name)) +
  stat_smooth(data = gv_plt, aes(x = month_year, y = count), se = FALSE, span = .2, color = "darkgrey") +
  labs(x = "",
       y = "",
       title = "Incidents of Gun Violence in Charlottesville & Albemarle and Number of Victims by Month",
       caption = "Source: Gun Violence Archive (Victim Statuses) & Local Police Reports (Incident Counts)") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(labels = c("Injured", "Killed"),
                    values = c("#F8BE3D", "#007BAB"),
                    guide = guide_legend(title = "Victim Status")) +
  theme_bw() +
  theme(legend.position = c(.88, .85),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25)) +
  annotate("label", x = mdy("1-1-2019"), y = 16, 
           label = paste("Number of incidents (shots\nfired, assaults, and homicides"),
           size = 3) +
  geom_segment(aes(x = mdy("1-1-2019"), y = 15, xend = mdy("1-10-2020"), yend = 12),
               arrow = arrow(length = unit(0.25, "cm"))) 


# N incidents 
gv_narrow <- gv_plt %>%
  filter(between(month_year, mdy("7-31-2022"), mdy("3-31-2023")))

sum(gv_narrow$count)


# Victims and Incidents over time by YEAR -- not as helpful

gva_yr <- gva %>%
  filter(between(incident_date, mdy("12-31-2017"), mdy("12-31-2024"))) %>%
  group_by(yr = year(incident_date)) %>%
  summarise(total_injured = sum(victims_injured + suspects_injured),
            total_killed = sum(victims_killed + suspects_killed)) %>%
  ungroup() %>%
  pivot_longer(matches("total")) 

gv_yr <- gv %>%
  group_by(yr = year(reported_date)) %>% 
  summarise(count=n())


ggplot() +
  geom_col(data = gva_yr, aes(x = yr, y = value, fill = name)) +
  stat_smooth(data = gv_yr, aes(x = yr, y = count), se = FALSE, color = "darkgrey") +
  labs(x = "Year",
       y = "Number of Victims") +
  scale_fill_manual(labels = c("Injured", "Killed"),
                    values = c("#F8BE3D", "#007BAB"),
                    guide = guide_legend(title = "Victim Status")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35)) +
  scale_x_continuous(breaks = gva_yr$yr)


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
  filter(cpov_est > 31 |
         percent_incidents > 5)

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
       caption = "Source: American Community Survey 2022 & Local Police Reports (2019-2024)",
       size = "Estimated Population") +
  guides(size = "none") +
  theme_bw() +
  theme(legend.position = "top")

# Theft ----

nibrs_theft <- read_csv("data/nibrs_theft.csv") %>%
  filter(district != "Virginia")

ggplot(nibrs_theft, aes(year, n_stolen, colour = district)) +
  geom_line(linewidth = 1.5) +
  labs(x = "",
       y = "Number of Firearms Stolen",
       title = "Theft of Firearms from Vehicles",
       caption = "Source: VA State Police Unified Crime Reporting") +
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
  