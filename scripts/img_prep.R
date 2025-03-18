library(ggrepel)
library(janitor)
library(lubridate)
library(sf)
#library(svglite)
library(tidyverse)
library(ggpubr)
library(png)

# Format images for print media and slide decks

# Incidents and Victim Statuses ----

# GVA Victim Statuses (Jan 2018 - Dec 2024)
gva <- read_csv("data/raw/gva_incidents_24.csv") %>%
  clean_names() %>%
  mutate(incident_date = mdy(incident_date)) %>%
  select(-operations)

gva$month_year <- floor_date(gva$incident_date, unit = "month")

# N Police Incidents Over Time by Month (Jan 1 2019 - May 18 2024)
gv <- read_csv("data/regional_gv.csv") %>%
  mutate(reported_date = ymd(as.Date(reported_date)))

gv$month_year <- floor_date(gv$reported_date, unit = "month")

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

# *Datawalk ----

img <- readPNG("img/paper_light.png")

ggplot() +
  background_image(img) +
  geom_col(data = gva_plt, aes(x = month_year, y = value, fill = name), width = 25) +
  stat_smooth(data = gv_plt, aes(x = month_year, y = count), se = FALSE, span = .2, color = "#2f9aa0ff") +
  labs(x = NULL,
       y = NULL) +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks="1 month") +
  scale_fill_manual(labels = c("Injured", "Killed"),
                    values = c("#440154FF", "#7AD151FF"),
                    guide = guide_legend(title = "Victim Status")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  annotate("label", x = mdy("1-1-2019"), y = 16, 
           label = paste("Number of incidents (shots\nfired, assaults, and homicides"),
           size = 3) +
  geom_segment(aes(x = mdy("1-1-2019"), y = 15, xend = mdy("1-10-2020"), yend = 12),
               arrow = arrow(length = unit(0.25, "cm"))) 

# *One-pager ----
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

# *One-pager ----
gv_sf_summary %>%
  filter(tract_name != "JPA - Fontaine") %>%
  ggplot(aes(x = cpov_est, y = percent_incidents, size = pop_est, color = locality)) +
  geom_point(alpha = 0.8) +
  scale_size(range = c(.1, 12)) +
  geom_label_repel(data = scatter_text2, 
                   aes(x = cpov_est, y = percent_incidents, label = tract_name), size = 3, 
                   min.segment.length = unit(0, 'lines'),
                   inherit.aes = FALSE) +
  scale_color_manual(values = c("#007BAB", "#FCA636FF"), 
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
  filter(district != "Virginia") %>%
  mutate(district = factor(district)) %>%
  rename(Region = district)

# *One-pager ----
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

# *Datawalk ----
ggplot(nibrs_theft, aes(year, n_stolen, colour = Region, linetype = Region)) +
  geom_line(linewidth = 2) +
  labs(x = "",
       y = "Number of Firearms Stolen") +
  scale_color_manual(values = c("#007BAB", "#B12A90FF")) +
  geom_label(data = nibrs_theft, aes(label = n_stolen), show.legend = F, alpha = 1, fontface = "bold", size = 4) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7), guide = guide_axis(angle = 35)) +
  scale_y_continuous(limits = c(0, 32)) +
  scale_linetype_manual(values=c("longdash", "1111")) +
  theme_minimal() +
  labs(x=NULL) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 10),
        legend.margin = ggplot2::margin(t=1, unit = "pt"),
        legend.position = 'bottom')

# Deaths by Intent----

# *Datawalk ----

# Setup Font Awesome
# fonts()[grep("Awesome", fonts())]
# fonttable() %>% 
#   dplyr::filter(stringr::str_detect(FamilyName,"^Font.")) %>% 
#   select(FontName, fontfile)

# library(showtext)
# font_add(family = "FontAwesome5Free-Solid", regular = "/Users/stoet/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/waffle/fonts/fa-solid-900.ttf")
# font_add(family = "FontAwesome5Free-Regular", regular = "/Users/sct2td/Library/Fonts/fa-regular-400.ttf")
# font_add(family = "FontAwesome5Brands-Regular", regular = "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/waffle/fonts/fa-brands-400.ttf")
# showtext_auto()

vdh_intent <- read_csv("data/vdh_deaths_intent.csv") %>%
  rename(intent = intent_of_injury) %>%
  mutate(intent = case_when(intent == "Undetermined/legal/war" ~ "Undetermined or \nPolice Intervention",
                            intent == "Unintentional" ~ "Accidental",
                            TRUE ~ intent))

vdh_parts <- vdh_intent %>%
  select(intent, firearm_deaths) %>%
  arrange(desc(firearm_deaths)) %>%
  mutate(intent_fct = fct_inorder(intent))

vdh_parts %>%
  ggplot(aes(label = intent_fct, values = firearm_deaths)) +
  geom_pictogram(n_rows = 6, aes(colour = intent_fct), flip = FALSE, make_proportional = FALSE) +
  scale_color_manual(
    values = c("#440154FF", "#FDE725FF", "#7AD151FF", "#2f9aa0ff")) +
  scale_label_pictogram(
    values = c("male")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title=element_blank())


# Participant Ages ----

# *Datawalk
gva_participants <- read_csv("data/gva_participants.csv") %>%
  drop_na(age) %>%
  mutate(role = str_to_title(role),
         role = case_when(role == "Suspect" ~ "Defendant",
         TRUE ~ role))

# Facet version
age_labels <- c("Defendant" = "Defendants", "Victim" = "Victims")

gva_participants %>%
  ggplot(aes(age, fill = role)) +
  geom_histogram(position = 'identity', bins = 40) +
  facet_grid(~role, labeller = as_labeller(age_labels)) +
  geom_vline(xintercept = 18, color = "blue") +
  scale_fill_manual(values = c("#7AD151FF", "#440154FF"),
                    guide = guide_legend(title = "Participant Role")) +
  labs(x = "Age",
       y = "Number of People") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     guide = guide_axis(angle = 35)) +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size=11),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10),
        strip.text.x = element_text(size = 12))
