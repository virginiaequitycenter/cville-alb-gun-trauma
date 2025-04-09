library(ggrepel)
library(ggforce)
library(janitor)
library(lubridate)
library(patchwork)
library(tidyverse)
library(waffle)


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

#img <- readPNG("img/paper_light.png")
# ggplot() +
#   background_image(img) +
#   geom_col(data = gva_plt, aes(x = month_year, y = value, fill = name), width = 25) +
#   stat_smooth(data = gv_plt, aes(x = month_year, y = count), se = FALSE, span = .2, color = "#2f9aa0ff") +
#   labs(x = NULL,
#        y = NULL) +
#   scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18)) +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks="1 month") +
#   scale_fill_manual(labels = c("Injured", "Killed"),
#                     values = c("#440154FF", "#7AD151FF"),
#                     guide = guide_legend(title = "Victim Status")) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   annotate("label", x = mdy("1-1-2019"), y = 16, 
#            label = paste("Number of incidents (shots\nfired, assaults, and homicides"),
#            size = 3) +
#   geom_segment(aes(x = mdy("1-1-2019"), y = 15, xend = mdy("1-10-2020"), yend = 12),
#                arrow = arrow(length = unit(0.25, "cm"))) 

# high-res for slides 
ggplot() +
  geom_col(data = gva_plt, aes(x = month_year, y = value, fill = name), width = 25) +
  stat_smooth(data = gv_plt, aes(x = month_year, y = count), se = FALSE, span = .2, color = "#2f9aa0ff") +
  labs(x = NULL,
       y = "Monthly Counts") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y", date_minor_breaks="1 month") +
  scale_fill_manual(labels = c("Injured", "Killed"),
                       values = c("#440154FF", "#7AD151FF"),
                       guide = guide_legend(title = "Victim Status")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 35),
        panel.grid.major.x = element_line(size = 1.5)) +
  annotate("label", x = mdy("11-1-2018"), y = 5.5, 
           label = paste("Gun-related 911 calls\n(ex. shots fired or assaults)"),
           size = 3) +
  geom_segment(aes(x = mdy("7-1-2018"), y = 6.7, xend = mdy("12-20-2018"), yend = 7.8),
               arrow = arrow(length = unit(0.20, "cm"))) +
  geom_vline(xintercept = mdy("3-12-2020"), linetype = "dotted") +
  annotate("label", x = mdy("11-15-2020"), y = 9, label = "COVID lockdown", size = 3) +
  geom_segment(aes(x = mdy("7-1-2020"), y = 9.6, xend = mdy("4-1-2020"), yend = 10.5),
               arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("label", x = mdy("2-15-2022"), y = 19, label = paste(
    "Hearing or witnessing gun violence takes\na collective toll on our entire community"),
    size = 3) +
  geom_segment(aes(x = mdy("12-15-2020"), y = 19, xend = mdy("7-15-2020"), yend = 17.9),
    arrow = arrow(length = unit(0.2, "cm"))) 

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
# ggplot(nibrs_theft, aes(year, n_stolen, colour = district)) +
#   geom_line(linewidth = 1.5) +
#   labs(x = "",
#        y = "Number of Firearms Stolen",
#        title = "Theft of Firearms from Vehicles",
#        caption = "Source: VA State Police Unified Crime Reporting") +
#   scale_color_manual(values = c("#007BAB", "#F8BE3D"),
#                      name = "Region") +
#   geom_label(data = nibrs_theft,
#              aes(label = n_stolen),
#              show.legend = F,
#              alpha = 0.75,
#              fontface = "bold") +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 7),
#                      guide = guide_axis(angle = 35)) +
#   theme_bw() +
#   theme(legend.position = "top")

# *Datawalk ----

labs <- nibrs_theft %>%
  filter(year != 2022)

# High res
ggplot(nibrs_theft, aes(year, n_stolen, colour = Region, linetype = Region)) +
  geom_line(linewidth = 2) +
  labs(x = NULL,
       y = "Number of Firearms Stolen") +
  scale_color_manual(values = c("#007BAB", "#B12A90FF")) +
  geom_label(data = labs, aes(label = n_stolen), show.legend = F, alpha = 1, fontface = "bold", size = 5) +
  annotate("label", x = 2022, y = 29, label = "30", color = "#B12A90FF", alpha = 1, fontface = "bold", size = 5) +
  annotate("label", x = 2022, y = 32, label = "31", color = "#007BAB", alpha = 1, fontface = "bold", size = 5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7), guide = guide_axis(angle = 35)) +
  scale_y_continuous(limits = c(0, 32)) +
  scale_linetype_manual(values=c("longdash", "1111")) +
  theme_minimal() +
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14),
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
  geom_pictogram(n_rows = 6, size = 7, aes(colour = intent_fct), flip = FALSE, make_proportional = FALSE) +
  scale_color_manual(
    values = c("#440154FF", "#FDE725FF", "#7AD151FF", "#2f9aa0ff")) +
  scale_label_pictogram(
    values = c("male")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title=element_blank()) +
  ggtitle("Firearm Deaths in the Blue Ridge Health District (2018-2022)")


# Participant Ages ----

# *Datawalk
gva_participants <- read_csv("data/gva_participants.csv") %>%
  drop_na(age) %>%
  mutate(role = str_to_title(role),
         role = case_when(
           role == "Suspect" ~ "Defendant",
           TRUE ~ role),
         age_10yr = case_when(
           age <=10 ~ "0-10", 
           age > 10 & age <= 20 ~ "11-20",
           age > 20 & age <= 30 ~ "21-30",
           age > 30 & age <= 40 ~ "31-40",
           age > 40 & age <= 50 ~ "41-50",
           age > 50 & age <= 60 ~ "51-60",
           age > 60 & age <= 70 ~ "61-70",
           age > 70 ~ "71+"),
         age_grp = case_when(
           age <= 9 ~ "0-9",
           age > 9 & age <= 14 ~ "10-14", 
           age > 14 & age <= 17 ~ "15-17",
           age > 17 & age <= 19 ~ "18-19",
           age > 19 & age <= 24 ~ "20-24",
           age > 24 & age <= 30 ~ "25-30",
           age > 30 & age <= 35 ~ "31-35",
           age > 35 & age <= 45 ~ "36-45",
           age > 45 & age <= 55 ~ "46-55",
           age > 55 & age <= 65 ~ "56-65",
           age > 65 & age <= 75 ~ "66-75",
           age > 75 ~ "76+")
         )

# Facet version
# age_labels <- c("Defendant" = "Defendants", "Victim" = "Victims")
# 
# gva_participants %>%
#   ggplot(aes(age, fill = role)) +
#   geom_histogram(position = 'identity', bins = 40) +
#   facet_grid(~role, labeller = as_labeller(age_labels)) +
#   geom_vline(xintercept = 18, color = "blue") +
#   scale_fill_manual(values = c("#7AD151FF", "#440154FF"),
#                     guide = guide_legend(title = "Participant Role")) +
#   labs(x = "Age",
#        y = "Number of People") +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
#                      guide = guide_axis(angle = 35)) +
#   scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16)) +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         legend.title = element_text(size=11),
#         legend.text = element_text(size = 10),
#         axis.title = element_text(size = 12), 
#         axis.text = element_text(size = 10),
#         strip.text.x = element_text(size = 12))

# Counts - Population pyramid 
age_dat <- gva_participants %>%
  group_by(role, age_grp) %>%
  count() %>%
  group_by(role) %>%
  mutate(n_tot = ifelse(role == 'Defendant', -n / sum(n), n / sum(n)),
         n_plt = ifelse(role == 'Defendant', -n, n))

pretty_symmetric <- function(range, n = 7){
  range_1 <- c(-range[1], range[2])
  range_2 <- c(range[1], -range[2])
  pretty_vec_1 <- pretty(range_1)
  pretty_vec_2 <- pretty(range_2)
  pretty(
    c(pretty_vec_1, pretty_vec_2), 
    n = n
  )
}

pop_range <- range(age_dat$n_plt)
range_seq <- pretty_symmetric(pop_range, n = 7)

# ggplot(age_dat, aes(n_plt, age_grp, fill = role)) +
#   geom_col() +
#   geom_vline(xintercept = 0) +
#   labs(x = "Number of People",
#        y = "Age Range",
#        fill = "Role") +
#   scale_x_continuous(breaks = age_range_seq,
#                      labels = abs(age_range_seq)) +
#   expand_limits(x = range(age_range_seq)) +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   scale_fill_manual(values = c("#2f9aa0ff", "#B12A90FF"))

# Pcts - population pyramid 
pct_range <- range(age_dat$n_tot)
pct_range_seq <- pretty_symmetric(pct_range, n = 7)

# high res
ggplot(age_dat, aes(n_tot, age_grp, fill = role)) +
  geom_col() +
  geom_text(aes(x = n_tot, y = age_grp, label = scales::percent(round(abs(n_tot), 2))), 
            hjust = ifelse(age_dat$n_tot < 0, 1, -.1)) +
  geom_vline(xintercept = 0) +
  annotate("label", x = -.2, y = "76+", label = "Total Defendants: 115", 
           fill = "#2f9aa0ff", color = "white", fontface = 2, size = 4) +
  annotate("label", x = .2, y = "76+", label = "Total Victims: 81", 
           fill = "#B12A90FF", color = "white", fontface = 2, size = 4) +
  labs(y = "Age Range",
       x = "Percent of Defendants                                                                           Percent of Victims") +
  theme_minimal() +
  scale_x_continuous(breaks = pct_range_seq,
                     labels = scales::percent(abs(pct_range_seq))) +
  expand_limits(x = range(pct_range_seq)) +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 13),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(face = "bold", size = 13),
        axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c("#2f9aa0ff", "#B12A90FF"))

# Pct with jitter 
jittered_pts = age_dat %>% 
  select(role, age_10yr, n, n_tot) %>% 
  uncount(n) %>% 
  group_by(role, age_10yr) %>% 
  mutate(x_pos = runif(n(), 0, abs(n_tot)*.98),
         x_pos = ifelse(role == 'Defendant', -x_pos, x_pos))

ggplot(age_dat, aes(n_tot, age_10yr, fill = role)) +
  geom_col() +
  ggbeeswarm::geom_quasirandom(data = jittered_pts, aes(x = x_pos), groupOnX = F) +
  geom_text(aes(x = n_tot, y = age_10yr, label = scales::percent(round(abs(n_tot), 2))), 
            hjust = ifelse(age_dat$n_tot < 0, 1, -.1)) +
  geom_vline(xintercept = 0) +
  annotate("label", x = -.4, y = "71+", label = "Total Defendants: 115", 
           fill = "#2f9aa0ff", color = "white", fontface = 2) +
  annotate("label", x = .4, y = "71+", label = "Total Victims: 81", 
           fill = "#B12A90FF", color = "white", fontface = 2) +
  geom_vline(xintercept = 0) +
  labs(y = "Age Range",
       x = "Percent of Defendants                                                Percent of Victims") +
  theme_minimal() +
  scale_x_continuous(breaks = pct_range_seq,
                     labels = scales::percent(abs(pct_range_seq))) +
  expand_limits(x = range(pct_range_seq)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#2f9aa0ff", "#B12A90FF"))

# Sina plot
sina_dat <- gva_participants %>%
  select(role, age)

sina_dat %>%
  ggplot(aes(role, age, color = role)) +
  geom_sina(size = 2) +
  geom_violin(aes(colour = role, alpha = 0.5),  size=1) +
  scale_color_manual(values = c("#2f9aa0ff", "#B12A90FF")) +
  annotate("label", x = "Defendant", y = -5, label = "Total Defendants: 115", 
           fill = "#2f9aa0ff", color = "white", fontface = 2) +
  annotate("label", x = "Victim", y = -5, label = "Total Victims: 81", 
           fill = "#B12A90FF", color = "white", fontface = 2) +
  geom_hline(yintercept = 18, color = "darkgrey", size = 1) +
  annotate("text", x = "Victim", y = 15, label = "18 Years Old") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x=element_blank()) +
  labs(x = NULL, y = "Age") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


# Pcts with patchwork
age_labs <- tibble(age = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71+")) %>%
  mutate(age = fct_inorder(age))

age_labs_plt <- age_labs %>%
  ggplot(aes(x = 1, y = age, label = age)) +
  geom_text(size = 4) +
  theme_void()

def_plt <- age_dat %>%
  ungroup() %>%
  add_row(role = "Defendant", age_10yr = "0-10", n = 0, n_tot = 0, n_plt = 0) %>%
  add_row(role = "Defendant", age_10yr = "61-70", n = 0, n_tot = 0, n_plt = 0) %>%
  filter(role == "Defendant") %>%
  ggplot(aes(x = n_tot, y = age_10yr)) +
  geom_col(fill = "#2f9aa0ff") +
  annotate("label", x = -.2, y = "71+", label = "Total Defendants: 115", fill = "#2f9aa0ff", color = "white") +
  scale_x_continuous(
    labels = function(x) label_percent(accuracy = 1)(abs(x)),
    limits = c(-.5, 0)) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    panel.grid.major.x = element_line(color = "grey90")
  )

vic_plt <- age_dat %>%
  ungroup() %>%
  add_row(role = "Victim", age_10yr = "61-70", n = 0, n_tot = 0, n_plt = 0) %>%
  filter(role == "Victim") %>%
  ggplot(aes(x = n_tot, y = age_10yr)) +
  geom_col(fill = "#B12A90FF") +
  annotate("label", x = .2, y = "71+", label = "Total Victims: 81", fill = "#B12A90FF", color = "white") +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, .5)) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    panel.grid.major.x = element_line(color = "grey90")
  )

def_plt + 
  age_labs_plt + 
  vic_plt +
  plot_layout(
    widths = c(7.5, .5, 7.5)
  )
