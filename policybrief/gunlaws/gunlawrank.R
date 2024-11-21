library(tidyverse)
library(janitor)
library(patchwork)
library(ggpol)
library(ggtext)

# from https://everytownresearch.org/rankings/methodology/
gvlaws <- read_csv("Everytown-gun-laws-save-lives.csv") %>% 
  clean_names() %>% 
  rename(gun_law_strength = strength_of_gun_laws_out_of_100_points,
         gun_death_rate = gun_deaths_per_100_000_residents) %>% 
  mutate(region = case_when(
    label %in% c("AL", "AR", "DE", "FL", "GA", "KY", "LA", "MD", 
                   "MS", "NC", "OK", "SC", "TN", "TX", "WV") ~ "South",
    label == "VA" ~ "VA",
    TRUE ~ "Not South"))

laws <- ggplot(gvlaws, aes (y = fct_reorder(label, gun_law_strength), x = gun_law_strength)) +
  geom_col(aes(fill = region)) +
  scale_fill_manual(values = c("grey", "orange", "blue")) +
  labs(x = "Strength of Gun Laws (out of 100 points)",
       y = "", fill = "") +
  theme_minimal()

ggplot(gvlaws, aes (y = gun_death_rate, x = gun_law_strength)) +
  geom_point(aes(color = region)) +
  scale_color_manual(values = c("grey", "orange", "blue")) +
  labs(x = "Strength of Gun Laws (out of 100 points",
       y = "Gun Deaths per 100,000 Residents") +
  theme_minimal()

rates <- ggplot(gvlaws, aes (y = fct_reorder(label, gun_law_strength), x = gun_death_rate)) +
  geom_col(aes(fill = region)) +
  scale_fill_manual(values = c("grey", "orange", "blue")) +
  labs(x = "Gun Death Rates per 100,000 Residents",
       y = "", fill = "") +
  theme_minimal()

laws + rates

# butterfly version ----
gvlaws_long <- gvlaws %>%
  mutate(rank = rank(desc(gun_law_strength), ties.method = 'first')) %>% 
  pivot_longer(cols = c(gun_law_strength, gun_death_rate), 
               names_to = "variable", values_to = "value") %>% 
  mutate(value_mirror = ifelse(variable == "gun_death_rate", -value, value))
  

# fix facet titles
var_names <- as_labeller(
  c(`gun_death_rate` = "Gun Death Rate (per 100K residents)", 
  `gun_law_strength` = "Strength of Gun Laws (out of 100 points)"))

ggplot() +
  geom_bar(gvlaws_long, 
           mapping = aes(x = fct_reorder(label, -rank), y = value_mirror, fill = region),
           stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("grey", "orange", "blue")) +  # Apply custom colors here
  facet_share(~ variable,
              dir = "h",
              scales = "free",
              reverse_num = TRUE,
              labeller = var_names) +
  labs(title = "Gun Laws and Gun Deaths in U.S. States",
       subtitle = "Comparing <span style = 'color: blue;'>Virginia</span>, <span style = 'color: orange;'>Southern States</span> and <span style = 'color: grey40;'>Non-Southern States</span>",
       caption = "Data from Everytown Research") +
  coord_flip() +
  theme_light() +
  theme(plot.subtitle = element_markdown(),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = rel(1.2),
                                  # face = "bold",
                                  color = "black"),
        strip.background = element_rect(fill = "white", colour = "black", size = 0)
  )
