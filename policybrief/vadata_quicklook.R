
library(cowplot)
library(patchwork)
library(readxl)
library(tidyverse)

dv <- read_excel("policybrief/DV_SV_Data.xlsx") 

dv <- dv %>% 
  mutate(number_weapon = Services*Percent_Weapon,
         number_police = Services*Percent_Police_Report,
         Type = case_when(
           Type == "DV" ~ "Domestic Violence",
           Type == "SV" ~ "Sexual Violence"
         ))

v1 <- ggplot(dv, aes(x = Year, y = Percent_Weapon, color = Type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percent of Incidents",
       color = "Incident Type") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"))


v2 <- ggplot(dv, aes(x = Year, y = number_weapon, color = Type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) +
  scale_color_brewer(palette = "Paired") +
  labs(y = "Number of Incidents",
       color = "Incident Type", 
       x = NULL) 

v2 / v1 + plot_annotation("Weapons in Reported IPV Incidents", caption = "Data Source: VA Data",
                          theme = theme(
                            plot.title = element_text(hjust=0.5, face = "bold"),
                            plot.background = element_rect(color = "black", fill = NA)))
# something that shows these elements

ggsave("policybrief/image/va_data.png")



