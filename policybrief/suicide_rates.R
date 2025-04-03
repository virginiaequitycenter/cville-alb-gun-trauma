# Data from CDC Underlying Cause of Death VA & US 1999 - 2023
# https://wonder.cdc.gov/ucd-icd10-expanded.html

va <- data_frame(yr = c(1999:2023),
                      va = c(7.2, 6.8, 6.5, 6.7, 6.1, 6.6, 6.7, 6.5,  6.3, 6.7, 6.6,
                               7.2, 7.5, 7.2, 7.3, 7.5, 7.5, 8, 7.9, 8, 7.7, 8.1, 8.2, 8.3, 8.4))

us <- data_frame(yr = c(1999:2023),
                      us = c(5.9, 5.9, 5.9, 5.9, 5.8, 5.7, 5.8, 5.7, 5.8, 6, 6.1,
                               6.3, 6.4, 6.6,  6.7, 6.7, 6.9, 7.1, 7.3, 7.5, 7.3, 7.4, 7.9, 8.1, 8.2))

rates <- left_join(va, us) %>%
  pivot_longer(cols = c(us, va)) 

rates %>%
  ggplot(aes(x = yr, y = value, group = name, color = name)) +
  geom_line(size = 2) +
  annotate("text", x = 2025, y = 8.4, label = "Virginia", color = "#0066b2", fontface =2) +
  annotate("text", x = 2026, y = 8.2, label = "United States", color = "#6CB4EE", fontface =2) +
  scale_color_manual(values = c("#6CB4EE", "#0066b2")) +
  theme_bw() +
  labs(x = NULL, 
       y = "Rate per 100k Population",
       title = "Rates of Firearm Suicide in the US and Virginia") +
  scale_x_continuous(n.breaks = 10) +
  theme(legend.position="none") +
  xlim(c(1999, 2028))
