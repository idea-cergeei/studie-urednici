library(dplyr)
library(ggplot2)

# Use the datax object already loaded in the environment
# Create slope chart comparing 2003 vs 2024 employment levels

# Prepare data for slope chart
slope_data <- datax %>%
  filter(rok %in% c(2003, 2024)) %>%
  group_by(kap_zkr) %>%
  summarise(
    emp_2003 = sum(pocet_zamestnancu[rok == 2003], na.rm = TRUE),
    emp_2024 = sum(pocet_zamestnancu[rok == 2024], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(emp_2003 > 0 & emp_2024 > 0) %>%  # Only ministries with data in both years
  mutate(
    change = emp_2024 - emp_2003,
    change_percent = ((emp_2024 - emp_2003) / emp_2003) * 100,
    change_type = case_when(
      change_percent > 10 ~ "Velký růst (>10%)",
      change_percent > 0 ~ "Mírný růst (0-10%)",
      change_percent > -10 ~ "Mírný pokles (0 až -10%)",
      TRUE ~ "Velký pokles (<-10%)"
    )
  ) %>%
  arrange(desc(emp_2024))

cat("Employment changes 2003-2024:\n")
print(slope_data %>% select(kap_zkr, emp_2003, emp_2024, change_percent))

# Create slope chart
p <- ggplot(slope_data) +
  # Lines connecting 2003 to 2024
  geom_segment(aes(x = 1, xend = 2, y = emp_2003, yend = emp_2024, color = change_type),
               size = 1.2, alpha = 0.8) +
  # Points for 2003
  geom_point(aes(x = 1, y = emp_2003, color = change_type), size = 3) +
  # Points for 2024
  geom_point(aes(x = 2, y = emp_2024, color = change_type), size = 3) +
  # Labels for ministry codes
  geom_text(aes(x = 0.95, y = emp_2003, label = kap_zkr), 
            hjust = 1, size = 2.8, alpha = 0.8) +
  geom_text(aes(x = 2.05, y = emp_2024, label = kap_zkr), 
            hjust = 0, size = 2.8, alpha = 0.8) +
  # Styling
  scale_color_manual(values = c(
    "Velký růst (>10%)" = "#1a9850",
    "Mírný růst (0-10%)" = "#91bfdb", 
    "Mírný pokles (0 až -10%)" = "#fc8d59",
    "Velký pokles (<-10%)" = "#d73027"
  )) +
  scale_x_continuous(breaks = c(1, 2), labels = c("2003", "2024"), limits = c(0.8, 2.3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Změny zaměstnanosti podle ministerstva",
    subtitle = "Porovnání počtu zaměstnanců mezi roky 2003 a 2024",
    x = "",
    y = "Počet zaměstnanců",
    color = "Typ změny"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(p)

# Save the plot
ggsave("ministry_employment_slope_chart.png", plot = p, width = 12, height = 10, dpi = 300)
cat("\nPlot saved as ministry_employment_slope_chart.png\n")

# Print summary statistics
cat("\nSummary by change type:\n")
summary_stats <- slope_data %>%
  group_by(change_type) %>%
  summarise(
    count = n(),
    avg_change_percent = round(mean(change_percent), 1),
    .groups = "drop"
  )
print(summary_stats)