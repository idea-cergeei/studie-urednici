library(nanoparquet)
library(dplyr)
library(ggplot2)

# Load the parquet file
data <- read_parquet("data-export/data_all.parquet")

# Filter for ministries and calculate 2023 to 2024 real pay changes
ministry_pay_changes_2024 <- data %>%
  filter(kategorie_2014_cz == "Ministerstva", faze_rozpoctu == "SKUT", rok %in% c(2023, 2024)) %>%
  group_by(kap_zkr, kap_nazev) %>%
  arrange(rok) %>%
  summarise(
    pay_2023 = first(prumerny_plat_c2024),
    pay_2024 = last(prumerny_plat_c2024),
    real_change_percent = ((pay_2024 - pay_2023) / pay_2023) * 100,
    .groups = "drop"
  ) %>%
  filter(complete.cases(real_change_percent)) %>%
  arrange(real_change_percent)

cat("Ministry pay changes 2023-2024:\n")
print(ministry_pay_changes_2024 %>% select(kap_zkr, kap_nazev, real_change_percent))

# Get top 5 smallest and largest increases
smallest_5 <- head(ministry_pay_changes_2024, 5)
largest_5 <- tail(ministry_pay_changes_2024, 5)
top_bottom <- bind_rows(smallest_5, largest_5)

# Create labels and change type
top_bottom <- top_bottom %>%
  mutate(
    change_type = ifelse(real_change_percent < 0, "Nejmenší růst", "Největší růst")
  ) %>%
  arrange(real_change_percent)

cat("\nTop and bottom ministries by real pay change 2023-2024:\n")
print(top_bottom %>% select(kap_zkr, real_change_percent))

# Create the plot
p <- ggplot(top_bottom, aes(x = reorder(kap_zkr, real_change_percent), y = real_change_percent, fill = change_type)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Nejmenší růst" = "#d73027", "Největší růst" = "#1a9850")) +
  coord_flip() +
  labs(
    title = "Ministerstva s nejmenšími a největšími reálnými změnami platů",
    subtitle = "Změna průměrných platů v reálných cenách (2023-2024)",
    x = "Ministerstvo",
    y = "Reálná změna průměrného platu (%)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_text(aes(label = paste0(round(real_change_percent, 1), "%")), 
            hjust = ifelse(top_bottom$real_change_percent > 0, -0.1, 1.1), 
            size = 3.5)

print(p)

# Save the plot
ggsave("ministry_pay_changes_2023_2024.png", plot = p, width = 12, height = 8, dpi = 300)
cat("\nPlot saved as ministry_pay_changes_2023_2024.png\n")