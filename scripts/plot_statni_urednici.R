library(nanoparquet)
library(dplyr)
library(ggplot2)

# Load the parquet file
data <- read_parquet("data-export/data_all.parquet")

# Filter for státní úředníci and summarize by year
# Use only SKUT (actual) data to avoid double counting from different budget phases
statni_urednici_by_year <- data %>%
  filter(kategorie_2014_cz == "Státní úředníci", faze_rozpoctu == "SKUT") %>%
  group_by(rok) %>%
  summarise(count = sum(pocet_zamestnancu, na.rm = TRUE), .groups = "drop") %>%
  filter(rok >= 2011) %>%
  arrange(rok)

# Print the data
cat("Počet státních úředníků podle roku:\n")
print(statni_urednici_by_year)

# Create and save the plot
p <- ggplot(statni_urednici_by_year, aes(x = rok, y = count)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  scale_x_continuous(breaks = seq(2011, max(statni_urednici_by_year$rok), 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Počet státních úředníků podle roku",
    subtitle = "Od roku 2011",
    x = "Rok",
    y = "Počet státních úředníků xx"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)

# Save the plot
ggsave("statni_urednici_plot.png", plot = p, width = 10, height = 6, dpi = 300)
cat("\nPlot saved as statni_urednici_plot.png\n")