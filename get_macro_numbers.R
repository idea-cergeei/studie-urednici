library(czso)
library(dplyr)
library(cnbrrr) # remotes::install_github("petrbouchal/cnbrrr")

rok <- config::get("rok")

macro_numbers <- list()

# Get total employed persons in Czech Republic for 2024 using CZSO Labour Force Survey

# Download Labour Force Survey data
czso_get_catalogue('250180') # Check if the dataset is available
lfs_data <- czso_get_table('250180')


# Filter for total employed persons in 2024
employed_total <- lfs_data %>%
  filter(
    rok > 2021,
    stapro_txt == "Zaměstnaní",
    is.na(pohlavi_kod), # no gender breakdown
    ekak_kod == "3", # total category
    uzemi_txt == "Česká republika"
  ) %>%
  arrange(desc(rok), desc(ctvrtleti))

employed_total_thisyr <- employed_total %>%
  filter(.data$rok == .env$rok, ctvrtleti == 4) |>
  pull(hodnota)

macro_numbers$employed_total <- employed_total_thisyr * 1000

cnb_gdp <- cnbrrr::arad_get_data(
  indicator_ids = c("SHDPVYDY1B1GNMLSA", "SHDPZDRY1B1GMMLNA"),
  period_from = "20101231"
)

cnb_gdp_thisyr <- cnb_gdp %>%
  filter(year == rok, indicator_id == "SHDPZDRY1B1GMMLNA") %>%
  select(indicator_id, value) %>%
  mutate(value = value * 1e6) |>
  pull(value)

macro_numbers$gdp <- cnb_gdp_thisyr

vydaje_sr <- cnbrrr::arad_get_data(
  indicator_ids = "SRUMD08402C",
  period_from = "20031231",
  rename_value = "vydaje_sr"
) |>
  filter(month == 12)

vydaje_sr_thisyr <- vydaje_sr %>%
  filter(year == rok) |>
  pull(vydaje_sr)

macro_numbers$sr_vydaje <- vydaje_sr_thisyr * 1e6

macro_numbers

readr::write_rds(macro_numbers, "data-interim/macro_numbers.rds")