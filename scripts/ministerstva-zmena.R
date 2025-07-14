library(nanoparquet)
library(dplyr)
library(tidyr)

dta <- read_parquet("data-export/data_all.parquet")

names(dta)

dta |>
  filter(rok == 2022, faze_rozpoctu == "SKUT", kategorie_2014_cz == "Ministerstva") |>
  select(rok, kap_zkr, prumerny_plat) |>
  arrange(kap_zkr)

dta |>
  filter(rok %in% c(2022, 2023), faze_rozpoctu == "SKUT", kategorie_2014_cz == "Ministerstva") |>
  select(rok, kap_zkr, prumerny_plat) |>
  arrange(kap_zkr) |>
  spread(rok, prumerny_plat) |>
  mutate(diff = `2023`-`2022`, diff_pc = diff/`2022`)

dta |>
  filter(rok %in% c(2022, 2023), faze_rozpoctu == "SKUT", kategorie_2014_cz == "Ministerstva") |>
  select(rok, kap_zkr, pocet_zamestnancu) |>
  arrange(kap_zkr) |>
  spread(rok, pocet_zamestnancu) |>
  mutate(diff = `2023`-`2022`, diff_pc = diff/`2022`)
