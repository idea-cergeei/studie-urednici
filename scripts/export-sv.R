library(dplyr)
library(writexl)

dta <- nanoparquet::read_parquet("data-export/data_all.parquet")

dta |>
  filter(faze_rozpoctu == "SKUT", rok == 2022, kategorie == "UO") |>
  select(rok, kap_kod, kap_zkr, platy, prumerny_plat, pocet_zamestnancu, kategorie,
         kap_nazev, prumerna_mzda_pha, prumerny_plat_vucinh,
         prumerny_plat_vucinh_mezirocne, ceny_index, ceny_inflace) |>
  writexl::write_xlsx("data-export/szu2022-ustredni-pro-sv.xlsx")
