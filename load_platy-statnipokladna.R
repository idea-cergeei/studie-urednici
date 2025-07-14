library(statnipokladna)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(nanoparquet)
library(tidyr)

options(statnipokladna.dest_dir = "data-input/budget_data")
orgs <- read_parquet("data-interim/sp_orgs.parquet")

orgs_uo <- orgs |> filter(poddruhuj_nazev == "OSS - správce kapitoly")

rozp0 <- sp_get_table("budget-central", 2015:2023, 12)

rozp <- rozp0 |>
    filter(str_detect(polozka, "^50")) |>
    sp_add_codelist("polozka") |>
    sp_add_codelist(orgs) |>
    sp_add_codelist("kapitola") |>
    mutate(kapitola_nazev = str_squish(kapitola_nazev))

names(rozp)

rozp |>
  filter(vykaz_year %in% 2022:2023,
         orgs_nazev %in% c("Ministerstvo obrany", "Ministerstvo vnitra",
                           "Ministerstvo financí", "Ministerstvo spravedlnosti"),
         podseskupeni == "Platy") |>
  count(vykaz_year, orgs_nazev, polozka == 5012, wt = budget_spending/1e6) |>
  spread(vykaz_year, n)

rozp |>
  count(seskupeni, podseskupeni)

rozp_sum <- rozp |>
    group_by(vykaz_year, vykaz_date, polozka, polozka_nazev, kapitola,
             kapitola_nazev, orgs_nazev,
             druhuj_nazev, poddruhuj_nazev,
             ico, seskupeni, podseskupeni) |>
    summarise(across(starts_with("budget_"), sum), .groups = "drop")

names(rozp_sum)

rozp_sum |>
    filter(podseskupeni == "Platy") |>
    count(polozka, polozka_nazev)

names(rozp_sum)

count(rozp_sum, seskupeni) |> filter(str_detect(seskupeni, "Plat"))

rozp_sum |>
    count(vykaz_year, wt = budget_spending)

rozp_sum_uo <- rozp_sum |>
    filter(str_detect(polozka, "^50")) |>
    # filter(seskupeni == "Platy a podobné a související výdaje") |>
    # filter(podseskupeni %in% c("Platy", "Ostatní platby za provedenou práci")) |>
    filter(podseskupeni %in% c("Platy")) |>
    filter(polozka != "5012" | polozka == 5012 & kapitola_nazev == "Ministerstvo vnitra") |>
    group_by(kapitola_nazev, druhuj_nazev, kapitola,
        #   orgs_nazev,
        vykaz_year,
        poddruhuj_nazev,
    ) |>
  summarise(schv = sum(budget_adopted),
            uprav = sum(budget_amended),
            skut = sum(budget_spending), .groups = "drop") |>
    filter(
        # str_detect(orgs_nazev, "Ministerstvo"),
        druhuj_nazev == "OSS",
        poddruhuj_nazev == "OSS - správce kapitoly",
        # orgs_uo_nazev == "Ministerstvo obrany",
        # vykaz_year == 2023
    ) |>
  mutate(promenna = "platy",
         kap_kod = str_sub(kapitola, 2, 4), rok = as.numeric(vykaz_year)) |>
  select(kap_kod, schv, uprav, skut, rok, promenna)

# sp_get_codelist("polozka") |>
#   mutate(today = Sys.Date()) |>
#   filter(between(today, start_date, end_date)) |>
#   filter(podseskupeni == "Platy") |>
#   distinct(polozka, nazev)

rozp_sum_uo_oppp <- rozp_sum |>
  filter(str_detect(polozka, "^50")) |>
  # filter(seskupeni == "Platy a podobné a související výdaje") |>
  # filter(podseskupeni %in% c("Platy", "Ostatní platby za provedenou práci")) |>
  filter(podseskupeni %in% c("Výdaje na ostatní platby za provedenou práci",
                             "Ostatní platby za provedenou práci")) |>
  filter(polozka != "5012") |>
  group_by(kapitola_nazev, druhuj_nazev, kapitola,
           #   orgs_nazev,
           vykaz_year,
           poddruhuj_nazev,
  ) |>
  summarise(schv = sum(budget_adopted),
            uprav = sum(budget_amended),
            skut = sum(budget_spending), .groups = "drop") |>
  filter(
    # str_detect(orgs_nazev, "Ministerstvo"),
    druhuj_nazev == "OSS",
    poddruhuj_nazev == "OSS - správce kapitoly",
    # orgs_uo_nazev == "Ministerstvo obrany",
    # vykaz_year == 2023
  ) |>
  mutate(promenna = "oppp",
         kap_kod = str_sub(kapitola, 2, 4), rok = as.numeric(vykaz_year)) |>
  select(kap_kod, schv, uprav, skut, rok, promenna)

write_rds(rozp_sum_uo, "data-interim/sp_platy_uo.rds")
write_rds(rozp_sum_uo_oppp, "data-interim/sp_oppp_uo.rds")
