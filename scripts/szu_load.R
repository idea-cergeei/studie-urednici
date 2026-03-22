library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(purrr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forcats)
library(nanoparquet)

options(scipen = 100)

szu_names <- c("kap_kod", "kap_zkr", "kap_name",
               "skut_min", "schv", "uprav", "skut",
               "plneni_proc", "plneni_rozdil", "skut_skut",
               "rok", "list", "tabulka", "file")

szu_excel_sheets_listed <- tibble(file = list.files("data-input/szu/",
                                                    pattern = "\\.xls$",
                                                    full.names = TRUE)) |>
  bind_rows(tibble(file = list.files("data-input/szu-eklep/",
                                     pattern = "\\.xls$",
                                     full.names = TRUE))) |>
  mutate(rok = str_extract(file, "20[012][0-9]"),
         sheets = map(file, readxl::excel_sheets)) |>
  unnest(cols = sheets) |>
  mutate(col_names = list(szu_names))

names(szu_excel_sheets_listed)

szu_excel_sheets_listed |>
  filter(str_detect(sheets, "([Tt]ab1[01])|(Str\\.)")) |>
  count(rok, file)

read_szu_excel <- function(path, sheet, year, col_names) {
  print(year)
  print(path)
  row1 <- read_excel(path, sheet, range = "A1:J3", col_names = FALSE) |>
    janitor::remove_empty("cols") |>
    janitor::remove_empty("rows") |>
    janitor::clean_names() |>
    gather()
  print(row1)
  tabulka_c <- case_when(any(str_detect(row1$value, "10")) ~ "10",
                         any(str_detect(row1$value, "11")) ~ "11",
                         .default = "none")
  dta <- read_excel(path, sheet, skip = ifelse(year < 2023, 11, 7)) |>
    janitor::remove_empty(which = "rows") |>
    janitor::remove_empty(which = "cols") |>
    mutate(rok = year, list = sheet,
           tabulka = tabulka_c,
           file = basename(path),)

  print(col_names)
  col_names_new <- col_names[col_names != "kap_zkr"]
  print(col_names_new)
  real_names <- if(year < 2022) col_names else col_names_new
  print(real_names)

  print(head(dta))

  dta1 <- dta |>
    rlang::set_names(real_names)

  if(!"kap_zkr" %in% names(dta1)) dta1 <- dta1 |> mutate(kap_zkr = NA_character_)

  return(dta1)

}


load_szu_sheet <- function(sheet_tibble, detection_string) {
  sheet_data <- sheet_tibble |>
    mutate(file_hash = tools::md5sum(file)) |>
    filter(str_detect(sheets, detection_string), str_detect(sheets, "Tab1[10]|Str")) |>
    distinct(file_hash, sheets, rok, .keep_all = TRUE) |>
    select(path = file, sheet = sheets, year = rok, col_names)

  dta0 <- pmap_dfr(sheet_data, read_szu_excel) |>
    janitor::clean_names() |>
    group_by(rok, list, file) |>
    mutate(promenna = case_match(tabulka,
                                 "11" ~ "platy_a_oppp",
                                 "10" ~ "pocet_zamestnancu",
                                 .default = "none")) |>
    ungroup()

  print(dta0)

  dta <- dta0 |>
    drop_na(kap_kod) |>
    mutate(skut_min = if_else(rok < 2013 & promenna == "platy_a_oppp", skut_min * 1000, skut_min),
           schv = if_else(rok < 2013 & promenna == "platy_a_oppp", schv * 1000, schv),
           uprav = if_else(rok < 2013 & promenna == "platy_a_oppp", uprav * 1000, uprav),
           skut = if_else(rok < 2013 & promenna == "platy_a_oppp", skut * 1000, skut),
           rok = as.integer(rok),
           kap_name = if_else(is.na(kap_name), kap_zkr, kap_name))

  return(dta)
}

szu_augment <- function(szu) {
  szu_augmented <- szu |>
    rows_update(read_csv("data-input/kapitoly.csv") |>
                  select(-kap_uo, -kap_vladni, -kap_ancient),
                by = "kap_kod") |>
    relocate(rok, .after = kap_name) |>
    mutate(kap_mini = str_detect(kap_name, "^Minis|Úřad [Vv]lád"),
           date = make_date(rok, 1, 1))

  return(szu_augmented)
}

szu_uo <- load_szu_sheet(szu_excel_sheets_listed, "ÚO")
szu_oss_sum <- load_szu_sheet(szu_excel_sheets_listed, "OSS sum")
# load_szu_sheet(szu_excel_sheets, "PO sum")
# load_szu_sheet(szu_excel_sheets, "ST.SPRÁVA")

szu_oss_sum |>
  count(promenna)

szu_uo |>
  count(tabulka, rok) |>
  arrange(desc(tabulka))

szu_oss_sum |>
  count(rok, kap_kod, promenna) |>
  count(n)

szu_uo |>
  count(rok, wt = is.na(kap_name))

szu_uo |>
  group_by(rok, kap_kod, promenna) |>
  filter(n() > 1) |>
  arrange(kap_kod)

rozp_sum_uo <- read_rds("data-interim/sp_platy_uo.rds")
deflators <- read_rds("data-interim/inflace.rds")
wages <- read_rds("data-interim/wages.rds")

szu_komplet <- bind_rows(szu_uo, rozp_sum_uo |> filter(rok == 2023)) |>
  select(c(schv, uprav, skut, kap_kod, rok, promenna)) |>
  pivot_longer(cols = c(schv, uprav, skut), names_to = "faze_rozpoctu") |>
  pivot_wider(values_from = value, names_from = promenna)

szu_komplet_wide <- szu_komplet |>
  pivot_wider(values_from = c(pocet_zamestnancu, platy, platy_a_oppp), names_from = faze_rozpoctu) |>
  mutate(prumerny_plat_skut = platy_skut/pocet_zamestnancu_skut/12,
         prumerny_plat_schv = platy_schv/pocet_zamestnancu_schv/12,
         kategorie = "UO") |>
  left_join(deflators, by = join_by(rok)) |>
  left_join(wages, by = join_by(rok)) |>
  left_join(read_csv("data-input/kapitoly.csv", col_types = "ccc") |>
              select(-kap_uo, -kap_vladni, -kap_ancient),
            by = join_by(kap_kod)) |>
  mutate(kap_mini = str_detect(kap_name, "^Minis|Úřad [Vv]lád"))

range(szu_komplet$rok)

szu_make_wide <- function(data, deflators, wages, add_previous = FALSE) {

  if (add_previous) {
    data_2009 <- data |>
      filter(rok == 2010) |>
      mutate(rok = 2009,
             skut = skut_min,
             skut_min = NA,
             plneni_proc = NA,
             plneni_rozdil = NA,
             schv = NA, uprav = NA, skut = NA)

    data <- data |>
      bind_rows(data_2009)
  }
  data |>
    pivot_wider(id_cols = c(kap_kod, kap_name, rok, promenna, kap_zkr),
                values_from = c(matches("^skut|^rozp|^plneni|^schv")),
                names_from = promenna, names_glue = "{promenna}_{.value}")
}

dta <- read_parquet("data-export/data_all.parquet")

names(dta)

dta |>
  filter(kap_zkr == "TAČR", pocet_zamestnancu > 0, faze_rozpoctu == "SKUT", rok == 2022) |>
  count(kategorie, rok) |>
  spread(rok, n)

dta_uo <- dta |>
  filter(kategorie_2014_cz %in% c("Ministerstva", "Ostatní ústřední")) |>
  select(kap_nazev, kap_zkr,
         platy, pocet_zamestnancu, platy_a_oppp,
         kap_kod, rok, faze_rozpoctu) |>
  mutate(kap_nazev = str_squish(kap_nazev)) |>
  mutate(platy = platy, kap_kod = as.character(kap_kod))

dta |>
  filter(kap_zkr == "TAČR")

dta |>
  count(kategorie_2014_cz)

dta_uo_all0 <- dta_uo |>
  bind_rows(szu_komplet |> mutate(faze_rozpoctu = toupper(faze_rozpoctu))) |>
  mutate(prumerny_plat = platy/pocet_zamestnancu/12) |>
  left_join(wages, by = join_by(rok)) |>
  left_join(deflators, by = join_by(rok)) |>
  rename(kap_name = kap_nazev) |>
  rows_update(read_csv("data-input/kapitoly.csv", col_types = "ccc") |>
                select(-kap_uo, -kap_vladni, -kap_ancient),,
              by = "kap_kod") |>
  mutate(kap_mini = str_detect(kap_name, "^Minis|Úřad [Vv]lád"),
         oppp = platy_a_oppp - platy)

kap_zkr_filter <- dta_uo_all0 |>
  group_by(kap_kod) |>
  summarise(only_2023 = any((2023 %in% rok) & !(2022 %in% rok) & (kap_kod != "364")))

dta_uo_all <- dta_uo_all0 |>
  left_join(kap_zkr_filter, by = join_by(kap_kod))

write_rds(dta_uo_all, "data-interim/szu-uo-all-do2023.rds")
