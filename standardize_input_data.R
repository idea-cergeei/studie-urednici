library(dplyr)
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(here)
library(stringr)
library(janitor)
library(czso)
source(here::here("R", "listSheets.R"))
source(here::here("R", "divideMain.R"))
library(config)
cfg <- config::get()
# derive target year from config; fallback to 2024 if absent
this_year <- if (!is.null(cfg$rok)) cfg$rok else 2024
options("scipen" = 100, "digits" = 4)
# readr::read_csv("http://vdb.czso.cz/pll/eweb/lkod_ld.seznam")
# catalogue <- czso_get_catalogue()

# catalogue %>%
#   filter(str_detect(title, "[Mm]zd[ay]")) %>%
#   select(dataset_id, title, description)

# load new data
input   <- read_excel_allsheets("./data-input/data_2023.xlsx")
input21 <- read_excel_allsheets("./data-input/Data_2022.xls")
# allow the config file to override the path for the current year's workbook
input_path <- if (!is.null(cfg$data_path) && nzchar(cfg$data_path)) {
  cfg$data_path
} else {
  paste0("./data-input/Data_", this_year, ".xlsx")
}
input_thisyr <- read_excel_allsheets(input_path)

# load old data
chapters_old <- read.csv("./data-input/legacy/chapters_ALL.csv", encoding = "UTF-8")
grp_old <- read.csv("./data-input/legacy/groups_ALL.csv", encoding = "UTF-8")

sapply(input, ncol)


# divide sheets to groups with similar format
main_sheets <- c("ROPO CELKEM", "OSS (RO)", "PO", "OOSS", "STATNI SPRAVA", "UO", "OSS SS", "SOBCPO")
sub_sheets <- c("ZAMCI_5011_platy", "VOJACI_5012", "ST_ZAMCI_5013", "ST_ZASTUP_5014", "UC_S_5022")
jednotl_sheets <- c("SOBCPO  JEDNOTLIVY", "OSS SS - jednotl")

main_names_thisyr <- vector(mode = "list")
polozky_names_thisyr <- vector(mode = "list")
jednotl_names_thisyr <- vector(mode = "list")

for (name in names(input_thisyr)) {
  print(name)
  if (name %in% main_sheets) {
    main_names_thisyr <- append(main_names_thisyr, input_thisyr[name])
  } else if (name %in% sub_sheets) {
    polozky_names_thisyr <- append(polozky_names_thisyr, input_thisyr[name])
  } else if (name %in% jednotl_sheets) {
    jednotl_names_thisyr <- append(jednotl_names_thisyr, input_thisyr[name])
  }
}
# load excel data into clean dataframe

# main sheets
section_names <- c(
  "Rozpocet_a_rok", "full_name", "kap_num", "kap_name", "Prostredky na platy a OPPP", "OPPP", "Prostredky na platy",
  "Pocet zamestnancu", "Prumerný plat", "Poradí prumerného platu", "Schv ke schv", "Skut k rozp", "Skut ke skut"
)

main_df <- data.frame(matrix(ncol = length(section_names), nrow = 0))
colnames(main_df) <- section_names

## populate lists for the previous-year data ("input") so main_names is defined
main_names <- vector(mode = "list")
polozky_names <- vector(mode = "list")
jednotl_names <- vector(mode = "list")

for (name in names(input)) {
  print(name)
  if (name %in% main_sheets) {
    main_names <- append(main_names, input[name])
  } else if (name %in% sub_sheets) {
    polozky_names <- append(polozky_names, input[name])
  } else if (name %in% jednotl_sheets) {
    jednotl_names <- append(jednotl_names, input[name])
  }
}

for (i in seq_along(main_names)) {
  res <- divide_sections(main_names[[i]], names(main_names[i]), section_names)
  main_df <- rbind(main_df, res)
}

main_names21 <- vector(mode = "list")
polozky_names21 <- vector(mode = "list")
jednotl_names21 <- vector(mode = "list")

for (name in names(input21)) {
  print(name)
  if (name %in% main_sheets) {
    main_names21 <- append(main_names21, input21[name])
  } else if (name %in% sub_sheets) {
    polozky_names21 <- append(polozky_names21, input21[name])
  } else if (name %in% jednotl_sheets) {
    jednotl_names21 <- append(jednotl_names21, input21[name])
  }
}


## (main_names_thisyr already populated above by reading input_thisyr)

# load excel data into clean dataframe

main_df21 <- data.frame(matrix(ncol = length(section_names), nrow = 0))
colnames(main_df21) <- section_names

for (i in seq_along(main_names21)) {
  res21 <- divide_sections(main_names21[[i]], names(main_names21[i]), section_names)
  main_df21 <- rbind(main_df21, res21)
}

main_df_thisyr <- data.frame(matrix(ncol = length(section_names), nrow = 0))
colnames(main_df_thisyr) <- section_names

for (i in seq_along(main_names_thisyr)) {
  res24 <- divide_sections(main_names_thisyr[[i]], names(main_names_thisyr[i]), section_names)
  main_df_thisyr <- rbind(main_df_thisyr, res24)
}

# sheets with various types of polozky
# polozky <- data.frame(matrix(ncol = length(section_names), nrow = 0))
# colnames(polozky) <- section_names
#
# for (i in 1:length(polozky_names)) {
#   res <- divide_sections(polozky_names[[i]], names(polozky_names[i]), section_names)
#   polozky <- rbind(polozky, res)
# }

# sheets with subgroups

jednotl_section_names <- c(
  "Rozpocet_a_rok", "full_name", "kap_num", "kap_name", "Organizace", "Prostredky na platy a OPPP", "OPPP", "Prostredky na platy",
  "Pocet zamestnancu", "Prumerný plat", "Poradí prumerného platu", "Schv ke schv", "Skut k rozp", "Skut ke skut"
)
jednotl_df <- data.frame(matrix(ncol = length(jednotl_section_names), nrow = 0))
for (i in seq_along(jednotl_names)) {
  res <- divide_jednotl(jednotl_names[[i]], names(jednotl_names[i]), jednotl_section_names)
  jednotl_df <- rbind(jednotl_df, res)
}

jednotl_df21 <- data.frame(matrix(ncol = length(jednotl_section_names), nrow = 0))
for (i in seq_along(jednotl_names21)) {
  res21 <- divide_jednotl(jednotl_names21[[i]], names(jednotl_names21[i]), jednotl_section_names)
  jednotl_df21 <- rbind(jednotl_df21, res21)
}

# jednotl_df24 <- data.frame(matrix(ncol = length(jednotl_section_names), nrow = 0))
# for (i in 1:length(jednotl_names24)) {
#   print(names(jednotl_names24[i]))
#   res24 <- divide_jednotl(jednotl_names24[[i]], names(jednotl_names24[i]), jednotl_section_names)
#   jednotl_df24 <- rbind(jednotl_df24, res24)
# }


# summary sheet
summary_names <- c(
  "Rozpocet_a_rok", "full_name", "Prostredky na platy a OPPP", "OPPP", "Prostredky na platy",
  "Pocet zamestnancu", "Prumerný plat", "Poradí prumerného platu", "Schv ke schv", "Skut k rozp", "Skut ke skut"
)

summary <- divide_summary(input[[length(input)]], "Summary", summary_names)
summary21 <- divide_summary(input[[length(input21)]], "Summary", summary_names)
summary_thisyr <- divide_summary(input[[length(input_thisyr)]], "Summary", summary_names)

main_df <- bind_rows(
  main_df,
  main_df21 |> filter(rok == 2021),
  main_df_thisyr |> filter(rok %in% as.character(c(2024, this_year)))
  )

jednotl_df <- bind_rows(
  jednotl_df,
  jednotl_df21 |> filter(rok == 2021),
  # jednotl_df_thisyr |> filter(rok == as.character(c(2024, this_year)))
  )

summary <- bind_rows(
  summary,
  summary21 |> filter(rok == 2021),
  summary_thisyr |> filter(rok %in% as.character(c(2024, this_year)))
  )

# now we will extend the new datasets with data from previous years

# chapter dataframe
kap_slovnik <- main_df %>%
  select(kap_num, kap_name, full_kap_name, cz_kap_name) %>%
  filter(!is.na(kap_num)) %>%
  filter(!(kap_name == "TAČR" & kap_num == 378)) |>
  rows_update(tribble(~kap_num, ~kap_name, ~full_kap_name, ~cz_kap_name,
                      364, "DIA", "digitalni_a_informacni_agentura", "Digitální a informační agentura")) |>
  unique()
df_addition <- data.frame(
  c(347, 338, 341), c("KCP", "MI", "UVIS"),
  c("komise_pro_cenne_papiry", "ministerstvo_informatiky", "urad_pro_verejne_informacni_systemy"),
  c("Komise pro cenn\u00E9 pap\u00EDry", "Ministerstvo informatiky", "\u00DA\u0159ad pro ve\u0159ejn\u00E9 informa\u010Dn\u00ED syst\u00E9my")
)
names(df_addition) <- c("kap_num", "kap_name", "full_kap_name", "cz_kap_name")
kap_slovnik <- rbind(kap_slovnik, df_addition) |> unique()


old_dt <- chapters_old %>%
  select(rok = Year, name = grp, kap_num = KapNum, variable, value) %>%
  mutate(typ_rozpoctu = case_when(
    str_detect(variable, "schvaleny") ~ "SCHV",
    str_detect(variable, "upraveny") ~ "UPRAV",
    str_detect(variable, "skutecnost") ~ "SKUT"
  )) %>%
  mutate(variable = case_when(
    str_detect(variable, "schvaleny") ~ str_remove(variable, "_schvaleny"),
    str_detect(variable, "upraveny") ~ str_remove(variable, "_upraveny"),
    str_detect(variable, "skutecnost") ~ str_remove(variable, "_skutecnost")
  )) %>%
  filter(!is.na(typ_rozpoctu)) %>%
  filter(!is.na(value)) %>%
  filter(name != "Exekutiva") %>%
  mutate(name = case_when(
    name == "SOBCPO" ~ "SOBCPO",
    name == "St. sprava se SOBCPO" ~ "SS",
    name == "UO - Ministerstva" ~ "UO",
    name == "UO - Ostatní" ~ "UO",
    name == "UO" ~ "UO",
    name == "OSS-RO" ~ "OSS",
    name == "OSS-SS" ~ "OSS_SS",
    name == "PO" ~ "PO",
    name == "OOSS" ~ "OOSS",
    name == "ROPO celkem" ~ "ROPO"
  )) %>%
  mutate(rok = substr(rok, 1, 4)) %>%
  reshape(timevar = "variable", idvar = c("rok", "name", "kap_num", "typ_rozpoctu"), direction = "wide") %>%
  rename(
    "prostredky_na_platy_a_oppp" = value.PlatyOPPP,
    "prostredky_na_platy" = "value.Platy",
    "oppp" = value.OPPP,
    "pocet_zamestnancu" = value.Zam,
    "prumerny_plat" = value.AvgSal
  ) %>%
  left_join(kap_slovnik, by = "kap_num") %>%
  mutate(
    skut_k_rozp = NA, schv_ke_schv = NA, skut_ke_skut = NA, poradi_prumerneho_platu = NA,
    prostredky_na_platy_a_oppp = prostredky_na_platy_a_oppp * 1000,
    prostredky_na_platy = prostredky_na_platy * 1000,
    oppp = oppp * 1000
  ) %>%
  filter(rok != 2013)


main_df <- rbind(main_df, old_dt) |>
  rows_upsert(kap_slovnik)

# group dataframe

grp_slovnik <- summary %>%
  select(name, full_name) %>%
  unique()

old_dt <- grp_old %>%
  select(rok = Year, name = grp, variable, value) %>%
  mutate(typ_rozpoctu = case_when(
    str_detect(variable, "schvaleny") ~ "SCHV",
    str_detect(variable, "upraveny") ~ "UPRAV",
    str_detect(variable, "skutecnost") ~ "SKUT"
  )) %>%
  mutate(variable = case_when(
    str_detect(variable, "schvaleny") ~ str_remove(variable, "_schvaleny"),
    str_detect(variable, "upraveny") ~ str_remove(variable, "_upraveny"),
    str_detect(variable, "skutecnost") ~ str_remove(variable, "_skutecnost")
  )) %>%
  filter(!is.na(typ_rozpoctu)) %>%
  filter(!is.na(value)) %>%
  filter(name != "Exekutiva") %>%
  mutate(name = case_when(
    name == "SOBCPO" ~ "SOBCPO",
    name == "St. sprava se SOBCPO" ~ "SS",
    name == "UO - Ministerstva" ~ "UO",
    name == "UO - Ostatní" ~ "UO",
    name == "UO" ~ "UO",
    name == "OSS-RO" ~ "OSS",
    name == "OSS-SS" ~ "OSS_SS",
    name == "PO" ~ "PO",
    name == "OOSS" ~ "OOSS",
    name == "ROPO celkem" ~ "ROPO"
  )) %>%
  mutate(rok = substr(rok, 1, 4)) %>%
  reshape(timevar = "variable", idvar = c("rok", "name", "typ_rozpoctu"), direction = "wide") %>%
  rename(
    "prostredky_na_platy_a_oppp" = value.PlatyOPPP,
    "prostredky_na_platy" = "value.Platy",
    "oppp" = value.OPPP,
    "pocet_zamestnancu" = value.Zam,
    "prumerny_plat" = value.AvgSal
  ) %>%
  left_join(grp_slovnik, by = "name") %>%
  mutate(
    skut_k_rozp = NA, schv_ke_schv = NA, skut_ke_skut = NA, poradi_prumerneho_platu = NA,
    prostredky_na_platy_a_oppp = prostredky_na_platy_a_oppp * 1000,
    prostredky_na_platy = prostredky_na_platy * 1000,
    oppp = oppp * 1000
  ) %>%
  filter(rok != 2013)

summary <- rbind(summary, old_dt)


## extend other useful columns
# special signs
kategorie_2014 <- c("UO", "Statni sprava", "OSS", "Ministerstva", "Ostatni ustredni", "Sbory", "Neustredni st. sprava", "Ostatní vc. armady", "Prispevkove organizace")
kategorie_2014_cz <- c(
  "\u00DAst\u0159edn\u00ED org\u00E1ny", "St\u00E1tn\u00ED spr\u00E1va", "Organiza\u010Dn\u00ED slo\u017Eky st\u00E1tu", "Ministerstva", "Ostatn\u00ED \u00FAst\u0159edn\u00ED", "Sbory",
  "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va", "Ostatn\u00ED v\u010D. arm\u00E1dy", "P\u0159\u00EDsp\u011Bvkov\u00E9 organizace"
)

kategorie_2021 <- c("UO", "Statni sprava", "OSS", "Ministerstva", "Ostatni ustredni",
                    "Sbory", "Neustredni st. sprava", "Ostatní vc. armady",
                    "Prispevkove organizace", "Statni urednici")
kategorie_2021_cz <- c(
  "\u00DAst\u0159edn\u00ED org\u00E1ny", "St\u00E1tn\u00ED spr\u00E1va",
  "Organiza\u010Dn\u00ED slo\u017Eky st\u00E1tu", "Ministerstva", "Ostatn\u00ED \u00FAst\u0159edn\u00ED",
  "Sbory",
  "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va", "Ostatn\u00ED v\u010D. arm\u00E1dy",
  "P\u0159\u00EDsp\u011Bvkov\u00E9 organizace","Státní úředníci"
)

# czech_signs_dict <- data.frame(kategorie_2014, kategorie_2014_cz)
czech_signs_dict <- data.frame(kategorie_2014 = kategorie_2021,
                               kategorie_2014_cz = kategorie_2021_cz)

### wages
wages_later <- czso_get_table("110080", force_redownload = TRUE) %>%
  filter(is.na(POHLAVI_txt), is.na(SPKVANTIL_txt), uzemi_kod %in% c(19, 3018)) %>%
  select(rok, hodnota, uzemi_kod) %>%
  spread(key = uzemi_kod, value = hodnota) %>%
  rename("czsal_all" = 2, "phasal_all" = 3)


#### Only as a temp patch before official stats are released

wages_lastyr <- wages_later |> 
  filter(rok == max(rok)) |> 
  mutate(czsal_all = czsal_all * 1.072, 
    phasal_all = phasal_all * 1.096,
    rok = "2025")

wages_early <- chapters_old %>%
  select(Year, czsal_all, phasal_all) %>%
  unique() %>%
  mutate(Year = substr(Year, 1, 4))

wages_benchmark <- wages_early %>%
  filter(Year < 2011) %>%
  rename("rok" = Year) %>%
  rbind(wages_later) %>%
  bind_rows(wages_lastyr) |> 
  mutate(rok = as.integer(rok))


### inflation
MINISTERSTVA <- c(304, 306, 307, 312, 313, 314, 315, 317, 322, 327, 329, 333, 334, 335, 336, 338)
OSTATNI_UO <- c(301, 302, 303, 304, 308, 309, 321, 328, 341, 343, 344, 345, 346, 347, 348, 349, 353, 355, 358, 361, 362, 371, 372, 373, 374, 375, 376, 377, 378, 381, 364)
df_infl <- data.frame(
  rok = seq(2003, 2020),
  inflation = c(0.1, 2.8, 1.9, 2.5, 2.8, 6.3, 1.0, 1.5, 1.9, 3.3, 1.4, 0.4, 0.3, 0.7, 2.5, 2.1, 2.8, 3.2) / 100 + 1
)

df_infl <- czso_get_table("010022", dest_dir = "data-input/czso", force_redownload = TRUE) %>%
  filter(is.na(ucel_txt)) %>%
  filter(casz_txt == "stejné období předchozího roku") %>%
  group_by(rok) %>%
  summarise(hodnota = mean(hodnota)) %>%
  select(contains("obdobi"),rok, hodnota) %>%
  mutate(inflation=hodnota/100)%>%
  arrange(rok)%>%
  filter(rok>=2003,rok<=this_year)

df_infl$base_2003 <- 0
df_infl$base_thisyr <- 0
df_infl[1, "base_2003"] <- 1
df_infl[nrow(df_infl), "base_thisyr"] <- 1


for (i in 2:nrow(df_infl)) {
  df_infl[i, "base_2003"] <- df_infl[i - 1, "base_2003"] * df_infl[i, "inflation"]
}


for (i in (nrow(df_infl) - 1):1) {
  df_infl[i, "base_thisyr"] <- df_infl[i + 1, "base_thisyr"] * df_infl[i+1, "inflation"]
}

main_df_recat <- main_df %>%
  mutate(kategorie_2014 = case_when(
    (prostredky_na_platy > 0 & name == "OSS_SS") ~ "Neustredni st. sprava",
    (prostredky_na_platy > 0 & name == "SS") ~ "Statni sprava",
    (prostredky_na_platy > 0 & name == "OOSS") ~ "Ostatní vc. armady",
    (prostredky_na_platy > 0 & name == "PO") ~ "Prispevkove organizace",
    (prostredky_na_platy > 0 & name == "SOBCPO") ~ "Sbory",
    (name == "UO" & kap_num %in% MINISTERSTVA) ~ "Ministerstva",
    (name == "UO" & kap_num %in% OSTATNI_UO) ~ "Ostatni ustredni"
  ))

main_df_urednici <- main_df_recat |>
  filter(name %in% c("UO", "OSS_SS")) |>
  group_by(rok, typ_rozpoctu, kap_name, kap_num, full_kap_name, cz_kap_name) |>
  summarise(across(.cols = c(prostredky_na_platy, oppp, prostredky_na_platy_a_oppp,
                             pocet_zamestnancu), .fns = ~sum(.x, na.rm = TRUE)),
            prumerny_plat = prostredky_na_platy / pocet_zamestnancu / 12) |>
  mutate(kategorie_2014 = "Statni urednici")

main_df_update <- bind_rows(main_df_recat,
                            main_df_urednici) |>
  filter(!is.na(kap_num)) %>%
  filter(prostredky_na_platy_a_oppp > 0) %>%
  group_by(kategorie_2014, typ_rozpoctu, kap_num) %>%
  mutate(rok = as.integer(rok)) %>%
  left_join(df_infl, by = "rok") %>%
  left_join(wages_benchmark, by = "rok") %>%
  arrange(rok) %>%
  mutate(platy_skut_ke_skut = (prostredky_na_platy - lag(prostredky_na_platy)) / lag(prostredky_na_platy)) %>%
  mutate(mzda_prumer_skut_ke_skut = (prumerny_plat / lag(prumerny_plat) - 1)) %>%
  mutate(plat_base = prostredky_na_platy[1]) %>%
  mutate(cum_pct_wage_change_real = (prostredky_na_platy / base_2003 - plat_base) / plat_base) %>%
  mutate(wage_in_thisyr = prumerny_plat * base_thisyr) %>%
  mutate(wage_in_thisyr_change = wage_in_thisyr / lag(wage_in_thisyr) - 1) %>%
  mutate(wage_base = wage_in_thisyr[1]) %>%
  mutate(cum_pct_wage_change = (wage_in_thisyr - wage_base) / wage_base) %>%
  mutate(wage_to_general = (ifelse(kategorie_2014 %in% c("Ministerstva", "Ostatni ustredni"), prumerny_plat / phasal_all, prumerny_plat / czsal_all))) %>%
  mutate(mzda_k_nh = wage_to_general / lag(wage_to_general) - 1) %>%
  ungroup() %>%
  left_join(czech_signs_dict, by = "kategorie_2014")




main_df_update %>% filter( typ_rozpoctu == "SCHV", rok == this_year) %>% select("kategorie_2014") %>% unique()
main_df %>% filter( typ_rozpoctu == "SCHV", rok == this_year) %>% select(kap_num,name) %>% unique()
# save all dataframes
saveRDS(main_df_update, file = "./data-interim/sections.rds")
# saveRDS(jednotl_df, file = "./data-interim/jednotlivci.rds")
# saveRDS(polozky, file = "./data-interim/organizace.rds")
saveRDS(summary, file = "./data-interim/summary.rds")


main_df_update %>%
  filter(typ_rozpoctu=="SKUT",kap_num ==306,name=="SS") %>%
  select(name,kategorie_2014,kategorie_2014_cz) %>% unique()

