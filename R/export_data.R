library(pointblank)
library(readr)
library(dplyr)
library(lubridate)

options(scipen = 99)

main_df_update <- readRDS("./data-interim/sections.rds") |>
  rename(faze_rozpoctu = typ_rozpoctu,
         kap_kod = kap_num,
         kap_zkr = kap_name,
         platy_a_oppp = prostredky_na_platy_a_oppp,
         platy = prostredky_na_platy,
         platy_schv_schv = schv_ke_schv,
         platy_skut_rozp = skut_k_rozp,
         platy_skut_skut = skut_ke_skut,
         kap_nazev_cc = full_kap_name,
         kap_nazev = cz_kap_name,
         kategorie = name,
         ceny_index = hodnota,
         ceny_inflace = inflation,
         ceny_deflator_2003 = base_2003,
         ceny_deflator_2024 = base_2024,
         prumerna_mzda_cr = czsal_all,
         prumerna_mzda_pha = phasal_all,
         prumerny_plat_skut_skut = platy_skut_ke_skut,
         prumerny_plat_c2024 = wage_in_2024,
         prumerny_plat_c2024_mezirocne = wage_in_2024_change,
         prumerny_plat_real_od2024 = cum_pct_wage_change_real,
         prumerny_plat_nomi_od2024 = cum_pct_wage_change,
         prumerny_plat_2003 = wage_base,
         prumerny_plat_vucinh  = wage_to_general,
         prumerny_plat_vucinh_mezirocne  = mzda_k_nh
         ) |>
  mutate(date = make_date(rok))

main_df_update$kap_zkr[main_df_update$kap_zkr == "Ksen"] <- "KSen"
main_df_update$kap_zkr[main_df_update$kap_zkr == "Kparl"] <- "KSněm"
main_df_update$kap_zkr[main_df_update$kap_zkr == "Mzdr"] <- "MZd"
main_df_update$kap_zkr[main_df_update$kap_zkr == "Mspr"] <- "MSp"


readr::write_excel_csv2(main_df_update, "data-export/data_all.csv")
arrow::write_parquet(main_df_update, "data-export/data_all.parquet")

cdbk <- create_informant(main_df_update, label = "main export", tbl_name = "tabulka") |>
  info_tabular(info = "Pokud není uvedeno jinak, zdrojem jsou data MF odpovídající Státnímu závěrečnému účtu") |>
  info_columns("faze_rozpoctu",
               info = "fáze rozpočtu - schválený, po změnách, konečný nebo skutečnost") |>
  info_columns("kap_kod",
               info = "Číslo rozpočtové kapitoly") |>
  info_columns("kap_zkr",
               info = "Zkratka rozpočtové kapitoly",
               zdroj = "Autoři") |>
  info_columns("platy_a_oppp",
               info = "Prostředky na platy a ostatní provedenou práci",
               upřesnění = "Hrubý, bez odvodů zaměstnavatele a dalších nákladů práce",
               jednotka = "Kč za rok") |>
  info_columns("platy",
               info = "Prostředky na platy",
               jednotka = "Kč za rok") |>
  info_columns("oppp",
               info = "Prostředky na ostatní provedenou práci (mimo platy, tj. DPP, DPČ aj.)",
               upřesnění = "Hrubý, bez odvodů zaměstnavatele a dalších nákladů práce",
               jednotka = "Kč za rok") |>
  info_columns("prumerny_plat",
               info = "Průměrný plat",
               upřesnění = "Hrubý, bez odvodů zaměstnavatele a dalších nákladů práce",
               jednotka = "Kč za měsíc") |>
  info_columns("pocet_zamestnancu",
               info = "Počet zaměstnanců, přepočteno na plné úvazky") |>
  info_columns("platy_schv_schv",
               info = "Index změny schválených rozpočtů mezi lety R a R-1",
               měřítko = "1 = žádná změna, > 1 = meziroční nárůst schválených rozpočtů") |>
  info_columns("platy_skut_rozp",
               info = "Index změny mezi skutečností a rozpočtem (skutečnost děleno rozpočet).",
               měřítko = "1 = žádná změna, > 1 = skutečnost více než schválený rozpočet") |>
  info_columns("platy_skut_skut",
               info = "Index změny skutečých výdajů mezi lety R a R-1",
               měřítko = "1 = žádná změna, > 1 = nárůst") |>
  info_columns("kap_nazev_cc",
               info = "Název kapitoly, v_kodu_bez_hacku") |>
  info_columns("kategorie", info = "Zkratka kategorie zaměstnanců") |>
  info_columns("kap_nazev",
               info = "Název kapitoly, česky") |>
  info_columns("kategorie_2014",
               info = "Název kategorie zaměstnanců ve studii z roku 2014") |>
  info_columns("ceny_index",
               info = "Inflace (deflátor)",
               upřesnění = "index spotřebitelských cen, meziroční změna vypočtena jako průměr měsíčních indexů proti stejnému měsíci předchozího roku",
               zdroj = "ČSÚ, tabulka 01022, 'Indexy spotřebitelských cen', https://www.czso.cz/csu/czso/indexy-spotrebitelskych-cen",
               měřítko = "1 = nulová inflace") |>
  info_columns("ceny_inflace",
               info = "Inflace v procentním vyjádření",
               měřítko = "0 = nulová inflace",
               zdroj = "Odvozeno ze sloupce 'ceny_index'") |>
  info_columns("ceny_deflator_2003",
               info = "Cenový index vůči roku 2003",
               měřítko = "1 = nulová inflace",
               zdroj = "Odvozeno ze sloupce 'ceny_index'") |>
  info_columns("ceny_deflator_2024",
               info = "Cenový index vůči roku 2024") |>
  info_columns("prumerna_mzda_cr",
               info = "Průměrná mzda v národním hospodářství",
               upřesnění = "průměrná hrubá měsíční mzda",
               zdroj = "ČSÚ, datová sada 11080, https://www.czso.cz/csu/czso/prumerna-hruba-mesicni-mzda-a-median-mezd-v-krajich") |>
  info_columns("prumerna_mzda_pha",
               info = "Průměrná mzda v Praze",
               upřesnění = "průměrná hrubá měsíční mzda za Prahu",
               zdroj = "ČSÚ, datová sada 11080, https://www.czso.cz/csu/czso/prumerna-hruba-mesicni-mzda-a-median-mezd-v-krajich") |>
  info_columns("prumerny_plat_skut_skut",
               info = "Index změny průměrného platu oproti předchozímu roku",
               měřítko = "1 = žádná změna, > 1 = nárůst") |>
  info_columns("prumerny_plat_real_od2024",
               info = "Změna průměrného platu od roku 2003 očištěná o inflaci",
               měřítko = "0 = žádná změna, > 0 = nárůst") |>
  info_columns("prumerny_plat_c2024",
               info = "Průměrný plat v cenách roku 2024") |>
  info_columns("prumerny_plat_c2024_mezirocne",
               info = "Meziroční změna platů v reálném vyjádření",
               upřesnění = "Podle vyjádření v cenách roku 2024",
               měřítko = "0 = žádná změna, 0.01 = nárůst o 1 %") |>
  info_columns("prumerny_plat_2003",
               info = "Průměrný plat roku 2003 (pro výpočet)") |>
  info_columns("prumerny_plat_nomi_od2024",
               info = "Změna průměrného platu od roku 2003, neočištěno o inflaci",
               měřítko = "0 = žádná změna, 1 = nárůst o 100 %") |>
  info_columns("prumerny_plat_vucinh",
               info = "Poměr průměrného platu a průměrné mzdy v národním hospodářství/Praze",
               upřesnění = "Pro ústřední orgány počítán",
               měřítko = "1 = průměrný plat stejný jako průměrná mzda v NH, > 1 vyšší") |>
  info_columns("prumerny_plat_vucinh_mezirocne",
               info = "Meziroční změna poměru průměrného platu k průměrné mzdě v národním hospodářství/Praze",
               měřítka = "0 = žádná změna, 0.01 = nárůst o 1 p.b.") |>
  info_columns("kategorie_2014_cz",
               info = "Název kategorie zaměstnanců ve studii z roku 2014, česky")

write_rds(cdbk, "data-interim/codebook.rds")
cdbk$read_fn <- "tabulka"
yaml_write(informant = cdbk, path = "data-export", filename = "codebook.yml")
