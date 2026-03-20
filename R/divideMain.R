
library(dplyr)
library(tidyr)

# Column-detection helpers shared by all three functions.
# Rather than relying on the exact number of columns in each section group
# (which varies between file vintages), we identify each data column by its
# row-3 / row-4 header text within the section's column span, build a named
# tibble directly, and let bind_rows() fill missing optional columns with NA.

# Internal: find the first df column whose row-3 or row-4 header matches
# `pattern` within `col_range`.  Returns NA_integer_ when nothing matches.
.find_col <- function(pattern, col_range, hdr) {
  hits <- grep(pattern, hdr[col_range], ignore.case = TRUE)
  if (!length(hits)) NA_integer_ else col_range[[hits[[1L]]]]
}

# Internal: normalise typ_rozpoctu labels
.clean_typ <- function(x) {
  dplyr::case_when(
    grepl("SKUT",  x) ~ "SKUT",
    grepl("SCHV",  x) ~ "SCHV",
    grepl("UPRAV", x) ~ "UPRAV",
    .default = x
  )
}


# loads and clean the main sections of input data
divide_sections <- function(df, sheet_name, section_names) {
  kap_num_vec <- c(
    301, 302, 303, 304, 306, 307, 308, 309, 312, 313, 314, 315, 317, 321, 322, 327, 328, 329, 333, 334, 335, 336, 343, 344, 345, 346, 348, 349,
    353, 355, 358, 359, 361, 362, 371, 372, 373, 374, 375, 376, 377, 378, 381
  )
  full_kap_name <- c(
    "kancelar_prezidenta", "parlament", "kancelar_senatu", "urad_vlady", "ministerstvo_zahranicnich_veci", "ministerstvo_obrany",
    "narodni_bezpecnostni_urad", "kancelar_verejneho_ochrance_prav", "ministerstvo_financi", "ministerstvo_prace_a_socialnich_veci",
    "ministerstvo_vnitra", "ministerstvo_zivotniho_prostredi", "ministerstvo_pro_mistni_rozvoj", "grantova_agentura",
    "ministerstvo_prumyslu_a_obchodu", "ministerstvo_dopravy", "cesky_telekomunikacni_urad", "ministerstvo_zemedelstvi",
    "ministerstvo_skolstvi_mladeze_a_telovychovy", "ministerstvo_kultury", "ministerstvo_zdravotnictvi", "ministerstvo_spravedlnosti",
    "urad_pro_ochranu_osobnich_udaju", "urad_prumysloveho_vlastnictvi", "cesky_statisticky_urad", "cesky_urad_zememericky_a_katastralni",
    "cesky_bansky_urad", "energeticky_regulacni_urad", "ministerstvo_pro_hospodarskou_soutez", "ustav_pro_studium_totalitnich_rezimu",
    "ustavni_soud", "urad_narodni_rozpoctove_rady", "akademie_ved", "narodni_sportovni_agentura",
    "urad_pro_dohled_nad_hospodarenim_politickych_stran_a_politickych_hnuti", "rada_pro_rozhlasove_a_televizni_vysilani",
    "urad_pro_pristup_k_dopravni_infrastrukture", "sprava_statnich_hmotnych_rezerv", "statni_urad_pro_jadernou_bezpecnost",
    "generalni_inspekce_bezpecnostnich_sboru", "technologicka_agentura_cr", "narodni_urad_pro_kybernetickou_a_informacni_bezpecnost",
    "nejvyssi_kontrolni_urad"
  )
  cz_kap_name <- c(
    "Kancel\u00e1\u0159 prezidenta",
    "Parlament",
    "Kancel\u00e1\u0159 Sen\u00e1tu",
    "\u00da\u0159ad vl\u00e1dy",
    "Ministerstvo zahrani\u010dn\u00edch v\u011bc\u00ed",
    "Ministerstvo obrany                                      ",
    "N\u00e1rodn\u00ed bezpe\u010dnostn\u00ed \u00fa\u0159ad",
    "Kancel\u00e1\u0159 ve\u0159ejn\u00e9ho ochr\u00e1nce pr\u00e1v",
    "Ministerstvo financ\u00ed                       ",
    "Ministerstvo pr\u00e1ce a soci\u00e1ln\u00edch v\u011bc\u00ed",
    "Ministerstvo vnitra                                ",
    "Ministerstvo \u017eivotn\u00edho prost\u0159ed\u00ed",
    "Ministerstvo pro m\u00edstn\u00ed rozvoj",
    "Grantov\u00e1 agentura",
    "Ministerstvo pr\u016fmyslu a obchodu",
    "Ministerstvo dopravy",
    "\u010cesk\u00fd telekomunika\u010dn\u00ed \u00fa\u0159ad",
    "Ministerstvo zem\u011bd\u011blstv\u00ed",
    "Ministerstvo \u0161kolstv\u00ed, ml\u00e1de\u017ee a t\u011blov\u00fdchovy",
    "Ministerstvo kultury",
    "Ministerstvo zdravotnictv\u00ed",
    "Ministerstvo spravedlnosti",
    "\u00da\u0159ad pro ochranu osobn\u00edch \u00fadaj\u016f",
    "\u00da\u0159ad pr\u016fmyslov\u00e9ho vlastnictv\u00ed",
    "\u010cesk\u00fd statistick\u00fd \u00fa\u0159ad                               ",
    "\u010cesk\u00fd \u00fa\u0159ad zem\u011bm\u011b\u0159ick\u00fd a katastr\u00e1ln\u00ed",
    "\u010cesk\u00fd b\u00e1\u0148sk\u00fd \u00fa\u0159ad",
    "Energetick\u00fd regula\u010dn\u00ed \u00fa\u0159ad",
    "Ministerstvo pro hospod\u00e1\u0159skou sout\u011b\u017e",
    "\u00dastav pro studium totalit\u00ednch re\u017eim\u016f",
    "\u00dastavni soud",
    "\u00da\u0159ad N\u00e1rodn\u00ed rozpo\u010dtov\u00e9 rady",
    "Akademie v\u011bd",
    "N\u00e1rodn\u00ed sportovn\u00ed agentura",
    "\u00da\u0159ad pro dohled nad hospoda\u0159en\u00edm politick\u00fdch stran a politick\u00fdch hnut\u00ed",
    "Rada pro rozhlasov\u00e9 a televizn\u00ed vys\u00edl\u00e1n\u00ed",
    "\u00da\u0159ad pro p\u0159\u00edstup k dopravn\u00ed infrastruktu\u0159e",
    "Spr\u00e1va st\u00e1tn\u00edch hmotn\u00fdch rezerv",
    "St\u00e1tn\u00ed \u00fa\u0159ad pro jadernou bezpe\u010dnost",
    "Gener\u00e1ln\u00ed inspekce bezpe\u010dnostn\u00edch sbor\u016f",
    "Technologick\u00e1 agentura \u010cR",
    "N\u00e1rodn\u00ed \u00fa\u0159ad pro kybernetickou a informa\u010dn\u00ed bezpe\u010dnost",
    "Nejvy\u0161\u0161\u00ed kontroln\u00ed \u00fa\u0159ad"
  )
  names_df <- data.frame(kap_num = kap_num_vec, full_kap_name, cz_kap_name)

  # Trim at the CELKEM summary row
  last_val <- grep("C E L K E M", df[[3]])
  if (length(last_val)) df <- df[seq_len(last_val), ]

  # Pre-compute row-1 headers and ASCII-transliterated rows 3&4 for column detection
  hdr1 <- as.character(unlist(df[1, ]))
  hdr3 <- iconv(as.character(unlist(df[3, ])), from = "UTF-8", to = "ASCII//TRANSLIT")
  hdr4 <- iconv(as.character(unlist(df[4, ])), from = "UTF-8", to = "ASCII//TRANSLIT")

  # Pre-extract ID columns for all data rows, then apply CELKEM row treatment:
  # the last row's display name lives in col 1 of the second-to-last row.
  full_rows    <- seq(6L, nrow(df))
  kap_num_raw  <- as.character(df[full_rows, 1, drop = TRUE])
  kap_name_raw <- as.character(df[full_rows, 2, drop = TRUE])
  n            <- length(kap_num_raw)
  kap_name_raw[n] <- kap_num_raw[n - 1L]
  kap_num_raw[n]  <- NA_character_
  keep          <- setdiff(seq_len(n), n - 1L)
  kap_num_data  <- kap_num_raw[keep]
  kap_name_data <- kap_name_raw[keep]
  data_rows     <- full_rows[keep]

  find_col <- function(pattern, col_range, hdr) .find_col(pattern, col_range, hdr)

  get_num <- function(col) {
    if (is.na(col)) return(rep(NA_real_, length(data_rows)))
    suppressWarnings(as.numeric(df[data_rows, col, drop = TRUE]))
  }

  res <- vector("list")
  for (i in seq(3L, ncol(df))) {
    h1 <- hdr1[[i]]
    if (is.na(h1) || grepl("INDEX", h1)) next

    # Section extent: count consecutive NA/INDEX columns in row 1
    ncols <- 1L
    for (j in seq(i + 1L, ncol(df))) {
      v <- hdr1[[j]]
      if (is.na(v) || grepl("INDEX", v)) ncols <- ncols + 1L else break
    }
    sec_cols <- seq(i, i + ncols - 1L)

    # Identify each data column by its row-3 / row-4 header name
    platy_oppp_col <- find_col("PLATY A OPPP", sec_cols, hdr3)
    oppp_col       <- find_col("^OPPP",        sec_cols, hdr4)
    platy_col      <- find_col("NA PLATY$",    sec_cols, hdr4)
    pocet_col      <- find_col("POCET",        sec_cols, hdr3)
    prum_col       <- find_col("plat v K",     sec_cols, hdr3)
    poradi_col     <- find_col("Poradi",       sec_cols, hdr3)

    res[[length(res) + 1L]] <- tibble(
      Rozpocet_a_rok             = h1,
      kap_num                    = kap_num_data,
      kap_name                   = kap_name_data,
      prostredky_na_platy_a_oppp = get_num(platy_oppp_col),
      oppp                       = get_num(oppp_col),
      prostredky_na_platy        = get_num(platy_col),
      pocet_zamestnancu          = get_num(pocet_col),
      prumerny_plat              = get_num(prum_col),
      poradi_prumerneho_platu    = get_num(poradi_col),
      schv_ke_schv               = NA_real_,
      skut_k_rozp                = NA_real_,
      skut_ke_skut               = NA_real_
    )
  }

  if (!length(res)) return(tibble())

  bind_rows(res) |>
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) |>
    mutate(
      name = case_when(
        sheet_name == "ROPO CELKEM"   ~ "ROPO",
        sheet_name == "OSS (RO)"      ~ "OSS",
        sheet_name == "PO"            ~ "PO",
        sheet_name == "OOSS"          ~ "OOSS",
        sheet_name == "STATNI SPRAVA" ~ "SS",
        sheet_name == "UO"            ~ "UO",
        sheet_name == "OSS SS"        ~ "OSS_SS",
        sheet_name == "SOBCPO"        ~ "SOBCPO"
      ),
      kap_name = ifelse(
        str_detect(iconv(kap_name, from = "UTF-8", to = "ASCII//TRANSLIT"),
                   "UDHPSH|UPDSH|UDHPS"),
        "UDHPS", kap_name
      ),
      kap_num           = as.numeric(kap_num),
      pocet_zamestnancu = as.integer(round(pocet_zamestnancu)),
      rok               = as.integer(rok),
      typ_rozpoctu      = .clean_typ(typ_rozpoctu)
    ) |>
    left_join(names_df, by = join_by(kap_num))
}


# loads and clean the sections of input data containing detailed granularity
divide_jednotl <- function(df, sheet_name, section_names) {
  # Fill cols 1&2 downward: kap_num and kap_name are merged cells in the source.
  filled <- df |> dplyr::select(1:2) |> tidyr::fill(1, 2)
  df[seq_len(nrow(df) - 1L), 1:2] <- filled[seq_len(nrow(df) - 1L), ]

  hdr1 <- as.character(unlist(df[1, ]))
  hdr3 <- iconv(as.character(unlist(df[3, ])), from = "UTF-8", to = "ASCII//TRANSLIT")
  hdr4 <- iconv(as.character(unlist(df[4, ])), from = "UTF-8", to = "ASCII//TRANSLIT")

  # No CELKEM treatment needed for individual-granularity sheets
  data_rows     <- seq(6L, nrow(df))
  kap_num_data  <- as.character(df[data_rows, 1, drop = TRUE])
  kap_name_data <- as.character(df[data_rows, 2, drop = TRUE])
  org_data      <- as.character(df[data_rows, 3, drop = TRUE])

  find_col <- function(pattern, col_range, hdr) .find_col(pattern, col_range, hdr)

  get_num <- function(col) {
    if (is.na(col)) return(rep(NA_real_, length(data_rows)))
    suppressWarnings(as.numeric(df[data_rows, col, drop = TRUE]))
  }

  res <- vector("list")
  for (i in seq(4L, ncol(df))) {
    h1 <- hdr1[[i]]
    if (is.na(h1) || grepl("INDEX", h1)) next

    ncols <- 1L
    for (j in seq(i + 1L, ncol(df))) {
      v <- hdr1[[j]]
      if (is.na(v) || grepl("INDEX", v)) ncols <- ncols + 1L else break
    }
    sec_cols <- seq(i, i + ncols - 1L)

    platy_oppp_col <- find_col("PLATY A OPPP", sec_cols, hdr3)
    oppp_col       <- find_col("^OPPP",        sec_cols, hdr4)
    platy_col      <- find_col("NA PLATY$",    sec_cols, hdr4)
    pocet_col      <- find_col("POCET",        sec_cols, hdr3)
    prum_col       <- find_col("plat v K",     sec_cols, hdr3)
    poradi_col     <- find_col("Poradi",       sec_cols, hdr3)

    res[[length(res) + 1L]] <- tibble(
      Rozpocet_a_rok             = h1,
      kap_num                    = kap_num_data,
      kap_name                   = kap_name_data,
      organizace                 = org_data,
      prostredky_na_platy_a_oppp = get_num(platy_oppp_col),
      oppp                       = get_num(oppp_col),
      prostredky_na_platy        = get_num(platy_col),
      pocet_zamestnancu          = get_num(pocet_col),
      prumerny_plat              = get_num(prum_col),
      poradi_prumerneho_platu    = get_num(poradi_col),
      schv_ke_schv               = NA_real_,
      skut_k_rozp                = NA_real_,
      skut_ke_skut               = NA_real_
    )
  }

  if (!length(res)) return(tibble())

  bind_rows(res) |>
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) |>
    mutate(
      name = case_when(
        sheet_name == "SOBCPO  JEDNOTLIVY" ~ "SOBCPO_JEDNOTL",
        sheet_name == "OSS SS - jednotl"   ~ "OSS_SS_JEDNOTL"
      ),
      kap_num           = as.numeric(kap_num),
      pocet_zamestnancu = as.integer(round(pocet_zamestnancu)),
      rok               = as.integer(rok),
      typ_rozpoctu      = .clean_typ(typ_rozpoctu)
    )
}


# slightly different procedure used for loading the SUMMARY data
divide_summary <- function(df, sheet_name, section_names) {
  hdr1 <- as.character(unlist(df[1, ]))
  hdr3 <- iconv(as.character(unlist(df[3, ])), from = "UTF-8", to = "ASCII//TRANSLIT")
  hdr4 <- iconv(as.character(unlist(df[4, ])), from = "UTF-8", to = "ASCII//TRANSLIT")

  # col 2 of the summary sheet contains the group display names (full_name)
  data_rows      <- seq(6L, nrow(df))
  full_name_data <- as.character(df[data_rows, 2, drop = TRUE])

  find_col <- function(pattern, col_range, hdr) .find_col(pattern, col_range, hdr)

  get_num <- function(col) {
    if (is.na(col)) return(rep(NA_real_, length(data_rows)))
    suppressWarnings(as.numeric(df[data_rows, col, drop = TRUE]))
  }

  res <- vector("list")
  for (i in seq(3L, ncol(df))) {
    h1 <- hdr1[[i]]
    if (is.na(h1) || grepl("INDEX", h1)) next

    ncols <- 1L
    for (j in seq(i + 1L, ncol(df))) {
      v <- hdr1[[j]]
      if (is.na(v) || grepl("INDEX", v)) ncols <- ncols + 1L else break
    }
    sec_cols <- seq(i, i + ncols - 1L)

    platy_oppp_col <- find_col("PLATY A OPPP", sec_cols, hdr3)
    oppp_col       <- find_col("^OPPP",        sec_cols, hdr4)
    platy_col      <- find_col("NA PLATY$",    sec_cols, hdr4)
    pocet_col      <- find_col("POCET",        sec_cols, hdr3)
    prum_col       <- find_col("plat v K",     sec_cols, hdr3)
    poradi_col     <- find_col("Poradi",       sec_cols, hdr3)

    res[[length(res) + 1L]] <- tibble(
      Rozpocet_a_rok             = h1,
      full_name                  = full_name_data,
      prostredky_na_platy_a_oppp = get_num(platy_oppp_col),
      oppp                       = get_num(oppp_col),
      prostredky_na_platy        = get_num(platy_col),
      pocet_zamestnancu          = get_num(pocet_col),
      prumerny_plat              = get_num(prum_col),
      poradi_prumerneho_platu    = get_num(poradi_col),
      schv_ke_schv               = NA_real_,
      skut_k_rozp                = NA_real_,
      skut_ke_skut               = NA_real_
    )
  }

  if (!length(res)) return(tibble())

  bind_rows(res) |>
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) |>
    mutate(
      aux = iconv(full_name, from = "UTF-8", to = "ASCII//TRANSLIT"),
      name = case_when(
        str_detect(aux, "SOBCPO")              ~ "SOBCPO",
        str_detect(aux, "OSS.*Statni|Statni.*OSS") ~ "OSS_SS",
        str_detect(aux, "bez SOBCPO")          ~ "SS_bez_SOBCPO",
        str_detect(aux, "Ostatni organizacni") ~ "OOSS",
        str_detect(aux, "Organizacni slozky")  ~ "OSS",
        str_detect(aux, "Statni sprava")       ~ "SS",
        str_detect(aux, "Prispevkove")         ~ "PO",
        str_detect(aux, "OSS A PO")            ~ "ROPO",
        str_detect(aux, "Ustredni organy")     ~ "UO"
      ),
      pocet_zamestnancu = as.integer(round(pocet_zamestnancu)),
      rok               = as.integer(rok),
      typ_rozpoctu      = .clean_typ(typ_rozpoctu)
    ) |>
    select(-"aux")
}
