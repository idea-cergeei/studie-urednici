library(pointblank)
library(nanoparquet)
library(dplyr)

cfg       <- config::get()
this_year <- if (!is.null(cfg$rok)) cfg$rok else 2024L

data_all <- read_parquet("data-export/data_all.parquet")

# Action levels: warn at 1 failure, stop at 5% failure rate
al <- action_levels(warn_at = 1, stop_at = 0.05)

# ── Agent 1: structural integrity ─────────────────────────────────────────────
# Checks that apply to every row regardless of faze or year.

agent_structure <- create_agent(
  tbl       = data_all,
  label     = "Structural integrity",
  actions   = al
) |>
  # Required identifiers must be present.
  # kategorie is NA only on the synthetic "Statni urednici" aggregate rows
  # (kategorie_2014 == "Statni urednici"); all other rows must have it.
  col_vals_not_null(
    columns       = c(faze_rozpoctu, rok, kap_kod, kap_zkr),
    label         = "core identifiers not null"
  ) |>
  col_vals_not_null(
    columns       = kategorie,
    preconditions = \(x) dplyr::filter(x, kategorie_2014 != "Statni urednici" | is.na(kategorie_2014)),
    label         = "kategorie not null (except Statni urednici aggregate rows)"
  ) |>
  # Budget phase is one of the three valid codes
  col_vals_in_set(
    columns = faze_rozpoctu,
    set     = c("SCHV", "UPRAV", "SKUT"),
    label   = "faze_rozpoctu in valid set"
  ) |>
  # Year within the pipeline's operational range
  col_vals_between(
    columns = rok,
    left    = 2003L,
    right   = this_year,
    label   = "rok in expected range"
  ) |>
  # Each org × year × budget-phase × category should be unique
  rows_distinct(
    columns = c(faze_rozpoctu, rok, kap_kod, kategorie),
    label   = "no duplicate org-year-phase-category rows"
  ) |>
  # Salary must be positive where staff count is present
  col_vals_gt(
    columns      = platy,
    value        = 0,
    na_pass      = TRUE,
    preconditions = \(x) filter(x, pocet_zamestnancu > 0),
    label        = "platy > 0 when staff present"
  ) |>
  # Plausible monthly salary range (covers 2003–present in nominal CZK)
  col_vals_between(
    columns = prumerny_plat,
    left    = 3000,
    right   = 250000,
    na_pass = TRUE,
    label   = "prumerny_plat in plausible range (3 000–250 000 CZK)"
  ) |>
  # Plausible staff counts
  col_vals_between(
    columns = pocet_zamestnancu,
    left    = 1L,
    right   = 400000L,
    na_pass = TRUE,
    label   = "pocet_zamestnancu in plausible range (1–400 000)"
  ) |>
  # platy_a_oppp >= platy (total spend >= salary-only spend)
  col_vals_expr(
    expr  = expr(platy_a_oppp >= platy | is.na(platy_a_oppp) | is.na(platy)),
    label = "platy_a_oppp >= platy"
  ) |>
  interrogate()

# ── Agent 2: year-on-year changes (SKUT only, per org × category) ─────────────
# Flags implausibly large or small changes that suggest a data-ingestion problem
# (e.g., units wrong, year duplicated, org missing).

# Only compare truly consecutive years: skip new-org first appearances and
# data gaps (e.g. AV ceased being a budget chapter after 2006; MPSV OOSS has
# no data 2013–2024).  A non-consecutive gap means the ratio is meaningless.
yoy <- data_all |>
  filter(faze_rozpoctu == "SKUT", !is.na(kap_kod)) |>
  arrange(kap_kod, kategorie, rok) |>
  group_by(kap_kod, kategorie) |>
  mutate(
    lag_rok    = lag(rok),
    sal_ratio  = prumerny_plat      / lag(prumerny_plat),
    cnt_ratio  = pocet_zamestnancu  / lag(pocet_zamestnancu),
    plat_ratio = platy              / lag(platy)
  ) |>
  ungroup() |>
  filter(
    !is.na(sal_ratio) | !is.na(cnt_ratio) | !is.na(plat_ratio),
    rok == lag_rok + 1L   # consecutive years only
  )

agent_yoy <- create_agent(
  tbl     = yoy,
  label   = "Year-on-year changes (SKUT, per org)",
  actions = al
) |>
  # Average salary rarely moves more than ±90 % in a single year.
  # New-org first-full-year effects (e.g. TAČR 2009→2010: 8×) are excluded
  # by the consecutive-year filter above.  Bounds are intentionally generous
  # to catch magnitude errors (wrong units) not policy-driven changes.
  col_vals_between(
    columns = sal_ratio,
    left    = 0.10,
    right   = 10.0,
    na_pass = TRUE,
    label   = "prumerny_plat YoY ratio 0.10–10"
  ) |>
  # Staff counts can swing dramatically due to org restructuring (e.g. AV 2007:
  # ceased being a budget chapter, 7 340 → 104 employees = ratio 0.014).
  # Bounds detect true magnitude errors such as data reported in wrong units.
  col_vals_between(
    columns = cnt_ratio,
    left    = 0.001,
    right   = 1000.0,
    na_pass = TRUE,
    label   = "pocet_zamestnancu YoY ratio 0.001–1000"
  ) |>
  # Same reasoning for total salary spend.
  col_vals_between(
    columns = plat_ratio,
    left    = 0.001,
    right   = 1000.0,
    na_pass = TRUE,
    label   = "platy YoY ratio 0.001–1000"
  ) |>
  interrogate()

# ── Agent 3: aggregate category totals (SKUT) ─────────────────────────────────
# Checks that the totals for each broad category are in the range we have
# observed historically.  Bounds are set generously (2× historical min/max)
# so they catch magnitude errors while allowing for future growth.

cat_totals <- data_all |>
  filter(faze_rozpoctu == "SKUT", !is.na(kap_kod)) |>
  group_by(rok, kategorie) |>
  summarise(
    platy_bn    = sum(platy,             na.rm = TRUE) / 1e9,
    n_orgs      = n_distinct(kap_kod),
    avg_sal     = mean(prumerny_plat,    na.rm = TRUE),
    total_staff = sum(pocet_zamestnancu, na.rm = TRUE),
    .groups     = "drop"
  )

# Historical observed ranges (SKUT totals across all orgs per category, 2003–2025)
# with 0.5× lower buffer and 2× upper buffer to catch magnitude errors while
# allowing for genuine growth and new categories.
#   ROPO:   staff 414k–491k,  platy 103–284 bn CZK
#   SS:     staff 150k–167k,  platy  42– 98 bn
#   OSS:    staff 186k–217k,  platy  54–123 bn
#   UO:     staff  16k– 28k,  platy   5– 20 bn
#   PO:     staff 227k–295k,  platy  48–162 bn
#   OSS_SS: staff  47k– 52k,  platy  10– 26 bn
#   OOSS:   staff  34k– 51k,  platy  11– 24 bn
#   SOBCPO: staff  79k–100k,  platy  26– 52 bn
cat_bounds <- tribble(
  ~kategorie, ~platy_min_bn, ~platy_max_bn, ~staff_min, ~staff_max,
  "ROPO",          50,            600,         200000,   1000000,
  "SS",            20,            200,          70000,    350000,
  "OSS",           25,            250,          90000,    450000,
  "UO",             2,             50,           8000,     60000,
  "PO",            20,            350,         110000,    600000,
  "OSS_SS",         5,             60,          20000,    110000,
  "OOSS",           5,             55,          15000,    110000,
  "SOBCPO",        10,            120,          35000,    220000
)

cat_check <- cat_totals |>
  inner_join(cat_bounds, by = "kategorie")

agent_totals <- create_agent(
  tbl     = cat_check,
  label   = "Category aggregate totals (SKUT, per year)",
  actions = al
) |>
  col_vals_expr(
    expr  = expr(platy_bn >= platy_min_bn & platy_bn <= platy_max_bn),
    label = "total platy per category within historical range"
  ) |>
  col_vals_expr(
    expr  = expr(total_staff >= staff_min & total_staff <= staff_max),
    label = "total staff per category within historical range"
  ) |>
  # Average salary across category should be in a plausible range
  col_vals_between(
    columns = avg_sal,
    left    = 3000,
    right   = 250000,
    na_pass = TRUE,
    label   = "category avg salary in plausible range"
  ) |>
  interrogate()

# ── Results ────────────────────────────────────────────────────────────────────

cat("\n=== VALIDATION RESULTS ===\n\n")

for (agent in list(agent_structure, agent_yoy, agent_totals)) {
  lbl    <- agent$label
  passed <- all_passed(agent)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", lbl))
  if (!passed) {
    fails <- agent$validation_set |>
      dplyr::filter(n_failed > 0) |>
      dplyr::select(assertion_type, columns_expr, n_failed, f_failed, label)
    print(fails)
    cat("\n")
  }
}

all_ok <- all(
  all_passed(agent_structure),
  all_passed(agent_yoy),
  all_passed(agent_totals)
)

cat("\n", if (all_ok) "ALL CHECKS PASSED." else "SOME CHECKS FAILED — review output above.", "\n")

if (!all_ok) stop("Validation failed.", call. = FALSE)
