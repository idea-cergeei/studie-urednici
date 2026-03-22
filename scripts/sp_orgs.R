library(statnipokladna)
library(nanoparquet)
orgs0 <- sp_get_codelist("ucjed")

orgs <- orgs0 |>
  sp_add_codelist("druhuj") |>
  sp_add_codelist("poddruhuj")

nanoparquet::write_parquet(orgs, "data-interim/sp_orgs.parquet")
