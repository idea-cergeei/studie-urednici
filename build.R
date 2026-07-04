library(purrr)

# see howto-update-nextyear.md

source("standardize_input_data.R", local = new.env())
source("get_macro_numbers.R", local = new.env())

source("R/validate_data.R", local = new.env())
source("R/export_data.R", local = new.env())
rmarkdown::render("codebook.Rmd")

quarto::quarto_render("shrnuti.qmd")
quarto::quarto_render("metodologie.qmd")
quarto::quarto_render("zkratky.qmd")

# now update header year in shrnuti.docx

fs::file_move("shrnuti.docx", "word-docs/urednici-shrnuti_2026.docx")
fs::file_move("metodologie.docx", "word-docs/metodologie-redo.docx")
fs::file_move("zkratky.docx", "word-docs/zkratky.docx")

# create PDFs by hand from Word, put them into www/pdfs

source("graphs.R", local = new.env())
source("dashboard.R", local = new.env())
source("graphs_mod.R", local = new.env())
source("graphs_mod_long.r", local = new.env())

# Convert individual numbered graphs to PDF using Chrome headless
chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
graf_htmls <- list.files("graphs", pattern = "^graf_[A-Z0-9]+\\.html$", full.names = TRUE)
pdf_dir <- "graphs_mod/pdf/grafy"
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)
walk(graf_htmls, \(html) {
  pdf_out <- file.path(pdf_dir, sub("\\.html$", ".pdf", basename(html)))
  system2(chrome, args = c(
    "--headless", "--no-sandbox", "--disable-gpu",
    paste0("--print-to-pdf=", normalizePath(pdf_out, mustWork = FALSE)),
    normalizePath(html, mustWork = TRUE)
  ), stdout = FALSE, stderr = FALSE)
})

# Zip the PDFs into graphs_mod/files/Grafy.zip
files_dir <- "graphs_mod/files"
dir.create(files_dir, showWarnings = FALSE, recursive = TRUE)
zip_path <- file.path(files_dir, "Grafy.zip")
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
zip(zipfile = normalizePath(zip_path, mustWork = FALSE),
    files   = normalizePath(pdf_files, mustWork = TRUE))

# system("scp -r graphs_mod/* root@194.182.65.144:/srv/shiny-server/zamestnancistatu")
# unlink(x = paste0("graphs/",grep("graf",list.files("graphs"),value = T)))


# quarto::quarto_render("szu-analyza-stsl.qmd")
# system2("http-server", args = c("web_partial" , "--o"))
# rmarkdown::render("results.Rmd")

system2("http-server", args = c("graphs_mod" , "--o"))

system2("netlify", args = c("deploy",
"--dir", "graphs_mod" ,
"--site", "studie-urednici",
"--prod"))

