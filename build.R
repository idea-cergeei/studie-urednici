source("standardize_input_data.R")

source("R/export_data.R")
rmarkdown::render("codebook.Rmd")

quarto::quarto_render("szu-analyza-stsl.qmd")
system2("http-server", args = c("web_partial" , "--o"))
system2("netlify", args = c("deploy",
                            "--dir", "web_partial" ,
                            "--site", "curious-profiterole-ba0eee"))
system2("netlify", args = c("deploy",
                            "--dir", "web_partial" ,
                            "--site", "curious-profiterole-ba0eee",
                            "--prod"))

rmarkdown::render("results.Rmd")

source("graphs.R")
source("pay-change-nace_plot.R")
source("graf_rocni-zmeny-dekompozice.R")
source("graphs_mod.R")
source("graphs_mod_long.r")

system2("http-server", args = c("graphs_mod" , "--o"))
system2("netlify", args = c("deploy",
                            "--dir", "graphs_mod" ,
                            "--site", "studie-urednici"))
system2("netlify", args = c("deploy",
                            "--dir", "graphs_mod" ,
                            "--site", "studie-urednici",
                            "--prod"))
