# see howto-update-nextyear.md

source("standardize_input_data.R")
source("get_macro_numbers.R")

source("graphs.R")
source("graphs_mod.R")
source("dashboard.R")
source("graphs_mod_long.r")
# system("scp -r graphs_mod/* root@194.182.65.144:/srv/shiny-server/zamestnancistatu")
# unlink(x = paste0("graphs/",grep("graf",list.files("graphs"),value = T)))

source("R/export_data.R")
source("R/validate_data.R")
rmarkdown::render("codebook.Rmd")

# quarto::quarto_render("szu-analyza-stsl.qmd")
# system2("http-server", args = c("web_partial" , "--o"))
# system2("netlify", args = c("deploy",
# "--dir", "web_partial" ,
# "--site", "curious-profiterole-ba0eee"))
# system2("netlify", args = c("deploy",
#                             "--dir", "web_partial" ,
#                             "--site", "curious-profiterole-ba0eee",
#                             "--prod"))

# rmarkdown::render("results.Rmd")

system2("http-server", args = c("graphs_mod" , "--o"))
# system2("netlify", args = c("deploy",
# "--dir", "graphs_mod" ,
# "--site", "studie-urednici"))
# system2("netlify", args = c("deploy",
# "--dir", "graphs_mod" ,
# "--site", "studie-urednici",
# "--prod")).

