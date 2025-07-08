pdf_dir <- "graphs_pdf/"
zip_file <- "Grafy.zip"

graph_titles <- data.table::fread("graph_titles.csv")

for(graph_id in graph_titles$graph){
  f_name <- paste0(gsub("^([^\\.]+)\\..*","\\1",graph_titles$title[graph_titles$graph==graph_id]),".pdf")
  orca(get(graph_id),paste0(pdf_dir,f_name),width = 1094,height = 700)
}

if(!dir.exists("graphs_mod/files")){dir.create("graphs_mod/files")}
system(paste("7z a ",paste0("graphs_mod/files/",zip_file),"graphs_pdf/."))

system("scp -r graphs_mod/files root@194.182.65.144:/srv/shiny-server/zamestnancistatu/")

unlink(x = paste0("graphs_mod/files/",list.files("graphs_mod/files")))
unlink(x = paste0("graphs_pdf/",list.files("graphs_pdf/")))
