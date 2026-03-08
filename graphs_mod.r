library(ff)

crosstalk_dir <- fs::dir_ls("graphs/js/", type = "directory", regexp = "crosstalk")[1]
file.copy(file.path(crosstalk_dir, "js/crosstalk.js"), "www/js", overwrite = TRUE)

plotly_binding_dir <- fs::dir_ls("graphs/js/", type = "directory", regexp = "plotly\\-binding")[1]
file.copy(file.path(plotly_binding_dir, "plotly.js"),
          "www/js/plotly-htmlwidgets.js", overwrite = TRUE)

plotly_main_dir <- fs::dir_ls("graphs/js/", type = "directory", regexp = "plotly\\-main")[1]
file.copy(file.path(plotly_main_dir, "plotly-latest.min.js"),
          "www/js/plotly.js", overwrite = TRUE)

htmlwidgets_dir <- fs::dir_ls("graphs/js/", type = "directory", regexp = "htmlwidgets")[1]
file.copy(file.path(htmlwidgets_dir, "htmlwidgets.js"),
          "www/js/htmlwidgets.js", overwrite = TRUE)

jquery_dir <- fs::dir_ls("graphs/js/", type = "directory", regexp = "jquery")[1]
file.copy(file.path(jquery_dir, "jquery.js"),
          "www/js/jquery.js", overwrite = TRUE)




if(!dir.exists("graphs_mod")){dir.create("graphs_mod")}

fs::dir_copy("www/icons/","graphs_mod/icons", overwrite = TRUE)
fs::dir_copy("www/js/","graphs_mod/js", overwrite = TRUE)
fs::dir_copy("www/styles/","graphs_mod/styles", overwrite = TRUE)
fs::dir_copy("www/img/","graphs_mod/img", overwrite = TRUE)

template <- readLines("www/template.html")

keywords <- data.table::fread("keywords.csv")
keywords_template <- readLines("www/keywords_template.html")

# use config year to replace static '2024' in textual CSV fields
cfg <- config::get()
this_year <- if (!is.null(cfg$rok)) cfg$rok else if (!is.null(cfg$this_year)) cfg$this_year else 2024
this_year_chr <- as.character(this_year)

# normalize year mentions in loaded CSV/text data
if (nrow(keywords) > 0 && "keyword_definition" %in% names(keywords)) {
  keywords[, keyword_definition := gsub("\\{YEAR\\}", this_year_chr, keyword_definition)]
}

lfiles <- list.files("graphs", full.names = FALSE)
lfiles <- lfiles[grepl("html",lfiles) & !grepl("mod",lfiles)]
lfiles <- lfiles[order(as.numeric(gsub("[^0-9]","",lfiles)))]

graph_titles <- data.table::fread("graph_titles.csv")
## replace year placeholder in graph titles with config year
if (nrow(graph_titles) > 0) {
  if ("title" %in% names(graph_titles)) graph_titles[, title := gsub("\\{YEAR\\}", this_year_chr, title)]
  if ("title_short" %in% names(graph_titles)) graph_titles[, title_short := gsub("\\{YEAR\\}", this_year_chr, title_short)]
  if ("title_sub" %in% names(graph_titles)) graph_titles[, title_sub := gsub("\\{YEAR\\}", this_year_chr, title_sub)]
}
tooltips <- readLines("graphs_mod/js/tooltips.js")
tooltips <- c(sapply(gsub("\\.html$","",lfiles),function(g)gsub("id",g,tooltips)))
writeLines(tooltips,"graphs_mod/js/tooltips.js")

toc_list <- c(
  # '<ul class="tocify-header nav nav-list" style="display: block;"><li class="tocify-item" style="cursor: pointer;"><a href="zkratky.html" target="_blank">Seznam zkratek</a></li>',
  '<ul class="tocify-header nav nav-list" style="display: block;">
  <li class="tocify-item" style="cursor: pointer;"><a href="https://idea.cerge-ei.cz/images/studie/2022/IDEA_Studie_2_2022_Statni_zamestnanci_a_urednici.pdf#page=6"
   onclick="window.open(this.href,\'targetWindow\',
                                   `toolbar=no,
                                    location=no,
                                    status=no,
                                    menubar=no,
                                    scrollbars=yes,
                                    resizable=yes,
                                    width=600,
                                    height=800`);
 return false;">Seznam zkratek</a></li>
  <li class="tocify-item" style="cursor: pointer;"><a href="https://idea.cerge-ei.cz/images/studie/2022/IDEA_Studie_2_2022_Statni_zamestnanci_a_urednici.pdf#page=19"
   onclick="window.open(this.href,\'targetWindow\',
                                   `toolbar=no,
                                    location=no,
                                    status=no,
                                    menubar=no,
                                    scrollbars=yes,
                                    resizable=yes,
                                    width=600,
                                    height=800`);
 return false;">Data a metodologie</a></li>',
  '<ul class="tocify-header nav nav-list" style="display: block;"><li>Hlavní grafy</li>',
  sapply(lfiles[!grepl("A",lfiles,ignore.case = F)],function(li){
    paste0('<ul class="tocify-subheader nav nav-list" style="display: block;"><li class="tocify-item" style="cursor: pointer;"><a id="',gsub("\\.html$","",li),'" href="',li,'" data-toggle="tooltip" data-placement="bottom" title="',graph_titles$title[graph_titles$graph==gsub("\\.html$","",li)],'">',
           stringr::str_to_title(gsub("[[:punct:]]"," ",gsub("\\.html","",li))),': ',graph_titles$title_short[graph_titles$graph==gsub("\\.html$","",li)],'</a></li></ul>')
  }),'</ul>',
  '<ul class="tocify-header nav nav-list" style="display: block;"><li>Grafy z přílohy</li>',
  sapply(lfiles[grepl("A",lfiles,ignore.case = F)],function(li){
    paste0('<ul class="tocify-subheader nav nav-list" style="display: block;"><li class="tocify-item" style="cursor: pointer;"><a id="',gsub("\\.html$","",li),'" href="',li,'" data-toggle="tooltip" data-placement="top" title="',graph_titles$title[graph_titles$graph==gsub("\\.html$","",li)],'">',stringr::str_to_title(gsub("[[:punct:]]"," ",gsub("\\.html","",li))),'</a></li></ul>')
  }),'</ul>')

inx <- 1:length(lfiles)
# inx <- c(1,3)
for(i in inx){
  f <- lfiles[i]
  cat(f,"\n")

  gr_keywords <- keywords[keywords$graph==gsub("\\.html$","",f),c("keyword_name","keyword_definition")]
  gr_keywords <- setNames(as.character(gr_keywords$keyword_definition),gr_keywords$keyword_name)
  gr_keywords <- c(sapply(gr_keywords, function(k) gsub("keyword_definition",unname(k),gsub("keyword_name",names(gr_keywords)[unname(gr_keywords)==k],keywords_template[grep("details",keywords_template)[1]:grep("details",keywords_template)[2]]))))
  gr_keywords <- c(keywords_template[1:(grep("details",keywords_template)[1]-1)],gr_keywords,keywords_template[(grep("details",keywords_template)[2]+1):length(keywords_template)])

  gr <- readLines(file.path("graphs", f))
  gr_content <- gr[grepl('id="htmlwidget',gr) | grepl('type="application',gr)]
  gr_content <- gsub('id=\\"htmlwidget_container\\"','class="htmlwidget_container" style="height:100%"',gr_content)
  # gr_content <- gsub('id=\\"htmlwidget_container\\"','class="htmlwidget_container"',gr_content)
  gr_content <- gsub('height\\:400px','height:100%; min-height:700px"',gr_content)

  extra_script <- paste0('<script>
function placeLegendAnnot() {
plot_height = $(".plot-container").outerHeight();
plot_width = $(".plot-container").outerWidth();
annotation_width = $(".annotation .cursor-pointer rect.bg").outerWidth();
legend_width = $(".legend rect.bg").outerWidth();
if ($(".gtitle").length == 1){document.querySelector(".gtitle").style.transform = "translate(0px,',ifelse(i %in% c(11,12),-10,ifelse(i == 18,10,0)),'px)"};
if ($(".annotation .cursor-pointer").length != 0){document.querySelector(".annotation .cursor-pointer").style.transform = "translate(" + (plot_width-annotation_width)/2 + "px," + (plot_height - ',
                         ifelse(i %in% c(11),60,
                                ifelse(i %in% c(14,5),70,
                                       ifelse(i %in% c(1,2),30,
                                              ifelse(i %in% c(3,4,7,8,9,10,12,13,20),50,
                                                     80)))),') + "px)"};
if ($(".legend").length == 1){document.querySelector(".legend").style.transform = "translate(" + (plot_width-legend_width)/2 + "px," + (plot_height -',
                         ifelse(i %in% c(6),30,
                                ifelse(i %in% c(11),100,
                                       90)),') + "px)"};
};
window.addEventListener("DOMContentLoaded", placeLegendAnnot, false);
</script>')

  toc <- toc_list
  toc[grepl(f,toc)] <- gsub("tocify-item","tocify-item active",toc[grepl(f,toc)])
  toc <- paste(toc,collapse="\n")

  gr <- unlist(lapply(as.list(template), function(x)
    if(grepl("page title here",x)) stringr::str_to_title(gsub("[[:punct:]]"," ",gsub("\\.html","",f)))
    else if(grepl("list of graphs here",x)) toc
    else if(grepl("graph content here",x)) gr_content
    else if(grepl("keywords here",x)) gr_keywords
    else if(grepl("extra script here",x)) extra_script
    else x
  ))

  writeLines(gr,paste0("graphs_mod/",f))
}

{
  template <- readLines("www/template_table.html")
  table <- read.csv("zkratky.csv",stringsAsFactors = F)
  table <- lapply(table[],function(x)gsub("^[[:space:]]+|[[:space:]]+$","",x))
  tb_content <- paste(
    c("<table id='table' class='display' style='width:100%; max-width:650px; margin:0'>",
      paste0(c("<thead><tr>",paste0("<th style='text-align:left'>",gsub("\\."," ",names(table)),"</th>"),"</tr></thead>"),collapse=""),
      paste0(c("<tbody>",sapply(data.table::transpose(table),function(row)paste0(c("<tr>",paste0("<td>",row,"</td>"),"</tr>"),collapse="")),"</tbody>"),collapse=""),
      "</table>"),
    collapse=""
  )

  toc <- toc_list
  toc[grepl("zkratky",toc)] <- gsub("tocify-item","tocify-item active",toc[grepl("zkratky",toc)])
  toc <- paste(toc,collapse="\n")

  tb <- unlist(lapply(as.list(template), function(x)
    if(grepl("page title here",x)) "Seznam zkratek"
    else if(grepl("list of graphs here",x)) toc
    else if(grepl("table content here",x)) tb_content
    else if(grepl("extra script here",x)) ""
    else x
  ))

  writeLines(tb,paste0("graphs_mod/zkratky.html"))
}
