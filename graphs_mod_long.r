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

template <- readLines("www/template_long.html")

keywords <- data.table::fread("keywords.csv")
keywords_template <- readLines("www/keywords_template.html")

lfiles <- list.files("graphs", full.names = FALSE)
lfiles <- lfiles[grepl("html",lfiles) & !grepl("mod",lfiles)]
lfiles <- lfiles[order(as.numeric(gsub("[^0-9]","",lfiles)))]

graph_titles <- data.table::fread("graph_titles.csv")
tooltips <- readLines("graphs_mod/js/tooltips.js")
tooltips <- c(sapply(gsub("\\.html$","",lfiles),function(g)gsub("id",g,tooltips)))
writeLines(tooltips,"graphs_mod/js/tooltips.js")

long <- TRUE

group_graphs <- function(group,withsub = FALSE){
  if(withsub){
    return(
      c('<ul class="tocify-header nav nav-list" style="display: block;">',
        paste0('<li class="tocify-item" style="cursor: pointer;">',
               '<a id="group_',group,'" ',
               'href="',graph_titles$graph[graph_titles$graph_group==group][1],'.html">',
               graph_titles$title_short[graph_titles$graph_group==group][1],
               '</a>',
               '</li>'),
        '<ul class="tocify-subheader nav nav-list" style="display: block;">',
        sapply(graph_titles$title_sub[graph_titles$graph_group==group],function(sub)
          paste0('<li class="tocify-item" style="cursor: pointer;">',
                 '<a id="toc_',graph_titles$graph[graph_titles$graph_group==group & graph_titles$title_sub==sub],'" ',
                 'href="',graph_titles$graph[graph_titles$graph_group==group & graph_titles$title_sub==sub],'.html" data-toggle="tooltip" data-placement="bottom" ',
                 'title="',graph_titles$title[graph_titles$graph_group==group & graph_titles$title_sub==sub],'">',
                 sub,
                 '</a>',
                 '</li>'),USE.NAMES = F),
        '</ul>','</ul>')
    )
  }else{
    return(
      paste0('<ul class="tocify-header nav nav-list" style="display: block;"><li class="tocify-item" style="cursor: pointer;">',
             '<a id="toc_',graph_titles$graph[graph_titles$graph_group==group],'" ',
             'href="',graph_titles$graph[graph_titles$graph_group==group],'.html" data-toggle="tooltip" data-placement="bottom" ',
             'title="',graph_titles$title[graph_titles$graph_group==group],'">',
             # stringr::str_to_title(gsub("[[:punct:]]"," ",graph_titles$graph[graph_titles$graph_group==group])),': ',
             graph_titles$title_short[graph_titles$graph_group==group],
             '</a>',
             '</li>',
             '</ul>')
    )
  }
}

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
  '<li class="tocify-item" style="display: block;">Hlavní grafy</li>',
  sapply(sort(unique(graph_titles$graph_group[graph_titles$graph_cat=="hlav"])),function(group){
    if(length(which(graph_titles$graph_group==group & graph_titles$title_sub!=""))>0){
      group_graphs(group,withsub = TRUE)
    }else{
      group_graphs(group)
    }
  },USE.NAMES = F),
  '<li class="tocify-item" style="display: block;">Dodatečné grafy</li>',
  sapply(sort(unique(graph_titles$graph_group[graph_titles$graph_cat=="dodat"])),function(group){
    group_graphs(group)
  })
  ,'</ul>')
# sapply(lfiles[grepl("A",lfiles,ignore.case = F)],function(li){
#   paste0('<ul class="tocify-subheader nav nav-list" style="display: block;"><li class="tocify-item" style="cursor: pointer;"><a id="',gsub("\\.html$","",li),'" href="',li,'" data-toggle="tooltip" data-placement="top" title="',graph_titles$title[graph_titles$graph==gsub("\\.html$","",li)],'">',stringr::str_to_title(gsub("[[:punct:]]"," ",gsub("\\.html","",li))),'</a></li></ul>')
# }),'</ul>'

names(toc_list) <- c("open",
                     "hlav",paste("hlav",unique(graph_titles$graph_group[graph_titles$graph_cat=="hlav"]),sep="_"),
                     "dodat",paste("dodat",unique(graph_titles$graph_group[graph_titles$graph_cat=="dodat"]),sep="_"),
                     "close")

gr_content_all <- vector(mode = "character")
extra_script_all <- vector(mode = "character")

inx <- 1:length(lfiles)
# inx <- c(1,3)
for(i in inx){
  f <- lfiles[i]
  gr_id <- gsub("\\.html","",f)
  toc_id <- paste(graph_titles$graph_cat[graph_titles$graph==gr_id],graph_titles$graph_group[graph_titles$graph==gr_id],sep="_")
  cat(f,"\n")

  # gr_keywords <- keywords[keywords$graph==gsub("\\.html$","",f),c("keyword_name","keyword_definition")]
  # gr_keywords <- setNames(as.character(gr_keywords$keyword_definition),gr_keywords$keyword_name)
  # gr_keywords <- c(sapply(gr_keywords, function(k) gsub("keyword_definition",unname(k),gsub("keyword_name",names(gr_keywords)[unname(gr_keywords)==k],keywords_template[grep("details",keywords_template)[1]:grep("details",keywords_template)[2]]))))
  # gr_keywords <- c(keywords_template[1:(grep("details",keywords_template)[1]-1)],gr_keywords,keywords_template[(grep("details",keywords_template)[2]+1):length(keywords_template)])

  gr <- readLines(file.path("graphs", f))
  gr_content <- gr[grepl('id="htmlwidget',gr) | grepl('type="application',gr)]
  gr_content <- gsub('id=\\"htmlwidget_container\\"',
                     paste0('id="',gr_id,'" class="htmlwidget_container" style="height:100%"'),
                     gr_content)
  gr_content <- gsub('height\\:400px','height:100%; min-height:700px"',gr_content)

  extra_script <- paste0('window.addEventListener("DOMContentLoaded", placeLegendAnnot("',gr_id,'"), false);')

  gr_content_all <- append(gr_content_all,c(gr_content, "</div><br><br>"))
  extra_script_all <- append(extra_script_all,extra_script)
}

gr_all <- unlist(lapply(as.list(template), function(x)
  if(grepl("page title here",x)) "Státní zaměstnanci a úředníci: kde pracují a za kolik?"
  else if(grepl("list of graphs here",x)) paste(unlist(toc_list),collapse="\n")
  else if(grepl("graph content here",x)) gr_content_all
  else if(grepl("keywords here",x)) ""
  else if(grepl("extra script here",x)) extra_script_all
  else x
))


gr_content_all <- vector(mode = "character")
extra_script_all <- vector(mode = "character")

inx <- 1:length(lfiles)
# inx <- c(1,3)
for(i in inx){
  f <- lfiles[i]
  gr_id <- gsub("\\.html","",f)
  gr_group <- graph_titles$graph_group[graph_titles$graph==gr_id]
  gr_cat <- graph_titles$graph_cat[graph_titles$graph==gr_id]
  title_sub <- graph_titles$title_sub[graph_titles$graph==gr_id]
  title_short <- graph_titles$title_short[graph_titles$graph==gr_id]
  toc_id <- paste(gr_cat,gr_group,sep="_")

  header_toc <- ifelse(title_sub!="",
                       ifelse(which(graph_titles$graph[graph_titles$graph_group==gr_group]==gr_id)==1,
                              paste0("<h2 style='font-size:0px;margin:1px'>",title_short,"</h2>",
                                     "<h3 style='font-size:0px;margin:1px'>",title_sub,"</h3>"),
                              paste0("<h3 style='font-size:0px;margin:1px'>",title_sub,"</h3>")),
                              paste0("<h2 style='font-size:0px;margin:1px'>",title_short,"</h2>"))
  header_toc <- ifelse(which(graph_titles$graph[graph_titles$graph_cat==gr_cat]==gr_id)==1,
                       paste0("<h1 style='font-size:0px!important;margin:1px'>",ifelse(gr_cat=="hlav","Hlavní grafy","Dodatečné grafy"),"</h1>",header_toc),
                       header_toc)
  header_page <- graph_titles$title[graph_titles$graph==gr_id]
  cat(f,"\n")

  gr <- readLines(file.path("graphs", f))
  gr_content <- gr[grepl('id="htmlwidget',gr) | grepl('type="application',gr)]
  gr_content <- gsub('id=\\"htmlwidget_container\\"',
                     paste0('id="',gr_id,'" class="htmlwidget_container" style="height:100%"'),
                     gr_content)
  gr_content <- gsub('height\\:400px','height:100%; min-height:700px"',gr_content)

  extra_script <- paste0('window.addEventListener("DOMContentLoaded", placeLegendAnnot("',gr_id,'"), false);')

  gr_content <- c(header_toc,gr_content)

  gr_content_all <- append(gr_content_all,c(gr_content, "</div><br>"))
  extra_script_all <- append(extra_script_all,extra_script)
}

gr_all <- unlist(lapply(as.list(template), function(x)
  if(grepl("page title here",x)) "Státní zaměstnanci a úředníci: kde pracují a za kolik?"
  # else if(grepl("list of graphs here",x)) paste(unlist(toc_list),collapse="\n")
  else if(grepl("graph content here",x)) gr_content_all
  # else if(grepl("keywords here",x)) ""
  else if(grepl("extra script here",x)) extra_script_all
  else x
))


writeLines(gr_all,paste0("graphs_mod/graphs_long.html"))