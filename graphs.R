library(dplyr)
library(plotly )
library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(forcats)
library(RColorBrewer)
library(gplots)
library(bslib)
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(here)
library(stringr)
library(janitor)
library(czso)
library(kableExtra)
library(htmlwidgets)
library(coloratio)
library(ggokabeito)
unloadNamespace("plyr")

library(nanoparquet)
options(scipen = 100, digits = 8)
library(config)
cfg <- config::get()
# derive target year from config; fallback to 2024 if absent
this_year <- if (!is.null(cfg$rok)) as.integer(cfg$rok) else 2024
this_year_chr <- as.character(this_year)

setWidgetIdSeed(123, kind = NULL, normal.kind = NULL)
set.seed(123)

local({
  counter <- 0L
  assignInNamespace("new_id", function() {
    counter <<- counter + 1L
    sprintf("plotly_visdat_%04d", counter)
  }, ns = "plotly")
})

dta <- readRDS("./data-interim/sections.rds")
# write_parquet(dta, "dashboard/dta.parquet")
dta_sum <- readRDS("./data-interim/summary.rds")

dta$kap_name[dta$kap_name == "Ksen"] <- "KSen"
dta$kap_name[dta$kap_name == "Kparl"] <- "KSněm"
dta$kap_name[dta$kap_name == "Mzdr"] <- "MZd"
dta$kap_name[dta$kap_name == "Mspr"] <- "MSp"
dta$kategorie_2014_cz <- plyr::revalue(dta$kategorie_2014_cz,c("Státní úředníci"="Státní úředníci (celkem)"))

## plotly-utils------------------------------------------------------------------------------------------------

btnrm <- c("zoomIn2d", "zoomOut2d", "pan2d", "lasso2d", "select2d", "autoScale2d")
grdclr <- "grey"
cap_col <- "mediumblue"
wrap_len <- 135

js <- "
function(el){
el.on('plotly_restyle', function() {
placeLegendAnnot();
});
el.on('plotly_relayout', function() {
placeLegendAnnot();
});
function activateButton(btns, activeIdx) {
btns.forEach((btn, i) => {
if (i === activeIdx) {
btn.classList.add('active');
} else {
btn.classList.remove('active');
}
});
}

requestAnimationFrame(() => {
const buttons = el.querySelectorAll('.updatemenu-button');

buttons.forEach((btn, i) => {
btn.addEventListener('click', () => {
activateButton(buttons, i);
});
});

activateButton(buttons, 0);
});
}"

cap_size <- 25
pozn_size <- 17
pozn_long_size <- 14
lbl_size <- 17
kat_tick_size <- 14
num_tick_size <- 17
axis_size <- 20
lgnd_size <- 17
hover_size <- 15
mrk_min_size <- 16
mrk_maj_size <- 25

mrg <- list(l = 0, r = 0, b = 0, t = 0, pad = 0, autoexpand = FALSE)
mrg2 <- list(t = 50,b=100, autoexpand = TRUE)
mrg3 <- list(t = 50, autoexpand = TRUE)
mrg4 <- list(t = 50,b=120, autoexpand = TRUE)
mrg5 <- list(t = 80,b=120, autoexpand = TRUE)
mrg6 <- list(t = 70,b=100, autoexpand = TRUE)
mrg7 <- list(t = 70, autoexpand = TRUE)
mrg8 <- list(t = 50,b=30, l=10, autoexpand = TRUE)

uni_font <- "Arial"

title_font <- list(color = cap_col,size=cap_size,
                   # family="Georgia,Times,Times New Roman,serif")
                   family=uni_font)
title_left_pos <- list(x = 0, xanchor = "left", xref = "paper")
pozn_font <- list(size = pozn_size)
pozn_font_small <- list(size = pozn_long_size)
axis_font <- list(color = "#000000",size=axis_size,
                  family=uni_font)
legend_below = list(x = 0.5, y = -0.2,orientation = "h",xanchor = "center",yanchor = "top",font=list(size=lgnd_size,family=uni_font))
legend_below_small = list(x = 0.5, y = -0.1,orientation = "h",xanchor = "center",yanchor = "top",font=list(size=lgnd_size,family=uni_font))
legend_below_mid = list(x = 0.5, y = -0.15,orientation = "h",xanchor = "center",yanchor = "top",font=list(size=lgnd_size,family=uni_font))
kat_ticks<-list(tickfont=list(size=kat_tick_size,family=uni_font),showticklabels = TRUE,tickangle = 0,tickmode = "array")
kat_ticks_rotated<-list(tickfont=list(size=kat_tick_size,family=uni_font),showticklabels = TRUE,tickangle = -90,tickmode = "array")
num_ticks <- list(tickfont=list(size=num_tick_size,family=uni_font))
num_tilt_ticks <- list(tickfont=list(size=num_tick_size,family=uni_font),tickangle = -45)
frame_y<-list(mirror=TRUE,linewidth = 2,ticks='outside',showline=TRUE,gridcolor = grdclr)
frame_x<-list(mirror=TRUE,linewidth = 2,ticks='outside',showline=TRUE,dtick=5)
annot_below<-list(                       align='left',
                                         xref='paper',
                                         yref="paper",
                                         x=0,
                                         y=0,
                                         font = pozn_font_small,
                                         bordercolor = 'rgba(0,0,0,0)',
                                         borderwidth=1,
                                         showarrow = FALSE)

kat_order_ss <- c("Ministerstva", "Ostatní ústřední", "Neústřední st. správa")
kat_order_all <- c("Ministerstva", "Ostatní ústřední", "Neústřední st. správa",
                   "Státní úředníci (celkem)",
                   "Ostatní vč. armády", "Sbory", "Příspěvkové organizace")

color_map <- c("Ministerstva" =             "#221669",
               "Ostatní ústřední" =         "#1C00C9",
               "Neústřední st. správa" =    "#0069B4",
               "Ostatní vč. armády" =       "#3CB450",
               "Příspěvkové organizace" =   "#C3C7C4",
               "Sbory" =                    "#BB133E",
               "Ústřední orgány" =          "#EB96D8",
               "Státní úředníci (celkem)" = "#C666BC",
               "Státní správa" =            "#9BC9E9",
               "Organizační složky státu" = "#C4DFF2")


lbls <- names(color_map)

color_map <- c(ggokabeito::palette_okabe_ito(1:4),
               "#CCCCCC",
               ggokabeito::palette_okabe_ito(5:9))
names(color_map) <- lbls


text_color_map <- coloratio::cr_choose_bw(color_map)
names(text_color_map) <- names(color_map)

kaps <- unique(dta$kap_name)
color_map_kap <- ifelse(startsWith(kaps, "M"), "dimgray", "cornflowerblue")
names(color_map_kap) <- kaps
cols_df <- tibble(labels = names(color_map), color = unname(color_map)) |>
  mutate(color_text = cr_choose_bw(color_map))
# colorspace::swatchplot(cols_df$color)

chart_type <- function(title_y,
                       title_y_share,
                       label_bar,
                       label_bar_share = "Sloupce (v %)",
                       label_line = "Trendy",
                       max_bar,max_bar_share=112,max_line,
                       dtick = 10,dtick_share = 20){
  return(
    list(
      type = "buttons",
      direction = "right",
      xanchor = 'center',
      yanchor = "top",
      x = 0.5,
      y = 1,
      buttons = list(
        list(
          label = label_bar,
          method = "update",
          bgcolor = "darkred",
          args = list( list(visible = list(TRUE,TRUE,TRUE,
                                           FALSE,FALSE,FALSE,
                                           FALSE,FALSE,FALSE,
                                           TRUE,FALSE)),
                       list( yaxis = c(num_ticks,frame_y,list(title = title_y,titlefont = axis_font,
                                                              dtick = dtick,range = c(0,max_bar)) ),
                             margin = list(t=50,b=100,r=20,l=70, autoexpand = TRUE))
          )),
        list(
          label = label_bar_share,
          method = "update",
          args = list( list(visible = list(FALSE,FALSE,FALSE,
                                           TRUE,TRUE,TRUE,
                                           FALSE,FALSE,FALSE,
                                           FALSE,TRUE)),
                       list( yaxis = c(num_ticks,frame_y,list(title = title_y_share,titlefont = axis_font,
                                                              dtick = dtick_share,range = c(0,max_bar_share)) ),
                             margin = list(t=50,b=100,r=20,l=70, autoexpand = TRUE))
          )),
        list(
          label = label_line,
          method = "update",
          args = list( list(visible = list(FALSE,FALSE,FALSE,
                                           FALSE,FALSE,FALSE,
                                           TRUE,TRUE,TRUE,
                                           FALSE,FALSE)),
                       list( yaxis = c(num_ticks,frame_y,list(title = title_y,titlefont = axis_font,
                                                              dtick = dtick,range = c(0,max_line)) ),
                             margin = list(t=50,b=100,r=20,l=70, autoexpand = TRUE))
          ))
      ))
  )
}

x_ticks <- function(dta, step = 2){
  start <- min(dta$rok)
  end   <- max(dta$rok)
  if (step == 2 && start %% 2 == 0) start <- start + 1  # align to odd years so end year is always labelled
  if (step  > 2) return(rev(seq(end, start, -step)))     # anchor from end so end year is always labelled
  return(seq(start, end, step))
}

# Theme gg ----------------------------------------------------------------

source("theme.R")

## tree_prep---------------------------------------------------------------------------------------------------
aux <- dta %>%
    filter(!is.na(kategorie_2014_cz), typ_rozpoctu == "SKUT",
      !kategorie_2014 %in% c("Statni sprava", "Statni urednici"),
      rok == this_year) %>%
  group_by(kategorie_2014_cz) %>%
  summarise(cost = sum(prostredky_na_platy),
            count = sum(pocet_zamestnancu)) %>%
  rename(labels = kategorie_2014_cz) %>%
  mutate(parents = case_match(labels,
                              "Příspěvkové organizace" ~ "",
                              "Sbory" ~ "Státní správa",
                              "Ostatní vč. armády" ~ "Organizační složky státu",
                              "Neústřední st. správa" ~ "Státní úředníci (celkem)",
                              "Ostatní ústřední" ~ "Ústřední orgány",
                              "Ministerstva" ~ "Ústřední orgány"

  ))

aux_sum <- aux |>
  summarise(across(c(cost, count), sum), .by = parents)

to_append = tribble(
  ~labels,               ~parents,
  "Ústřední orgány", "Státní úředníci (celkem)",
  "Státní úředníci (celkem)", "Státní správa",
  "Státní správa", "Organizační složky státu",
  "Organizační složky státu", "") |>
  mutate(
  cost = c(
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"],
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"]+
      aux$cost[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$cost[aux$labels == "Sbory"] +
      aux$cost[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$cost[aux$labels == "Ministerstva"] +
      aux$cost[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$cost[aux$labels == "Sbory"] +
      aux$cost[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"] +
      aux$cost[aux$labels == "Ostatn\u00ED v\u010D. arm\u00E1dy"]
  ),
  count = c(
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"],
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"]+
      aux$count[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$count[aux$labels == "Sbory"] +
      aux$count[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"],
    aux$count[aux$labels == "Ministerstva"] +
      aux$count[aux$labels == "Ostatn\u00ED \u00FAst\u0159edn\u00ED"] +
      aux$count[aux$labels == "Sbory"] +
      aux$count[aux$labels == "Ne\u00FAst\u0159edn\u00ED st. spr\u00E1va"] +
      aux$count[aux$labels == "Ostatn\u00ED v\u010D. arm\u00E1dy"]
  ))

tree_data <- bind_rows(aux, to_append)

macro_numbers <- readr::read_rds("data-interim/macro_numbers.rds")

pracovni_sila <- macro_numbers$employed_total #  # dataset ČSÚ 250180, LFS Q4
state_budget <- macro_numbers$sr_vydaje # via CNB ARAD indicator SRUMD08402C
gdp <- macro_numbers$gdp # see SHDPZDRY1B1GMMLNA na https://www.cnb.cz/arad/#/cs/indicators

tree_data <- tree_data %>% mutate("cost_perc" = cost/sum(cost[which(tree_data$parents == "")]),
                                  "cost_perc_budget" = cost/state_budget,
                                  "count_perc" = count/sum(count[which(tree_data$parents  == "")]),
                                  "count_perc_sila" = count/pracovni_sila,
                                  labels = as.character(labels))

## priprava_zbytek------------------------------------------------------------------------------------------------
dta <- dta %>%
  # mutate(kategorie_2014_cz = ifelse(kategorie_2014_cz == "Státní správa",
  #                                   "Státní úředníci celkem",kategorie_2014_cz)) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel(kat_order_all) %>% fct_rev())

abs_metrics <- dta %>%
  filter(rok == this_year, typ_rozpoctu == "SCHV") %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO", "SS", "OSS")) %>%
  group_by(kap_name) %>%
  summarise(
    pocet_zamestnancu = sum(pocet_zamestnancu),
    prostredky_na_platy = sum(prostredky_na_platy),
    prumerny_plat = sum(prumerny_plat)
  )

bar_dt <- dta %>%
  filter(rok == this_year, typ_rozpoctu == "SKUT") %>%
  filter(!is.na(kategorie_2014_cz)) %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO", "SS", "OSS"),
         kategorie_2014_cz != "Státní úředníci (celkem)") %>%
  group_by(kap_name)%>%
  mutate(bar_name_count = paste(kap_name, " Celkem: ",sum(pocet_zamestnancu))) %>%
  mutate(bar_name_cost = paste(kap_name, " Celkem: ",sum(prostredky_na_platy))) %>%
  mutate(pocet_zamestnancu_agg = sum(pocet_zamestnancu),
         prostredky_na_platy_agg = sum(prostredky_na_platy),
         prumerny_plat_agg = sum(prumerny_plat))
lty <- c(schvaleny = "dash", skutecnost = "solid")

## tree_plot---------------------------------------------------------------------------------------------------
root_label <- "Zaměstnanci státu"
root_color <- "#f0f0f0"

graf_A1 <- tree_data %>%
  left_join(cols_df, by = "labels")

graf_A1$labels_tree <- graf_A1$labels
graf_A1$labels_tree[graf_A1$labels_tree=="Ostatní ústřední"] <- "  "
graf_A1$labels_tree[graf_A1$labels_tree=="Ostatní vč. armády"] <- "Ostatní<br>vč. armády"
graf_A1$labels_tree[graf_A1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")] <-
  paste0(graf_A1$labels_tree[graf_A1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")],"<br><sup>",
         gsub("^\\s+","",format(round(graf_A1$cost[graf_A1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")]/10^9,1))),
         " mld. Kč</sup>")
graf_A1$parents[graf_A1$parents==""] <- root_label
graf_A1 <- rbind(graf_A1,data.frame(labels = root_label,
                                    labels_tree = root_label,
                                    cost = sum(graf_A1$cost[graf_A1$parents==root_label]),
                                    count = sum(graf_A1$count[graf_A1$parents==root_label]),
                                    cost_perc = sum(graf_A1$cost_perc[graf_A1$parents==root_label]),
                                    cost_perc_budget = sum(graf_A1$cost_perc_budget[graf_A1$parents==root_label]),
                                    count_perc = sum(graf_A1$count_perc[graf_A1$parents==root_label]),
                                    count_perc_sila = sum(graf_A1$count_perc_sila[graf_A1$parents==root_label]),
                                    color = root_color,
                                    color_text = "black",
                                    parents = ""))

graf_A1 <- graf_A1 |>
  plot_ly(
    type = "treemap",
    branchvalues = "total",
    labels = ~labels_tree,
    parents = ~parents,
    marker = list(colors = ~color),
    pathbar = list(side = "bottom", thickness = 30),
    values = ~cost,
    textfont = list(family = uni_font,size = 18,color = ~color_text),
    hovertemplate = ~ paste("<extra></extra>", " Kategorie: ",
                            labels, "<br>", " Rozpo\u010Det:",
                            format(round(cost / 1e9, digits = 2),
                                   big.mark = " "), "mld. K\u010D", "<br>",
                            " Podíl na celkových výdajích na platy:", round(cost_perc * 100, 1), "%",
                            "<br>", " Podíl na výdajích státního rozpočtu:",
                            round(cost_perc_budget * 100,1), "%"),
    hoverlabel = list(font = list(size = hover_size, color = ~color_text)),
    domain = list(column = 0)
  ) %>%
  layout(title = list(font=title_font,
                      text = paste0("<b>Graf 1b. Výdaje na zaměstnance státu dle regulace zaměstnanosti (", this_year, ")</b>",
                                    "<br>","<sup>","Velikost obdélníků je úměrná podílu dané skupiny na celkových výdajích","</sup>"),
                      y = 0.97, x = 0, xanchor = "left", xref = "paper"),
         margin = mrg8) %>%
  # layout(annotations = list(text = "<i>Pozn.: Pro bližší detail lze kategorie rozkliknout.</i>",
  #                           x = 1, y = -0.05, showarrow = FALSE, font = pozn_font_small)) %>%
  layout(uniformtext = list(minsize = lbl_size, mode = 'show')) %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = btnrm, displayModeBar = TRUE) %>%
  onRender(js)

tree_data %>%
  left_join(cols_df, by = "labels") |>
  count(color)

graf_A1

graf_1 <- tree_data %>%
  left_join(cols_df, by = "labels")
graf_1$labels_tree <- graf_1$labels
graf_1$labels_tree[graf_1$labels_tree=="Ostatní ústřední"] <- "  "
graf_1$labels_tree[graf_1$labels_tree=="Ostatní vč. armády"] <- "Ostatní<br>vč. armády"
graf_1$labels_tree[graf_1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")] <-
  paste0(graf_1$labels_tree[graf_1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")],"<br><sup>",
        gsub("^\\s+","",format(graf_1$count[graf_1$labels %in% c("Příspěvkové organizace","Sbory","Neústřední st. správa","Ministerstva","Ostatní vč. armády")],big.mark = " ")),
        " zaměst.</sup>")
graf_1$parents[graf_1$parents==""] <- root_label
graf_1 <- rbind(graf_1,data.frame(labels = root_label,
                                  labels_tree = root_label,
                                  cost = sum(graf_1$cost[graf_1$parents==root_label]),
                                  count = sum(graf_1$count[graf_1$parents==root_label]),
                                  cost_perc = sum(graf_1$cost_perc[graf_1$parents==root_label]),
                                  cost_perc_budget = sum(graf_1$cost_perc_budget[graf_1$parents==root_label]),
                                  count_perc = sum(graf_1$count_perc[graf_1$parents==root_label]),
                                  count_perc_sila = sum(graf_1$count_perc_sila[graf_1$parents==root_label]),
                                  color = root_color,
                                  color_text = "black",
                                  parents = ""))

graf_1 <- graf_1 |>
  plot_ly(
    type = "treemap",
    branchvalues = "total",
    labels = ~labels_tree,
    parents = ~parents,
    marker = list(colors = ~color),
    pathbar = list(side = "bottom", thickness = 30),
    values = ~count,
    textfont = list(family=uni_font, size = 18,color = ~color_text),
    hovertemplate = ~ paste("<extra></extra>", " Kategorie: ", labels, "<br>",
                            " Po\u010Det zam\u011Bstnanc\u016F:",
                            format(count, big.mark = " "), "<br>", " Pod\u00EDl na zaměstnancích státu:",
                            round(count_perc * 100, 1), "%", "<br>", " Pod\u00EDl na pracovní síle ČR:",
                            round(count_perc_sila * 100,1), "%"),
    hoverlabel = list(font = list(size = hover_size,color = text_color_map)),
    domain = list(column = 0)
  ) %>%
  layout(title = list(font = title_font,
                      text = paste0("<b>Graf 1a. Počet zaměstnanců státu dle regulace zaměstnanosti (", this_year, ")</b>",
                                    "<br>","<sup>","Velikost obdélníků je úměrná podílu dané skupiny na celkovém počtu zaměstnanců státu","</sup>"),
                      y = 0.97, x = 0, xanchor = "left", xref = "paper"),
         margin = mrg8) %>%
  # layout(annotations = list(text = "<i>Pozn.: Pro bližší detail lze kategorie rozkliknout.</i>", x = 1,
  #                           y = -0.05, showarrow = FALSE, font = pozn_font_small)) %>%
  layout(uniformtext = list(minsize=lbl_size, mode = 'show')) %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = btnrm,displayModeBar = TRUE) %>%
  onRender(js)

graf_1


## counts------------------------------------------------------------------------------------------------------

graf_2 <- bar_dt %>% group_by(kategorie_2014_cz)%>%
  plot_ly(
    x = ~kap_name, y = ~ pocet_zamestnancu/1000, type="bar",
    color = ~kategorie_2014_cz, colors = color_map,
    #marker=list(size=10, colors=color_map[bar_dt$kategorie_2014_cz]),
    hovertemplate = ~ ifelse(pocet_zamestnancu > 0, paste(
      "<extra></extra>", "Kategorie:", kategorie_2014_cz, "<br>", "Kapitola:", cz_kap_name, "<br>",
      "Po\u010Det zam\u011Bstnanc\u016F:",
      format(pocet_zamestnancu, big.mark = " "), "<br>", "Celkem za kapitolu: ",
      format(pocet_zamestnancu_agg, big.mark = " ")
    ), ""), hoverinfo = "text",hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
  layout(
    # hovermode = "x",
    legend = legend_below,
    # annotations = c(list(text = str_wrap("<i>Pozn.: Ministerstvo školství, mládeže a tělovýchovy zkresluje graf vzhledem k zahrnutí učitelů v kategorii “Příspěvkové organizace”. Lze odflitrovat v legendě nebo v grafu.</i>",wrap_len),
    #                      font = pozn_font_small),
    #                 annot_below),
    title = list(font=title_font,
           text = paste0("<b>Graf 2a. Počet zaměstnanců státu dle rozpo\u010Dtov\u00FDch kapitol (", this_year, ")</b>"), y = 0.97, x = 0, xanchor = "left", xref = "paper"),
    xaxis = c(kat_ticks_rotated,frame_x,
              list(title = "<b>Kapitoly státního rozpočtu (seznam zkratek)</b>",categoryorder = "array",categoryarray = arrange(bar_dt, desc(pocet_zamestnancu_agg))$kap_name,
                   titlefont = axis_font)),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Po\u010Det zam\u011Bstnanc\u016F (v tisících)</b>",
                                     titlefont = axis_font)),
    barmode = "stack",
    margin = mrg4) %>%config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)
graf_2

## costs, include = F------------------------------------------------------------------------------------------

# annot_below_A2 <- list(align='left',
#                        xref='paper',
#                        yref="paper",
#                        x=0,
#                        y=-0,
#                        bordercolor = 'rgba(0,0,0,0)',
#                        borderwidth=1,
#                        showarrow = FALSE)
legend_below_A2 = list(x = 0.5, y = -0.2,orientation = "h",xanchor = "center",yanchor = "top",font=list(size=lgnd_size,family=uni_font))

graf_A2 <- plot_ly(bar_dt,
                   x = ~kap_name, y = ~ prostredky_na_platy / 1e9,
                   color = ~kategorie_2014_cz, colors = color_map,
                   hovertemplate = ~ ifelse(prostredky_na_platy > 0,
                                            paste("<extra></extra>", "Kategorie:",
                                                  kategorie_2014_cz, "<br>", "Kapitola:",
                                                  cz_kap_name, "<br>", "Platy celkem:",
                                                  format(prostredky_na_platy, big.mark = " "),
                                                  "K\u010D", "<br>", "Celkem za kapitolu: ",
                                                  format(prostredky_na_platy_agg, big.mark = " "),
                                                  "K\u010D"),
                                            ""),
                   hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                   hoverinfo = "text") %>%
  add_bars() %>%
  layout(
    legend = legend_below_A2,
    # hovermode = "x",
    # annotations = c(list(text = str_wrap("<i>Pozn.: Ministerstvo školství, mládeže a tělovýchovy zkresluje graf vzhledem k zahrnutí učitelů v kategorii “Příspěvkové organizace”. Lze odflitrovat v legendě nebo v grafu.</i>",wrap_len),
    #                      font = pozn_font_small),
    #                 annot_below_A2),
    title = list(font=title_font,
           text = str_wrap(paste0("<b>Graf 2b. Výdaje na  zaměstnance státu dle rozpo\u010Dtov\u00FDch kapitol (rok ", this_year, ", mld. K\u010D)</b>"),100), y = 0.96, x = 0, xanchor = "left", xref = "paper"),
    xaxis = c(kat_ticks_rotated,frame_x, list(title="<b>Kapitoly státního rozpočtu (seznam zkratek)</b>",titlefont = axis_font,categoryorder = "array", categoryarray = arrange(bar_dt, desc(prostredky_na_platy_agg))$kap_name)),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Platy (mld. Kč)</b>",titlefont = axis_font)),
    barmode = "stack",
    margin = mrg5) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)


## mean_costs_ALL----------------------------------------------------------------------------------------------

kat_means <- bar_dt %>% filter(kategorie_2014_cz != "Státní úředníci (celkem)") %>% group_by(kategorie_2014_cz)%>%
  summarise(prumerny_plat_mean = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)/12)/1e3)

vline <- function(x = 0, color = "gray",width=1) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color,width=1)
  )
}

hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,width=2)
  )
}

kat_order_graf3 <- c("Ministerstva", "Ostatní ústřední", "Neústřední st. správa",
                     "Ostatní vč. armády", "Sbory", "Příspěvkové organizace")

dt_mean_salary_all <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         !kategorie_2014 %in% c("Statni sprava", "Statni urednici"), rok == this_year) %>%
  summarise(prumerny_plat = round(sum(prostredky_na_platy)/sum(pocet_zamestnancu)/12/1e3)) %>%
  pull(prumerny_plat)

graf_3 <- bar_dt %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel(kat_order_graf3) %>% fct_rev())%>%
  filter(kategorie_2014_cz != "Státní úředníci (celkem)") %>%
  filter(prumerny_plat>25000) %>% left_join(kat_means,by="kategorie_2014_cz")%>%
  mutate(prumerny_plat = prumerny_plat/1e3) %>%
  plot_ly(line = list(color='#D3D3D3',width=2),
          x = ~prumerny_plat, y = ~ kategorie_2014_cz, color = ~kategorie_2014_cz,
          colors = color_map,marker=list(size=mrk_min_size, line = list(color = "white",width=1)),
          opacity = 1,
          type = "scatter" , mode = "line+markers",
          hovertemplate = ~ ifelse(prostredky_na_platy > 0,
                                   paste("<extra></extra>", "Kategorie:", kategorie_2014_cz, "<br>",
                                         "Kapitola:", cz_kap_name, "<br>", "Pr\u016Fm\u011Brn\u00FD plat:",
                                         format(prumerny_plat, big.mark = " "), "K\u010D"),
                                   ""),
          hoverlabel = list(font=list(size=hover_size,family=uni_font)),
          hoverinfo = "text"
  ) %>%
  add_trace(type="scatter",mode="markers",
            opacity=1,
            hovertemplate = ~ ifelse(prostredky_na_platy > 0,
                                     paste("<extra></extra>", "Pr\u016Fm\u011Br za kategorii:", "<br>",
                                           format(round(prumerny_plat_mean*1e3,0), big.mark = " "),
                                           "K\u010D"),
                                     ""),
            hoverinfo = "text",
            marker=list(size=mrk_maj_size),
            x = ~prumerny_plat_mean, y=~kategorie_2014_cz,color = ~kategorie_2014_cz,
            colors = color_map) %>%
    layout(hovermode = "closest",
      title = list(font=title_font,
         text = paste0("<b>Graf 3a. Pr\u016Fm\u011Brn\u00E9 platy zaměstnanců státu dle rozp. kapitoly (", this_year, ")</b>"), y = 1.1, x = 0, xanchor = "left", xref = "paper"),
         # annotations = list(align='left',
         #                    xref='paper',
         #                    yref="paper",
         #                    x=0,
         #                    y=-0.1,
         #                    font = pozn_font_small,
         #                    bordercolor = 'rgba(0,0,0,0)',
         #                    borderwidth=1,
         #                    showarrow = FALSE,
         #                    text = str_wrap("<i>Pozn.: Průměrné platy se liší dle typu organizací i napříč jednotlivými organizacemi.</i>",wrap_len)),
         xaxis = c(num_ticks,frame_y,list(dtick = 5,title = "<b>Průměrný hrubý měsíční plat (v tisících Kč)</b>",
                                          titlefont = axis_font)),
         yaxis = c(num_ticks,frame_y,list(title = "",titlefont = axis_font)),showlegend = FALSE,
         margin = mrg2) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)
graf_3

bar_dt$width<-0.8
## mean_costs_v2-----------------------------------------------------------------------------------------------
graf_A3 <- bar_dt %>%
  group_by(kategorie_2014_cz) %>%
  filter(kategorie_2014 != "Prispevkove organizace") %>%
  filter(kategorie_2014_cz != "Sbory") %>%
  mutate(wage_to_general = (ifelse(kategorie_2014 %in% c("Ministerstva", "Ostatni ustredni"),
                                   prumerny_plat / phasal_all - 1,
                                   prumerny_plat / czsal_all-1))) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa")) %>%
  group_map(~ plot_ly(
    data = .,
    hovertemplate = ~  paste(
      "<extra></extra>", "Kategorie:", kategorie_2014_cz, "<br>", "Kapitola:", cz_kap_name, "<br>",
      "Pr\u016Fm\u011Brn\u00FD plat:",
      format(prumerny_plat, big.mark = " "), "K\u010D", "<br>",
      "Rozd\u00EDl k pr\u016Fm\u011Brn\u00E9 mzd\u011B:",
      round(wage_to_general*100,1), "%"),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>% add_bars(x = ~kap_name, y = ~ (wage_to_general) * 100,
                 type = "bar", width=~0.8,color = ~kategorie_2014_cz, colors = color_map)%>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.25,
      yref = "paper",
      xref = "paper",
      font = list(family = uni_font, size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(margin = list(t = 150,b=100,l=20),
           yaxis = c(kat_ticks,frame_y,list(title = list(text="<b>Procenta</b>"),
                                            titlefont = axis_font,range=c(-65,65),
                                            ticktext = lapply(seq(-50,50,50), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                            tickvals = seq(-50,50,50),
                                            tickmode = "array",
                                            ticksuffix="%")),
           xaxis = c(kat_ticks_rotated,frame_x,list("categoryorder" = "total ascending"),
                     titlefont = axis_font),
           title = list(font=title_font,
                        text = str_wrap(paste0("<b>Graf 3b. Pr\u016Fm\u011Brn\u00E9 platy zaměstnanců státu v poměru k průměrné mzdě v ekonomice dle rozp. kapitoly (", this_year, ")</b>"),70),
                        x = 50, y = 0.95, x = 0, xanchor = "left", xref = "paper"), legend = list(x = 50, y = 0.5),
           showlegend = TRUE
    ), keep = TRUE) %>%
  subplot(nrows = 2, shareY = FALSE, margin = c(0.07,0.07,0.15,0.15),titleY =TRUE) %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = btnrm,displayModeBar = TRUE) %>%
  onRender(js)
graf_A3

## count_2014--------------------------------------------------------------------------------------------------
#kapitoly s velkým nárůstem zaměstnanců v období 2011-2012
zk <- dta %>% filter(rok %in% c(2011,2012)) %>%
  filter(typ_rozpoctu == "SKUT", kategorie_2014 %in% c("Ministerstva")) %>%
  select(rok,full_kap_name, kap_name, pocet_zamestnancu) %>%
  pivot_wider(names_from = c("rok"),names_prefix = "rok_",
              values_from = c("pocet_zamestnancu")) %>%
  mutate(perc_change = round((rok_2012-rok_2011)/rok_2011*100,1)) %>%
  arrange(desc(perc_change))

graf_4_dta <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(pocet_zamestnancu = sum(pocet_zamestnancu)) %>%
  group_by(rok) %>%
  mutate(pocet_zamestnancu_agg = sum(pocet_zamestnancu))

graf_4_dta_shares <- graf_4_dta %>%
  group_by(rok) %>%
  mutate(pocet_zamestnancu_share = pocet_zamestnancu / sum(pocet_zamestnancu)) %>%
  ungroup()

labels_df <- graf_4_dta %>%
  group_by(rok) %>%
  summarise(
    total_label = first(pocet_zamestnancu_agg) / 1000,
    label_text = round(first(pocet_zamestnancu_agg) / 1000,0)
  )

graf_4 <- graf_4_dta %>%
  plot_ly(
    x = ~rok, y = ~ pocet_zamestnancu / 1000, color = ~kategorie_2014_cz,
    colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
      "Zam\u011Bstnanc\u016F:", format(pocet_zamestnancu, big.mark = " "), "<br>",
      "Celkem za rok: ", format(pocet_zamestnancu_agg, big.mark = " ")
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>%
  add_bars()%>%
  layout(barmode="stack",bargap=0.5,
         title = list(font=title_font,
                      text = paste0("<b>Graf 4a. Počet státních úředníků, bez MV a MZV (2003–", this_year, ")</b>"),
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         # annotations = c(annot_below,list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. Graf A14 s kapitolami ministerstev vnitra a zahraničních věcí je v příloze.</i>",wrap_len),
         #                                  font = pozn_font_small)),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",
                                                       standoff=10),
                                          dtick=2,
                                          titlefont = axis_font),
                   list(tickvals = x_ticks(graf_4_dta))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Počet státních úředníků (v tisících)</b>",
                                          titlefont = axis_font)),
         legend = legend_below_small,
         margin = mrg2 ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)

graf_4 <- graf_4_dta_shares %>%
  plot_ly(color = ~kategorie_2014_cz, colors = color_map,
          hovertemplate = ~ paste0(
            "<extra></extra>", "Rok: ", rok, "<br>", "Kategorie: ", kategorie_2014_cz, "<br>",
            "Zam\u011Bstnanc\u016F: ", format(pocet_zamestnancu, big.mark = " ")," (",round(pocet_zamestnancu_share*100,1)," %)", "<br>",
            "Celkem za rok: ", format(pocet_zamestnancu_agg, big.mark = " ")
          ),
          hoverlabel = list(font=list(size=hover_size,family=uni_font)),
          hoverinfo = "text") %>%
  add_trace(x = ~as.character(rok), y = ~pocet_zamestnancu / 1000, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~pocet_zamestnancu_share*100, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~pocet_zamestnancu / 1000, type = 'scatter',
            mode = "line", line = list(width = 7),
            marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3))) %>%
  add_trace(data = labels_df, x = ~rok, y = ~total_label, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
  add_trace(data = labels_df, x = ~rok, y = ~100, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
    layout(barmode="stack",bargap=0.5,
      title = list(font=title_font,
         text = paste0("<b>Graf 4a. Počet státních úředníků (2003–", this_year, ")</b>"),
         y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),
                                          dtick=2,titlefont = axis_font),
                   list(tickvals = x_ticks(graf_4_dta_shares))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Počet státních úředníků (v tisících)</b>",
                                          titlefont = axis_font,
                                          dtick = 10, range = c(0,75))),
         legend = legend_below_small, margin = mrg2,
         updatemenus = list( chart_type(title_y = "<b>Počet státních úředníků (v tisících)</b>",
                                        title_y_share = "<b>Podíl státních úředníků (v %)</b>",
                                        label_bar = "Sloupce (v tisících)",
                                        max_bar = 75,max_line = 60) )) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js) %>%
  style(visible = FALSE, traces = 4:9)

graf_4


## cost_2014---------------------------------------------------------------------------------------------------
vyvoj_bar <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    prostredky_na_platy_nom = sum(prostredky_na_platy),
    prostredky_na_platy_real = sum(prostredky_na_platy * base_thisyr)
  ) %>%
  group_by(rok) %>%
  mutate(prostredky_na_platy_nom_agg = sum(prostredky_na_platy_nom),
         prostredky_na_platy_real_agg = sum(prostredky_na_platy_real),
         width_bar=0.8)

graf_A4 <- vyvoj_bar %>%
  plot_ly(
    x = ~as.character(rok), y = ~ round(prostredky_na_platy_nom / 1e9, digits = 2),
    type = "bar",
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
      "Rozpo\u010Det:", format(prostredky_na_platy_nom, big.mark = " "), "K\u010D",
      "<br>",
      "Celkem za rok: ", format(prostredky_na_platy_nom_agg, big.mark = " "), "K\u010D"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>%
    layout(barmode='stack',bargap=0.5,
      title = list(font=title_font,
         text = paste0("<b>Graf 4c. Výdaje na platy státních úředníků, bez MV a MZV (2003–", this_year, ")</b>"),
         y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         # annotations = c(list(text =str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
         #                      font = pozn_font_small),
         #                 annot_below),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),
                                                  dtick=2,titlefont = axis_font),
                           list(tickvals = x_ticks(vyvoj_bar))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Výdaje na platy (v mld. Kč)</b>",titlefont = axis_font,
                                          dtick = 5)),
         legend = legend_below_small, margin = mrg2
  ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)

vyvoj_bar_shares <- vyvoj_bar %>%
  group_by(rok) %>%
  mutate(prostredky_na_platy_nom_share = prostredky_na_platy_nom / sum(prostredky_na_platy_nom),
         prostredky_na_platy_real_share = prostredky_na_platy_real / sum(prostredky_na_platy_real)) %>%
  ungroup()

labels_df <- vyvoj_bar_shares %>%
  group_by(rok) %>%
  summarise(
    total_label = first(prostredky_na_platy_nom_agg) / 1e9,
    label_text = round(first(prostredky_na_platy_nom_agg) / 1e9,0)
  )

graf_A4 <- vyvoj_bar_shares %>%
  plot_ly(color = ~kategorie_2014_cz, colors = color_map,
          hovertemplate = ~ paste0(
            "<extra></extra>", "Rok: ", rok, "<br>", "Kategorie: ", kategorie_2014_cz, "<br>",
            "Výdaje na platy: ", format(round(prostredky_na_platy_nom/1e6, digits = 2), big.mark = " "), " mil. K\u010D (",round(prostredky_na_platy_nom_share * 100,1)," %)",
            "<br>",
            "Celkem za rok: ", format(round(prostredky_na_platy_nom_agg/1e6, digits = 2), big.mark = " "), " mil. K\u010D"
          ),
          hoverlabel = list(font=list(size=hover_size,family=uni_font)),
          hoverinfo = "text") %>%
  add_trace(x = ~as.character(rok), y = ~prostredky_na_platy_nom / 1e9, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~prostredky_na_platy_nom_share * 100, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~prostredky_na_platy_nom / 1e9, type = 'scatter',
            mode = "line", line = list(width = 7),
            marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3))) %>%
  add_trace(data = labels_df, x = ~rok, y = ~total_label, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
  add_trace(data = labels_df, x = ~rok, y = ~100, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
  layout(barmode='stack',bargap=0.5,
         title = list(font=title_font,
                      text = paste0("<b>Graf 4c. Výdaje na platy státních úředníků (2003–", this_year_chr, ")</b>"),
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),
                                          dtick=2,titlefont = axis_font),
                   list(tickvals = x_ticks(vyvoj_bar_shares))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Výdaje na platy (v mld. Kč)</b>",titlefont = axis_font,
                                          dtick = 5, range = c(0,38))),
         legend = legend_below_small, margin = mrg2,
         updatemenus = list( chart_type(title_y = "<b>Výdaje na platy (v mld. Kč)</b>",
                                        title_y_share = "<b>Výdaje na platy (v %)</b>",
                                        label_bar = "Sloupce (v mld. Kč)",
                                        max_bar = 38,max_line = 25,
                                        dtick = 5) )) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js) %>%
  style(visible = FALSE, traces = 4:9)
graf_A4

graf_A5 <- vyvoj_bar %>%
  plot_ly(
    x = ~rok, y = ~prostredky_na_platy_real / 1e9,
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", rok, "<br>", "Kategorie:",
      kategorie_2014_cz, "<br>",
      "Výdaje na platy:", format(round(prostredky_na_platy_real/1e6, digits = 2), big.mark = " "),
      " mil. K\u010D",
      "<br>",
      "Celkem za rok: ", format(round(prostredky_na_platy_real_agg/1e6, digits = 2), big.mark = " "),
      " mil. K\u010D"
    ),
    hoverlabel = list(font = list(size=hover_size)),
    hoverinfo = "text"
  ) %>%
  add_bars() %>%
  layout(barmode='stack',bargap=0.5,
         title = list(font=title_font,
                      text = paste0("<b>Graf 4d. Reálné výdaje na platy státních úředníků, bez MV a MZV (2003–", this_year_chr, ")</b>"),
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         # annotations = c(list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
         #                      font = pozn_font_small),
         #                 annot_below),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),titlefont = axis_font),
                                          # xaxis = list(categoryarray = seq(2003,this_year), categoryorder = "array"),
                   list(tickvals = x_ticks(vyvoj_bar))),
         yaxis = c(num_ticks,frame_y,list(title = paste0("<b>Reálné výdaje na platy (v mld. Kč, ceny roku ", this_year, ")</b>"),titlefont = axis_font,
                                          dtick = 10, range = c(0,50))),
         legend = legend_below_small, margin = mrg2
  ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)

labels_df <- vyvoj_bar_shares %>%
  group_by(rok) %>%
  summarise(
    total_label = first(prostredky_na_platy_real_agg) / 1e9,
    label_text = round(first(prostredky_na_platy_real_agg) / 1e9,0)
  )

graf_A5 <- vyvoj_bar_shares %>%
  plot_ly(color = ~kategorie_2014_cz, colors = color_map,
          hovertemplate = ~ paste0(
            "<extra></extra>", "Rok: ", rok, "<br>", "Kategorie: ", kategorie_2014_cz, "<br>",
            "Výdaje na platy: ", format(round(prostredky_na_platy_real/1e6, digits = 2), big.mark = " "), " mil. K\u010D (",round(prostredky_na_platy_real_share * 100,1)," %)",
            "<br>",
            "Celkem za rok: ", format(round(prostredky_na_platy_real_agg/1e6, digits = 2), big.mark = " "), " mil. K\u010D"
          ),
          hoverlabel = list(font=list(size=hover_size,family=uni_font)),
          hoverinfo = "text") %>%
  add_trace(x = ~as.character(rok), y = ~prostredky_na_platy_real / 1e9, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~prostredky_na_platy_real_share * 100, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~prostredky_na_platy_real / 1e9, type = 'scatter',
            mode = "line", line = list(width = 7),
            marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3))) %>%
  add_trace(data = labels_df, x = ~rok, y = ~total_label, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
  add_trace(data = labels_df, x = ~rok, y = ~100, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
  layout(barmode='stack',bargap=0.5,
         title = list(font=title_font,
                      text = paste0("<b>Graf 4d. Reálné výdaje na platy státních úředníků (2003–", this_year_chr, ")</b>"),
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),
                                          dtick=2,titlefont = axis_font),
                   list(tickvals = x_ticks(vyvoj_bar_shares))),
         yaxis = c(num_ticks,frame_y,list(title = paste0("<b>Reálné výdaje na platy (v mld. Kč, ceny roku ", this_year, ")</b>"),titlefont = axis_font,
                                          dtick = 10, range = c(0,50))),
         legend = legend_below_small, margin = mrg2,
         updatemenus = list( chart_type(title_y = paste0("<b>Reálné výdaje na platy (v mld. Kč, ceny roku ", this_year, ")</b>"),
                                        title_y_share = "<b>Reálné výdaje na platy (v %)</b>",
                                        label_bar = "Sloupce (v mld. Kč)",
                                        max_bar = 50,max_line = 50,dtick = 10) )) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js) %>%
  style(visible = FALSE, traces = c(4:9,11))

graf_A5

## cost_cumsum_2014--------------------------------------------------------------------------------------------
aux2 <- dta %>%
  filter(kategorie_2014_cz %in% c("Státní úředníci (celkem)", "Ministerstva",
                                  "Ostatní ústřední", "Neústřední st. správa"),
         typ_rozpoctu == "SKUT") %>%
  filter(!kap_num %in% c(314, 306)) %>%
  # not included in previous
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    prostredky_na_platy = sum(prostredky_na_platy),
    base_2003 = mean(base_2003),
    pocet_kapitol = length(kap_num),
    platy_weighted = prostredky_na_platy / pocet_kapitol
  ) %>%
  mutate(plat_base = prostredky_na_platy[1]) %>%
  mutate(plat_base_weighted = platy_weighted[1]) %>%
  mutate(output = (prostredky_na_platy / (base_2003 * plat_base) - 1)) %>%
  mutate(output_weighted = (prostredky_na_platy / base_2003 - plat_base) / plat_base) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa", "Státní úředníci (celkem)"))

graf_A6 <- aux2 %>%
  plot_ly(
    x = ~rok, y = ~ output * 100, type = "scatter", color = ~kategorie_2014_cz,
    colors = color_map,
    mode = "line", line = list(width = 7),
    marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3)),
    text = ~ paste(
      " Rok:", rok, "<br>", "Kategorie:", kategorie_2014_cz, "<br>",
      "Hodnota:", round(output, 4) * 100, "%"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text",
    legendgroup = ~kategorie_2014_cz
  ) %>%
  layout(
    title = list(font=title_font,
                 text = str_wrap("<b>Graf 4e. Změna reálných výdajů na platy státních úředníků, bez MV a MZV</b>",100), x = 0, xanchor = "left", xref = "paper"),
    # annotations = c(annot_below,list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie.</i>",200),
    #                                  font = pozn_font_small)),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",standoff=10),
                                     titlefont = axis_font),
              list(tickvals = x_ticks(aux2))),
    yaxis = c(num_ticks,frame_y,list(title = str_wrap("<b>Změna reálných výdajů na platy oproti roku 2003 (v % základny roku 2003)</b>",50),
                                     ticksuffix = "%",titlefont = axis_font,
                                     ticktext = lapply(seq(-10,70,10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                     tickvals = seq(-10,70,10),
                                     tickmode = "array")),
    margin = mrg2,
    legend=legend_below_small) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)


## mean_wage_thisyr----------------------------------------------------------------------------------------------

graf_5_dt <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici"),
         typ_rozpoctu == "SKUT") %>%
  #filter(!kap_num %in% c(314,306)) %>% #not included in previous
  group_by(rok, kategorie_2014_cz) %>%
  summarise(
    prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12,
    base_thisyr = mean(base_thisyr),
    max_change_kap = kap_name[which.max(wage_in_thisyr_change)],
    max_change = round(max(wage_in_thisyr_change, na.rm = TRUE), 4),
    min_change_kap = kap_name[which.min(wage_in_thisyr_change)],
    min_change = round(min(wage_in_thisyr_change, na.rm = TRUE), 4)
  ) %>%
  mutate(wage_in_thisyr = prumerny_plat_agg * base_thisyr) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci (celkem)"))
graf_5 <- plot_ly(graf_5_dt,
                  x = ~rok, y = ~ wage_in_thisyr / 1000, type = "scatter", color = ~kategorie_2014_cz,
                  colors = color_map,
                  mode = "line", line = list(width = 7),
                  marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3)),
                  text = ~ paste0(
                    " Rok: ", rok, " <br> ", "Kategorie: ", kategorie_2014_cz, " <br> ", "Hodnota: ",
                    format(round(wage_in_thisyr, 0), big.mark = " "), "K\u010D", "<br>",
                    "Nejv\u011Bt\u0161\u00ED nárůst: ", " <br> ", max_change_kap, ": ",
                    max_change * 100, " %", " <br> ",
                    ifelse(min_change>0,"Nejmen\u0161\u00ED nárůst: ","Nejv\u011Bt\u0161\u00ED pokles: "), "<br>",
                    min_change_kap, ": ",
                    min_change * 100, " %"),
                  hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                  hoverinfo = "text",
                  legendgroup = ~kategorie_2014_cz) %>%
  layout(
    legend = legend_below_mid,
    # annotations = c(list(text = str_wrap(paste0("<i>Pozn.: Reálné hrubé měsíční platy jsou uvedeny v cenách roku ", this_year, ".</i>"),wrap_len),
    #                      font = pozn_font_small),annot_below),
    title =list(text = paste0("<b>Graf 5a. Reálné průměrné platy státních úředníků 2004–", this_year, " (v cenách roku ", this_year, ")</b>"),
                y =0.98,
                font=title_font, x = 0, xanchor = "left", xref = "paper"),
    xaxis = c(num_ticks,frame_y,list(title = "<b>Rok</b>",titlefont = axis_font),
              list(tickvals = x_ticks(graf_5_dt, step=3))),
    yaxis = c(num_ticks,frame_y,list(title = paste0("<b>Reálné průměrné platy (tis. Kč, ceny roku ", this_year, ")</b>"),titlefont = axis_font)),
    margin = mrg2
  ) %>% config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)
graf_5


# G5 static ---------------------------------------------------------------

# graf_5_static <- ggplot(graf_5_dt, aes(rok, wage_in_thisyr/1e3, colour = kategorie_2014_cz)) +
#   geom_line(size = 1.9) +
#   geom_point(colour = "black", size = 1.9) +
#   theme_minimal(base_family = uni_font, base_size = 14) +
#   theme_urednici +
#   scale_color_manual(values = color_map, name = NULL, limits = force) +
#   scale_x_continuous(breaks = seq(2003, this_year, 2)) +
#   labs(title = paste0("Graf 5. Průměrné platy státních úředníků (2004–", this_year, ") v cenách roku ", this_year),
#        x = "Rok",
#        y = paste0("Reálné průměrné hrubé měsíční mzdy (tis. Kč) v cenách roku ", this_year),
#        caption = paste0("Pozn.: reálné hrubé měsíční platy, uvedené v cenách roku ", this_year))
# graf_5_static

# ggsave("graphs-static/graf-5.png", plot = graf_5_static, width = 8, height = 5, scale = 1.5, bg = "white")

## mean_wage_pct_change_thisyr-----------------------------------------------------------------------------------
graf_A7_dt <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni"),
         typ_rozpoctu == "SKUT") %>%
  filter(!kap_num %in% c(314, 306)) %>%
  # not included in previous
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12,
    base_thisyr = mean(base_thisyr),
    max_change_kap = kap_name[which.max(mzda_prumer_skut_ke_skut)],
    max_change = round(max(mzda_prumer_skut_ke_skut, na.rm = TRUE), 4),
    min_change_kap = kap_name[which.min(mzda_prumer_skut_ke_skut)],
    min_change = round(min(mzda_prumer_skut_ke_skut, na.rm = TRUE), 4)
  ) %>%
  mutate(wage_in_thisyr = prumerny_plat_agg * base_thisyr) %>%
  group_by(kategorie_2014_cz) %>%
  arrange(rok) %>%
  mutate(wage_base = wage_in_thisyr[1]) %>%
  mutate(cum_pct_wage_change = (wage_in_thisyr - wage_base) / wage_base) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci (celkem)"))

graf_A7 <- plot_ly(graf_A7_dt,
                   x = ~rok, y = ~ cum_pct_wage_change * 100, type = "scatter",
                   color = ~kategorie_2014_cz, colors = color_map, mode = "line",
                   line = list(width = 7),
                   marker = list(size=5,symbol = "circle",line = list(width = 3,color="black")),
                   text = ~ paste0(
                     " Rok: ", rok, " <br> ", "Kategorie: ", kategorie_2014_cz, " <br> ", "Hodnota: ",
                     round(cum_pct_wage_change * 100,2), " %"," <br> ",
                     "Nejv\u011Bt\u0161\u00ED nárůst: ", " <br> ", max_change_kap, ": ",
                     max_change * 100, " %", " <br> ",
                     ifelse(min_change>0,"Nejmen\u0161\u00ED nárůst: ","Nejv\u011Bt\u0161\u00ED pokles: "), "<br>",
                     min_change_kap, ": ", min_change * 100, " %", "<br>"
                   ),
                   hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                   hoverinfo = "text",
                   legendgroup = ~kategorie_2014_cz
) %>%
  layout(
    title = list(font=title_font,
                 text="<b>Graf 5c. Změna reálných platů státních úředníků</b>",
                 y = 0.96, x = 0, xanchor = "left", xref = "paper"),
    # annotations = c(list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
    #                      font = pozn_font_small),annot_below),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",standoff=10),
                                     titlefont = axis_font),
              list(tickvals = x_ticks(graf_A7_dt, step=3))),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Změna reálného průměrného platu oproti roku 2004 \n (v % základny roku 2004)</b>",
                                     # tickprefix = "+",
                                     # showtickprefix = "last",
                                     ticktext = lapply(seq(0,40,10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                     tickvals = seq(0,40,10),
                                     tickmode = "array",
                                     ticksuffix = "%",
                                     showticksuffix = "all",
                                     titlefont = axis_font)),
    legend = legend_below_small, margin = mrg6) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)

graf_A7


## wage_to_general---------------------------------------------------------------------------------------------

annot_6<-list(                       align='left',
                                     xref='paper',
                                     yref="paper",
                                     x=0,
                                     y=-0.3,
                                     font = list(size = pozn_long_size),
                                     bordercolor = 'rgba(0,0,0,0)',
                                     borderwidth=1,
                                     showarrow = FALSE)

graf_6_dt <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni"),
         typ_rozpoctu == "SKUT") %>%
  #filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014, kategorie_2014_cz, rok) %>%
  summarise(
    prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12,
    max_change_kap = kap_name[which.max(mzda_k_nh)],
    phasal_all = mean(phasal_all),
    czsal_all = mean(czsal_all),
    max_change = round(max(mzda_k_nh, na.rm = TRUE), 4),
    min_change_kap = kap_name[which.min(mzda_k_nh)],
    min_change = round(min(mzda_k_nh, na.rm = TRUE), 4)
  ) %>%
  mutate(wage_to_general = (ifelse(kategorie_2014 %in% c("Ministerstva", "Ostatni ustredni"),
                                   prumerny_plat_agg / phasal_all,
                                   prumerny_plat_agg / czsal_all))) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci (celkem)"))

graf_6 <- plot_ly(graf_6_dt,
                  line = list(width = 7),
                  x = ~rok, y = ~ wage_to_general * 100, type = "scatter",
                  color = ~kategorie_2014_cz,
                  colors = color_map,mode = "line",
                  marker = list(size=5,symbol="circle-dot",
                                line = list(color="Black",width=3)),
                  text = ~ paste0(
                    " Rok: ", rok, " <br> ", "Kategorie: ", kategorie_2014_cz, " <br> ",
                    "Nejv\u011Bt\u0161\u00ED nárůst: ", "<br>", max_change_kap, ": ",
                    max_change * 100, " %", "<br>",
                    ifelse(min_change > 0, "Nejmen\u0161\u00ED nárůst: ","Nejv\u011Bt\u0161\u00ED pokles: "),
                    " <br> ", min_change_kap, ": ", min_change * 100, " %", " <br>"
                  ),
                  hoverlabel = list(font=list(size=hover_size,family=uni_font)),
                  hoverinfo = "text",
                  legendgroup = ~kategorie_2014_cz
) %>%layout(
  shapes = list(hline(100)),
  title = list(font=title_font,
               text = "<b>Graf 5b. Průměrné platy státních úředníků v poměru k průměrné mzdě v ekonomice</b>", x = 0, xanchor = "left", xref = "paper"),
  # annotations = c(annot_6,list(text = str_wrap("<i>Pozn.: Pro ministerstva a ostatní ústřední orgány použité hodnoty průměrné mzdy v Praze. V ostatních případech je jako reference použitý průměrný plat v národním hospodářství. Hodnota 100% znamená, že průměrný plat v kategorii je stejný jako průměrný plat v národním hospodářství.</i>",wrap_len),
  #                              font = pozn_font_small)),
  xaxis = c(num_ticks,frame_y,list(title = "<b>Rok</b>",titlefont = axis_font),
            list(tickvals = x_ticks(graf_6_dt, step=3))),
  yaxis = c(num_ticks,frame_y,list(title = "<b>Poměr platů státních úředníku a prům. mzdy (v %)</b>",
                                   # ticksuffix = "%",
                                   titlefont = axis_font)),
  margin = mrg5,
  legend=legend_below_mid) %>%config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)

graf_6

# G6 static ---------------------------------------------------------------

# graf_6_static <- ggplot(graf_6_dt, aes(rok, wage_to_general, colour = kategorie_2014_cz)) +
#   geom_line(size = 1.9) +
#   geom_point(colour = "black", size = 1.9) +
#   theme_minimal(base_family = uni_font, base_size = 14) +
#   theme_urednici +
#   scale_color_manual(values = color_map, name = NULL, limits = force) +
#   scale_x_continuous(breaks = seq(2003, this_year, 2)) +
#   ptrr::scale_y_percent_cz() +
#   labs(title = paste0("Graf 6. Průměrný plat státních úředníků \nvůči průměrné mzdě v národním hospodářství (2004–", this_year, ")"),
#        y = "Poměr platů státních úředníku a prům. mzdy (v %)",
#        x = "Rok",
#        caption = str_wrap("Pozn.: pro ministerstva a ostatní ústřední orgány použité hodnoty průměrné mzdy v Praze. V ostatních případech je jako reference použitý průměrný plat v národním hospodářství. Hodnota 100% znamená, že průměrný plat v kategorii je stejný jako průměrný plat v národním hospodářství.",
#                           150))
#
# graf_6_static

# ggsave("graphs-static/graf-6.png", plot = graf_6_static, width = 8, height = 5, scale = 1.5, bg = "white")


## thisyr_effect-------------------------------------------------------------------------------------------------

line <- list(
  type = "line",
  line = list(color = "pink"),
  xref = "paper",
  yref = "paper",
  "y0" = 0,
  "y1" = 1,
  "x0" = 0,
  "x1" = 1
)

infl <- dta %>% filter(rok == 2004) %>% select(base_thisyr) %>% first() %>% pull()

graf_A8_dt <- dta %>%
  filter(!is.na(kategorie_2014_cz), typ_rozpoctu == "SKUT",
         !kategorie_2014 %in% c("Statni sprava", "Statni urednici")) %>%
  filter(rok %in% c(2004, this_year)) %>%
  filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(
    base_thisyr = base_thisyr[1],
    zam_skutecnost = sum(pocet_zamestnancu),
    plat_skutecnost = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12
  ) %>%
  as.data.table() %>%
  dcast(kategorie_2014_cz ~ rok, value.var = c("zam_skutecnost", "plat_skutecnost")) %>%
  mutate(
    zam_change = (.data[[paste0("zam_skutecnost_", this_year)]] / .data[["zam_skutecnost_2004"]] - 1),
    plat_change = (.data[[paste0("plat_skutecnost_", this_year)]] / (.data[["plat_skutecnost_2004"]] * infl) - 1)
  )

graf_A8 <- graf_A8_dt %>%
  plot_ly(
    x = ~ plat_change * 100, y = ~ zam_change * 100, color = ~kategorie_2014_cz,
    colors = color_map,
    text = ~ paste(
      " Zm\u011Bna platu:", round(plat_change, 4) * 100, "%", "<br>",
      "Zm\u011Bna po\u010Dtu zam\u011Bstnanc\u016F:", round(zam_change, 4) * 100, "%"
    ),
    hoverlabel = list(font=list(size=hover_size)),
    hoverinfo = "text",
    legendgroup = ~kategorie_2014_cz
  ) %>%
  layout(
    title = list(font=title_font,
           text = paste0("<b>Graf 6. Celkové změny platů a počtu zaměstnanců v období 2004–", this_year_chr, "</b>"), y = 0.98, x = 0, xanchor = "left", xref = "paper"),
    # annotations = c(annot_below,list(text = str_wrap("<i>Pozn.: Pro srovnatelnost v čase graf nezahrnuje zaměstnance ministerstev vnitra a zahraničních věcí, viz Příloha 1: Data a metodologie. </i>",wrap_len),
    #                                  font = pozn_font_small)),
    showlegend = FALSE,
    xaxis = c(num_ticks,frame_x,list(title = "<b>Zm\u011Bna pr\u016Fm\u011Brn\u00E9ho platu (v %)</b>",
                                          titlefont = axis_font,
                                          ticksuffix = "%",range = c(0,40))),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Zm\u011Bna po\u010Dtu zam\u011Bstnanc\u016F (v %)</b>",
                                     titlefont = axis_font,
                                     range = c(-20,20),
                                     # tickprefix = "+",
                                     # showtickprefix = "last",
                                     ticksuffix = "%",
                                     ticktext = lapply(seq(-20,20,10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                     tickvals = seq(-20,20,10),
                                     tickmode = "array",
                                     showticksuffix = "all")),
    legend = legend_below, margin = mrg2
  ) %>%
  add_markers(marker=list(size=mrk_maj_size)) %>%
  add_text(text = ~ str_wrap(kategorie_2014_cz, 10), textposition = "top center",
           textfont = list(size=num_tick_size)) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)
graf_A8



## prac_mista_skut_rozp_kap------------------------------------------------------------------------------------
kaps_to_exclude <- dta %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_num, pocet_zamestnancu,
         full_kap_name) %>%
  spread(key = typ_rozpoctu, value = pocet_zamestnancu) %>%
  group_by(rok, kap_num) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  ungroup() %>%
  group_by(kap_num) %>%
  summarise(max_skut = max(SKUT)) %>%
  filter(max_skut < 100) %>%
  pull(kap_num)


graf_A9 <- dta %>%filter(!is.na(kap_name)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  filter(!kap_num %in% c(kaps_to_exclude, 312)) %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_name, pocet_zamestnancu,
         cz_kap_name) %>%
  spread(key = typ_rozpoctu, value = pocet_zamestnancu) %>%
  group_by(rok, kap_name, cz_kap_name) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  group_by(kap_name) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar", color = ~kap_name,
    colors = color_map_kap,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Kapitola:", cz_kap_name, "<br>",
      "Schválený po\u010Det zam\u011Bstnanc\u016F: ",
      format(UPRAV, big.mark = " "), "<br>",
      "Skute\u010Dn\u00FD po\u010Det zam\u011Bstnanc\u016F: ",
      format(SKUT, big.mark = " ")
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text =~paste("<b>",unique(kap_name),"</b>"),
      x = 0.5,
      y = 1.05,
      yref = "paper",
      xref = "paper",
      font = list(family = uni_font, size = kat_tick_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(margin=c(t=5),
           xaxis = c(list(title= "<b>Rok</b>",titlefont = axis_font,tickangle = -90),
                     list(tickvals = x_ticks(dta, step=4))),
           yaxis = c(list(title = "",titlefont = axis_font,range = c(-30, 10),
                          ticktext = lapply(seq(-30, 0, 10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                          tickvals = seq(-30, 0, 10),
                          tickmode = "array"))),
  keep = TRUE,margin=mrg2) %>%
  subplot(nrows = 5,shareX = TRUE,shareY = TRUE,titleY = FALSE,titleX=TRUE) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf 7a. Rozdíl mezi schváleným a skutečným počtem zaměstnanců (v %)</b>",
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         showlegend = FALSE,
         annotations = list(x = 0 , y = 0.5, text = "<b>Záporné = méně skutečných než schválených</b>",
                            font = list(size = axis_size),
                            xshift = -65, textangle = 270, showarrow = FALSE,
                            xref='paper', yref='paper'),
         margin = mrg7) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)


## platy_skut_rozp_kap-----------------------------------------------------------------------------------------
kap_order <- ifelse(startsWith(kaps, "M"),1,2)
names(kap_order) <- kaps
kap_order <- sort(kap_order)

graf_A10 <- dta %>%filter(!is.na(kap_name)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  filter(!kap_num %in% c(kaps_to_exclude, 312)) %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_name, prostredky_na_platy,
         pocet_zamestnancu, cz_kap_name) %>%
  group_by(rok, cz_kap_name,kap_name, typ_rozpoctu) %>%
  summarise(prumerny_plat = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)/12)) %>%
  spread(key = typ_rozpoctu, value = prumerny_plat) %>%
  group_by(rok, kap_name, cz_kap_name) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  group_by(kap_name) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar", color = ~kap_name,
    colors = color_map_kap,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Kapitola:", cz_kap_name, "<br>",
      "Schválený pr\u016Fm\u011Brn\u00FD plat: ",
      format(round(UPRAV, 0), big.mark = " "), "K\u010D", "<br>",
      "Skute\u010Dn\u00FD pr\u016Fm\u011Brn\u00FD plat: ",
      format(round(SKUT, 0), big.mark = " "), "K\u010D"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kap_name),"</b>"),
      x = 0.5,
      y = 1.15,
      yref = "paper",
      xref = "paper",
      font = list(family = uni_font, size = kat_tick_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(margin = mrg7,
           xaxis = list(title = "<b>Rok</b>",titlefont = axis_font,
                        tickangle = -90,tickvals = x_ticks(dta, step=4)),
           # yaxis = c(list(title = "",titlefont = axis_font,range = c(-30, 10),
           #                ticktext = lapply(seq(-30, 10, 10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
           #                tickvals = seq(-30, 10, 10),
           #                tickmode = "array")),
           legend = list(x = 100, y = 0.5)),
  keep = TRUE) %>%
  subplot(nrows = 5, shareX = TRUE, shareY = TRUE, margin = c(0.01,0.01,0.05,0),
          titleY = FALSE) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf 7c. Rozdíl v průměrných platech mezi schváleným rozpočtem a skutečností (v %)</b>",
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"), showlegend = FALSE,
         annotations = list(x = 0 , y = 0.5, text = "<b>Kladné = skutečný průměrný plat vyšší než schválený</b>",
                            font = list(size = axis_size),
                            xshift = -65, textangle = 270, showarrow = FALSE,
                            xref='paper', yref='paper')) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)
graf_A10


## prac_mista_skut_rozp----------------------------------------------------------------------------------------
graf_A11 <- dta %>% filter(!is.na(kategorie_2014_cz)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_num, pocet_zamestnancu) %>%
  spread(key = typ_rozpoctu, value = pocet_zamestnancu) %>%
  group_by(rok, kategorie_2014_cz) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci (celkem)")) %>%
  group_by(kategorie_2014_cz) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar",
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Schválený po\u010Det zam\u011Bstnanc\u016F: ",
      format(UPRAV, big.mark = " "), "<br>",
      "Skute\u010Dn\u00FD po\u010Det zam\u011Bstnanc\u016F: ",
      format(SKUT, big.mark = " ")
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.25,
      yref = "paper",
      xref = "paper",
      font = list(family = uni_font, size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(bargap=0.5,margin = list(t = 100,b=0,l=70),
           xaxis = c(num_ticks,frame_y,title="<b>Rok</b>",titlefont = axis_font,
                     list(tickvals = x_ticks(dta, step=4))),
           yaxis = c(num_ticks,frame_y,list(title = "<b></b>",titlefont = axis_font,
                                            range = c(-18, 1))),showlegend = FALSE),keep = TRUE) %>%
  subplot(nrows = 2, shareY = FALSE, margin = c(0.07,0.07,0.15,0.15),titleY =TRUE) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf 7b. Rozdíl mezi schváleným a skutečným počtem zaměstnanců (%)</b>",
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         annotations = list(x = 0 , y = 0.5, text = "<b>Záporné = skutečný počet nižší než schválený</b>",
                            font = list(size = axis_size),
                            xshift = -70, textangle = 270, showarrow = FALSE,
                            xref='paper', yref='paper')) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)
## platy_skut_rozp---------------------------------------------------------------------------------------------
graf_A12 <- dta %>% filter(!is.na(kategorie_2014_cz)) %>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu != "SCHV") %>%
  select(rok, kategorie_2014_cz, typ_rozpoctu, kap_num, prostredky_na_platy,
         pocet_zamestnancu) %>%
  group_by(rok, kategorie_2014_cz, typ_rozpoctu) %>%
  summarise(prumerny_plat_agg = (sum(prostredky_na_platy) / sum(pocet_zamestnancu)) / 12) %>%
  spread(key = typ_rozpoctu, value = prumerny_plat_agg) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci (celkem)")) %>%
  group_by(rok, kategorie_2014_cz) %>%
  summarise(
    SKUT = sum(SKUT),
    UPRAV = sum(UPRAV)
  ) %>%
  mutate(diff = (SKUT - UPRAV) / UPRAV) %>%
  group_by(kategorie_2014_cz) %>%
  arrange(rok) %>%
  group_map(~ plot_ly(
    data = ., x = ~rok, y = ~ diff * 100, type = "bar",
    color = ~kategorie_2014_cz,colors= color_map,
    hovertemplate = ~ paste(
      "<extra></extra>",
      "Rok:", rok, "<br>",
      "Schválený pr\u016Fm\u011Brn\u00FD plat: ",
      format(round(UPRAV, 0), big.mark = " "), "K\u010D", "<br>",
      "Skute\u010Dn\u00FD pr\u016Fm\u011Brn\u00FD plat: ",
      format(round(SKUT, 0), big.mark = " "), "K\u010D"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.25,
      yref = "paper",
      xref = "paper",
      font = list(family = uni_font, size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(bargap=0.5,margin = list(t = 120,b=0,l=70),
           xaxis = c(num_ticks,frame_y,list(title="<b>Rok</b>",titlefont = axis_font),
                     list(tickvals = x_ticks(dta, step=4))),
           yaxis = c(frame_y,list(title = "",
                                  titlefont = axis_font,
                                  # tickprefix = "+",
                                  ticksuffix = "%",
                                  # showticksuffix = "all",
                                  ticktext = lapply(seq(0,20,5), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                  tickvals = seq(0,20,5),
                                  tickmode = "array"
                                  )),
           showlegend = FALSE),
  keep = TRUE) %>%
  subplot(nrows = 2, shareY = FALSE, margin = c(0.07,0.07,0.15,0.15),titleY =TRUE) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf 7d. Rozdíl v průměrných platech mezi schváleným rozpočtem a skutečností</b>",
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"), annotations = list(x = 0 , y = 0.5, text = "<b>Kladné = skutečný průměrný plat vyšší než schválený</b>",
                                                    font = list(size = axis_size),
                                                    xshift = -70, textangle = 270,
                                                    showarrow = FALSE,
                                                    xref='paper', yref='paper')) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)


## Scatter plat narust-----------------------------------------------------------------------------------------

infl <- dta %>% filter(rok == 2003) %>% select(base_thisyr) %>% first() %>% pull()
graf_A13 <- dta %>%filter(!is.na(kategorie_2014_cz))%>%
  filter(kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni", "Statni urednici")) %>%
  filter(typ_rozpoctu == "SKUT") %>%
  filter(rok %in% c(2003,this_year)) %>%
  select(rok, kategorie_2014_cz, cz_kap_name,kap_name, prumerny_plat, pocet_zamestnancu) %>%
  pivot_wider(names_from = c("rok"),values_from = c("prumerny_plat","pocet_zamestnancu")) %>%
  mutate( #wont be used
    zam_change = (.data[[paste0("pocet_zamestnancu_", this_year)]] / .data[["pocet_zamestnancu_2003"]] - 1),
    plat_change = (.data[[paste0("prumerny_plat_", this_year)]] / (.data[["prumerny_plat_2003"]] * infl) - 1)
  ) %>%
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) %>%
           fct_relevel("Ministerstva", "Ostatní ústřední",
                       "Neústřední st. správa","Státní úředníci (celkem)")) %>%
  group_by(kategorie_2014_cz) %>%
  group_map(~ plot_ly(
    data = ., x = ~prumerny_plat_2003, y = ~ plat_change*100,
    type = "scatter" , mode = "markers",marker=list(size=mrk_min_size),
    color = ~kategorie_2014_cz, colors = color_map,
    hovertemplate = ~ paste0(
      "<extra></extra>",
      "Kapitola: ", cz_kap_name, " <br> ",
      "Pr\u016Fm\u011Brn\u00FD plat 2003: ",
      format(round(prumerny_plat_2003, 0), big.mark = " "), "K\u010D", "<br>",
      paste0("Pr\u016Fm\u011Brn\u00FD plat ", this_year_chr), ": ",
      format(round(.data[[paste0("prumerny_plat_", this_year)]], 0), big.mark = " "), "K\u010D", "<br>",
      "Zm\u011Bna: ", format(round(plat_change*100, 1),big.mark = " "), " %"
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font))
  ) %>%
    add_annotations(
      text = ~paste("<b>",unique(kategorie_2014_cz),"</b>"),
      x = 0.5,
      y = 1.2,
      yref = "paper",
      xref = "paper",
      font = list(family = uni_font, size = axis_size),
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE
    ) %>%
    layout(
      yaxis = c(num_ticks,frame_y,list(title = "Nárůst průměrného platu od roku 2003",titlefont = axis_font,
                                       range = c(-2, 37),
                                       ticktext = lapply(seq(0,30,10), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                       tickvals = seq(0,30,10),
                                       tickmode = "array",
                                       ticksuffix = "%")),
      xaxis = c(num_ticks,frame_y,list(title = "Pr\u016Fm\u011Brn\u00FD plat v roce 2003 (tisíce Kč)", titlefont = axis_font,range = c(15, 35)*1000)),
      legend = list(x = 100, y = 0.5), showlegend = FALSE) %>%
    add_text(text = ~ str_wrap(kap_name, 10), textposition = "bottom left",
             textfont = list(size = kat_tick_size)),keep = TRUE) %>%
  subplot(nrows = 2, titleY = FALSE, titleX = FALSE,margin=c(0.05,0.05,0.1,0.1)) %>%
  layout(title = list(font=title_font,
                      text = "<b>Graf 8. Nárůst průměrných platů od roku 2003 (v %)</b>",
                      xaxis = list(title = "",titlefont = axis_font), y = 0.98, x = 0, xanchor = "left", xref = "paper"), margin=c(t=50,l=90,b=80),
         annotations = list(list(x = 0 , y = 0.5, text = "<b>Nárůst průměrného platu od roku 2003</b>",
                                 font = axis_font,
                                 xshift = -80, textangle = 270, showarrow = FALSE,
                                 xref='paper', yref='paper'),
                            list(y = 0 , x = 0.5, text = "<b>Průměrný plat v roce 2003 (Kč)</b>",
                                 font = axis_font,
                                 yshift = -70,
                                 textangle = 0, showarrow = TRUE,
                                 xref='paper', yref='paper'
                            ))) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)


## graf_A14----------------------------------------------------------------------------------------------------
graf_A14_dta <- dta %>%
  filter(typ_rozpoctu == "SKUT",
         kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  #filter(!kap_num %in% c(314, 306)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(pocet_zamestnancu = sum(pocet_zamestnancu)) %>%
  group_by(rok) %>%
  mutate(pocet_zamestnancu_agg = sum(pocet_zamestnancu))

graf_A14_dta_shares <- graf_A14_dta %>%
  group_by(rok) %>%
  mutate(pocet_zamestnancu_share = pocet_zamestnancu / sum(pocet_zamestnancu)) %>%
  ungroup()

labels_df <- graf_A14_dta_shares %>%
  group_by(rok) %>%
  summarise(
    total_label = first(pocet_zamestnancu_agg) / 1000,
    label_text = round(first(pocet_zamestnancu_agg) / 1000,0)
  )

graf_A14 <- graf_A14_dta_shares %>%
  plot_ly(color = ~kategorie_2014_cz, colors = color_map,
          hovertemplate = ~ paste0(
            "<extra></extra>", "Rok: ", rok, "<br>", "Kategorie: ", kategorie_2014_cz, "<br>",
            "Zam\u011Bstnanc\u016F: ", format(pocet_zamestnancu, big.mark = " ")," (",round(pocet_zamestnancu_share*100,1)," %)", "<br>",
            "Celkem za rok: ", format(pocet_zamestnancu_agg, big.mark = " ")
          ),
          hoverlabel = list(font=list(size=hover_size,family=uni_font)),
          hoverinfo = "text") %>%
  add_trace(x = ~as.character(rok), y = ~pocet_zamestnancu / 1000, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~pocet_zamestnancu_share * 100, type = 'bar') %>%
  add_trace(x = ~as.character(rok), y = ~pocet_zamestnancu / 1000, type = 'scatter',
            mode = "line", line = list(width = 7),
            marker = list(size=5,symbol="circle-dot",line = list(color="Black",width=3))) %>%
  add_trace(data = labels_df, x = ~rok, y = ~total_label, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
  add_trace(data = labels_df, x = ~rok, y = ~100, type = "scatter", mode = "text", text = ~label_text,
            textposition = "top middle", showlegend = FALSE,
            textfont = list(size = 14,family = uni_font,color = "black"),
            inherit = FALSE
  ) %>%
  layout(barmode='stack',bargap=0.5,
         title = list(font=title_font,
                      text = paste0("<b>Graf 4b. Počet státních úředníků, včetně MV a MZV (2003–", this_year_chr, ")</b>"),
                      y = 0.98, x = 0, xanchor = "left", xref = "paper"),
         xaxis = c(num_ticks,frame_x,list(title = list(text="<b>Rok</b>",standoff=10),
                                          dtick=2,titlefont = axis_font),
                   list(tickvals = x_ticks(graf_A14_dta_shares))),
         yaxis = c(num_ticks,frame_y,list(title = "<b>Počet státních úředníků (v tisících)</b>",
                                          titlefont = axis_font,
                                          dtick = 10, range = c(0,90))),
         legend = legend_below_small, margin = mrg2,
         updatemenus = list( chart_type(title_y = "<b>Počet státních úředníků (v tisících)</b>",
                                        title_y_share = "<b>Podíl státních úředníků (v %)</b>",
                                        label_bar = "Sloupce (v tisících)",
                                        max_bar = 90,max_line = 55) )) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js) %>%
  style(visible = FALSE, traces = 4:9)


## graf_A16----------------------------------------------------------------------------------------------------
pubsec <- read_csv("./data-input/ver-sektor-csu-rocenka.csv")
graf_A16_dt <- plyr::rbind.fill(pubsec,data.frame(rok = 1993))

graf_A16 <- plot_ly(graf_A16_dt, type = "scatter", mode = "lines+markers",
                    line = list(width = 7),
                    marker = list(size = 5,symbol = "circle",
                                  line = list(width = 2,color="black")),
                    hovertemplate = ~ paste(
                      "<extra></extra>",
                      "Rok: ", rok, "<br>",
                      "Počet: ", pocet),
                    hoverlabel = list(font=title_font)) %>%
  add_trace(x = ~rok, y = ~pocet, color = I("grey"),
            name = "Počet zaměstnanců veřejného sektoru") %>%
  layout(
    yaxis = c(num_ticks,frame_y,list(title = "<b>Počet zaměstnanců (v tisících přepočtených osob)</b>",titlefont = axis_font, range = c(0, 1800))),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",standoff=10),titlefont = axis_font),
              list(tickvals = seq(1993, max(graf_A16_dt$rok, na.rm=TRUE), 2))
              ),showlegend=FALSE,
    # annotations = c(list(text ='<i>Pozn.:Kategorie: Statistické ročenky České republiky za jednotlivé roky, zde například údaje za rok 2020:</i><br><a href="https://www.csu.gov.cz/csu/czso/10-trh-prace-o73cun42om" target="_blank"><i>https://www.csu.gov.cz/csu/czso/10-trh-prace-o73cun42om</i></a>',
    #                      font = pozn_font_small),annot_below),
    title = list(font=list(color = cap_col,size=cap_size,family=uni_font),
                 text = "<b>Graf 9a. Počet zaměstnanců veřejného sektoru</b>",
                 y = 0.98, x = 0, xanchor = "left", xref = "paper"), margin = mrg3) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE,displayModeBar = TRUE) %>%
  onRender(js)
graf_A16


## graphs, eval = FALSE, include = FALSE-----------------------------------------------------------------------
graf_list<-list(
  graf_1,
  graf_2,
  graf_3,
  graf_4,
  graf_5,
  graf_6,
  graf_A1,
  graf_A2,
  graf_A3,
  graf_A4,
  graf_A5,
  graf_A6,
  graf_A7,
  graf_A8,
  graf_A9,
  graf_A10,
  graf_A11,
  graf_A12,
  graf_A13,
  graf_A14,
  graf_A16)
names(graf_list)<-c(
  "graf_1",
  "graf_2",
  "graf_3",
  "graf_4",
  "graf_5",
  "graf_6",
  "graf_A1",
  "graf_A2",
  "graf_A3",
  "graf_A4",
  "graf_A5",
  "graf_A6",
  "graf_A7",
  "graf_A8",
  "graf_A9",
  "graf_A10",
  "graf_A11",
  "graf_A12",
  "graf_A13",
  "graf_A14",
  "graf_A16")

saveRDS(graf_list,"data-interim/graf_list.rds")
saveRDS(tree_data,"data-interim/tree_data.rds")

for (i in seq_along(graf_list)){
  htmlwidgets::saveWidget(as_widget(graf_list[[i]]), paste0("graphs/",names(graf_list)[i],".html"), libdir = "js", selfcontained = FALSE)
}

source("graf_rocni-zmeny-dekompozice.R", local = environment())
source("pay-change-nace_plot.R", local = environment())

