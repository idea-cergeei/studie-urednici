library(dplyr)
library(ggplot2)
library(ggokabeito)
library(lubridate)
unloadNamespace("plyr")

# dta <- readRDS("./data-interim/sections.rds")
#
# dta |>
#   filter(typ_rozpoctu == "SKUT", !name %in% c("ROPO", "OSS"),
#          !kategorie_2014_cz %in% c("Státní správa", "Státní úředníci")) |>
#   mutate(kategorie = case_when(kategorie_2014_cz == "Příspěvkové organizace" & kap_name == "MŠMT" ~ "PO MŠMT - hlavně školy",
#                                .default = kategorie_2014_cz)) |>
#   summarise(pocet = sum(pocet_zamestnancu), .by = c(rok, kategorie)) |>
#   group_by(kategorie) |>
#   arrange(kategorie, rok) |>
#   mutate(delta = pocet - lag(pocet), datum = make_date(rok)) |>
#   filter(rok > 2003) |>
#   ggplot(aes(datum, delta, fill = kategorie, alpha = rok != 2012)) +
#   geom_col(colour = "white", linewidth = .5) +
#   geom_hline(yintercept = 0, colour = "grey92") +
#   scale_fill_okabe_ito(name = "Skupina") +
#   scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(c(.01, 0), c(0.01, 0))) +
#   # scale_x_continuous(n.breaks = 20, expansion(add = c(0, 0)), limits = c(2003, 2024)) +
#   scale_alpha_discrete(range = c(0.4, 1), guide = "none") +
#   guides(fill = guide_legend(keywidth = unit(6, "pt"), keyheight = unit(12, "pt"),
#                              override.aes = list(colour = NA))) +
#   ptrr::scale_y_number_cz(n.breaks = 12, expand = expansion(add = c(2000, 2000)),
#                           labels = scales::number_format(style_positive = "plus", style_negative = "minus")) +
#   # ptrr::theme_ptrr("both") +
#   labs(title = "Změny počtu zaměstnanců ve sféře rozpočtové regulace, podle kategorií organizací",
#        subtitle = "Meziroční změna skutečného počtu zaměstnanců podle Státního závěrečného účtu",
#        caption = "Změna v roce 2012 způsobena změnou klasifikace některých zaměstnanců MV a MZV") +
#   theme(panel.grid = element_line(colour = "grey95"),
#         text = element_text(family = "Arial"),
#         plot.subtitle = element_text(margin = margin(6, 6, 18, 6, "pt")),
#         legend.background = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         panel.background = element_rect("white"),
#         plot.background = element_rect(fill = "grey95"))dta <- readRDS("./data-interim/sections.rds")

# dta |>
#   filter(typ_rozpoctu == "SKUT", !name %in% c("ROPO", "OSS"),
#          !kategorie_2014_cz %in% c("Státní správa", "Státní úředníci")) |>
#   mutate(kategorie = case_when(kategorie_2014_cz == "Příspěvkové organizace" & kap_name == "MŠMT" ~ "PO MŠMT - hlavně školy",
#                                .default = kategorie_2014_cz)) |>
#   summarise(pocet = sum(pocet_zamestnancu), .by = c(rok, kategorie)) |>
#   group_by(kategorie) |>
#   arrange(kategorie, rok) |>
#   mutate(delta = pocet - lag(pocet), datum = make_date(rok)) |>
#   filter(rok > 2003) |>
#   ggplot(aes(datum, delta, fill = kategorie, alpha = rok != 2012)) +
#   geom_col(colour = "white", linewidth = .5) +
#   geom_hline(yintercept = 0, colour = "grey92") +
#   scale_fill_okabe_ito(name = "Skupina") +
#   scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(c(.01, 0), c(0.01, 0))) +
#   # scale_x_continuous(n.breaks = 20, expansion(add = c(0, 0)), limits = c(2003, 2024)) +
#   scale_alpha_discrete(range = c(0.4, 1), guide = "none") +
#   guides(fill = guide_legend(keywidth = unit(6, "pt"), keyheight = unit(12, "pt"),
#                              override.aes = list(colour = NA))) +
#   ptrr::scale_y_number_cz(n.breaks = 12, expand = expansion(add = c(2000, 2000)),
#                           labels = scales::number_format(style_positive = "plus", style_negative = "minus")) +
#   # ptrr::theme_ptrr("both") +
#   labs(title = "Změny počtu zaměstnanců ve sféře rozpočtové regulace, podle kategorií organizací",
#        subtitle = "Meziroční změna skutečného počtu zaměstnanců podle Státního závěrečného účtu",
#        caption = "Změna v roce 2012 způsobena změnou klasifikace některých zaměstnanců MV a MZV") +
#   theme(panel.grid = element_line(colour = "grey95"),
#         text = element_text(family = "Arial"),
#         plot.subtitle = element_text(margin = margin(6, 6, 18, 6, "pt")),
#         legend.background = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         panel.background = element_rect("white"),
#         plot.background = element_rect(fill = "grey95"))

# Plotly ----
library(plotly)
library(htmlwidgets)

color_map <- c("Ministerstva" =             "#221669",
               "Ostatní ústřední" =         "#1C00C9",
               "Neústřední st. správa" =    "#0069B4",
               "Ostatní vč. armády" =       "#3CB450",
               "Příspěvkové organizace" =   "#C3C7C4",
               "Sbory" =                    "#BB133E",
               "Ústřední orgány" =          "#EB96D8",
               "Státní úředníci" =          "#C666BC",
               "Státní správa" =            "#9BC9E9",
               "Organizační složky státu" = "#C4DFF2")


lbls <- names(color_map)

color_map <- c(ggokabeito::palette_okabe_ito(1:4),
               "#CCCCCC",
               ggokabeito::palette_okabe_ito(5:9))
names(color_map) <- lbls

color_map <- c("Ministerstva" =             "#E69F00",
               "Ostatní ústřední" =         "#56B4E9",
               "Neústřední st. správa" =    "#009E73",
               "Ostatní vč. armády" =       "#F0E442",
               "Příspěvkové organizace" =   "#CCCCCC",
               "Sbory" =                    "#0072B2",
               "Ústřední orgány" =          "#D55E00",
               "Státní úředníci" =          "#CC79A7",
               "Státní správa" =            "#999999",
               "Organizační složky státu" = "#000000")

dta <- readRDS("./data-interim/sections.rds")

dta <- dta |>
  filter(typ_rozpoctu == "SKUT", !name %in% c("ROPO", "OSS"),
         !kategorie_2014_cz %in% c("Státní správa", "Státní úředníci")) |>
  mutate(kategorie = case_when(kategorie_2014_cz == "Příspěvkové organizace" & kap_name == "MŠMT" ~ "PO MŠMT - hlavně školy",
                               .default = kategorie_2014_cz)) |>
  summarise(pocet = sum(pocet_zamestnancu), .by = c(rok, kategorie)) |>
  group_by(kategorie) |>
  arrange(kategorie, rok) |>
  mutate(delta = pocet - lag(pocet), datum = make_date(rok)) |>
  filter(rok > 2003)

color_map <- append(color_map,
                    c("PO MŠMT - hlavně školy" = "#2c3f48"))

mrg <- list(b = 130, t = 70, pad = 0, autoexpand = TRUE)

graf_A17 <- dta %>%
  plot_ly(
    type="bar",
    x = ~year(datum), y = ~ delta / 1000, color = ~kategorie,
    opacity = sapply(year(dta$datum),function(x)ifelse(x==2012,0.4,1)),
    colors = color_map,
    hovertemplate = ~ paste(
      "<extra></extra>", "Rok:", year(datum), "<br>", "Kategorie:", kategorie, "<br>",
      "Změna:", sapply(delta,function(x)ifelse(x>0,paste0("+",x),x)),"<br>",
      "Počet:", pocet
    ),
    hoverlabel = list(font=list(size=hover_size,family=uni_font)),
    hoverinfo = "text"
  ) %>%
  layout(
    # hovermode = "x",
    barmode = "relative",bargap=0.5,
    title = list(font=title_font,
                 text = paste0("<b>Graf 10. Změny počtu zaměstnanců ve sféře rozpočtové regulace, podle kategorií</b>",
                               "<br>","<sup>","Meziroční změna skutečného počtu zaměstnanců podle Státního závěrečného účtu","</sup>"),
                 y = 0.96),
    # annotations = c(annot_below,list(text = str_wrap("<i>Pozn.: Změna v roce 2012 způsobena změnou klasifikace některých zaměstnanců MV a MZV</i>",wrap_len),
    #                                  font = pozn_font)),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",
                                                  standoff=10),
                                     titlefont = axis_font),
              list(tickvals = seq(2003,2023,5))),
    # dtick=2,
    # titlefont = axis_font),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Meziroční změna (v tisících)</b>",
                                     titlefont = axis_font),
              # tickprefix = "+",
              # showtickprefix = "last",
              list(ticktext = lapply(seq(-10,15,5), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                   tickvals = seq(-10,15,5),
                   tickmode = "array")
              ),
    legend = legend_below,
    margin = mrg ) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js)

graf_A17

htmlwidgets::saveWidget(widget = as_widget(graf_A17), paste0("graphs/","graf_A17",".html"), libdir = "js", selfcontained = FALSE)
