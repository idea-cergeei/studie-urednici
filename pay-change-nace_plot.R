library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)
library(gplots)
unloadNamespace("plyr")

source("theme.R")

# source("theme.R")
options(czso.dest_dir = "data-input/czso")

infl <- czso::czso_get_table("010022", force_redownload = TRUE) # indexy spotř. cen

zm <- czso::czso_get_table("110079", force_redownload = TRUE) # mzdy podle NACE
count(zm, rok, ctvrtleti) |> arrange(desc(rok)) |> head()
count(zm, rok, ctvrtleti) |> filter(rok == 2023)

zm |> count(odvetvi_txt)
zm |> count(odvetvi_kod)
zm |> count(odvetvi_kod, odvetvi_txt)
zm |> count(stapro_txt)
zm |> count()


# FN ----------------------------------------------------------------------

make_nace_plot <- function(data, add_years = 5) {

  data <- data |>
    drop_na(realna_zmena) |>
    group_by(tm) |>
    mutate(is_minmax = realna_zmena == max(realna_zmena) | realna_zmena == min(realna_zmena)) |>
    ungroup() |>
    mutate(is_last_period = tm == max(tm),
           needs_label = is_last_period & (is_minmax | clr != "Ostatní"),
           name_for_label = ifelse(is_minmax, odvetvi_txt, as.character(clr)) |> str_wrap(30))

  new_labels <- c(seq(year(min(data$tm)), year(max(data$tm))), rep(" ", times = add_years))
  print(new_labels)

  new_breaks <- make_date(seq(year(min(data$tm)), year(max(data$tm))))
  print(new_breaks)

  fmt_pct_change <- scales::label_number(.1, 100, suffix = " %", decimal.mark = ",",
                                         style_positive = "plus", style_negative = "minus")
  fmt_pct_change_axis <- scales::label_number(1, 100, suffix = " %", decimal.mark = ",",
                                              style_positive = "plus", style_negative = "minus")

  zm_plt <- ggplot(data |>
                     # filter(month(tm) == 12) |> mutate(tm = floor_date(tm, "year")) |>
                     filter(),
                   aes(tm, realna_zmena, colour = clr, size = clr != "Ostatní",
                       group = clr)) +
    geom_hline(yintercept = 0, colour = "grey10", linetype = "solid") +
    geom_point(aes(alpha = clr != "Ostatní"), fill = "white") +
    scale_alpha_discrete(range = c(.6, 1), guide = "none") +
    geom_line(data = ~subset(., clr == "Celá ekonomika")) +
    geom_point(data = ~subset(., clr == "Celá ekonomika"), size = 2) +
    geom_point(data = ~subset(., clr == "Celá ekonomika"), colour = "white", size = 1.2) +
    geom_line(data = ~subset(., public == TRUE)) +
    geom_point(data = ~subset(., public == TRUE), size = 2) +
    geom_point(data = ~subset(., public == TRUE), colour = "white", size = 1.2) +
    geom_label(data = ~subset(., needs_label),
               aes(label = paste(fmt_pct_change(realna_zmena), name_for_label), fill = clr),
               label.padding = unit(0.2, "lines"),
               hjust = 0, nudge_x = 40, color = "white", family = "Arial", size = 3, fontface = "bold") +
    scale_color_manual(values = c(Ostatní = "grey40", Profesní = "blue3",
                                  `Celá ekonomika` = "grey20",
                                  `ICT` = "goldenrod", `Veřejná správa` = "red3")) +
    scale_fill_manual(values = c(Ostatní = "grey40", Profesní = "blue3",
                                 `Celá ekonomika` = "grey20",
                                 `ICT` = "goldenrod", `Veřejná správa` = "red3")) +
    scale_size_manual(values = c(1, 2), guide = "none") +
    scale_x_date(date_breaks = "1 years",
                 date_labels = "%Y",
                 breaks = new_breaks,
                 expand = expansion(add = c(0, 365 * add_years)),
                 limits = c(as.Date("2000-09-01"), as.Date("2024-03-31")),
    ) +
    scale_y_continuous(expand = expansion(add = c(.02, 0.001)),
                       # limits = c(-.2, .2),
                       labels = fmt_pct_change_axis,
                       breaks = seq(-.3, .3, .05)) +
    guides(colour = guide_legend(reverse = T, title = "Skupina NACE (odvětví)"), size = "none", fill = "none") +
    labs(title = "Meziroční změny průměrných reálných mezd (v odvětvích dle NACE), v %",
         x = "Rok", y = "Reálná meziroční změna (očištěno o inflaci)",
         caption = "Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
    # theme_urednici +
    theme(panel.grid.major = element_line(color = "white")) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    theme_urednici
  zm_plt
}

infl_yony <- infl |>
  filter(casz_txt == "stejných 12 měsíců předchozího roku", mesic == 12,
         is.na(ucel_txt)) |> # všechny druhy zboží/služeb
  group_by(rok) |>
  select(inflace_yony = hodnota, rok)

zm_plt_dt_y <- zm |>
  filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
         typosoby_txt == "přepočtený") |>
  group_by(rok, odvetvi_kod, odvetvi_txt) |>
  summarise(hodnota = mean(hodnota), .groups = "drop") |>
  mutate(tm = make_date(rok, 01, 01),
         clr = case_when(odvetvi_kod == "O" ~ "Veřejná správa",
                         # odvetvi_kod %in% c("P") ~ "vzdělávání",
                         is.na(odvetvi_kod) ~ "Celá ekonomika",
                         odvetvi_kod %in% c("M") ~ "Profesní",
                         odvetvi_kod %in% c("J") ~ "ICT",
                         TRUE ~ "Ostatní") |>
           as_factor() |> fct_relevel("Ostatní", "Profesní", "ICT", "Veřejná správa"),
         public = odvetvi_kod %in% c("O"),
         public_broad = odvetvi_kod %in% c("O", "P", "Q")) |>
  ungroup() |>
  left_join(infl_yony, by = "rok") |>
  select(tm, clr, hodnota, public, public_broad, inflace = inflace_yony, odvetvi_kod, odvetvi_txt) |>
  arrange(odvetvi_kod, tm) |>
  group_by(odvetvi_kod) |>
  # počítáme nominální a reálnou změnu
  mutate(nominalni_zmena = hodnota/lag(hodnota, 1),
         realna_zmena = nominalni_zmena/inflace * 100 - 1) |>
  ungroup() |>
  # select(tm, mzda = hodnota, realna_mzda, realna_zmena, clr, hodnota, public, realna_zmena_2) |>
  ungroup() |>
  arrange(clr)

# Plot and export ---------------------------------------------------------

zm_plt_y <- make_nace_plot(zm_plt_dt_y)
zm_plt_y
ggsave(plot = zm_plt_y, "plt_twitter.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 675, limitsize = FALSE, units = "px")
ggsave(plot = zm_plt_y, "plt_facebook.png", bg = "white", scale = 3, dpi = 300, device = ragg::agg_png,
       width = 1200, height = 630, limitsize = FALSE, units = "px")


# zm_plt_dt_y |> filter(tm > "2021-12-31", is.na(odvetvi_kod))
# zm_plt_dt_y2 |> filter(tm > "2021-12-31", is.na(odvetvi_kod))
# make_nace_plot(zm_plt_dt_y2)
#
# writexl::write_xlsx(zm_plt_dt_y2, "data-export/platy_nace_realne.xlsx")

# Plotly ---------------------------------------------------------
library(plyr)
library(plotly)
library(htmlwidgets)

add_years <- 5

data <- zm_plt_dt_y |>
  drop_na(realna_zmena) |>
  group_by(tm) |>
  mutate(is_minmax = realna_zmena == max(realna_zmena) | realna_zmena == min(realna_zmena)) |>
  ungroup() |>
  mutate(is_last_period = tm == max(tm),
         needs_label = is_last_period & (is_minmax | clr != "Ostatní"),
         name_for_label = ifelse(is_minmax, odvetvi_txt, as.character(clr)) |> str_wrap(30)) |>
  replace_na(list(odvetvi_txt = "Celkem"))

new_labels <- c(seq(year(min(data$tm)), year(max(data$tm))), rep(" ", times = add_years))
print(new_labels)

new_breaks <- make_date(seq(year(min(data$tm)), year(max(data$tm))))
print(new_breaks)

fmt_pct_change <- scales::label_number(.1, 100, suffix = " %", decimal.mark = ",",
                                       style_positive = "plus", style_negative = "minus")
fmt_pct_change_axis <- scales::label_number(1, 100, suffix = " %", decimal.mark = ",",
                                            style_positive = "plus", style_negative = "minus")

color_map <- c(Ostatní = "grey40", Profesní = "blue3",
               `Celá ekonomika` = "grey20",
               `ICT` = "goldenrod", `Veřejná správa` = "red3")
color_map <- setNames(col2hex(color_map),names(color_map))

linewidth_map <- c(Ostatní = 0, Profesní = 0,
                   `Celá ekonomika` = 7,
                   `ICT` = 0, `Veřejná správa` = 7)

marker_clr_map <- c(Ostatní = "grey40", Profesní = "blue3",
                    `Celá ekonomika` = "white",
                    `ICT` = "goldenrod", `Veřejná správa` = "white")
marker_clr_map <- setNames(col2hex(marker_clr_map),names(marker_clr_map))

marker_size_map <- c(Ostatní = 1, Profesní = 2,
                     `Celá ekonomika` = 2,
                     `ICT` = 2, `Veřejná správa` = 2)*4

marker_opac_map <- c(Ostatní = 0.6, Profesní = 1,
                     `Celá ekonomika` = 1,
                     `ICT` = 1, `Veřejná správa` = 1)

text_size_map <- c(Ostatní = hover_size, Profesní = hover_size,
                   `Celá ekonomika` = hover_size,
                   `ICT` = hover_size, `Veřejná správa` = hover_size)

data$tm <- year(data$tm)

# Updated JavaScript for adding/removing hover lines
js_hover <- "
function(el, x) {
  var hoverTraceIndex = null;

  el.on('plotly_hover', function(data) {
    var point = data.points[0];
    var hoveredOdvetvi = point.customdata;

    // Remove any existing hover trace
    if (hoverTraceIndex !== null) {
      Plotly.deleteTraces(el, hoverTraceIndex);
      hoverTraceIndex = null;
    }

    // Collect all points for this odvetvi_txt
    var matchingPoints = [];
    var allTraces = el.data;

    for (var i = 0; i < allTraces.length; i++) {
      var trace = allTraces[i];
      if (trace.customdata) {
        for (var j = 0; j < trace.customdata.length; j++) {
          if (trace.customdata[j] === hoveredOdvetvi) {
            matchingPoints.push({
              x: trace.x[j],
              y: trace.y[j]
            });
          }
        }
      }
    }

    // Sort points by x value (time) to ensure proper line connection
    matchingPoints.sort(function(a, b) { return a.x - b.x; });

    // Extract sorted x and y arrays
    var xVals = matchingPoints.map(function(p) { return p.x; });
    var yVals = matchingPoints.map(function(p) { return p.y; });

    // Add new trace for the hover line
    var hoverTrace = {
      x: xVals,
      y: yVals,
      type: 'scatter',
      mode: 'lines+markers',
      line: {
        color: 'grey30',
        width: 3
      },
      marker: {
        color: 'grey30',
        size: 6
      },
      showlegend: false,
      hoverinfo: 'skip'
    };

    Plotly.addTraces(el, hoverTrace).then(function() {
      hoverTraceIndex = el.data.length - 1;
    });
  });

  el.on('plotly_unhover', function(data) {
    // Remove hover trace
    if (hoverTraceIndex !== null) {
      Plotly.deleteTraces(el, hoverTraceIndex);
      hoverTraceIndex = null;
    }
  });
}
"

# Updated main graph code
graf_A15 <- data %>%
  plot_ly(
    x = ~tm, y = ~ realna_zmena * 100, type = "scatter",
    color = ~clr, colors = color_map, mode = "line",
    line = list(width = revalue(data$clr,linewidth_map),
                color = revalue(as.character(data$clr),color_map)),
    marker = list(symbol = "circle",
                  line = list(width = 1,color=revalue(data$clr,color_map)),
                  opacity=revalue(data$clr,marker_opac_map),
                  size=revalue(data$clr,marker_size_map),
                  color = revalue(as.character(data$clr),marker_clr_map)),
    text = ~ paste(
      " Rok:", tm, "<br>", "Skupina NACE (odvětví):",
      str_wrap(odvetvi_txt, 30),
      "<br>", "Hodnota:",
      round(realna_zmena * 100,2), "%"
    ),
    customdata = ~odvetvi_txt,  # Pass odvetvi_txt to JavaScript
    hoverlabel = list(font=list(size=revalue(data$clr,text_size_map),
                                family=uni_font),
                      bgcolor=revalue(as.character(data$clr),color_map)),
    hoverinfo = "text",
    legendgroup = ~clr
  ) %>%
  layout(
    # hovermode = "x",
    title = list(font=title_font,
                 text="<b>Graf 9b. Meziroční změny průměrných reálných mezd (v odvětvích dle NACE), v %</b>",
                 y = 0.96),
    # annotations = c(list(text = str_wrap("<i>Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)</i>",wrap_len),
    #                      font = pozn_font),annot_below),
    xaxis = c(num_ticks,frame_y,list(title = list(text="<b>Rok</b>",standoff=10),
                                     titlefont = axis_font),
              list(tickvals = seq(2003,2023,5))),
    yaxis = c(num_ticks,frame_y,list(title = "<b>Reálná meziroční změna (očištěno o inflaci)</b>",
                                     # tickprefix = "+",
                                     ticksuffix = " %",
                                     # showtickprefix = "last",
                                     ticktext = lapply(seq(-15,10,5), function(x) ifelse(x > 0, paste0("+", x), as.character(x))),
                                     tickvals = seq(-15,10,5),
                                     tickmode = "array",
                                     titlefont = axis_font)),
    legend = legend_below_mid, margin = mrg6) %>%
  config(modeBarButtonsToRemove = btnrm, displaylogo = FALSE) %>%
  onRender(js_hover)

graf_A15

htmlwidgets::saveWidget(as_widget(graf_A15), paste0("graphs/","graf_A15",".html"), libdir = "js", selfcontained = FALSE)
