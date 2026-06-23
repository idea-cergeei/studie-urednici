# dashboard.R — generates dashboard.html (static Plotly dashboard, no backend)
# Run: source("dashboard.R")  or  Rscript dashboard.R

library(nanoparquet)
library(dplyr)
library(stringr)
library(ggokabeito)
library(jsonlite)
library(htmltools)

# ── Config ────────────────────────────────────────────────────────────────────
cfg       <- config::get()
this_year <- if (!is.null(cfg$rok)) as.integer(cfg$rok) else 2024L
defl_col  <- paste0("ceny_deflator_", this_year)

# ── Category labels and colours (match graphs.R) ──────────────────────────────
kat_lbls <- c(
  "Všichni",
  "Ministerstva",
  "Ostatní ústřední",
  "Neústřední státní správa",
  "Všichni státní úředníci",
  "Sbory",
  "Ostatní vč. armády",
  "Příspěvkové organizace"
)
kat_colors <- setNames(
  c("#333333", palette_okabe_ito(c(1, 2, 3, 7, 5, 4)), "#CCCCCC"),
  kat_lbls
)

# ── Data ──────────────────────────────────────────────────────────────────────
data_raw <- read_parquet("data-export/data_all.parquet") |>
  dplyr::filter(!is.na(kap_kod), !is.na(kategorie_2014_cz), faze_rozpoctu == "SKUT") |>
  dplyr::mutate(
    kap_nazev         = str_trim(kap_nazev),
    kategorie_2014_cz = dplyr::case_when(
      kategorie_2014_cz == "Státní úředníci"        ~ "Všichni státní úředníci",
      kategorie_2014_cz == "Neústřední st. správa"  ~ "Neústřední státní správa",
      .default = kategorie_2014_cz
    ),
    platy_real         = platy         * .data[[defl_col]],
    prumerny_plat_real = prumerny_plat * .data[[defl_col]]
  )

CELKEM_VAL <- "__CELKEM__"
CELKEM_LBL <- "Celkem (všechny kapitoly)"

celkem_data <- data_raw |>
  dplyr::group_by(rok, kategorie_2014_cz) |>
  dplyr::summarise(
    platy             = sum(platy,             na.rm = TRUE),
    platy_real        = sum(platy_real,        na.rm = TRUE),
    pocet_zamestnancu = sum(pocet_zamestnancu, na.rm = TRUE),
    .groups           = "drop"
  ) |>
  dplyr::mutate(
    prumerny_plat      = platy      / pocet_zamestnancu / 12,
    prumerny_plat_real = platy_real / pocet_zamestnancu / 12,
    kap_nazev          = CELKEM_LBL,
    kap_zkr            = "CELKEM"
  )

vsichni_data <- data_raw |>
  dplyr::group_by(rok, kap_nazev, kap_zkr) |>
  dplyr::summarise(
    platy             = sum(platy,             na.rm = TRUE),
    platy_real        = sum(platy_real,        na.rm = TRUE),
    pocet_zamestnancu = sum(pocet_zamestnancu, na.rm = TRUE),
    .groups           = "drop"
  ) |>
  dplyr::mutate(
    prumerny_plat      = platy      / pocet_zamestnancu / 12,
    prumerny_plat_real = platy_real / pocet_zamestnancu / 12,
    kategorie_2014_cz  = "Všichni"
  )

vsichni_celkem <- celkem_data |>
  dplyr::group_by(rok) |>
  dplyr::summarise(
    platy             = sum(platy,             na.rm = TRUE),
    platy_real        = sum(platy_real,        na.rm = TRUE),
    pocet_zamestnancu = sum(pocet_zamestnancu, na.rm = TRUE),
    .groups           = "drop"
  ) |>
  dplyr::mutate(
    prumerny_plat      = platy      / pocet_zamestnancu / 12,
    prumerny_plat_real = platy_real / pocet_zamestnancu / 12,
    kap_nazev          = CELKEM_LBL,
    kap_zkr            = "CELKEM",
    kategorie_2014_cz  = "Všichni"
  )

keep_cols <- c("kap_nazev", "kap_zkr", "kategorie_2014_cz", "rok",
               "platy", "platy_real", "prumerny_plat", "prumerny_plat_real",
               "pocet_zamestnancu")

js_data <- dplyr::bind_rows(
  data_raw       |> dplyr::select(dplyr::all_of(keep_cols)),
  celkem_data    |> dplyr::select(dplyr::all_of(keep_cols)),
  vsichni_data   |> dplyr::select(dplyr::all_of(keep_cols)),
  vsichni_celkem |> dplyr::select(dplyr::all_of(keep_cols))
) |>
  dplyr::mutate(dplyr::across(c(platy, platy_real, prumerny_plat, prumerny_plat_real,
                  pocet_zamestnancu), round))

kap_df <- data_raw |>
  dplyr::distinct(kap_nazev, kap_zkr) |>
  dplyr::arrange(kap_nazev) |>
  dplyr::mutate(lbl = paste0(kap_zkr, " – ", kap_nazev))

# Average wages per year (benchmark for sal_vs_avg)
mzda_by_year <- data_raw |>
  dplyr::filter(!is.na(prumerna_mzda_cr)) |>
  dplyr::distinct(rok, prumerna_mzda_cr, prumerna_mzda_pha) |>
  dplyr::arrange(rok)
mzda_list <- setNames(
  lapply(seq_len(nrow(mzda_by_year)), \(i)
    list(cr  = round(mzda_by_year$prumerna_mzda_cr[i]),
         pha = round(mzda_by_year$prumerna_mzda_pha[i]))
  ),
  as.character(mzda_by_year$rok)
)

# ── Serialise ─────────────────────────────────────────────────────────────────
data_json <- toJSON(js_data, dataframe = "rows", na = "null", auto_unbox = FALSE)

meta_json <- toJSON(list(
  this_year  = this_year,
  kat_lbls   = kat_lbls,
  kat_colors = unname(kat_colors),
  celkem_val = CELKEM_VAL,
  celkem_lbl = CELKEM_LBL,
  kap_list   = lapply(seq_len(nrow(kap_df)), \(i)
    list(val = kap_df$kap_nazev[i], lbl = kap_df$lbl[i], zkr = kap_df$kap_zkr[i])
  ),
  mzda = mzda_list
), auto_unbox = TRUE)

# ── CSS ───────────────────────────────────────────────────────────────────────
css <- "
* { box-sizing: border-box; margin: 0; padding: 0; }
html, body { height: 100%; }
body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; padding: 16px 20px; background: #fff; color: #333; display: flex; flex-direction: column; }
h4   { color: #c90239; margin-bottom: 14px; font-size: 1.1rem; }
.row  { display: flex; gap: 16px; align-items: flex-start; flex-wrap: wrap; margin-bottom: 10px; }
.lbl { font-weight: 600; font-size: .82rem; color: #555; margin-bottom: 3px; }
.kap-box {
  position: relative; border: 1px solid #ccc; border-radius: 4px;
  padding: 5px 6px; min-height: 38px; background: #fff; cursor: text;
  display: flex; flex-wrap: wrap; gap: 4px; align-items: center;
}
.kap-box:focus-within { border-color: #c90239; }
#kap-tags { display: inline-flex; flex-wrap: wrap; gap: 3px; align-items: center; }
.kap-tag {
  display: inline-flex; align-items: center; gap: 3px;
  background: #c90239; color: #fff; border-radius: 3px;
  padding: 2px 6px; font-size: 12px; white-space: nowrap;
}
.kap-tag button {
  background: none; border: none; color: #fff; cursor: pointer;
  padding: 0 1px; font-size: 15px; line-height: 1; opacity: .8;
}
.kap-tag button:hover { opacity: 1; }
#kap-input {
  border: none; outline: none; font-size: 14px;
  flex: 1 1 120px; min-width: 60px; padding: 2px;
}
.kap-dropdown {
  position: absolute; top: calc(100% + 1px); left: -1px; right: -1px;
  background: #fff; border: 1px solid #ccc; border-top: none;
  border-radius: 0 0 4px 4px; z-index: 200; max-height: 220px; overflow-y: auto;
  box-shadow: 0 4px 8px rgba(0,0,0,.1);
}
.dd-item  { padding: 7px 10px; cursor: pointer; font-size: 14px; }
.dd-item:hover, .dd-item.focused { background: #f5f5f5; }
.dd-empty { padding: 7px 10px; font-size: 14px; color: #999; font-style: italic; }
.btn-group { display: flex; gap: 4px; flex-wrap: wrap; }
.btn {
  padding: 6px 12px; font-size: 14px; cursor: pointer;
  border: 1px solid #ccc; border-radius: 4px;
  background: #fff; color: #333; transition: background .1s, color .1s;
  font-family: inherit;
}
.btn:hover  { background: #e6e6e6; border-color: #adadad; }
.btn.active { background: #c90239; color: #fff; border-color: #a50131; }
#cat-pills { display: flex; flex-wrap: wrap; gap: 4px; margin: 4px 0 10px; }
.pill {
  padding: 3px 10px; font-size: 12px; cursor: pointer;
  border: 1px solid #bbb; border-radius: 12px;
  background: #fff; color: #555; transition: background .1s, color .1s;
}
.pill:hover  { border-color: #c90239; color: #c90239; }
.pill.active { background: #c90239; color: #fff; border-color: #c90239; }
.mode-row { margin-bottom: 4px; }
.mode-controls {
  display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 6px;
}
.metric-col {
  display: flex; flex-direction: column; gap: 3px; align-items: stretch;
  border: 1px solid #ddd; border-radius: 5px; padding: 5px 6px; background: #f8f8f8;
}
.btn-row { display: flex; gap: 3px; }
.btn-row .btn { flex: 1; }
.metric-col-lbl { font-size: 13px; font-weight: 600; color: #555; white-space: nowrap; }
.chk-label {
  display: flex; align-items: center; gap: 6px;
  font-size: 13px; cursor: pointer; user-select: none;
  white-space: nowrap; padding-bottom: 4px;
}
.chk-label input[type=checkbox] { width: 16px; height: 16px; cursor: pointer; accent-color: #c90239; }
.chk-label.disabled { opacity: 0.35; pointer-events: none; }
.shapelayer path { cursor: pointer !important; }
#pct-hint   { display: none; color: #666; font-size: 12px; margin: 2px 0 4px; }
#mzda-note  { display: none; color: #888; font-size: 11px; margin: 2px 0 4px; }
#chart { flex: 1 1 auto; min-height: 300px; }
#y-title { font-size: 14px; font-family: inherit; color: #000; margin: 0; padding: 0 0 3px 50px; }
"

# ── JavaScript ────────────────────────────────────────────────────────────────

js1 <- paste0(
'const DATA = ', data_json, ';\n',
'const META = ', meta_json, ';\n')

js2 <- paste0(
'const KAT_COLOR = {};
META.kat_lbls.forEach((k, i) => { KAT_COLOR[k] = META.kat_colors[i]; });
const CELKEM_VAL  = META.celkem_val;
const CELKEM_LBL  = META.celkem_lbl;
const THIS_YEAR   = META.this_year;
const ALL_KAPS    = [{ val: CELKEM_VAL, lbl: CELKEM_LBL, zkr: "CELKEM" },
                    ...META.kap_list];
const DASH_STYLES = ["solid", "dash", "dot", "dashdot", "longdash"];

let selKaps  = [CELKEM_VAL];
let selCats  = null;
let mode     = "sal_level";
let isNom    = false;  // checkbox checked by default → real prices
let baseYear = 2019;

function rowsForKap(kap) {
  const nazev = kap === CELKEM_VAL ? CELKEM_LBL : kap;
  return DATA.filter(r => r.kap_nazev === nazev);
}
function availCats() {
  const have = new Set(selKaps.flatMap(k => rowsForKap(k).map(r => r.kategorie_2014_cz)));
  return META.kat_lbls.filter(k => have.has(k));
}
// Modes where real/nominal doesn\'t apply (ratios, absolute counts, or staff % change)
function isRelativeMode() { return mode === "sal_vs_avg" || mode === "staff" || mode === "staff_change"; }

function computeY(r, baseRow) {
  if (mode === "sal_level")  return isNom ? r.prumerny_plat       : r.prumerny_plat_real;
  if (mode === "cost_level") return isNom ? r.platy / 1e9         : r.platy_real / 1e9;
  if (mode === "staff")      return r.pocet_zamestnancu;
  if (mode === "staff_change") {
    const b = baseRow ? baseRow.pocet_zamestnancu : null;
    const v = r.pocet_zamestnancu;
    return (b && v != null) ? (v / b - 1) * 100 : null;
  }
  if (mode === "sal_vs_avg") {
    const mzda = META.mzda[r.rok];
    if (!mzda || r.prumerny_plat == null) return null;
    const bm = (r.kategorie_2014_cz === "Ministerstva" || r.kategorie_2014_cz === "Ostatní ústřední") ? mzda.pha : mzda.cr;
    return bm ? (r.prumerny_plat / bm - 1) * 100 : null;
  }
  if (!baseRow) return null;
  if (mode === "sal_change") {
    const b = isNom ? baseRow.prumerny_plat : baseRow.prumerny_plat_real;
    const v = isNom ? r.prumerny_plat       : r.prumerny_plat_real;
    return (b && v != null) ? (v / b - 1) * 100 : null;
  }
  const b = isNom ? baseRow.platy : baseRow.platy_real;
  const v = isNom ? r.platy       : r.platy_real;
  return (b && v != null) ? (v / b - 1) * 100 : null;
}
')

js3 <- paste0(
'function fmtHover(v, isRel) {
  if (v == null) return "";
  const str = v.toLocaleString("cs-CZ", {
    minimumFractionDigits: isRel ? 1 : 0,
    maximumFractionDigits: isRel ? 1 : 0
  });
  return (isRel && v > 0 ? "+" : "") + str + (isRel ? "\u00a0%" : "");
}
function effectiveBase() {
  if (!mode.endsWith("_change")) return baseYear;
  const cats = (selCats && selCats.length) ? selCats : availCats();
  let minYear = Infinity;
  selKaps.forEach(kap => {
    rowsForKap(kap).forEach(r => {
      if (cats.includes(r.kategorie_2014_cz) && r.rok < minYear) minYear = r.rok;
    });
  });
  return isFinite(minYear) ? Math.max(baseYear, minYear) : baseYear;
}
function buildTraces() {
  const cats     = (selCats && selCats.length) ? selCats : availCats();
  const isPct    = mode.endsWith("_change");
  const isRelPct = isPct || mode === "sal_vs_avg";
  const by       = effectiveBase();
  const traces   = [];
  selKaps.forEach((kap, ki) => {
    const kapRows = rowsForKap(kap);
    const entry   = ALL_KAPS.find(k => k.val === kap);
    const zkr     = entry ? entry.zkr : kap;
    const dsh     = DASH_STYLES[Math.min(ki, DASH_STYLES.length - 1)];
    META.kat_lbls.forEach(kat => {
      if (!cats.includes(kat)) return;
      const rows = kapRows.filter(r => r.kategorie_2014_cz === kat)
                          .sort((a, b) => a.rok - b.rok);
      if (!rows.length) return;
      const baseRow = isPct ? (rows.find(r => r.rok === by) || null) : null;
      const xs = [], ys = [], texts = [];
      rows.forEach(r => {
        const y = computeY(r, baseRow);
        if (y == null) return;
        xs.push(r.rok);
        ys.push(+(mode === "sal_level" ? y / 1000 : y).toFixed(2));
        texts.push(fmtHover(y, isRelPct));
      });
      if (!xs.length) return;
      const col  = KAT_COLOR[kat] || "#888888";
      const nm   = selKaps.length > 1 ? kat + " – " + zkr : kat;
      const htpl = selKaps.length > 1
        ? "<b>" + kat + "</b><br>" + zkr + "<br>%{text}<extra></extra>"
        : "<b>" + kat + "</b><br>%{text}<extra></extra>";
      traces.push({
        x: xs, y: ys, text: texts,
        type: "scatter", mode: "lines+markers",
        name: nm, legendgroup: nm,
        line:   { color: col, width: 2.5, dash: dsh },
        marker: { color: "#000000", size: 7 },
        hovertemplate: htpl
      });
    });
  });
  return traces;
}
')

# y-axis labels (UTF-8 in R strings, no \u sequences needed in JS)
lbl_sal_nom  <- paste0("Průměrný plat (tis. Kč/měsíc)")
lbl_sal_real <- paste0("Průměrný plat (tis. Kč/měsíc, v cenách roku ", this_year, ")")
lbl_sal_chg  <- paste0("Průměrný plat – změna od roku BASE (%)")
lbl_cst_nom  <- paste0("Náklady na platy (mld. Kč)")
lbl_cst_real <- paste0("Náklady na platy (mld. Kč, v cenách roku ", this_year, ")")
lbl_cst_chg  <- paste0("Náklady – změna od roku BASE (%)")
lbl_vsavg    <- paste0("Odchylka průměrného platu od průměrné mzdy v ekonomice (%)")
lbl_staff    <- paste0("Počet zaměstnanců (FTE)")
lbl_stf_chg  <- paste0("Počet zam. – změna od roku BASE (%)")
lbl_zaklad   <- paste0("Základ: ")

js4 <- paste0(
'function makePctTicks(yMin, yMax) {
  if (!isFinite(yMin) || !isFinite(yMax) || yMax === yMin) return {};
  const range = yMax - yMin;
  const steps = [1, 2, 5, 10, 20, 25, 50, 100, 200, 500];
  const step  = steps.find(s => s >= range / 6) || 500;
  const vals  = [];
  for (let v = Math.ceil(yMin / step) * step; v <= Math.floor(yMax / step) * step + 1e-9; v += step)
    vals.push(Math.round(v * 10) / 10);
  if (!vals.length) return {};
  return {
    tickvals: vals,
    ticktext: vals.map(v => (v > 0 ? "+" : "") + v),
    tickmode: "array"
  };
}
function buildLayout() {
  const isPct    = mode.endsWith("_change");
  const isRelPct = isPct || mode === "sal_vs_avg";
  const by       = effectiveBase();
  let yTitle;
  if      (mode === "sal_level")   yTitle = isNom ? "', lbl_sal_nom, '" : "', lbl_sal_real, '";
  else if (mode === "sal_change")  yTitle = "', lbl_sal_chg, '".replace("BASE", by);
  else if (mode === "cost_level")  yTitle = isNom ? "', lbl_cst_nom, '" : "', lbl_cst_real, '";
  else if (mode === "cost_change") yTitle = "', lbl_cst_chg, '".replace("BASE", by);
  else if (mode === "sal_vs_avg")    yTitle = "', lbl_vsavg, '";
  else if (mode === "staff_change")  yTitle = "', lbl_stf_chg, '".replace("BASE", by);
  else                               yTitle = "', lbl_staff, '";

  let pctTicks = {};
  if (isRelPct) {
    const cats = (selCats && selCats.length) ? selCats : availCats();
    let yMin = Infinity, yMax = -Infinity;
    selKaps.forEach(kap => {
      const kapRows = rowsForKap(kap);
      META.kat_lbls.forEach(kat => {
        if (!cats.includes(kat)) return;
        const rows = kapRows.filter(r => r.kategorie_2014_cz === kat)
                            .sort((a, b) => a.rok - b.rok);
        const baseRow = isPct ? (rows.find(r => r.rok === by) || null) : null;
        rows.forEach(r => {
          const y = computeY(r, baseRow);
          if (y != null) { yMin = Math.min(yMin, y); yMax = Math.max(yMax, y); }
        });
      });
    });
    if (isFinite(yMin)) {
      const pad = Math.max((yMax - yMin) * 0.1, 2);
      pctTicks = makePctTicks(yMin - pad, yMax + pad);
    }
  }

  const shapes = isPct ? [{
    type: "line", x0: by, x1: by, y0: 0, y1: 1, yref: "paper",
    line: { color: "#e67e22", width: 3, dash: "solid" }
  }] : [];
  const annotations = isPct ? [{
    xref: "x", yref: "paper",
    x: by, y: 0.99,
    xanchor: "left", yanchor: "top",
    text: "', lbl_zaklad, '" + by,
    showarrow: false,
    font: { size: 11, family: "Arial", color: "#e67e22" },
    bgcolor: "rgba(255,255,255,0.7)",
    borderpad: 3
  }] : [];

  return {
    shapes, annotations,
    xaxis: {
      // title:    { text: "Rok", font: { size: 16, family: "Arial", color: "#000" } },
      tickfont: { size: 14, family: "Arial" },
      dtick: 2, tick0: 2003,
      mirror: true, linewidth: 2, ticks: "outside", showline: true, gridcolor: "grey"
    },
    yaxis: {
      title:       { text: yTitle, font: { size: 14, family: "Arial", color: "#000" } },
      tickfont:    { size: 14, family: "Arial" },
      ...pctTicks,
      zeroline:    isRelPct,
      zerolinewidth: 2.5,
      zerolinecolor: "rgba(80,80,80,0.7)",
      mirror: true, linewidth: 2, ticks: "outside", showline: true, gridcolor: "grey"
    },
    legend: {
      x: 0.5, y: -0.18, orientation: "h",
      xanchor: "center", yanchor: "top",
      font: { size: 13, family: "Arial" }
    },
    hovermode:  "closest",
    hoverlabel: { font: { size: 13, family: "Arial" } },
    margin:     { t: 10, b: 130, l: 50, r: 20 },
    paper_bgcolor: "white", plot_bgcolor: "white"
  };
}
const PLOT_CFG = {
  modeBarButtonsToRemove: ["zoomIn2d","zoomOut2d","pan2d","lasso2d","select2d","autoScale2d"],
  edits: { shapePosition: true },
  displaylogo: false
};
function renderPlot() {
  const layout = buildLayout();
  document.getElementById("y-title").textContent = layout.yaxis.title.text;
  delete layout.yaxis.title;
  return Plotly.react("chart", buildTraces(), layout, PLOT_CFG);
}
')

js5 <- '
function updateCatPills() {
  const cats = availCats();
  if (!selCats) selCats = [...cats];
  selCats = selCats.filter(c => cats.includes(c));
  if (!selCats.length) selCats = [...cats];
  const el = document.getElementById("cat-pills");
  el.innerHTML = "";
  cats.forEach(cat => {
    const btn = document.createElement("button");
    btn.className = "pill" + (selCats.includes(cat) ? " active" : "");
    btn.textContent = cat;
    btn.dataset.cat = cat;
    // Single click: toggle this category
    btn.addEventListener("click", () => {
      if (selCats.includes(cat)) {
        if (selCats.length > 1) {
          selCats = selCats.filter(c => c !== cat);
          btn.classList.remove("active");
        }
      } else {
        selCats = [...selCats, cat];
        btn.classList.add("active");
      }
      renderPlot();
    });
    // Double click: isolate this category
    btn.addEventListener("dblclick", () => {
      selCats = [cat];
      document.querySelectorAll("#cat-pills .pill")
        .forEach(p => p.classList.toggle("active", p.dataset.cat === cat));
      renderPlot();
    });
    el.appendChild(btn);
  });
}

let ddFocusIdx = -1;

function renderKapTags() {
  const tagEl = document.getElementById("kap-tags");
  tagEl.innerHTML = "";
  selKaps.forEach(kap => {
    const entry = ALL_KAPS.find(k => k.val === kap);
    if (!entry) return;
    const tag  = document.createElement("span");
    tag.className = "kap-tag";
    const lbl  = document.createElement("span");
    lbl.textContent = entry.lbl;
    const xbtn = document.createElement("button");
    xbtn.textContent = "×";
    xbtn.addEventListener("mousedown", e => { e.preventDefault(); removeKap(kap); });
    tag.appendChild(lbl);
    tag.appendChild(xbtn);
    tagEl.appendChild(tag);
  });
  document.getElementById("kap-input").style.display = selKaps.length >= 6 ? "none" : "";
}
function removeKap(val) {
  selKaps = selKaps.filter(k => k !== val);
  if (!selKaps.length) selKaps = [CELKEM_VAL];
  // preserve selCats — updateCatPills handles intersection
  renderKapTags(); updateCatPills(); renderPlot();
}
function addKap(val) {
  if (selKaps.includes(val) || selKaps.length >= 6) return;
  selKaps = [...selKaps, val];
  // preserve selCats — updateCatPills handles intersection
  document.getElementById("kap-input").value = "";
  hideDropdown();
  renderKapTags(); updateCatPills(); renderPlot();
}
function hideDropdown() {
  document.getElementById("kap-dropdown").hidden = true;
  ddFocusIdx = -1;
}
function kapMatches(k, q) {
  if (q === "") return true;
  const ql = q.toLowerCase();
  return k.lbl.toLowerCase().includes(ql) || k.zkr.toLowerCase().includes(ql);
}
function showDropdown(q) {
  const dd = document.getElementById("kap-dropdown");
  const matches = ALL_KAPS
    .filter(k => !selKaps.includes(k.val) && kapMatches(k, q))
    .slice(0, 12);
  if (!matches.length) {
    dd.innerHTML = `<div class="dd-empty">Nic nenalezeno</div>`;
    dd.hidden = false; return;
  }
  dd.innerHTML = ""; ddFocusIdx = -1;
  matches.forEach((k, i) => {
    const item = document.createElement("div");
    item.className = "dd-item"; item.textContent = k.lbl;
    item.addEventListener("mouseover", () => setDdFocus(i));
    item.addEventListener("mousedown", e => { e.preventDefault(); addKap(k.val); });
    dd.appendChild(item);
  });
  dd.hidden = false;
}
function setDdFocus(idx) {
  document.querySelectorAll("#kap-dropdown .dd-item")
    .forEach((el, i) => el.classList.toggle("focused", i === idx));
  ddFocusIdx = idx;
}
'

js6 <- '
document.addEventListener("DOMContentLoaded", () => {
  const realChk  = document.getElementById("real-chk");
  const realWrap = document.getElementById("real-chk-wrap");

  function setRealChkEnabled() {
    const dis = isRelativeMode();
    realWrap.classList.toggle("disabled", dis);
    realChk.disabled = dis;
  }

  // Mode buttons
  document.querySelectorAll(".mode-btn").forEach(btn => {
    btn.addEventListener("click", () => {
      mode = btn.dataset.mode;
      document.querySelectorAll(".mode-btn").forEach(b => b.classList.remove("active"));
      btn.classList.add("active");
      document.getElementById("pct-hint").style.display  = mode.endsWith("_change") ? "" : "none";
      document.getElementById("mzda-note").style.display = mode === "sal_vs_avg" ? "" : "none";
      setRealChkEnabled();
      renderPlot();
    });
  });

  // Real / nominal checkbox
  realChk.addEventListener("change", () => {
    isNom = !realChk.checked;
    renderPlot();
  });

  // Reset button
  document.getElementById("reset-btn").addEventListener("click", () => {
    selKaps  = [CELKEM_VAL];
    selCats  = null;
    mode     = "sal_level";
    baseYear = 2019;
    isNom    = false;
    realChk.checked = true;
    document.querySelectorAll(".mode-btn").forEach(b => b.classList.remove("active"));
    document.querySelector(".mode-btn[data-mode=\'sal_level\']").classList.add("active");
    document.getElementById("pct-hint").style.display  = "none";
    document.getElementById("mzda-note").style.display = "none";
    setRealChkEnabled();
    document.getElementById("kap-input").value = "";
    hideDropdown();
    renderKapTags(); updateCatPills(); renderPlot();
  });

  // Kapitola type-ahead
  const inp = document.getElementById("kap-input");
  inp.addEventListener("input",  e => showDropdown(e.target.value));
  inp.addEventListener("focus",  e => showDropdown(e.target.value));
  inp.addEventListener("click",  e => showDropdown(e.target.value));
  inp.addEventListener("blur",   () => setTimeout(hideDropdown, 160));
  inp.addEventListener("keydown", e => {
    const visible = ALL_KAPS.filter(k => !selKaps.includes(k.val) &&
      kapMatches(k, inp.value)).slice(0, 12);
    if (e.key === "ArrowDown") {
      e.preventDefault(); setDdFocus(Math.min(ddFocusIdx + 1, visible.length - 1));
    } else if (e.key === "ArrowUp") {
      e.preventDefault(); setDdFocus(Math.max(ddFocusIdx - 1, 0));
    } else if (e.key === "Enter" && ddFocusIdx >= 0) {
      e.preventDefault(); if (visible[ddFocusIdx]) addKap(visible[ddFocusIdx].val);
    } else if (e.key === "Escape") {
      hideDropdown();
    } else if (e.key === "Backspace" && inp.value === "" && selKaps.length > 0) {
      e.preventDefault(); removeKap(selKaps[selKaps.length - 1]);
    }
  });
  document.getElementById("kap-box").addEventListener("click", e => {
    if (e.target.tagName !== "BUTTON") inp.focus();
  });

  // Initial render — sync isNom with checkbox (browser may restore checked state on reload)
  isNom = !realChk.checked;
  renderKapTags(); updateCatPills();
  renderPlot().then(() => {
    // Snap base-year line to nearest year on every relayout event
    document.getElementById("chart").on("plotly_relayout", ev => {
      const x0 = ev["shapes[0].x0"];
      if (x0 !== undefined) {
        const by = Math.round(Math.max(2003, Math.min(THIS_YEAR, x0)));
        if (by !== baseYear) {
          baseYear = by;
          renderPlot();
        } else if (Math.abs(x0 - by) > 0.001) {
          // snap shape without rebuilding all traces
          Plotly.relayout("chart", {"shapes[0].x0": by, "shapes[0].x1": by});
        }
      }
    });
  });
});
'

js <- paste0(js1, js2, js3, js4, js5, js6)

# ── UI text helpers ───────────────────────────────────────────────────────────
hdr_lbl   <- "Platy ve státní správě – přehled kapitol"
kap_lbl   <- "Kapitola"
metrika_lbl <- "Metrika"
metrika_hint <- "(výchozí rok pro výpočet změnových metrik můžeme změnit posununím oranžové čáry)"
kap_hint  <- "(vyberte až 6: vyhledejte psaním, pak vyberte kliknutím nebo enterem)"
kat_lbl   <- "Kategorie"
kat_hint <- "(dvojklikem vypnete ostatní)"
kap_ph    <- "Vybrat rozpočtovou kapitolu…"
chk_lbl   <- paste0("Zobrazit v cenách roku ", this_year)
src_note  <- paste0(
  "Zdroj: Ministerstvo financí ČR, Státní závěrečný účet. ",
  "Skutečné výdaje (SKUT). ",
  "Reálné hodnoty v cenách ", this_year, " Kč (CPI, ČSÚ)."
)
pct_hint  <- "☜ Přetáhněte svislou čáru v grafu pro výběr základního roku."
mzda_note <- "Ministerstva jsou porovnávána s průměrnou mzdou v Praze, ostatní s průměrnou mzdou v ČR."

btn_sal   <- "Průměrný plat"
btn_cst   <- "Platové náklady"
btn_avg   <- "Průměr vs. trh práce"
btn_stf   <- "Počty zaměstnanců"
btn_reset <- "Obnovit výchozí"

# ── Assemble HTML ─────────────────────────────────────────────────────────────
html <- paste0(
'<!DOCTYPE html>
<html lang="cs">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>', hdr_lbl, '</title>
  <script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
  <style>', css, '</style>
</head>
<body>

<div class="row">
  <div style="flex:1 1 320px">
    <p class="lbl">', kap_lbl, ' <span style="font-weight:400;color:#999">', kap_hint, '</span></p>
    <div class="kap-box" id="kap-box">
      <span id="kap-tags"></span>
      <input type="text" id="kap-input" placeholder="', kap_ph, '"
             autocomplete="off" spellcheck="false">
      <div class="kap-dropdown" id="kap-dropdown" hidden></div>
    </div>
  </div>
</div>
<p class="lbl">', kat_lbl, ' <span style="font-weight:400;color:#999">', kat_hint, '</span></p>
<div id="cat-pills"></div>

<div class="mode-row">
  <p class="lbl">', metrika_lbl, ' <span style="font-weight:400;color:#999">', metrika_hint, '</span></p>
  <div class="mode-controls" style="width:100%;">
    <div class="metric-col">
      <span class="metric-col-lbl">', btn_sal, '</span>
      <div class="btn-row">
        <button class="btn mode-btn active" data-mode="sal_level">vývoj</button>
        <button class="btn mode-btn"        data-mode="sal_change">změny</button>
      </div>
    </div>
    <div class="metric-col">
      <span class="metric-col-lbl">', btn_cst, '</span>
      <div class="btn-row">
        <button class="btn mode-btn" data-mode="cost_level">vývoj</button>
        <button class="btn mode-btn" data-mode="cost_change">změny</button>
      </div>
    </div>
    <div class="metric-col">
      <span class="metric-col-lbl">', btn_stf, '</span>
      <div class="btn-row">
        <button class="btn mode-btn" data-mode="staff">vývoj</button>
        <button class="btn mode-btn" data-mode="staff_change">změny</button>
      </div>
    </div>
    <div class="metric-col">
      <span class="metric-col-lbl">Relativní platy</span>
      <div class="btn-row">
        <button class="btn mode-btn" data-mode="sal_vs_avg">', btn_avg, '</button>
      </div>
    </div>
    <div class="metric-col">
      <span class="metric-col-lbl">Očištění o inflaci</span>
      <label class="chk-label" id="real-chk-wrap" style="align-items:center; padding:6px 0; border:1px solid transparent; border-radius:4px;">
        <input type="checkbox" id="real-chk" checked> ', chk_lbl, '
      </label>
    </div>
    <div style="display:flex; align-self:stretch;">
      <button id="reset-btn" style="width:100%; padding:7px 18px; font-size:14px; font-weight:700; background:#fff; color:#c90239; border:2px solid #c90239; border-radius:4px; cursor:pointer; white-space:nowrap;">', btn_reset, '</button>
    </div>
  </div>
  <p id="pct-hint">',   pct_hint,  '</p>
  <p id="mzda-note">', mzda_note, '</p>
</div>

<div id="y-title"></div>
<div id="chart"></div>

<script>
', js, '
</script>
</body>
</html>')

writeLines(html, "dashboard.html", useBytes = FALSE)
message("Written: dashboard.html (", round(nchar(html) / 1024), " KB)")
