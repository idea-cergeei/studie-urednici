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
  "Ministerstva",
  "Ostatn\u00ed \u00fast\u0159edn\u00ed",
  "Ne\u00fast\u0159edn\u00ed st\u00e1tn\u00ed spr\u00e1va",
  "V\u0161ichni st\u00e1tn\u00ed \u00fa\u0159edn\u00edci",
  "Sbory",
  "Ostatn\u00ed v\u010d. arm\u00e1dy",
  "P\u0159\u00edsp\u011bvkov\u00e9 organizace"
)
kat_colors <- setNames(
  c(palette_okabe_ito(c(1, 2, 3, 7, 5, 4)), "#CCCCCC"),
  kat_lbls
)

# ── Data ──────────────────────────────────────────────────────────────────────
data_raw <- read_parquet("data-export/data_all.parquet") |>
  filter(!is.na(kap_kod), !is.na(kategorie_2014_cz), faze_rozpoctu == "SKUT") |>
  mutate(
    kap_nazev         = str_trim(kap_nazev),
    kategorie_2014_cz = case_when(
      kategorie_2014_cz == "St\u00e1tn\u00ed \u00fa\u0159edn\u00edci"   ~ "V\u0161ichni st\u00e1tn\u00ed \u00fa\u0159edn\u00edci",
      kategorie_2014_cz == "Ne\u00fast\u0159edn\u00ed st. spr\u00e1va" ~ "Ne\u00fast\u0159edn\u00ed st\u00e1tn\u00ed spr\u00e1va",
      .default = kategorie_2014_cz
    ),
    platy_real         = platy         * .data[[defl_col]],
    prumerny_plat_real = prumerny_plat * .data[[defl_col]]
  )

CELKEM_VAL <- "__CELKEM__"
CELKEM_LBL <- "\u2014 Celkem (v\u0161echny kapitoly) \u2014"

celkem_data <- data_raw |>
  group_by(rok, kategorie_2014_cz) |>
  summarise(
    platy             = sum(platy,             na.rm = TRUE),
    platy_real        = sum(platy_real,        na.rm = TRUE),
    pocet_zamestnancu = sum(pocet_zamestnancu, na.rm = TRUE),
    .groups           = "drop"
  ) |>
  mutate(
    prumerny_plat      = platy      / pocet_zamestnancu / 12,
    prumerny_plat_real = platy_real / pocet_zamestnancu / 12,
    kap_nazev          = CELKEM_LBL,
    kap_zkr            = "CELKEM"
  )

keep_cols <- c("kap_nazev", "kap_zkr", "kategorie_2014_cz", "rok",
               "platy", "platy_real", "prumerny_plat", "prumerny_plat_real",
               "pocet_zamestnancu")

js_data <- bind_rows(
  data_raw    |> select(all_of(keep_cols)),
  celkem_data |> select(all_of(keep_cols))
) |>
  mutate(across(c(platy, platy_real, prumerny_plat, prumerny_plat_real,
                  pocet_zamestnancu), round))

kap_df <- data_raw |>
  distinct(kap_nazev, kap_zkr) |>
  arrange(kap_nazev) |>
  mutate(lbl = paste0(kap_zkr, " \u2013 ", kap_nazev))

# Average wages per year (benchmark for sal_vs_avg)
mzda_by_year <- data_raw |>
  filter(!is.na(prumerna_mzda_cr)) |>
  distinct(rok, prumerna_mzda_cr, prumerna_mzda_pha) |>
  arrange(rok)
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
  display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 6px;
}
.chk-label {
  display: flex; align-items: center; gap: 6px;
  font-size: 13px; cursor: pointer; user-select: none;
  white-space: nowrap; padding-bottom: 4px;
}
.chk-label input[type=checkbox] { width: 16px; height: 16px; cursor: pointer; accent-color: #c90239; }
.chk-label.disabled { opacity: 0.35; pointer-events: none; }
.shapelayer path { cursor: ew-resize !important; }
#pct-hint   { display: none; color: #666; font-size: 12px; margin: 2px 0 4px; }
#mzda-note  { display: none; color: #888; font-size: 11px; margin: 2px 0 4px; }
#chart { flex: 1 1 auto; min-height: 300px; }
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
    const bm = (r.kategorie_2014_cz === "Ministerstva" || r.kategorie_2014_cz === "Ostatn\u00ed \u00fast\u0159edn\u00ed") ? mzda.pha : mzda.cr;
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
        ys.push(+y.toFixed(2));
        texts.push(fmtHover(y, isRelPct));
      });
      if (!xs.length) return;
      const col = KAT_COLOR[kat] || "#888888";
      const nm  = selKaps.length > 1 ? kat + " (" + zkr + ")" : kat;
      traces.push({
        x: xs, y: ys, text: texts,
        type: "scatter", mode: "lines+markers",
        name: nm, legendgroup: nm,
        line:   { color: col, width: 2.5, dash: dsh },
        marker: { color: col, size: 7 },
        hovertemplate: "<b>" + nm + "</b><br>Rok: %{x}<br>%{text}<extra></extra>"
      });
    });
  });
  return traces;
}
')

# y-axis labels (UTF-8 in R strings, no \u sequences needed in JS)
lbl_sal_nom  <- paste0("Pr\u016fm\u011brn\u00fd plat (K\u010d/m\u011bs\u00edc)")
lbl_sal_real <- paste0("Pr\u016fm\u011brn\u00fd plat (K\u010d, ceny ", this_year, ")")
lbl_sal_chg  <- paste0("Pr\u016fm. plat \u2013 zm\u011bna od BASE (%)")
lbl_cst_nom  <- paste0("N\u00e1klady na platy (mld. K\u010d)")
lbl_cst_real <- paste0("N\u00e1klady na platy (mld. K\u010d, ceny ", this_year, ")")
lbl_cst_chg  <- paste0("N\u00e1klady \u2013 zm\u011bna od BASE (%)")
lbl_vsavg    <- paste0("Odchylka pr\u016fm. platu od pr\u016fm. mzdy (%)")
lbl_staff    <- paste0("Po\u010det zam\u011bstnanc\u016f (FTE)")
lbl_stf_chg  <- paste0("Po\u010det zam. \u2013 zm\u011bna od BASE (%)")
lbl_zaklad   <- paste0("Z\u00e1klad: ")

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
      title:    { text: "Rok", font: { size: 16, family: "Arial", color: "#000" } },
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
    hovermode:  "x unified",
    hoverlabel: { font: { size: 13, family: "Arial" } },
    margin:     { t: 30, b: 130, l: 80, r: 20 },
    paper_bgcolor: "white", plot_bgcolor: "white"
  };
}
const PLOT_CFG = {
  modeBarButtonsToRemove: ["zoomIn2d","zoomOut2d","pan2d","lasso2d","select2d","autoScale2d"],
  edits: { shapePosition: true },
  displaylogo: false
};
function renderPlot() {
  return Plotly.react("chart", buildTraces(), buildLayout(), PLOT_CFG);
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
    xbtn.textContent = "\u00d7";
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

  // Kapitola type-ahead
  const inp = document.getElementById("kap-input");
  inp.addEventListener("input",  e => showDropdown(e.target.value));
  inp.addEventListener("focus",  e => showDropdown(e.target.value));
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
hdr_lbl   <- "Platy ve st\u00e1tn\u00ed spr\u00e1v\u011b \u2013 p\u0159ehled kapit\u00f3l"
kap_lbl   <- "Kapit\u00f3la"
kap_hint  <- "(max 6, Enter nebo klik pro v\u00fdb\u011br)"
kap_ph    <- "Vyhledat kapit\u00f3lu\u2026"
chk_lbl   <- paste0("Zobrazit v cen\u00e1ch roku ", this_year)
src_note  <- paste0(
  "Zdroj: Ministerstvo financ\u00ed \u010cR, St\u00e1tn\u00ed z\u00e1v\u011bre\u010dn\u00fd \u00fa\u010det. ",
  "Skute\u010dn\u00e9 v\u00fddaje (SKUT). ",
  "Re\u00e1ln\u00e9 hodnoty v cen\u00e1ch ", this_year, " K\u010d (CPI, \u010cS\u00da)."
)
pct_hint  <- "\u261c P\u0159et\u00e1hn\u011bte svislou \u010d\u00e1ru v grafu pro v\u00fdb\u011br z\u00e1kladn\u00edho roku."
mzda_note <- "Ministerstva jsou porovn\u00e1v\u00e1na s pr\u016fm\u011brnou mzdou v Praze, ostatn\u00ed s pr\u016fm\u011brnou mzdou v \u010cR."

btn_sal   <- "\u00d8 plat"
btn_sal_c <- "\u00d8 plat \u2013 v\u00fdvoj"
btn_cst   <- "Plat. n\u00e1klady"
btn_cst_c <- "Plat. n\u00e1klady \u2013 v\u00fdvoj"
btn_avg   <- "Plat vs. trh pr\u00e1ce"
btn_stf   <- "Zam\u011bstnanci"
btn_stf_c <- "Zam\u011bstnanci \u2013 v\u00fdvoj"

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

<h4>Prohlíže\u010dka</h4>

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

<p class="lbl">Kategorie</p>
<div id="cat-pills"></div>

<div class="mode-row">
  <p class="lbl">Zobrazit</p>
  <div class="mode-controls">
    <div class="btn-group">
      <button class="btn mode-btn active" data-mode="sal_level">',  btn_sal,   '</button>
      <button class="btn mode-btn" data-mode="sal_change">',        btn_sal_c, '</button>
      <button class="btn mode-btn" data-mode="cost_level">',        btn_cst,   '</button>
      <button class="btn mode-btn" data-mode="cost_change">',       btn_cst_c, '</button>
      <button class="btn mode-btn" data-mode="sal_vs_avg">',        btn_avg,   '</button>
      <button class="btn mode-btn" data-mode="staff">',             btn_stf,   '</button>
      <button class="btn mode-btn" data-mode="staff_change">',      btn_stf_c, '</button>
    </div>
    <label class="chk-label" id="real-chk-wrap">
      <input type="checkbox" id="real-chk" checked> ', chk_lbl, '
    </label>
  </div>
  <p id="pct-hint">',   pct_hint,  '</p>
  <p id="mzda-note">', mzda_note, '</p>
</div>

<div id="chart"></div>

<script>
', js, '
</script>
</body>
</html>')

writeLines(html, "dashboard.html", useBytes = FALSE)
message("Written: dashboard.html (", round(nchar(html) / 1024), " KB)")
