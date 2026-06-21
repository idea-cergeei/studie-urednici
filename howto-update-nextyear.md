# How to Update Analysis for Next Year

## Data Input Updates

- [ ] Update main data file: `data-input/Data_YYYY.xlsx` with new year's data

## Code Updates

- Update year references in text and code:
  - [ ] `standardize_input_data.R`: Update year in file paths, variable names and comments
  - `graphs.R`: 
    - [ ] Update year in file paths, variables and comments
    - [ ] Update graph titles and annotations in `graph_titles.csv` and `annotations.csv`
    - [ ] update labour force, GDP and budget size parameters in `graphs.R`
  - [ ] update `pay-change-nace_plot.R` with new year in file paths, variables and comments
  - [ ] `graphs_mod_long.r`: Update year in title/text
  - [ ] `www/template_long.html`: Update year references

- **Manual step:** `text.csv` (and `annotations.csv` where needed) use `XXX` as placeholders for specific numbers; update them by hand with current values when you change the year.

## Output Updates

- Update static PDF documents:
  - use source word docx: `word-docs/urednici-shrnuti_YYYY.docx`, `word-docs/urednici-metodologie_YYYY.docx`
  - [ ] `www/pdf/shrnuti.pdf`, will show up in `graphs_mod/pdf/shrnuti.pdf`
  - [ ] `www/pdf/metodologie.pdf` will show up in `graphs_mod/pdf/metodologie.pdf`
  - [ ] commit new docx files: `word-docs/urednici-shrnuti_YYYY.docx`, `word-docs/urednici-metodologie_YYYY.docx`
- Update cover page
  - [ ] `www/index.html` with new year and data references
  - [ ] acknowledgments in (i) modal in `www/index.html`
  - [ ] `www/img/title_v3_paths.svg` with new cover image (edit `www/img/title_v3_source.svg` SVG as needed and export as paths, using e.g. [`svg-text2path`](https://github.com/Emasoft/svg-text2path))
    - update links to previous versions
    - update years and authors
    - update logos as needed
- [ ] Update social image: export graph 5a into `www/img/opengraph.png`
- [ ] update social teaser: `cover.html` (claude design) using `cover-base.png`, then export to `cover.png`

## Run the whole pipeline to generate the app and other outputs

- [ ] Run full build pipeline via `build.R`:
  - Data standardization and processing:
    - Run `standardize_input_data.R` to process new input data
  - Graph generation (`graphs.R`, `pay-change-nace_plot.R`, `graf_rocni-zmeny-dekompozice.R`)
  - Modified graphs (`graphs_mod.R`, `graphs_mod_long.r`)
  - Codebook generation
- [ ] Export data: Run `data_export.R` to generate `data-export/` files
- [ ] Generate PDF versions of graphs in `graphs_pdf/`

## Publication

- [ ] Test Deploy to Netlify (automated in build script)
- [ ] Move last year's app to new URL
- [ ] Deploy new version on IDEA server
- [ ] Update links to the new app in the `README.md` and other documentation
- [ ] Update this guidance with any new steps or changes
- [ ] Commit and tag final version
