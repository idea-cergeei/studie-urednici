# Státní úředníci: studie / aplikace IDEA při CERGE-EI

Datová analýza: Státní zaměstnanci a úředníci.

Kód, data a zdrojové soubory pro 

- studii IDEA č. 2/2022
- aplikaci IDEA na adrese https://ideaapps.cerge-ei.cz/urednici/ a předchozích verzích

Hlavním vstupem je verze dat Státního závěrečného účtu poskytnutá Ministerstvem financí. Jde o verzi tabulek 10 a 11 v SZÚ. Pro výpočty dále používáme data z ČSÚ (průměrné mzdy, velikost pracovní síly), státní pokladny (rozpočet) a národních účtů (HDP).

## Autoři

Na studii pracovali Petr Bouchal, Daniel Bartušek a Petr Janský. Spoluautorem aplikace od roku 2022 je Taras Hrendash. Daniel Münich tuto práci provází od začátku a v poslední verzi aplikace je spoluautorem. 

## Aplikace 

- aktuální verze aplikace je na adrese <https://ideaapps.cerge-ei.cz/urednici/>
- předchozí verze aplikace je <https://ideaapps.cerge-ei.cz/urednici/2024/>

## Studie

Zveřejněno IDEA v lednu 2022:

Bartušek, Daniel, Bouchal, Petr, & Janský, Petr. (2022). Státní zaměstnanci a úředníci: Kde pracují a za kolik? IDEA. https://idea.cerge-ei.cz/files/IDEA_Studie_2_2022_Statni_zamestnanci_a_urednici/IDEA_Studie_2_2022_Statni_zamestnanci_a_urednici.html#p=2

Interaktivní grafy zveřejněny jako online apendix studie na https://ideaapps.cerge-ei.cz/urednici_2021/

Text v souboru `results.Rmd` neodpovídá textu studie, který prošel odděleným editačním procesem. Odpovídají ale interaktivní grafy.

## Data

Exportována v CSV a parquet do adresáře `data-export`, spolu s codebookem - viz [Github](https://github.com/idea-cergeei/studie-urednici/tree/main/data-export)

- lidsky čitelný popis na [webu](https://idea-cergeei.github.io/studie-urednici/codebook.html)
- YAML export v souboru `codebook.yml`

Názvy sloupců v exportech jsou upraveny pro srozumitelnost, neodpovídají názvům používaným v kódu při tvorbě grafů. Rozdíly lze odvodit ze skriptu `export_data.R`.

### Data o platech z ČSÚ

Toto je poznámka for posterity - před publikací oficiálních ČSÚ (typicky druhá polovina května) je třeba ručně vložit data o mzdách je potřeba vybrat ta správná.

- datová sada ČSÚ 110080, kterou ČSÚ publikuje v otevřených datech vychází ze šetření struktury výdělků
- oproti tomu čísla ve VDB pochází *většinou* z firemních výkazů
- průměrné platy dle VDB jsou o cca 5 % nižší
- ve VDB jsou ale místy i výstupy ze strukturálních statistik - pozná se to tak, že tam je jiný výstup než průměr (medián), např. MZD11/13 <https://vdb.czso.cz/vdbvo2/faces/cs/index.jsf?page=vystup-objekt&pvo=MZD11&z=T&f=TABULKA&katalog=30852&str=v377&c=v3~8__RP2023>, ty potom lícují přesně s čísly v open datové sadě
- viz k tomu [dokumentaci dat od ČSÚ](https://csu.gov.cz/docs/107508/a54c8df8-eeb1-b403-5382-951daf59ab43/110080-22dds.docx?version=1.0)

## Reprodukování výstupů a dat

Jak aktualizovat data z nového zdroje viz [howto-update-nextyear.md](howto-update-nextyear.md).

```r
install.packages("renv")
renv::restore()
source("standardize_input_data.R")
rmarkdown::render("results.Rmd")
source("graphs.R")
source("graphs_mod.R")
source("graphs_mod_long.R")
source("export_data.R")
rmarkdown::render("codebook.Rmd")
```

