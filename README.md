
<!-- README.md is generated from README.Rmd. Please edit that file -->

# obt

<!-- badges: start -->

[![R-CMD-check](https://github.com/epicentre-msf/obt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epicentre-msf/obt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`obt` drives the OutbreakTools designer and the linelist workbook from
R. The steps normally done by clicking through Excel — download a
release, fill a setup, add a geobase, generate a linelist, run an export
— are written as R code and run in one go.

Each verb records one operation takes the `obt` object as input. The
verbs chain with `|>`. Nothing reaches the disk while a recipe is built.
`obt_commit()` is the one function that opens Excel.

## Installation

``` r
# install.packages("pak")
pak::pak("epicentre-msf/obt")
```

`obt` reads closed `.xlsb` workbooks through
[readxlsb](https://github.com/velofrog/readxlsb).

Opening Excel needs **macOS or Windows**, with Microsoft Excel
installed. On any other system the package loads, recipes build and
print, and `obt_commit()` is turned down with a warning message.

On macOS the package copies a run into Excel’s own folder, because Excel
for Mac opens a file there without asking permission. Reaching that
folder needs Full Disk Access for the app running your R session. If it
can not be reached, the run works in your own working folder and Excel
asks permission access for each file/folder it opens.

`obt_platform()` gives you infos about what your system can do for a
run: whether a recipe can run, the staging folder, the version of Excel
installed, and the app your R session runs in.

``` r
obt_platform()
```

### Download the release

Name a working folder, choose the channel, and the run downloads the zip
into that folder and unzips it. `"main"` is the stable release and
`"dev"` is the development build.

``` r
library(obt)

run <- obt(folder = "measles-2026") |>
  obt_designer_add(type = "main")

run
#> 
#> ── OBT recipe ──────────────────────────────────────────────────────────────────
#> Working folder: 'measles-2026'
#> 1 operation: Add the designer.
#> ✔ Every operation can run. Use `obt_commit()`.
```

Execute the run with `obt_commit()`

``` r
obt_commit(run)
```

Everything the run downloads or writes lives under the one folder, in a
fixed layout.

``` r
names(obt_paths(run))
#>  [1] "folder"       "obt"          "obt_main"     "obt_dev"      "setup"       
#>  [6] "setup_source" "geo"          "linelist"     "export"       "log"         
#> [11] "setup_file"
```

## Turn a setup into a linelist

Fill the release’s empty setup from a setup you already have, add a
geobase, choose the two languages, and generate.

``` r
run <- obt(folder = "measles-2026") |>
  obt_designer_add(type = "main") |>
  obt_setup_convert(from = "setup.xlsb") |>
  obt_designer_geobase(path = "geobase.xlsx") |>
  obt_designer_languages(dict = "English", form = "ENG") |>
  obt_designer_generate(name = "measles-2026")
```

`obt_describe()` shows every value the run will use and every operation
with its arguments. Read it before `obt_commit()`.

``` r
obt_describe(run)
#> 
#> ── OBT recipe ──────────────────────────────────────────────────────────────────
#> 
#> ── The run will use: ──
#> 
#> Working folder: measles-2026
#> Channel: "main"
#> Setup: (not set)
#> Setup to convert: "setup.xlsb"
#> Geobase: "geobase.xlsx"
#> Setup language: "English"
#> Interface language: "ENG"
#> Output name: "measles-2026"
#> Fake records: (not set)
#> 
#> ── Operations ──
#> 
#> 1. Add the designer `designer-add`
#>    type: "main"
#>    force: FALSE
#> 2. Add the setup to convert `convert-add`
#>    from: "setup.xlsb"
#> 3. Write the old setup out `convert-export`
#>    to: "export"
#> 4. Fill the new setup `convert-import`
#>    overwrite: FALSE
#> 5. Add a geobase `designer-geobase`
#>    path: "geobase.xlsx"
#> 6. Set the languages `designer-languages`
#>    dict: "English"
#>    form: "ENG"
#> 7. Generate the linelist `designer-generate`
#>    name: "measles-2026"
#>    overwrite: FALSE
#>    password: (not set)
#>    debug_password: (not set)
```

`obt_commit()` runs the operations in the order the verbs recorded them,
prints each one as it starts and ends, stops at the first failure naming
the operation and the reason. Every run writes one text log under
`log/`.

## Working on one workbook

Two verbs narrow a recipe to a single workbook, and each holds the
operations that run inside it. `obt_setup()` holds a setup;
`obt_linelist()` holds a linelist that is already built. Both take the
file itself, or an `obt` recipe to narrow.

``` r
obt_linelist(from = "measles-2026.xlsb", folder = "measles-2026") |>
  obt_linelist_geobase(path = "geobase.xlsx") |>
  obt_linelist_import(from = "cases.xlsx") |>
  obt_describe()
#> 
#> ── OBT linelist recipe ─────────────────────────────────────────────────────────
#> 
#> ── The run will use: ──
#> 
#> Working folder: measles-2026
#> Linelist: "measles-2026.xlsb"
#> Geobase: "geobase.xlsx"
#> Read in from: "cases.xlsx"
#> Export: (not set)
#> 
#> ── Operations ──
#> 
#> 1. Add the linelist `linelist-add`
#>    from: "measles-2026.xlsb"
#>    overwrite: FALSE
#>    password: (not set)
#> 2. Import a geobase `linelist-geobase`
#>    path: "geobase.xlsx"
#> 3. Import a migrated file `linelist-import`
#>    from: "cases.xlsx"
#>    rule: "append"
#>    force: FALSE
```

For more informations, read the vignette: `vignette(package = "obt")`

## Licence

MIT. See `LICENSE`.
