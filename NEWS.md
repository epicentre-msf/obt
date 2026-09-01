# obt 0.0.0.9000

First working version. The package builds a recipe of operations and runs it
in one go, so the input/output steps normally clicked through the OutbreakTools workbooks
can be scripted from R.

## The recipe

* `obt()` starts a recipe on one working folder. Every verb records one
  operation and answers the recipe, so verbs chain with `|>`. Nothing reaches
  the disk while a recipe is built.
* `obt_commit()` runs the operations in the order they were recorded, stops at
  the first failure naming the operation and the reason, and writes one text
  log per run under `log/`.
* `obt_summary()`, `obt_operations()`, `obt_describe()` and the `print()`
  method read a recipe at three depths. An operation whose entry point has yet
  to ship prints as waiting, with what it needs.
* `obt_remove()` takes one step back out, named by the number or the type
  that the two lists print beside it. `obt_pop()` takes the last one off. A
  verb that recorded several steps in one call has all of them taken out
  together, and the message says so.
* `obt_paths()` resolves the fixed layout of the working folder and is the one
  thing that builds a path.

## Operations

* `obt_designer_add()` downloads an OutbreakTools release, `"main"` or
  `"dev"`, and unzips it under the working folder. File names are read from
  the zip.
* `obt_setup()` narrows a recipe to one setup workbook, and
  `obt_setup_export()`, `obt_setup_import()` and `obt_setup_tags()` are the
  three verbs that work on it.
* `obt_linelist()` narrows a recipe to one generated linelist, and
  `obt_linelist_geobase()`, `obt_linelist_import()` and
  `obt_linelist_export()` are the verbs that work on it. Both narrowings take
  the workbook itself or an `obt` recipe, and a verb takes only the class its
  workbook belongs to.
* `obt_setup_convert()` fills a fresh copy of the release's empty setup from a
  setup you already have. The route runs through an `.xlsx`.
* `obt_fake()` makes up records against a setup: one table per worksheet the
  dictionary names, written as a single `.xlsx` with a sheet per worksheet.
* `obt_setup_languages()` reads the dictionary languages off a closed setup
  file, `.xlsb` or `.xlsx`, with no Excel running.
* `obt_designer_languages()` and `obt_languages()` carry and print the two
  languages a linelist takes: the dictionary language and the interface one.
* `obt_designer_generate()` fills the designer's `Main` sheet and generates the
  linelist.
* `obt_designer_geobase()` points a generation at a geobase, and
  `obt_linelist_geobase()` reads one into a linelist that is already built.
* `obt_linelist_import()` reads a migration file into a linelist, and stops
  where the workbook warns unless `force = TRUE`.
* `obt_linelist_export()` writes a migration file or runs an export the setup
  defines, behind one `type` argument.
* `obt_silent_get()` and `obt_silent_set()` read and write what a workbook does
  with the message boxes it shows while it opens.

## The machine

* `obt_platform()` reads the operating system and, on macOS, the staging folder
  inside Excel's own container and the version of Excel installed.
  `obt_staging_clean()` empties what an interrupted run left behind.
* Running a recipe needs macOS or Windows. Everywhere else the package loads,
  recipes build and print, and only `obt_commit()` will not work.
