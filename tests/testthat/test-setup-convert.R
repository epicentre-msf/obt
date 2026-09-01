test_that("a setup workbook records the three steps in run order", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)
  types <- vapply(recipe$operations, function(op) op$type, character(1))

  expect_identical(
    types,
    c("designer-add", "convert-add", "convert-export", "convert-import")
  )
})

test_that("an .xlsx records two steps and skips the export", {
  folder <- withr::local_tempdir()
  old <- empty_file(folder, "old-setup.xlsx")

  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)
  types <- vapply(recipe$operations, function(op) op$type, character(1))

  expect_identical(types, c("designer-add", "convert-add", "convert-import"))
})

test_that("the file to convert records relative to the working folder", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  expect_identical(operation_at(recipe, 2)$args$from, "old-setup.xlsb")
})

test_that("a file outside the working folder records as it stands", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  old <- old_setup(elsewhere)

  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  expect_identical(operation_at(recipe, 2)$args$from, old)
})

test_that("the export writes into the export folder of the layout", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  expect_identical(operation_at(recipe, 3)$args$to, "export")
})

test_that("the import carries the overwrite flag it was given", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  recipe <- convert_recipe(folder) |>
    obt_setup_convert(from = old, overwrite = TRUE)

  expect_true(operation_at(recipe, 4)$args$overwrite)
})

test_that("all three steps of a conversion run today", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  expect_false(operation_at(recipe, 2)$waiting)
  expect_false(operation_at(recipe, 3)$waiting)
  expect_false(operation_at(recipe, 4)$waiting)
})

test_that("a recipe with no designer is refused at the verb", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  expect_error(
    obt(folder = folder) |> obt_setup_convert(from = old),
    "OutbreakTools files first"
  )
  expect_error(
    obt(folder = folder) |> obt_setup_convert(from = old),
    "obt_designer_add"
  )
})

test_that("a setup recipe is refused, and the reason named", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  expect_error(
    setup_recipe(folder) |> obt_setup_convert(from = old),
    "must be an .*obt.* recipe"
  )
  expect_error(
    setup_recipe(folder) |> obt_setup_convert(from = old),
    "obt_designer_add"
  )
})

test_that("anything other than a recipe is refused", {
  expect_error(obt_setup_convert("recipe", from = "old.xlsb"), "must be an")
})

test_that("any other extension is refused by name", {
  folder <- withr::local_tempdir()
  old <- empty_file(folder, "old-setup.csv")

  expect_error(
    convert_recipe(folder) |> obt_setup_convert(from = old),
    "must end in"
  )
})

test_that("a file the verb cannot find is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    convert_recipe(folder) |>
      obt_setup_convert(from = file.path(folder, "missing.xlsb")),
    "Nothing sits at"
  )
})

test_that("the two arguments are checked at the verb", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  expect_error(
    convert_recipe(folder) |> obt_setup_convert(from = 1),
    "must be a single string"
  )
  expect_error(
    convert_recipe(folder) |> obt_setup_convert(from = old, overwrite = "yes"),
    "must be"
  )
})

test_that("the verb answers a recipe another verb can carry on from", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  recipe <- convert_recipe(folder) |>
    obt_setup_convert(from = old) |>
    obt_designer_generate(name = "measles-2026")

  expect_true(is_obt(recipe))
  expect_false(is_obt_setup(recipe))
  expect_length(recipe$operations, 5)
})

test_that("narrowing to a setup recipe leaves the conversion behind", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  narrowed <- convert_recipe(folder) |>
    obt_setup_convert(from = old) |>
    obt_setup()

  expect_true(is_obt_setup(narrowed))
  expect_length(narrowed$operations, 0)
})

test_that("the description names the setup being converted", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  expect_match(
    paste(cli::cli_fmt(obt_describe(recipe)), collapse = " "),
    "Setup to convert"
  )
})

test_that("the operations print the route the run takes", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)

  shown <- paste(
    cli::cli_fmt(obt_operations(
      convert_recipe(folder) |>
        obt_setup_convert(from = old)
    )),
    collapse = " "
  )

  expect_match(shown, "Copy the old setup in")
  expect_match(shown, "own export")
  expect_match(shown, "fresh copy of the empty setup")
})

test_that("a recipe holding a conversion is turned down before it runs", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  local_run_machine()
  local_waiting_type("convert-export")

  expect_error(
    convert_recipe(folder) |>
      obt_setup_convert(from = old) |>
      obt_commit(verbose = FALSE),
    "wait"
  )
})

test_that("the copy keeps the name the user knows the file by", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  answer <- run_convert_add(
    recipe,
    operation_at(recipe, 2),
    stage = NULL,
    state = list()
  )

  kept <- file.path(obt_paths(recipe)$setup_source, "old-setup.xlsb")

  expect_true(file.exists(kept))
  expect_true(file.exists(old))
  expect_identical(unname(answer$produced[["source"]]), kept)
  expect_identical(answer$state$convert_source, kept)
})

test_that("the copy of an .xlsx is what the import will read", {
  folder <- withr::local_tempdir()
  old <- empty_file(folder, "old-setup.xlsx")
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  answer <- run_convert_add(
    recipe,
    operation_at(recipe, 2),
    stage = NULL,
    state = list()
  )

  expect_identical(answer$state$converted, answer$state$convert_source)
})

test_that("the copy of an .xlsb leaves the import nothing to read yet", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  answer <- run_convert_add(
    recipe,
    operation_at(recipe, 2),
    stage = NULL,
    state = list()
  )

  expect_null(answer$state$converted)
})

test_that("a file that moved between the verb and the run stops the copy", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)
  unlink(old)

  expect_error(
    run_convert_add(recipe, operation_at(recipe, 2), NULL, list()),
    "the setup to convert"
  )
})

test_that("the export runs on the copy and writes into the export folder", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  paths <- obt_paths(recipe)
  source <- file.path(paths$setup_source, "old-setup.xlsb")
  written <- empty_file(paths$export, "old-setup.xlsx")
  seen <- NULL

  local_mocked_bindings(
    driver_setup_export = function(setup, to, ...) {
      seen <<- list(setup = setup, to = to)
      export_answer(written)
    }
  )

  answer <- run_convert_export(
    recipe,
    operation_at(recipe, 3),
    stage = NULL,
    state = list(convert_source = source)
  )

  expect_identical(seen$setup, source)
  expect_identical(seen$to, paths$export)
  expect_identical(answer$state$converted, written)
  expect_identical(unname(answer$produced[["exported"]]), written)
})

test_that("the export creates the folder it writes into", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)
  paths <- obt_paths(recipe)

  local_mocked_bindings(
    driver_setup_export = function(setup, to, ...) {
      expect_true(dir.exists(to))
      export_answer(empty_file(to, "old-setup.xlsx"))
    }
  )

  run_convert_export(
    recipe,
    operation_at(recipe, 3),
    stage = NULL,
    state = list(convert_source = old)
  )

  expect_true(dir.exists(paths$export))
})

test_that("an export that names no file stops the run", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  local_mocked_bindings(
    driver_setup_export = function(setup, to, ...) export_answer()
  )

  expect_error(
    run_convert_export(
      recipe,
      operation_at(recipe, 3),
      NULL,
      list(convert_source = old)
    ),
    "named no file"
  )
})

test_that("an export naming a file nobody wrote stops the run", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  local_mocked_bindings(
    driver_setup_export = function(setup, to, ...) {
      export_answer(file.path(folder, "export", "gone.xlsx"))
    }
  )

  expect_error(
    run_convert_export(
      recipe,
      operation_at(recipe, 3),
      NULL,
      list(convert_source = old)
    ),
    "Nothing sits at"
  )
})

test_that("the export stops when no copy was made", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  expect_error(
    run_convert_export(recipe, operation_at(recipe, 3), NULL, list()),
    "no setup to convert"
  )
})

test_that("the import fills a fresh copy of the empty setup", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  paths <- obt_paths(recipe)
  template <- empty_file(paths$obt_dev, "setup_dev-2.0.0.xlsb")
  writeLines("the empty setup", template)
  written <- empty_file(paths$export, "old-setup.xlsx")
  seen <- NULL

  local_mocked_bindings(
    driver_setup_import = function(setup, from, ...) {
      seen <<- list(setup = setup, from = from)
      list(produced = character(), state = list())
    }
  )

  answer <- run_convert_import(
    recipe,
    operation_at(recipe, 4),
    stage = NULL,
    state = list(converted = written, empty_setup = template)
  )

  expect_identical(seen$setup, paths$setup_file)
  expect_identical(seen$from, written)
  expect_identical(readLines(paths$setup_file), "the empty setup")
  expect_identical(unname(answer$produced[["setup"]]), paths$setup_file)
  expect_identical(answer$state$setup, paths$setup_file)
})

test_that("a working setup already there stops the import", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)

  paths <- obt_paths(recipe)
  template <- empty_file(paths$obt_dev, "setup_dev-2.0.0.xlsb")
  written <- empty_file(paths$export, "old-setup.xlsx")
  ensure_folder(paths$setup)
  writeLines("the setup already there", paths$setup_file)

  expect_error(
    run_convert_import(
      recipe,
      operation_at(recipe, 4),
      NULL,
      list(converted = written, empty_setup = template)
    ),
    "is already there"
  )
  expect_identical(readLines(paths$setup_file), "the setup already there")
})

test_that("overwrite lets the import put a fresh copy over it", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |>
    obt_setup_convert(from = old, overwrite = TRUE)

  paths <- obt_paths(recipe)
  template <- empty_file(paths$obt_dev, "setup_dev-2.0.0.xlsb")
  writeLines("the empty setup", template)
  written <- empty_file(paths$export, "old-setup.xlsx")
  ensure_folder(paths$setup)
  writeLines("the setup already there", paths$setup_file)

  local_mocked_bindings(
    driver_setup_import = function(setup, from, ...) {
      list(produced = character(), state = list())
    }
  )

  run_convert_import(
    recipe,
    operation_at(recipe, 4),
    NULL,
    list(converted = written, empty_setup = template, overwrite = TRUE)
  )

  expect_identical(readLines(paths$setup_file), "the empty setup")
})

test_that("the import stops when the run holds no empty setup", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)
  written <- empty_file(obt_paths(recipe)$export, "old-setup.xlsx")

  expect_error(
    run_convert_import(
      recipe,
      operation_at(recipe, 4),
      NULL,
      list(converted = written)
    ),
    "no empty setup"
  )
})

test_that("the import stops when nothing was written out", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)
  template <- empty_file(obt_paths(recipe)$obt_dev, "setup_dev-2.0.0.xlsb")

  expect_error(
    run_convert_import(
      recipe,
      operation_at(recipe, 4),
      NULL,
      list(empty_setup = template)
    ),
    "no .*xlsx.* file to read"
  )
})

test_that("the import stops when the file written out has gone", {
  folder <- withr::local_tempdir()
  old <- old_setup(folder)
  recipe <- convert_recipe(folder) |> obt_setup_convert(from = old)
  template <- empty_file(obt_paths(recipe)$obt_dev, "setup_dev-2.0.0.xlsb")

  expect_error(
    run_convert_import(
      recipe,
      operation_at(recipe, 4),
      NULL,
      list(
        converted = file.path(folder, "export", "gone.xlsx"),
        empty_setup = template
      )
    ),
    "Nothing sits at"
  )
})

test_that("every step of a conversion has a runner", {
  for (type in c("convert-add", "convert-export", "convert-import")) {
    expect_true(is.function(operation_runner(type)))
  }
})
