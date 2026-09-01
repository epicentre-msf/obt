test_that("the export records the folder it will write into", {
  folder <- withr::local_tempdir()

  recipe <- setup_recipe(folder) |> obt_setup_export()
  operation <- recipe$operations[[2]]

  expect_identical(operation$type, "setup-export")
  expect_identical(operation$args$to, "export")
  expect_true(operation$waiting)
})

test_that("the export takes a folder of its own", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()

  recipe <- setup_recipe(folder) |> obt_setup_export(to = elsewhere)

  expect_identical(recipe$operations[[2]]$args$to, elsewhere)
})

test_that("an export folder under the working folder records relative", {
  folder <- withr::local_tempdir()

  recipe <- setup_recipe(folder) |>
    obt_setup_export(to = file.path(folder, "export", "setups"))

  expect_identical(recipe$operations[[2]]$args$to, "export/setups")
})

test_that("a file where the export folder goes is refused", {
  folder <- withr::local_tempdir()
  path <- empty_file(folder, "export-here")

  expect_error(
    setup_recipe(folder) |> obt_setup_export(to = path),
    "must name a folder"
  )
})

test_that("the import records the file it will read", {
  folder <- withr::local_tempdir()
  content <- empty_file(folder, "content.xlsx")

  recipe <- setup_recipe(folder) |> obt_setup_import(from = content)
  operation <- recipe$operations[[2]]

  expect_identical(operation$type, "setup-import")
  expect_identical(operation$args$from, "content.xlsx")
  expect_true(operation$waiting)
})

test_that("a file the import cannot find is refused at the verb", {
  folder <- withr::local_tempdir()

  expect_error(
    setup_recipe(folder) |>
      obt_setup_import(from = file.path(folder, "missing.xlsx")),
    "Nothing sits at"
  )
})

test_that("an .xlsb handed to the import is refused, and the route named", {
  folder <- withr::local_tempdir()

  expect_error(
    setup_recipe(folder) |> obt_setup_import(from = setup_workbook()),
    "must end in"
  )
  expect_error(
    setup_recipe(folder) |> obt_setup_import(from = setup_workbook()),
    "takes a click"
  )
})

test_that("any other extension handed to the import is refused", {
  folder <- withr::local_tempdir()
  content <- empty_file(folder, "content.csv")

  expect_error(
    setup_recipe(folder) |> obt_setup_import(from = content),
    "must end in"
  )
})

test_that("the tag verb records one operation with no argument", {
  folder <- withr::local_tempdir()

  recipe <- setup_recipe(folder) |> obt_setup_tags()
  operation <- recipe$operations[[2]]

  expect_identical(operation$type, "setup-tags")
  expect_identical(operation$args, list())
  expect_true(operation$waiting)
})

test_that("the three verbs chain and keep their order", {
  folder <- withr::local_tempdir()
  content <- empty_file(folder, "content.xlsx")

  recipe <- setup_recipe(folder) |>
    obt_setup_import(from = content) |>
    obt_setup_tags() |>
    obt_setup_export()

  expect_identical(
    vapply(recipe$operations, function(x) x$type, character(1)),
    c("setup-add", "setup-import", "setup-tags", "setup-export")
  )
})

test_that("a plain obt recipe is refused, and the message says to narrow", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_setup_export(recipe), "obt_setup")
  expect_error(obt_setup_export(recipe), "Narrow it")
  expect_error(obt_setup_tags(recipe), "Narrow it")
})

test_that("something that is no recipe at all is refused", {
  expect_error(obt_setup_tags("a folder path"), "Start one with")
})

test_that("the three verbs print as waiting on the workbooks", {
  folder <- withr::local_tempdir()

  recipe <- setup_recipe(folder) |>
    obt_setup_tags() |>
    obt_setup_export()

  out <- printed(obt_operations(recipe))

  expect_length(gregexpr("waiting on the workbooks", out)[[1]], 2)
  expect_match(printed(obt_summary(recipe)), "Waiting on the workbooks: 2 of 3")
})

test_that("the description says what each setup verb waits for", {
  folder <- withr::local_tempdir()

  out <- printed(obt_describe(setup_recipe(folder) |> obt_setup_tags()))

  expect_match(out, "Update the setup tags")
  expect_match(out, "an entry point a script can call")
})

test_that("a setup recipe holding a waiting verb is turned down up front", {
  folder <- withr::local_tempdir()
  local_run_machine()

  expect_error(
    setup_recipe(folder) |> obt_setup_tags() |> obt_commit(verbose = FALSE),
    "wait"
  )
  expect_false(dir.exists(obt_paths(setup_recipe(folder))$log))
})

test_that("the export hands the wrapper the setup and the folder", {
  folder <- withr::local_tempdir()
  seen <- driver_recorder()
  local_mocked_bindings(driver_setup_export = seen$record)

  recipe <- setup_recipe(folder) |> obt_setup_export()
  run_setup_add(recipe, recipe$operations[[1]], stage = NULL, state = list())
  run_setup_export(
    recipe,
    recipe$operations[[2]],
    stage = NULL,
    state = list()
  )

  paths <- obt_paths(recipe)

  expect_identical(seen$seen()$setup, paths$setup_file)
  expect_identical(seen$seen()$to, paths$export)
  expect_true(dir.exists(paths$export))
})

test_that("the import hands the wrapper the setup and the file", {
  folder <- withr::local_tempdir()
  content <- empty_file(folder, "content.xlsx")
  seen <- driver_recorder()
  local_mocked_bindings(driver_setup_import = seen$record)

  recipe <- setup_recipe(folder) |> obt_setup_import(from = content)
  run_setup_add(recipe, recipe$operations[[1]], stage = NULL, state = list())
  run_setup_import(
    recipe,
    recipe$operations[[2]],
    stage = NULL,
    state = list()
  )

  expect_identical(seen$seen()$setup, obt_paths(recipe)$setup_file)
  expect_identical(seen$seen()$from, absolute_path(content))
})

test_that("the tag update hands the wrapper the setup", {
  folder <- withr::local_tempdir()
  seen <- driver_recorder()
  local_mocked_bindings(driver_setup_tags = seen$record)

  recipe <- setup_recipe(folder) |> obt_setup_tags()
  run_setup_add(recipe, recipe$operations[[1]], stage = NULL, state = list())
  run_setup_tags(recipe, recipe$operations[[2]], stage = NULL, state = list())

  expect_identical(seen$seen()$setup, obt_paths(recipe)$setup_file)
})

test_that("a working folder holding no setup stops every setup verb", {
  folder <- withr::local_tempdir()
  content <- empty_file(folder, "content.xlsx")

  recipe <- setup_recipe(folder) |>
    obt_setup_import(from = content) |>
    obt_setup_tags() |>
    obt_setup_export()

  for (index in 2:4) {
    runner <- operation_runner(recipe$operations[[index]]$type)
    expect_error(
      runner(recipe, recipe$operations[[index]], stage = NULL, state = list()),
      "holds no setup"
    )
  }
})

test_that("a file the import lost between the verb and the run stops it", {
  folder <- withr::local_tempdir()
  content <- empty_file(folder, "content.xlsx")

  recipe <- setup_recipe(folder) |> obt_setup_import(from = content)
  run_setup_add(recipe, recipe$operations[[1]], stage = NULL, state = list())
  file.remove(content)

  expect_error(
    run_setup_import(
      recipe,
      recipe$operations[[2]],
      stage = NULL,
      state = list()
    ),
    "Nothing sits at"
  )
})

test_that("every setup operation has a runner", {
  for (type in SETUP_OPERATION_TYPES) {
    expect_true(is.function(operation_runner(type)))
  }
})

test_that("the three setup wrappers build their own command lines", {
  runs <- test_recording_call()

  local_mocked_bindings(driver_call = runs$call, os_name = function() "macos")

  driver_setup_export(setup = "/work/setup.xlsb", to = "/work/export")

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("setup-export"),
        "/work/setup.xlsb",
        "/work/export",
        "/work/export/setup-obt-summary.txt"
      ),
      type = "sh"
    )
  )

  driver_setup_import(setup = "/work/setup.xlsb", from = "/work/in.xlsx")

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("setup-import"),
        "/work/setup.xlsb",
        "/work/in.xlsx",
        "/work/in-obt-summary.txt"
      ),
      type = "sh"
    )
  )

  driver_setup_tags(setup = "/work/setup.xlsb")

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("setup-tags"),
        "/work/setup.xlsb",
        "/work/setup-obt-summary.txt"
      ),
      type = "sh"
    )
  )
})
