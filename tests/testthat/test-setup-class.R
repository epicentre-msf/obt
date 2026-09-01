test_that("a setup workbook builds a setup recipe", {
  folder <- withr::local_tempdir()

  recipe <- setup_recipe(folder)

  expect_true(is_obt_setup(recipe))
  expect_true(is_obt(recipe))
  expect_identical(class(recipe), c("obt_setup", "obt"))
  expect_identical(recipe$folder, path.expand(folder))
})

test_that("the recipe records the setup it was given", {
  folder <- withr::local_tempdir()

  recipe <- setup_recipe(folder)
  operation <- recipe$operations[[1]]

  expect_length(recipe$operations, 1)
  expect_identical(operation$type, "setup-add")
  expect_identical(operation$args$from, setup_workbook())
  expect_false(operation$args$overwrite)
  expect_false(operation$waiting)
})

test_that("a setup inside the working folder records relative to it", {
  folder <- withr::local_tempdir()
  inside <- file.path(folder, "handed-over.xlsb")
  file.copy(setup_workbook(), inside)

  recipe <- obt_setup(from = inside, folder = folder)

  expect_identical(recipe$operations[[1]]$args$from, "handed-over.xlsb")
})

test_that("a setup that is not there is refused at the verb", {
  folder <- withr::local_tempdir()

  expect_error(
    obt_setup(from = file.path(folder, "missing.xlsb"), folder = folder),
    "Nothing sits at"
  )
})

test_that("a folder handed over as the setup is refused", {
  folder <- withr::local_tempdir()
  ensure_folder(file.path(folder, "setup.xlsb"))

  expect_error(
    obt_setup(from = file.path(folder, "setup.xlsb"), folder = folder),
    "A folder sits at"
  )
})

test_that("an .xlsx is refused and the message says what carries the code", {
  folder <- withr::local_tempdir()
  path <- empty_file(folder, "content.xlsx")

  expect_error(obt_setup(from = path, folder = folder), "setup workbook")
  expect_error(obt_setup(from = path, folder = folder), "carries the code")
})

test_that("any other extension is refused", {
  folder <- withr::local_tempdir()
  path <- empty_file(folder, "content.csv")

  expect_error(obt_setup(from = path, folder = folder), "\\.xlsb")
})

test_that("a setup workbook with no folder is refused", {
  expect_error(obt_setup(from = setup_workbook()), "working folder")
})

test_that("the overwrite flag is checked at the verb", {
  folder <- withr::local_tempdir()

  expect_error(
    setup_recipe(folder, overwrite = "yes"),
    "must be .*TRUE.* or .*FALSE"
  )
})

test_that("an obt recipe narrows to a setup one", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_add(type = "dev") |>
    add_operation("setup-tags") |>
    obt_designer_generate(name = "measles-2026")

  narrowed <- obt_setup(recipe)

  expect_true(is_obt_setup(narrowed))
  expect_identical(narrowed$folder, "/tmp/measles")
  expect_identical(
    vapply(narrowed$operations, function(x) x$type, character(1)),
    "setup-tags"
  )
})

test_that("narrowing keeps the setup operations in the order they came in", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("setup-tags") |>
    obt_designer_add(type = "dev") |>
    add_operation("setup-export", list(to = "export"))

  narrowed <- obt_setup(recipe)

  expect_identical(
    vapply(narrowed$operations, function(x) x$type, character(1)),
    c("setup-tags", "setup-export")
  )
})

test_that("narrowing leaves the recipe it came from as it was", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_add(type = "dev") |>
    add_operation("setup-tags")

  obt_setup(recipe)

  expect_false(is_obt_setup(recipe))
  expect_length(recipe$operations, 2)
})

test_that("narrowing a setup recipe answers a setup recipe", {
  folder <- withr::local_tempdir()
  recipe <- setup_recipe(folder)

  expect_identical(obt_setup(recipe), recipe)
})

test_that("a narrowed recipe carries no result of the run it came from", {
  recipe <- obt(folder = "/tmp/measles") |> add_operation("setup-tags")
  recipe$operations[[1]]$result <- list(outcome = "ok", seconds = 1)
  recipe$run <- list(outcome = "ok")

  narrowed <- obt_setup(recipe)

  expect_null(narrowed$operations[[1]]$result)
  expect_null(narrowed$run)
})

test_that("narrowing takes the recipe on its own", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_setup(recipe, folder = "/tmp/other"), "narrowed on its own")
  expect_error(obt_setup(recipe, folder = "/tmp/other"), "folder")
  expect_error(obt_setup(recipe, overwrite = TRUE), "overwrite")
})

test_that("a value that is neither a recipe nor a path is refused", {
  expect_error(obt_setup(from = 1), "must be a single string")
})

test_that("the run copies the setup in twice, and keeps the name", {
  folder <- withr::local_tempdir()
  recipe <- setup_recipe(folder)
  paths <- obt_paths(recipe)

  answer <- run_setup_add(
    recipe,
    recipe$operations[[1]],
    stage = NULL,
    state = list()
  )

  expect_true(file.exists(paths$setup_file))
  expect_true(
    file.exists(file.path(paths$setup_source, basename(setup_workbook())))
  )
  expect_identical(names(answer$produced), c("source", "setup"))
  expect_identical(answer$state$setup, paths$setup_file)
})

test_that("the file the user handed over is left as it was", {
  folder <- withr::local_tempdir()
  before <- file.info(setup_workbook())$mtime

  recipe <- setup_recipe(folder)
  run_setup_add(recipe, recipe$operations[[1]], stage = NULL, state = list())

  expect_identical(file.info(setup_workbook())$mtime, before)
})

test_that("a working setup already there stops the run", {
  folder <- withr::local_tempdir()
  recipe <- setup_recipe(folder)
  operation <- recipe$operations[[1]]

  run_setup_add(recipe, operation, stage = NULL, state = list())

  expect_error(
    run_setup_add(recipe, operation, stage = NULL, state = list()),
    "already there"
  )
})

test_that("overwrite writes over the working setup", {
  folder <- withr::local_tempdir()
  recipe <- setup_recipe(folder)
  run_setup_add(
    recipe,
    recipe$operations[[1]],
    stage = NULL,
    state = list()
  )

  again <- setup_recipe(folder, overwrite = TRUE)

  expect_no_error(
    run_setup_add(again, again$operations[[1]], stage = NULL, state = list())
  )
})

test_that("a setup that moved between the verb and the run stops the run", {
  folder <- withr::local_tempdir()
  handed <- file.path(folder, "handed-over.xlsb")
  file.copy(setup_workbook(), handed)

  recipe <- obt_setup(from = handed, folder = withr::local_tempdir())
  file.remove(handed)

  expect_error(
    run_setup_add(recipe, recipe$operations[[1]], stage = NULL, state = list()),
    "Nothing sits at"
  )
})

test_that("a setup recipe prints under its own name", {
  folder <- withr::local_tempdir()
  recipe <- setup_recipe(folder)

  expect_match(printed(obt_summary(recipe)), "OBT setup recipe")
  expect_match(printed(obt_describe(recipe)), "OBT setup recipe")
  expect_match(printed(print(recipe)), "OBT setup recipe")
})

test_that("a setup description shows the setup and leaves the rest out", {
  folder <- withr::local_tempdir()

  out <- printed(obt_describe(setup_recipe(folder)))

  expect_match(out, "Setup")
  expect_match(out, basename(setup_workbook()), fixed = TRUE)
  expect_false(grepl("Channel", out))
  expect_false(grepl("Output name", out))
})

test_that("an obt description keeps the rows it always had", {
  out <- printed(obt_describe(mixed_recipe("/tmp/measles")))

  expect_match(out, "Channel")
  expect_match(out, "Output name")
})

test_that("the copy of the setup is what a run of the recipe writes", {
  folder <- withr::local_tempdir()
  local_run_machine()

  answer <- setup_recipe(folder) |> obt_commit(verbose = FALSE)
  paths <- obt_paths(answer)

  expect_true(file.exists(paths$setup_file))
  expect_identical(answer$run$outcome, RUN_OK)
  expect_identical(
    unname(answer$operations[[1]]$result$produced[["setup"]]),
    "setup/setup.xlsb"
  )
})
