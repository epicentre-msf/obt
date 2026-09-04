test_that("a linelist workbook builds a linelist recipe", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  recipe <- obt_linelist(from = linelist, folder = folder)

  expect_true(is_obt_linelist(recipe))
  expect_true(is_obt(recipe))
  expect_identical(class(recipe), c("obt_linelist", "obt"))
  expect_identical(recipe$folder, path.expand(folder))
})

test_that("the recipe records the linelist it was given", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  recipe <- obt_linelist(from = linelist, folder = folder)
  operation <- recipe$operations[[1]]

  expect_length(recipe$operations, 1)
  expect_identical(operation$type, "linelist-add")
  expect_identical(operation$args$from, "linelist/measles-2026.xlsb")
  expect_false(operation$args$overwrite)
  expect_null(operation$args$password)
  expect_false(operation$waiting)
})

test_that("a linelist outside the working folder keeps its own path", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  linelist <- empty_file(elsewhere, "measles-2026.xlsb")

  recipe <- obt_linelist(from = linelist, folder = folder)

  expect_identical(recipe$operations[[1]]$args$from, linelist)
})

test_that("a linelist that is not there is refused at the verb", {
  folder <- withr::local_tempdir()

  expect_error(
    obt_linelist(from = file.path(folder, "missing.xlsb"), folder = folder),
    "Nothing sits at"
  )
})

test_that("any other extension is refused", {
  folder <- withr::local_tempdir()
  path <- empty_file(folder, "measles-2026.xlsx")

  expect_error(obt_linelist(from = path, folder = folder), "\\.xlsb")
  expect_error(obt_linelist(from = path, folder = folder), "linelist workbook")
})

test_that("a linelist workbook with no folder is refused", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  expect_error(obt_linelist(from = linelist), "working folder")
})

test_that("the overwrite flag is checked at the verb", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  expect_error(
    obt_linelist(from = linelist, folder = folder, overwrite = "yes"),
    "must be .*TRUE.* or .*FALSE"
  )
})

test_that("an obt recipe narrows to a linelist one", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_add(type = "dev") |>
    add_operation("linelist-import", list(from = "cases.xlsx")) |>
    obt_designer_generate(name = "measles-2026")

  narrowed <- obt_linelist(recipe)

  expect_true(is_obt_linelist(narrowed))
  expect_identical(narrowed$folder, "/tmp/measles")
  expect_identical(
    vapply(narrowed$operations, function(x) x$type, character(1)),
    "linelist-import"
  )
})

test_that("narrowing keeps the linelist operations in the order they came", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("linelist-geobase", list(path = "geo.xlsx")) |>
    add_operation("linelist-export", list(type = "migration")) |>
    add_operation("linelist-import", list(from = "cases.xlsx"))

  narrowed <- obt_linelist(recipe)

  expect_identical(
    vapply(narrowed$operations, function(x) x$type, character(1)),
    c("linelist-geobase", "linelist-export", "linelist-import")
  )
})

test_that("narrowing leaves the recipe it came from as it was", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_add(type = "dev") |>
    add_operation("linelist-import", list(from = "cases.xlsx"))

  obt_linelist(recipe)

  expect_length(recipe$operations, 2)
  expect_false(is_obt_linelist(recipe))
})

test_that("a narrowing takes no other argument", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_linelist(recipe, folder = "/tmp/other"), "on its own")
  expect_error(obt_linelist(recipe, overwrite = TRUE), "on its own")
})

test_that("a setup recipe does not narrow to a linelist one", {
  folder <- withr::local_tempdir()

  expect_error(
    obt_linelist(setup_recipe(folder)),
    "does not narrow"
  )
})

test_that("a linelist recipe does not narrow to a setup one", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  expect_error(
    obt_setup(obt_linelist(from = linelist, folder = folder)),
    "does not narrow"
  )
})

test_that("the result of an earlier run is dropped by a narrowing", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("linelist-import", list(from = "cases.xlsx"))

  recipe$operations[[1]]$result <- list(outcome = RUN_OK)

  narrowed <- obt_linelist(recipe)

  expect_null(narrowed$operations[[1]]$result)
})

test_that("is_obt_linelist tells the classes apart", {
  folder <- withr::local_tempdir()

  expect_true(is_obt_linelist(obt_linelist(obt(folder = folder))))
  expect_false(is_obt_linelist(obt(folder = folder)))
  expect_false(is_obt_linelist(setup_recipe(folder)))
  expect_false(is_obt_linelist("a folder path"))
})

# The verbs each class holds -----------------------------------------------

test_that("a linelist verb refuses a plain obt recipe", {
  folder <- withr::local_tempdir()
  recipe <- obt(folder = folder)

  expect_error(
    obt_linelist_geobase(recipe, path = geobase_file(folder)),
    "obt_linelist"
  )
  expect_error(
    obt_linelist_import(recipe, from = migration_file(folder)),
    "Narrow it with"
  )
  expect_error(obt_linelist_export(recipe), "Narrow it with")
})

test_that("a linelist verb refuses anything that is not a recipe", {
  expect_error(obt_linelist_export("recipe"), "Start one with")
})

test_that("a designer verb refuses a narrowed recipe", {
  folder <- withr::local_tempdir()
  linelist <- obt_linelist(obt(folder = folder))

  expect_error(
    obt_designer_generate(linelist, name = "measles-2026"),
    "holds one workbook on its own"
  )
  expect_error(obt_designer_add(linelist, type = "dev"), "obt_designer_add")
  expect_error(
    obt_designer_languages(linelist, form = "ENG"),
    "must be an"
  )
  expect_error(
    obt_designer_geobase(linelist, path = geobase_file(folder)),
    "must be an"
  )
})

test_that("a designer verb refuses a setup recipe too", {
  folder <- withr::local_tempdir()

  expect_error(
    obt_designer_generate(setup_recipe(folder), name = "measles-2026"),
    "obt_setup"
  )
})

# What the run does with the linelist it was handed -------------------------

test_that("the run copies the linelist into the working folder", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  from <- empty_file(elsewhere, "measles-2026.xlsb")

  recipe <- obt_linelist(from = from, folder = folder)
  answer <- run_linelist_add(
    recipe,
    recipe$operations[[1]],
    stage = NULL,
    state = list()
  )

  kept <- file.path(obt_paths(recipe)$linelist, "measles-2026.xlsb")

  expect_true(file.exists(kept))
  expect_true(file.exists(from))
  expect_identical(unname(answer$produced[["linelist"]]), kept)
  expect_identical(answer$state$linelist, kept)
})

test_that("the run hands the copy on as the linelist to work on", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  from <- empty_file(elsewhere, "measles-2026.xlsb")

  recipe <- obt_linelist(from = from, folder = folder)
  answer <- run_linelist_add(
    recipe,
    recipe$operations[[1]],
    stage = NULL,
    state = list()
  )

  expect_identical(linelist_target(recipe, answer$state), answer$state$linelist)
})

test_that("a linelist already in the folder is refused without overwrite", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  from <- empty_file(elsewhere, "measles-2026.xlsb")
  built_linelist(folder)

  recipe <- obt_linelist(from = from, folder = folder)

  expect_error(
    run_linelist_add(
      recipe,
      recipe$operations[[1]],
      stage = NULL,
      state = list()
    ),
    "overwrite"
  )
})

test_that("a linelist already in the folder is replaced with overwrite", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  from <- empty_file(elsewhere, "measles-2026.xlsb")
  writeLines("the new one", from)
  built_linelist(folder)

  recipe <- obt_linelist(from = from, folder = folder, overwrite = TRUE)
  answer <- run_linelist_add(
    recipe,
    recipe$operations[[1]],
    stage = NULL,
    state = list()
  )

  expect_identical(readLines(answer$state$linelist), "the new one")
})

test_that("a linelist that moved between the verb and the run is reported", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  from <- empty_file(elsewhere, "measles-2026.xlsb")

  recipe <- obt_linelist(from = from, folder = folder)
  unlink(from)

  expect_error(
    run_linelist_add(
      recipe,
      recipe$operations[[1]],
      stage = NULL,
      state = list()
    ),
    "the linelist it was given"
  )
})

test_that("every linelist operation type answers a runner", {
  for (type in LINELIST_OPERATION_TYPES) {
    expect_true(is.function(operation_runner(type)), info = type)
  }
})

# How a linelist recipe prints ---------------------------------------------

test_that("a linelist recipe prints under its own title", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  said <- printed(
    obt_linelist(from = linelist, folder = folder) |> obt_describe()
  )

  expect_match(said, "OBT linelist recipe")
})

test_that("the description shows the linelist rows", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  said <- printed(
    obt_linelist(from = linelist, folder = folder) |>
      obt_linelist_import(from = migration_file(folder)) |>
      obt_describe()
  )

  expect_match(said, "Linelist")
  expect_match(said, "measles-2026.xlsb")
  expect_match(said, "Read in from")
  expect_match(said, "cases-from-the-field.xlsx")
})

test_that("a narrowed recipe names no linelist of its own", {
  folder <- withr::local_tempdir()

  said <- printed(linelist_recipe(folder) |> obt_describe())

  expect_match(said, "Linelist")
  expect_match(said, UNSET_MARK, fixed = TRUE)
})

# The password a linelist opens with ---------------------------------------

test_that("the password the linelist opens with is recorded with it", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  recipe <- obt_linelist(from = linelist, folder = folder, password = " pw ")

  expect_identical(recipe$operations[[1]]$args$password, " pw ")
})

test_that("a linelist takes no password unless one is named", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  recipe <- obt_linelist(from = linelist, folder = folder)
  args <- recipe$operations[[1]]$args

  expect_true("password" %in% names(args))
  expect_null(args$password)
})

test_that("a password that is not one string is refused at the verb", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  expect_error(
    obt_linelist(from = linelist, folder = folder, password = ""),
    "must carry a value"
  )
  expect_error(
    obt_linelist(from = linelist, folder = folder, password = 1),
    "must be a single string"
  )
})

test_that("the password is hidden wherever a linelist recipe is printed", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  said <- printed(
    obt_linelist(from = linelist, folder = folder, password = "open-sesame") |>
      obt_describe()
  )

  expect_false(grepl("open-sesame", said, fixed = TRUE))
  expect_match(said, "<hidden>", fixed = TRUE)
})

test_that("a narrowing takes no password", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_linelist(recipe, password = "pw"), "on its own")
})

test_that("the run hands the password on with the linelist", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  from <- empty_file(elsewhere, "measles-2026.xlsb")

  recipe <- obt_linelist(from = from, folder = folder, password = "open-pw")
  answer <- run_linelist_add(
    recipe,
    recipe$operations[[1]],
    stage = NULL,
    state = list()
  )

  expect_identical(answer$state$linelist_password, "open-pw")
})

test_that("a linelist added with no password hands none on", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  from <- empty_file(elsewhere, "measles-2026.xlsb")

  recipe <- obt_linelist(from = from, folder = folder)
  answer <- run_linelist_add(
    recipe,
    recipe$operations[[1]],
    stage = NULL,
    state = list()
  )

  expect_true(is.na(answer$state$linelist_password))
})

test_that("the password a run opens the linelist with comes from the state", {
  expect_identical(linelist_password(list(linelist_password = "pw")), "pw")
  expect_true(is.na(linelist_password(list())))
  expect_true(is.na(linelist_password(list(linelist_password = ""))))
  expect_true(is.na(linelist_password(list(linelist_password = NULL))))
})
