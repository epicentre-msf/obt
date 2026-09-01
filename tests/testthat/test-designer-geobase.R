test_that("the verb records the geobase it was given", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)

  recipe <- obt(folder = folder) |> obt_designer_geobase(path = geo)
  operation <- recipe$operations[[1]]

  expect_identical(operation$type, "designer-geobase")
  expect_identical(operation$args$path, "geobase.xlsx")
  expect_false(operation$waiting)
})

test_that("a geobase outside the working folder records as it stands", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  geo <- geobase_file(elsewhere)

  recipe <- obt(folder = folder) |> obt_designer_geobase(path = geo)

  expect_identical(recipe$operations[[1]]$args$path, geo)
})

test_that("the verb answers the recipe, so it sits in a chain", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)

  recipe <- obt(folder = folder) |>
    obt_designer_geobase(path = geo) |>
    obt_designer_generate(name = "measles-2026")

  types <- vapply(recipe$operations, function(op) op$type, character(1))

  expect_identical(types, c("designer-geobase", "designer-generate"))
})

test_that("a file the verb cannot find is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    obt(folder = folder) |>
      obt_designer_geobase(path = file.path(folder, "missing.xlsx")),
    "Nothing sits at"
  )
})

test_that("a folder handed as a geobase is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    obt(folder = folder) |> obt_designer_geobase(path = folder),
    "A folder sits at"
  )
})

test_that("the path is checked at the verb", {
  folder <- withr::local_tempdir()

  expect_error(
    obt(folder = folder) |> obt_designer_geobase(path = 1),
    "must be a single string"
  )
  expect_error(
    obt(folder = folder) |> obt_designer_geobase(path = c("a.xlsx", "b.xlsx")),
    "must be a single string"
  )
})

test_that("anything other than a recipe is refused", {
  expect_error(obt_designer_geobase("recipe", path = "geo.xlsx"), "must be an")
})

test_that("the verb creates nothing", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  geo <- geobase_file(elsewhere)

  obt(folder = folder) |> obt_designer_geobase(path = geo)

  expect_identical(list.files(folder), character())
})

test_that("any extension is taken, because the web app names the file", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder, "geobase.xlsb")

  recipe <- obt(folder = folder) |> obt_designer_geobase(path = geo)

  expect_identical(recipe$operations[[1]]$args$path, "geobase.xlsb")
})

test_that("the run copies the geobase into geo/ and keeps its name", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  geo <- geobase_file(elsewhere)
  recipe <- obt(folder = folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  answer <- run_designer_geobase(
    recipe,
    new_operation("designer-geobase", list(path = geo)),
    stage = stage,
    state = list()
  )

  kept <- file.path(obt_paths(recipe)$geo, "geobase.xlsx")

  expect_true(file.exists(kept))
  expect_identical(unname(answer$produced[["geobase"]]), kept)
})

test_that("the run leaves the user's own file where it was", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  geo <- geobase_file(elsewhere)
  recipe <- obt(folder = folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  run_designer_geobase(
    recipe,
    new_operation("designer-geobase", list(path = geo)),
    stage = stage,
    state = list()
  )

  expect_true(file.exists(geo))
})

test_that("the run hands the copy on, so the generation reads it", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)
  recipe <- obt(folder = folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  answer <- run_designer_geobase(
    recipe,
    new_operation("designer-geobase", list(path = "geobase.xlsx")),
    stage = stage,
    state = list()
  )

  expect_identical(
    answer$state$geobase,
    file.path(obt_paths(recipe)$geo, "geobase.xlsx")
  )
})

test_that("a geobase already sitting in geo/ is left as it is", {
  folder <- withr::local_tempdir()
  recipe <- obt(folder = folder)
  paths <- obt_paths(recipe)

  ensure_folder(paths$geo)
  geo <- file.path(paths$geo, "geobase.xlsx")
  writeLines("geobase", geo)

  stage <- test_stage(folder, folder, staged = FALSE)

  answer <- run_designer_geobase(
    recipe,
    new_operation("designer-geobase", list(path = "geo/geobase.xlsx")),
    stage = stage,
    state = list()
  )

  expect_identical(answer$state$geobase, geo)
  expect_identical(readLines(geo), "geobase")
})

test_that("a geobase moved between the verb and the run stops the run", {
  folder <- withr::local_tempdir()
  recipe <- obt(folder = folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  expect_error(
    run_designer_geobase(
      recipe,
      new_operation("designer-geobase", list(path = "gone.xlsx")),
      stage = stage,
      state = list()
    ),
    "the geobase it was given"
  )
})

test_that("the geobase a run will build with shows in the description", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)

  said <- printed(
    obt(folder = folder) |> obt_designer_geobase(path = geo) |> obt_describe()
  )

  expect_match(said, "Geobase")
  expect_match(said, "geobase.xlsx")
})

test_that("a recipe recording the geobase twice shows the second one", {
  folder <- withr::local_tempdir()
  first <- geobase_file(folder, "old-geobase.xlsx")
  second <- geobase_file(folder, "new-geobase.xlsx")

  said <- printed(
    obt(folder = folder) |>
      obt_designer_geobase(path = first) |>
      obt_designer_geobase(path = second) |>
      obt_describe()
  )

  expect_match(said, "new-geobase.xlsx")
})
