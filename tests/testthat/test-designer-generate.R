test_that("generating records the name and the overwrite setting", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_generate(name = "measles-2026")

  operation <- recipe$operations[[1]]

  expect_identical(operation$type, "designer-generate")
  expect_identical(
    operation$args,
    list(name = "measles-2026", overwrite = FALSE)
  )
  expect_false(operation$waiting)
})

test_that("overwrite is carried as it was given", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_generate(name = "measles-2026", overwrite = TRUE)

  expect_true(recipe$operations[[1]]$args$overwrite)
})

test_that("a name that is not one string is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_designer_generate(recipe, name = 1), "single string")
  expect_error(obt_designer_generate(recipe, name = ""), "must carry a value")
})

test_that("a name carrying a path is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    obt_designer_generate(recipe, name = "linelist/measles"),
    "file name on its own"
  )
  expect_error(
    obt_designer_generate(recipe, name = "measles?2026"),
    "file name on its own"
  )
})

test_that("a name carrying an extension is refused, and the bare one named", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    obt_designer_generate(recipe, name = "measles-2026.xlsb"),
    "without an extension"
  )
  expect_error(
    obt_designer_generate(recipe, name = "measles-2026.xlsb"),
    "measles-2026"
  )
})

test_that("an overwrite that is not TRUE or FALSE is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    obt_designer_generate(recipe, name = "measles", overwrite = "yes"),
    "must be"
  )
  expect_error(
    obt_designer_generate(recipe, name = "measles", overwrite = NA),
    "must be"
  )
})
