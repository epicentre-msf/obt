test_that("a new recipe holds the folder and no operation", {
  recipe <- obt(folder = "/tmp/measles")

  expect_true(is_obt(recipe))
  expect_identical(recipe$folder, "/tmp/measles")
  expect_identical(recipe$operations, list())
})

test_that("building a recipe touches no disk", {
  folder <- file.path(tempdir(), "obt-untouched")

  obt(folder = folder) |>
    obt_designer_generate(name = "measles-2026")

  expect_false(dir.exists(folder))
  expect_false(file.exists(folder))
})

test_that("a tilde in the folder is expanded once, at the start", {
  recipe <- obt(folder = "~/measles")

  expect_identical(recipe$folder, path.expand("~/measles"))
  expect_false(startsWith(recipe$folder, "~"))
})

test_that("surrounding space in the folder is dropped", {
  expect_identical(obt(folder = "  /tmp/measles  ")$folder, "/tmp/measles")
})

test_that("a folder that is not one string is refused", {
  expect_error(obt(folder = 1), "single string")
  expect_error(obt(folder = c("a", "b")), "single string")
  expect_error(obt(folder = NA_character_), "single string")
  expect_error(obt(folder = ""), "must carry a value")
})

test_that("is_obt answers FALSE for anything else", {
  expect_false(is_obt("a folder path"))
  expect_false(is_obt(list(folder = "x", operations = list())))
})

test_that("a verb given something that is not a recipe says so", {
  expect_error(
    obt_designer_generate("a folder path", name = "measles"),
    "must be an"
  )
})
