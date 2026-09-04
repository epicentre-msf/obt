test_that("generating records the name and the overwrite setting", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_generate(name = "measles-2026")

  operation <- recipe$operations[[1]]

  expect_identical(operation$type, "designer-generate")
  expect_identical(
    operation$args,
    list(
      name = "measles-2026",
      overwrite = FALSE,
      password = NULL,
      debug_password = NULL
    )
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

test_that("the two passwords of the linelist are recorded as they stand", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_generate(
      name = "measles-2026",
      password = " open pw ",
      debug_password = "debug pw"
    )

  args <- recipe$operations[[1]]$args

  expect_identical(args$password, " open pw ")
  expect_identical(args$debug_password, "debug pw")
})

test_that("a linelist takes no password unless one is named", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_generate(name = "measles-2026")

  args <- recipe$operations[[1]]$args

  expect_true("password" %in% names(args))
  expect_true("debug_password" %in% names(args))
  expect_null(args$password)
  expect_null(args$debug_password)
})

test_that("a password that is not one string is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    obt_designer_generate(recipe, name = "measles", password = 1),
    "must be a single string"
  )
  expect_error(
    obt_designer_generate(recipe, name = "measles", password = ""),
    "must carry a value"
  )
  expect_error(
    obt_designer_generate(
      recipe,
      name = "measles",
      debug_password = c("a", "b")
    ),
    "must be a single string"
  )
  expect_error(
    obt_designer_generate(
      recipe,
      name = "measles",
      debug_password = NA_character_
    ),
    "must be a single string"
  )
})

test_that("both passwords are hidden wherever a recipe is printed", {
  said <- printed(
    obt(folder = "/tmp/measles") |>
      obt_designer_generate(
        name = "measles-2026",
        password = "open-sesame",
        debug_password = "debug-sesame"
      ) |>
      obt_describe()
  )

  expect_false(grepl("open-sesame", said, fixed = TRUE))
  expect_false(grepl("debug-sesame", said, fixed = TRUE))
  expect_match(said, "<hidden>", fixed = TRUE)
})

test_that("a password that was never set prints as unset", {
  said <- printed(
    obt(folder = "/tmp/measles") |>
      obt_designer_generate(name = "measles-2026") |>
      obt_describe()
  )

  expect_false(grepl("<hidden>", said, fixed = TRUE))
})
