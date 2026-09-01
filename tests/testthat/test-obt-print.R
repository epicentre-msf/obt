test_that("the summary names the folder, the operations and what waits", {
  out <- printed(obt_summary(mixed_recipe("/tmp/measles")))

  expect_match(out, "OBT recipe")
  expect_match(out, "/tmp/measles", fixed = TRUE)
  expect_match(out, "3 operations")
  expect_match(out, "Add the designer")
  expect_match(out, "Generate the linelist")
  expect_match(out, "Waiting on the workbooks: 1 of 3")
})

test_that("a recipe every operation of which can run says so", {
  recipe <- obt(folder = "/tmp/measles") |> add_operation("designer-add")

  out <- printed(obt_summary(recipe))

  expect_match(out, "Every operation can run")
  expect_match(out, "obt_commit()", fixed = TRUE)
})

test_that("an empty recipe says it holds nothing", {
  recipe <- obt(folder = "/tmp/measles")

  expect_match(printed(obt_summary(recipe)), "No operation recorded yet")
  expect_match(printed(obt_operations(recipe)), "holds none yet")
  expect_match(printed(obt_describe(recipe)), "holds none yet")
})

test_that("printing a recipe prints its summary", {
  recipe <- mixed_recipe("/tmp/measles")

  expect_identical(printed(print(recipe)), printed(obt_summary(recipe)))
})

test_that("the operations print numbered, in run order", {
  out <- printed(obt_operations(mixed_recipe("/tmp/measles")))

  expect_match(out, "Operations")
  expect_match(out, "1. Download the OutbreakTools files", fixed = TRUE)
  expect_match(out, "2. Carry the setup and interface languages", fixed = TRUE)
  expect_match(out, "3. Fill the designer's Main sheet", fixed = TRUE)
})

test_that("an operation the workbooks cannot run yet prints as waiting", {
  out <- printed(obt_operations(mixed_recipe("/tmp/measles")))

  expect_match(out, "waiting on the workbooks")
  expect_length(gregexpr("waiting on the workbooks", out)[[1]], 1)
})

test_that("the description says what a waiting operation waits for", {
  out <- printed(obt_describe(mixed_recipe("/tmp/measles")))

  expect_match(out, "Needs")
  expect_match(out, "silence, and a summary a script can read")
})

test_that("the description shows the values the run will use", {
  out <- printed(obt_describe(mixed_recipe("/tmp/measles")))

  expect_match(out, "Working folder")
  expect_match(out, "Channel")
  expect_match(out, "\"dev\"", fixed = TRUE)
  expect_match(out, "Setup language")
  expect_match(out, "\"English\"", fixed = TRUE)
  expect_match(out, "Interface language")
  expect_match(out, "Output name")
  expect_match(out, "\"measles-2026\"", fixed = TRUE)
})

test_that("a value the recipe never set prints as not set", {
  out <- printed(obt_describe(mixed_recipe("/tmp/measles")))

  expect_match(out, "Geobase")
  expect_match(out, "(not set)", fixed = TRUE)
})

test_that("the description shows every argument of every operation", {
  out <- printed(obt_describe(mixed_recipe("/tmp/measles")))

  expect_match(out, "overwrite")
  expect_match(out, "FALSE")
  expect_match(out, "dict")
  expect_match(out, "form")
})

test_that("the last value recorded is the one the description shows", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("designer-add", list(type = "main")) |>
    add_operation("designer-add", list(type = "dev"))

  out <- printed(obt_describe(recipe))

  expect_match(out, "Channel.*\"dev\"")
})

test_that("a password is never printed", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation(
      "linelist-export",
      list(type = "migration", password = "open-sesame")
    )

  out <- printed(obt_describe(recipe))

  expect_false(grepl("open-sesame", out, fixed = TRUE))
  expect_match(out, "<hidden>", fixed = TRUE)
})

test_that("a brace in a value is printed as it stands", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_generate(name = "measles-{2026}")

  out <- printed(obt_describe(recipe))

  expect_match(out, "measles-{2026}", fixed = TRUE)
})

test_that("all three printers answer the recipe unchanged and invisibly", {
  recipe <- mixed_recipe("/tmp/measles")

  for (printer in list(obt_summary, obt_describe, obt_operations)) {
    printed(expect_invisible(answer <- printer(recipe)))
    expect_identical(answer, recipe)
  }
})

test_that("a printer given something that is not a recipe says so", {
  expect_error(obt_summary("/tmp/measles"), "must be an")
  expect_error(obt_describe("/tmp/measles"), "must be an")
  expect_error(obt_operations("/tmp/measles"), "must be an")
})
