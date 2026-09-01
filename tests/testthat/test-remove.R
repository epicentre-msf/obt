test_that("a step comes out by its number", {
  recipe <- three_step_recipe()

  left <- obt_remove(recipe, step = 2)

  expect_identical(step_types(left), c("designer-add", "designer-generate"))
})

test_that("a step comes out by its type", {
  recipe <- three_step_recipe()

  left <- obt_remove(recipe, step = "designer-languages")

  expect_identical(step_types(left), c("designer-add", "designer-generate"))
})

test_that("the recipe it came from is left as it was", {
  recipe <- three_step_recipe()

  obt_remove(recipe, step = 1)

  expect_length(recipe$operations, 3)
})

test_that("the first and the last both come out", {
  recipe <- three_step_recipe()

  expect_identical(
    step_types(obt_remove(recipe, step = 1)),
    c("designer-languages", "designer-generate")
  )
  expect_identical(
    step_types(obt_remove(recipe, step = 3)),
    c("designer-add", "designer-languages")
  )
})

test_that("a number that is not a whole one is refused", {
  recipe <- three_step_recipe()

  expect_error(obt_remove(recipe, step = 1.5), "number of a step")
  expect_error(obt_remove(recipe, step = TRUE), "number of a step")
  expect_error(obt_remove(recipe, step = NA), "number of a step")
  expect_error(obt_remove(recipe, step = c(1, 2)), "number of a step")
  expect_error(obt_remove(recipe, step = NULL), "number of a step")
})

test_that("a number outside the recipe is refused", {
  recipe <- three_step_recipe()

  expect_error(obt_remove(recipe, step = 0), "between")
  expect_error(obt_remove(recipe, step = 4), "between")
  expect_error(obt_remove(recipe, step = -1), "between")
  expect_error(obt_remove(recipe, step = 4), "holds 3 steps")
})

test_that("a type the recipe does not hold is refused", {
  recipe <- three_step_recipe()

  expect_error(obt_remove(recipe, step = "linelist-import"), "holds no")
  expect_error(obt_remove(recipe, step = "linelist-import"), "designer-add")
})

test_that("a type the recipe holds twice is refused by its numbers", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_add(type = "main") |>
    obt_designer_languages(form = "ENG") |>
    obt_designer_add(type = "dev")

  expect_error(obt_remove(recipe, step = "designer-add"), "holds 2")
  expect_error(obt_remove(recipe, step = "designer-add"), "steps 1, 3")
  expect_error(obt_remove(recipe, step = "designer-add"), "by its number")
})

test_that("a number still names one of a type recorded twice", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_add(type = "main") |>
    obt_designer_add(type = "dev")

  left <- obt_remove(recipe, step = 1)

  expect_length(left$operations, 1)
  expect_identical(left$operations[[1]]$args$type, "dev")
})

test_that("an empty recipe has nothing to take out", {
  expect_error(obt_remove(obt(folder = "/tmp/measles"), step = 1), "no step")
  expect_error(obt_pop(obt(folder = "/tmp/measles")), "no step")
})

test_that("anything that is not a recipe is refused", {
  expect_error(obt_remove("a folder path", step = 1), "must be an")
  expect_error(obt_pop("a folder path"), "must be an")
})

# Taking the last one off ---------------------------------------------------

test_that("the last step comes off", {
  recipe <- three_step_recipe()

  expect_identical(
    step_types(obt_pop(recipe)),
    c("designer-add", "designer-languages")
  )
})

test_that("popping every step leaves an empty recipe of the same class", {
  recipe <- three_step_recipe()

  empty <- obt_pop(obt_pop(obt_pop(recipe)))

  expect_length(empty$operations, 0)
  expect_true(is_obt(empty))
  expect_identical(empty$folder, recipe$folder)
})

test_that("a narrowed recipe pops the way any other does", {
  folder <- withr::local_tempdir()

  recipe <- linelist_recipe(folder) |>
    obt_linelist_geobase(path = geobase_file(folder)) |>
    obt_linelist_import(from = migration_file(folder))

  left <- obt_pop(recipe)

  expect_true(is_obt_linelist(left))
  expect_identical(step_types(left), "linelist-geobase")
})

# Steps one verb call recorded ----------------------------------------------

test_that("a conversion comes out whole, whichever of its steps is named", {
  for (step in 2:4) {
    left <- suppressMessages(obt_remove(conversion_recipe(), step = step))

    expect_identical(
      step_types(left),
      c("designer-add", "designer-generate"),
      info = paste("step", step)
    )
  }
})

test_that("taking a conversion out says how many steps went with it", {
  expect_message(
    obt_remove(conversion_recipe(), step = 3),
    "recorded 3 steps in one call"
  )
  expect_message(obt_remove(conversion_recipe(), step = 3), "Steps 2, 3, 4")
  expect_message(
    obt_remove(conversion_recipe(), step = 3),
    "obt_setup_convert"
  )
})

test_that("popping the last step of a conversion takes the whole of it", {
  recipe <- conversion_recipe() |> obt_pop()

  expect_identical(
    step_types(suppressMessages(obt_pop(recipe))),
    "designer-add"
  )
})

test_that("a step recorded on its own says nothing about siblings", {
  expect_message(obt_remove(three_step_recipe(), step = 2), NA)
  expect_message(obt_pop(three_step_recipe()), NA)
})

test_that("two conversions come out one at a time", {
  folder <- withr::local_tempdir()
  first <- empty_file(folder, "one.xlsx")
  second <- empty_file(folder, "two.xlsx")

  recipe <- obt(folder = folder) |>
    obt_designer_add() |>
    obt_setup_convert(from = first) |>
    obt_setup_convert(from = second)

  left <- suppressMessages(obt_remove(recipe, step = 2))

  expect_identical(
    step_types(left),
    c("designer-add", "convert-add", "convert-import")
  )
  expect_identical(left$operations[[2]]$args$from, "two.xlsx")
})

test_that("a record built with no verb call of its own stands alone", {
  recipe <- obt(folder = "/tmp/measles")
  recipe$operations <- list(
    new_operation("designer-add", list(type = "main")),
    new_operation("designer-generate", list(name = "measles-2026"))
  )

  expect_identical(
    step_types(obt_remove(recipe, step = 1)),
    "designer-generate"
  )
})

# What a recipe is left holding ---------------------------------------------

test_that("a conversion left with no designer is reported", {
  expect_warning(
    obt_remove(conversion_recipe(), step = "designer-add"),
    "no longer adds"
  )
})

test_that("taking the conversion out leaves nothing to report", {
  expect_warning(
    suppressMessages(obt_remove(conversion_recipe(), step = 2)),
    NA
  )
})

test_that("a recipe with no conversion says nothing about the designer", {
  expect_warning(obt_remove(three_step_recipe(), step = "designer-add"), NA)
})

# A recipe that has already run ---------------------------------------------

test_that("a recipe that has run is left alone", {
  recipe <- three_step_recipe()
  recipe$run <- list(outcome = RUN_OK)

  expect_error(obt_remove(recipe, step = 1), "already run")
  expect_error(obt_pop(recipe), "already run")
  expect_error(obt_pop(recipe), "Build the recipe you want next")
})

# What the printers show ----------------------------------------------------

test_that("both lists print the type of every step", {
  recipe <- three_step_recipe()

  expect_match(printed(obt_operations(recipe)), "designer-languages")
  expect_match(printed(obt_describe(recipe)), "designer-languages")
})

test_that("a waiting step prints its type and its wait", {
  said <- printed(obt_describe(three_step_recipe()))

  expect_match(said, "designer-generate")
  expect_match(said, "waiting on the workbooks")
})

# The registry ---------------------------------------------------------------

test_that("every operation type names the verb that records it", {
  for (type in names(OBT_OPERATIONS)) {
    verb <- operation_spec(type)$verb

    expect_true(is.character(verb) && nzchar(verb), info = type)
    expect_match(verb, "^obt_[a-z_]*\\(\\)$", info = type)
  }
})

test_that("the three conversion steps name one verb", {
  verbs <- vapply(
    CONVERT_OPERATION_TYPES,
    function(type) operation_spec(type)$verb,
    character(1)
  )

  expect_identical(unname(unique(verbs)), "obt_setup_convert()")
})

test_that("a verb opens a new call for every step it records", {
  recipe <- three_step_recipe()

  expect_identical(length(unique(operation_groups(recipe))), 3L)
})

test_that("a conversion records its three steps under one call", {
  groups <- operation_groups(conversion_recipe())

  expect_identical(length(unique(groups)), 3L)
  expect_identical(groups[[2]], groups[[3]])
  expect_identical(groups[[3]], groups[[4]])
})
