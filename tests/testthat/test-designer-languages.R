# The interface language, checked at the verb

test_that("every code the designer holds is accepted", {
  for (code in names(FORM_LANGUAGES)) {
    recipe <- obt(folder = "/tmp/measles") |>
      obt_designer_languages(form = code)

    expect_identical(recipe$operations[[1]]$args$form, code)
  }
})

test_that("a code is read whatever case it is typed in", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "fra")

  expect_identical(recipe$operations[[1]]$args$form, "FRA")
})

test_that("the CODE-Name form is accepted and only the code is kept", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "ENG-English")

  expect_identical(recipe$operations[[1]]$args$form, "ENG")
})

test_that("the name after the dash is never compared", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "POR-Portugues do Brasil")

  expect_identical(recipe$operations[[1]]$args$form, "POR")
})

test_that("spaces around the value are dropped", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "  spa  ")

  expect_identical(recipe$operations[[1]]$args$form, "SPA")
})

test_that("a code the designer does not hold is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    obt_designer_languages(recipe, form = "DEU"),
    "must be one of"
  )
})

test_that("the refusal names the five codes and the CODE-Name form", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    obt_designer_languages(recipe, form = "DEU"),
    "ARA"
  )
  expect_error(
    obt_designer_languages(recipe, form = "DEU"),
    "CODE-Name"
  )
})

test_that("the everyday code for Spanish is refused and points at SPA", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_designer_languages(recipe, form = "ESP"), "SPA")
  expect_error(obt_designer_languages(recipe, form = "ESP-Espanol"), "SPA")
})

test_that("an empty or non-string form is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_designer_languages(recipe, form = ""), "must carry a value")
  expect_error(obt_designer_languages(recipe, form = "-ENG"), "must be one of")
  expect_error(obt_designer_languages(recipe, form = 1), "single string")
  expect_error(
    obt_designer_languages(recipe, form = c("ENG", "FRA")),
    "single string"
  )
  expect_error(
    obt_designer_languages(recipe, form = NA_character_),
    "single string"
  )
})

# The dictionary language, checked against the setup file

test_that("a dictionary language is recorded unchecked with no setup there", {
  folder <- withr::local_tempdir()

  recipe <- obt(folder = folder) |>
    obt_designer_languages(dict = "Klingon")

  expect_identical(recipe$operations[[1]]$args$dict, "Klingon")
})

test_that("a language the setup holds is accepted", {
  recipe <- setup_in_folder(withr::local_tempdir()) |>
    obt_designer_languages(dict = "English")

  expect_identical(recipe$operations[[1]]$args$dict, "English")
})

test_that("a language the setup does not hold is refused", {
  recipe <- setup_in_folder(withr::local_tempdir())

  expect_error(
    obt_designer_languages(recipe, dict = "Klingon"),
    "must be a language"
  )
})

test_that("the refusal names the languages the file does hold", {
  recipe <- setup_in_folder(withr::local_tempdir())

  expect_error(obt_designer_languages(recipe, dict = "Klingon"), "English")
  expect_error(obt_designer_languages(recipe, dict = "Klingon"), "Arabic")
})

test_that("the file's own spelling is what the recipe keeps", {
  recipe <- setup_in_folder(withr::local_tempdir()) |>
    obt_designer_languages(dict = "arabic")

  expect_identical(recipe$operations[[1]]$args$dict, "Arabic")
})

test_that("every language the setup holds is accepted", {
  recipe <- setup_in_folder(withr::local_tempdir())

  for (language in setup_fixture_languages()) {
    added <- obt_designer_languages(recipe, dict = language)
    expect_identical(added$operations[[1]]$args$dict, language)
  }
})

test_that("a non-string dict is refused", {
  recipe <- obt(folder = withr::local_tempdir())

  expect_error(obt_designer_languages(recipe, dict = 1), "single string")
  expect_error(obt_designer_languages(recipe, dict = ""), "must carry a value")
})

# What the verb records

test_that("the verb records one operation that can run today", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(dict = "English", form = "ENG")

  expect_length(recipe$operations, 1)
  expect_identical(recipe$operations[[1]]$type, "designer-languages")
  expect_false(recipe$operations[[1]]$waiting)
})

test_that("either language alone is enough", {
  only_dict <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(dict = "English")
  only_form <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "ENG")

  expect_null(only_dict$operations[[1]]$args$form)
  expect_null(only_form$operations[[1]]$args$dict)
})

test_that("both names are kept on a record that carries one value", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "ENG")

  expect_identical(names(recipe$operations[[1]]$args), c("dict", "form"))
})

test_that("a verb with neither language is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_designer_languages(recipe), "Give")
})

test_that("the verb refuses anything that is not a recipe", {
  expect_error(obt_designer_languages("a folder", form = "ENG"), "must be an")
})

test_that("the verb answers a recipe, so it chains", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "ENG") |>
    obt_designer_generate(name = "measles-2026")

  expect_length(recipe$operations, 2)
})

test_that("a second call is the one the printers show", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "ENG") |>
    obt_designer_languages(form = "FRA")

  expect_identical(
    last_operation(recipe, "designer-languages")$args$form,
    "FRA"
  )
})

# What obt_languages() prints

test_that("both lists print, grouped", {
  out <- printed(obt_languages(obt(folder = "/tmp/measles")))

  expect_match(out, "Languages")
  expect_match(out, "Dictionary")
  expect_match(out, "Interface")
})

test_that("the interface list names every code with its language", {
  out <- printed(obt_languages(obt(folder = "/tmp/measles")))

  for (code in names(FORM_LANGUAGES)) {
    expect_match(out, code)
    expect_match(out, FORM_LANGUAGES[[code]])
  }
})

test_that("a folder with no setup says where the list will be read from", {
  folder <- withr::local_tempdir()

  out <- printed(obt_languages(obt(folder = folder)))

  expect_match(out, "holds no setup yet")
  expect_match(out, "setup.xlsb")
})

test_that("a folder with a setup lists the languages it holds", {
  out <- printed(obt_languages(setup_in_folder(withr::local_tempdir())))

  expect_match(out, "English")
  expect_match(out, "Arabic")
  expect_match(out, "Read from")
})

test_that("the languages the recipe picked are named under their list", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(dict = "Klingon", form = "FRA")

  out <- printed(obt_languages(recipe))

  expect_match(out, "The dictionary is built in")
  expect_match(out, "Klingon")
  expect_match(out, "The interface is built in")
})

test_that("a recipe that picked nothing names no choice", {
  out <- printed(obt_languages(obt(folder = "/tmp/measles")))

  expect_no_match(out, "is built in")
})

test_that("printing the languages changes nothing and chains", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "ENG")

  answered <- printed_value(obt_languages(recipe))

  expect_identical(answered, recipe)
})

test_that("obt_languages refuses anything that is not a recipe", {
  expect_error(obt_languages("a folder"), "must be an")
})

# What the recipe printers show

test_that("the description shows both languages", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(dict = "English", form = "ENG")

  out <- printed(obt_describe(recipe))

  expect_match(out, "Setup language")
  expect_match(out, "Interface language")
  expect_match(out, "English")
})

test_that("a language left out prints as not set", {
  recipe <- obt(folder = "/tmp/measles") |>
    obt_designer_languages(form = "ENG")

  out <- printed(obt_describe(recipe))

  expect_match(out, "(not set)", fixed = TRUE)
})
