fixture <- function(name) {
  path <- system.file("extdata", name, package = "obt")
  skip_if(!nzchar(path), paste0("fixture not installed: ", name))
  path
}

test_that("the languages of a setup come back in file order", {
  expect_identical(
    obt_setup_languages(fixture("setup-translations.xlsx")),
    c("English", "Français", "Español", "Portugese")
  )
})

test_that("internal tag columns are left out", {
  languages <- obt_setup_languages(fixture("setup-translations.xlsx"))

  expect_false(any(startsWith(languages, "__")))
})

test_that("a blank header column is left out", {
  languages <- obt_setup_languages(fixture("setup-translations.xlsx"))

  expect_true(all(nzchar(languages)))
})

test_that("a file with no translations sheet names the sheets it has", {
  expect_error(
    obt_setup_languages(fixture("setup-no-translations.xlsx")),
    "carries no"
  )
})

test_that("a sheet of tag columns only is an error, never an empty answer", {
  expect_error(
    obt_setup_languages(fixture("setup-tags-only.xlsx")),
    "holds no languages"
  )
})

test_that("a missing file is named in the error", {
  expect_error(obt_setup_languages("no-such-setup.xlsx"), "No file at")
})

test_that("an unsupported extension is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", path)

  expect_error(obt_setup_languages(path), "xlsb")
})

test_that("path must be a single string", {
  expect_error(obt_setup_languages(1), "single file path")
  expect_error(obt_setup_languages(c("a.xlsx", "b.xlsx")), "single file path")
  expect_error(obt_setup_languages(NA_character_), "single file path")
})

test_that("an .xlsb setup answers its languages", {
  expect_identical(
    obt_setup_languages(fixture("generic-test-setup.xlsb")),
    c("English", "Français", "Espanol", "Portugues", "Arabic")
  )
})

test_that("the legacy tag column is not offered as a language", {
  languages <- obt_setup_languages(fixture("generic-test-setup.xlsb"))

  expect_false("TranslationTag" %in% languages)
})

test_that("both formats are read by the same rules", {
  from_xlsb <- obt_setup_languages(fixture("generic-test-setup.xlsb"))
  from_xlsx <- obt_setup_languages(fixture("setup-translations.xlsx"))

  expect_type(from_xlsb, "character")
  expect_type(from_xlsx, "character")
  expect_true(all(nzchar(c(from_xlsb, from_xlsx))))
  expect_false(any(startsWith(c(from_xlsb, from_xlsx), "__")))
})
