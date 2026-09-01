# The verb ------------------------------------------------------------------

test_that("the verb records a step and answers the recipe", {
  recipe <- obt_setup(from = setup_workbook(), folder = "/tmp/measles") |>
    obt_fake(n = 20)

  expect_s3_class(recipe, "obt_setup")
  expect_identical(recipe$operations[[2]]$type, "setup-fake")
  expect_identical(recipe$operations[[2]]$args$n, 20L)
})

test_that("the step waits on nothing", {
  recipe <- obt_setup(from = setup_workbook(), folder = "/tmp/measles") |>
    obt_fake()

  expect_false(recipe$operations[[2]]$waiting)
  expect_identical(count_waiting(recipe), 0L)
})

test_that("an obt recipe takes the verb too", {
  recipe <- obt(folder = "/tmp/measles") |> obt_fake()

  expect_s3_class(recipe, "obt")
  expect_identical(recipe$operations[[1]]$type, "setup-fake")
})

test_that("a linelist recipe is refused, by what it holds", {
  recipe <- obt_linelist(obt(folder = "/tmp/measles"))

  expect_error(obt_fake(recipe), "holds a generated linelist and no setup")
})

test_that("anything that is not a recipe is refused", {
  expect_error(obt_fake("a folder"), "must be an <obt>")
})

test_that("narrowing to a setup keeps the step", {
  narrowed <- obt(folder = "/tmp/measles") |>
    obt_fake(n = 5) |>
    obt_setup()

  expect_s3_class(narrowed, "obt_setup")
  expect_identical(narrowed$operations[[1]]$type, "setup-fake")
})

test_that("the defaults are recorded as the run reads them", {
  recipe <- obt(folder = "/tmp/measles") |> obt_fake(n = 250)
  args <- recipe$operations[[1]]$args

  expect_identical(args$to, "export")
  expect_identical(args$name, "fake")
  expect_identical(args$id_width, 3L)
  expect_identical(args$seed, NA_integer_)
  expect_type(args$range_date, "character")
})

# The arguments -------------------------------------------------------------

test_that("a count that is not a whole number is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_fake(recipe, n = "many"), "single number")
  expect_error(obt_fake(recipe, n = 2.5), "whole number")
  expect_error(obt_fake(recipe, n = -1), "not below zero")
})

test_that("a name carrying a folder is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_fake(recipe, name = "out/fake"), "not a path")
  expect_error(obt_fake(recipe, name = ""), "must carry a value")
})

test_that("a seed has to be a number, and nothing is a seed too", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_fake(recipe, seed = "one"), "single number")
  expect_identical(obt_fake(recipe, seed = 3)$operations[[1]]$args$seed, 3L)
})

test_that("a range has to be two numbers, and is read lowest first", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_fake(recipe, range_int = 5), "two numbers")
  expect_error(obt_fake(recipe, nchar_text = c(0, 4)), "at least 1")
  expect_identical(
    obt_fake(recipe, range_int = c(9, 1))$operations[[1]]$args$range_int,
    c(1, 9)
  )
})

test_that("a date range has to be two dates, and is read earliest first", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_fake(recipe, range_date = Sys.Date()), "two dates")
  expect_identical(
    obt_fake(
      recipe,
      range_date = as.Date(c("2026-01-31", "2026-01-01"))
    )$operations[[1]]$args$range_date,
    c("2026-01-01", "2026-01-31")
  )
})

test_that("a share outside 0 and 1 is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(obt_fake(recipe, prop_na = 2), "between 0 and 1")
  expect_error(obt_fake(recipe, prop_na = NA), "single number")
})

test_that("an id is as wide as the count needs, unless it is told otherwise", {
  recipe <- obt(folder = "/tmp/measles")

  expect_identical(obt_fake(recipe, n = 0)$operations[[1]]$args$id_width, 1L)
  expect_identical(obt_fake(recipe, n = 9)$operations[[1]]$args$id_width, 1L)
  expect_identical(obt_fake(recipe, n = 10)$operations[[1]]$args$id_width, 2L)
  expect_identical(
    obt_fake(recipe, n = 10, id_width = 6)$operations[[1]]$args$id_width,
    6L
  )
  expect_error(obt_fake(recipe, id_width = 0), "at least 1")
})

# The run -------------------------------------------------------------------

test_that("the run writes one sheet per worksheet of the setup", {
  skip_if_no_run()

  folder <- withr::local_tempdir()
  recipe <- fake_committed_setup(folder) |> obt_fake(n = 6, seed = 1)

  ran <- obt_commit(recipe, verbose = FALSE)
  path <- file.path(folder, "export", "fake.xlsx")

  expect_true(file.exists(path))
  expect_identical(
    readxl::excel_sheets(path),
    c("vlist1D-sheet1", "hlist2D-sheet1", "hlist2D-sheet2")
  )
  expect_identical(
    unname(ran$operations[[1]]$result$produced),
    "export/fake.xlsx"
  )
})

test_that("a horizontal worksheet holds one record a row", {
  skip_if_no_run()

  folder <- withr::local_tempdir()

  invisible(obt_commit(
    fake_committed_setup(folder) |> obt_fake(n = 7, seed = 1),
    verbose = FALSE
  ))

  records <- fake_read_back(
    file.path(folder, "export", "fake.xlsx"),
    "hlist2D-sheet1"
  )

  expect_identical(nrow(records), 7L)
  expect_true("uni_h2" %in% names(records))
})

test_that("a vertical worksheet holds one record, laid out down the page", {
  skip_if_no_run()

  folder <- withr::local_tempdir()

  invisible(obt_commit(
    fake_committed_setup(folder) |> obt_fake(n = 7, seed = 1),
    verbose = FALSE
  ))

  records <- fake_read_back(
    file.path(folder, "export", "fake.xlsx"),
    "vlist1D-sheet1"
  )

  expect_identical(names(records), c("variable", "value"))
  expect_true("date_v1" %in% records$variable)
})

test_that("one seed answers one set of records", {
  skip_if_no_run()

  first <- withr::local_tempdir()
  second <- withr::local_tempdir()

  for (folder in c(first, second)) {
    invisible(obt_commit(
      fake_committed_setup(folder) |> obt_fake(n = 5, seed = 404),
      verbose = FALSE
    ))
  }

  expect_identical(
    fake_read_back(file.path(first, "export", "fake.xlsx"), "hlist2D-sheet1"),
    fake_read_back(file.path(second, "export", "fake.xlsx"), "hlist2D-sheet1")
  )
})

test_that("a run under a seed leaves the session's own draws alone", {
  skip_if_no_run()

  folder <- withr::local_tempdir()
  recipe <- fake_committed_setup(folder) |> obt_fake(n = 3, seed = 7)

  set.seed(11)
  before <- .Random.seed

  invisible(obt_commit(recipe, verbose = FALSE))

  expect_identical(.Random.seed, before)
})

test_that("a file already written is left alone unless the verb says so", {
  skip_if_no_run()

  folder <- withr::local_tempdir()
  setup <- fake_committed_setup(folder)

  invisible(obt_commit(setup |> obt_fake(n = 3, seed = 1), verbose = FALSE))

  expect_error(
    obt_commit(setup |> obt_fake(n = 3, seed = 1), verbose = FALSE),
    "already there"
  )
  expect_no_error(
    obt_commit(
      setup |> obt_fake(n = 3, seed = 1, overwrite = TRUE),
      verbose = FALSE
    )
  )
})

test_that("a working folder with no setup says so", {
  skip_if_no_run()

  folder <- withr::local_tempdir()

  expect_error(
    obt_commit(obt(folder = folder) |> obt_fake(n = 3), verbose = FALSE),
    "holds no setup"
  )
})

test_that("the dates of a run stay inside the bounds the setup wrote", {
  skip_if_no_run()

  folder <- withr::local_tempdir()

  invisible(obt_commit(
    fake_committed_setup(folder) |> obt_fake(n = 60, seed = 5, prop_na = 0),
    verbose = FALSE
  ))

  records <- fake_read_back(
    file.path(folder, "export", "fake.xlsx"),
    "hlist2D-sheet1"
  )

  entry <- as.Date(records$date_entry_h2)
  validated <- as.Date(records$date_valid_h2_1)

  expect_true(all(entry >= Sys.Date() - 365 & entry <= Sys.Date()))
  expect_true(all(validated >= entry))
  expect_true(all(validated <= Sys.Date() + 365))
})

test_that("the ids of a run are numbered and padded", {
  skip_if_no_run()

  folder <- withr::local_tempdir()

  invisible(obt_commit(
    fake_committed_setup(folder) |> obt_fake(n = 12, seed = 5, id_prefix = "C"),
    verbose = FALSE
  ))

  records <- fake_read_back(
    file.path(folder, "export", "fake.xlsx"),
    "hlist2D-sheet1"
  )

  expect_identical(records$uni_h2[[1]], "C01")
  expect_identical(records$uni_h2[[12]], "C12")
  expect_identical(anyDuplicated(records$uni_h2), 0L)
})

# The geobase ---------------------------------------------------------------

test_that("without a geobase the geo columns are there and empty", {
  skip_if_no_run()

  folder <- withr::local_tempdir()

  invisible(obt_commit(
    fake_committed_setup(folder) |> obt_fake(n = 4, seed = 1),
    verbose = FALSE
  ))

  records <- fake_read_back(
    file.path(folder, "export", "fake.xlsx"),
    "hlist2D-sheet1"
  )

  expect_true(all(paste0("adm", 1:4, "_geo_h2") %in% names(records)))
  expect_true(all(is.na(records$adm1_geo_h2)))
})

test_that("a geobase in the working folder fills the geo columns", {
  skip_if_no_run()

  folder <- withr::local_tempdir()
  setup <- fake_committed_setup(folder)
  fake_geobase_file(file.path(folder, "geo"))

  invisible(obt_commit(setup |> obt_fake(n = 30, seed = 2), verbose = FALSE))

  records <- fake_read_back(
    file.path(folder, "export", "fake.xlsx"),
    "hlist2D-sheet1"
  )

  expect_true(all(records$adm1_geo_h2 %in% c("TCD North", "TCD South")))
  expect_true(any(records$adm2_geo_h2 %in% c("Moussoro", "Bongor")))
  expect_true(all(is.na(records$adm3_geo_h2)))
})

test_that("a record names a place that exists", {
  folder <- withr::local_tempdir()
  geobase <- read_geobase(fake_geobase_file(folder))

  under <- geobase$adm1_name[geobase$adm2_name %in% "Moussoro"]

  expect_identical(under, "TCD North")
  expect_false(any(geobase$adm2_name %in% "N/A"))
  expect_identical(sort(unique(geobase$level)), c(1L, 2L))
})

test_that("every level of a geobase is drawn from as much as any other", {
  geobase <- read_geobase(fake_geobase_file(withr::local_tempdir()))

  by_level <- tapply(geobase$weight, geobase$level, sum)

  expect_equal(as.numeric(by_level), rep(1, 2))
})

test_that("a folder holding two geobases says which one to name", {
  skip_if_no_run()

  folder <- withr::local_tempdir()
  setup <- fake_committed_setup(folder)

  fake_geobase_file(file.path(folder, "geo"), name = "one.xlsx")
  fake_geobase_file(file.path(folder, "geo"), name = "two.xlsx")

  expect_error(
    obt_commit(setup |> obt_fake(n = 3), verbose = FALSE),
    "holds 2 geobases"
  )
})

test_that("a geobase naming no place is refused", {
  folder <- withr::local_tempdir()
  path <- file.path(folder, "empty.xlsx")

  writexl::write_xlsx(list(NAMES = data.frame(level = "adm1_name")), path)

  expect_error(read_geobase(path), "names no place")
})

# The engine ----------------------------------------------------------------

test_that("a worksheet answers a column per variable, in dictionary order", {
  dict <- fake_dict(
    fake_var("id", unique = "yes"),
    fake_var("age", variable_type = "integer", min = "0", max = "5"),
    fake_var("place", control = "geo")
  )

  records <- fake_test_sheet(dict, n = 4)

  expect_identical(
    names(records),
    c("id", "age", paste0("adm", 1:4, "_place"))
  )
  expect_identical(nrow(records), 4L)
})

test_that("a health facility variable takes a prefix of its own", {
  dict <- fake_dict(fake_var("clinic", control = "hf"))

  expect_identical(names(fake_test_sheet(dict, n = 2)), "hf_clinic")
})

test_that("a variable the linelist works out for itself is left empty", {
  dict <- fake_dict(
    fake_var("total", variable_type = "integer", control = "formula"),
    fake_var("band", variable_type = "text", control = "case_when")
  )

  records <- fake_test_sheet(dict, n = 3)

  expect_true(all(is.na(records$total)))
  expect_true(all(is.na(records$band)))
})

test_that("a choice variable reads its own list, and no other", {
  dict <- fake_dict(
    fake_var("sex", control = "choice_manual", control_details = "list_sex")
  )

  choices <- fake_choices(
    list_name = c("list_sex", "list_sex", "list_other"),
    label = c("Female", "Male", "Elsewhere")
  )

  records <- fake_test_sheet(dict, choices = choices, n = 30)

  expect_true(all(records$sex %in% c("Female", "Male")))
})

test_that("a choice variable whose list is missing is left empty", {
  dict <- fake_dict(
    fake_var("sex", control = "choice_manual", control_details = "list_gone")
  )

  records <- fake_test_sheet(dict, choices = fake_choices(), n = 3)

  expect_true(all(is.na(records$sex)))
})

test_that("a number stays inside the range the setup wrote", {
  dict <- fake_dict(
    fake_var("age", variable_type = "integer", min = "4", max = "6"),
    fake_var("temp", variable_type = "decimal", min = "36", max = "37")
  )

  records <- fake_test_sheet(dict, n = 40)

  expect_true(all(records$age %in% 4:6))
  expect_true(all(records$temp >= 36 & records$temp <= 37))
})

test_that("a range of one value is drawn from that value", {
  dict <- fake_dict(fake_var(
    "age",
    variable_type = "integer",
    min = "5",
    max = "5"
  ))

  expect_true(all(fake_test_sheet(dict, n = 20)$age == 5))
})

test_that("a number with no bound falls back on the range it was given", {
  dict <- fake_dict(fake_var("age", variable_type = "integer"))

  records <- fake_test_sheet(dict, n = 30, range_int = c(70, 72))

  expect_true(all(records$age %in% 70:72))
})

test_that("text is as long as the setup asks for", {
  dict <- fake_dict(fake_var(
    "note",
    variable_type = "text",
    min = "3",
    max = "5"
  ))

  lengths <- nchar(fake_test_sheet(dict, n = 20)$note)

  expect_true(all(lengths >= 3 & lengths <= 5))
})

test_that("the decimal type is read under both names a setup writes it as", {
  dict <- fake_dict(
    fake_var("old", variable_type = "decimal2", min = "1", max = "2"),
    fake_var("new", variable_type = "decimal", min = "1", max = "2")
  )

  records <- fake_test_sheet(dict, n = 5)

  expect_type(records$old, "double")
  expect_type(records$new, "double")
})

test_that("a variable of no type is left empty", {
  dict <- fake_dict(fake_var("unsaid"))

  expect_true(all(is.na(fake_test_sheet(dict, n = 3)$unsaid)))
})

test_that("today's date is read the way the setup writes it", {
  dict <- fake_dict(fake_var(
    "seen",
    variable_type = "date",
    min = "TODAY() - 1"
  ))

  records <- fake_test_sheet(
    dict,
    n = 10,
    range_date = c(Sys.Date() - 90, Sys.Date())
  )

  expect_true(all(records$seen >= Sys.Date() - 1))
})

test_that("a date bounded by another date is drawn after it", {
  dict <- fake_dict(
    fake_var("onset", variable_type = "date"),
    fake_var("seen", variable_type = "date", min = "onset"),
    fake_var("closed", variable_type = "date", min = "seen")
  )

  records <- fake_test_sheet(dict, n = 40)

  expect_true(all(records$seen >= records$onset))
  expect_true(all(records$closed >= records$seen))
})

test_that("two dates pointing at one another stop rather than run on", {
  dict <- fake_dict(
    fake_var("first", variable_type = "date", min = "second"),
    fake_var("second", variable_type = "date", min = "first")
  )

  records <- fake_test_sheet(dict, n = 3)

  expect_identical(nrow(records), 3L)
  expect_s3_class(records$first, "Date")
})

test_that("a bound above the one over it holds the day it names", {
  dict <- fake_dict(
    fake_var("onset", variable_type = "date", min = "TODAY()", max = "TODAY()"),
    fake_var(
      "seen",
      variable_type = "date",
      min = "onset",
      max = "TODAY() - 30"
    )
  )

  records <- fake_test_sheet(dict, n = 5)

  expect_true(all(records$seen == Sys.Date()))
})

test_that("a share of every column is left missing", {
  dict <- fake_dict(fake_var("age", variable_type = "integer"))

  expect_false(any(is.na(fake_test_sheet(dict, n = 50, prop_na = 0)$age)))
  expect_true(all(is.na(fake_test_sheet(dict, n = 50, prop_na = 1)$age)))
})

test_that("a worksheet of no records answers no rows", {
  dict <- fake_dict(fake_var("age", variable_type = "integer"))

  expect_identical(nrow(fake_test_sheet(dict, n = 0)), 0L)
})

test_that("a worksheet of no variable still answers the rows asked for", {
  records <- fake_sheet(
    fake_dict(fake_var("", variable_type = "integer")),
    choices = fake_choices(),
    geo = NULL,
    n = 4,
    settings = fake_test_settings()
  )

  expect_identical(nrow(records), 4L)
  expect_identical(ncol(records), 0L)
})

test_that("a worksheet named by no dictionary row is not a worksheet", {
  dict <- fake_dict(
    fake_var("age", variable_type = "integer"),
    list(variable_name = "loose", sheet_name = "", sheet_type = "")
  )

  expect_identical(fake_sheets(dict)$sheet_name, "ll")
})

test_that("a single record is laid out down the page as text", {
  record <- data.frame(
    seen = as.Date("2026-01-31"),
    age = 4L,
    stringsAsFactors = FALSE
  )

  upright <- fake_upright(record)

  expect_identical(upright$variable, c("seen", "age"))
  expect_identical(upright$value, c("2026-01-31", "4"))
})

# The bounds ----------------------------------------------------------------

test_that("a bound written as a date is read as one, not as a subtraction", {
  expect_identical(constraint_value("2026-08-16"), "2026-08-16")
  expect_true(is_date_text("2026-08-16"))
  expect_false(is_date_text("0"))
  expect_false(is_date_text(NA_character_))
})

test_that("a bound naming a column answers nothing on its own", {
  expect_identical(constraint_value("date_entry"), NA_character_)
  expect_identical(constraint_value(NA_character_), NA_character_)
})

test_that("a bound that is not readable answers nothing", {
  expect_identical(constraint_value("1 +"), NA_character_)
  expect_identical(expression_variables("1 +"), character())
})

test_that("a bound that works out answers its value", {
  expect_identical(constraint_value("0"), "0")
  expect_identical(constraint_value("2 * 3"), "6")
})

test_that("today's call is rewritten, and an empty bound is no bound", {
  expect_identical(fake_constraint_text("today() - 1"), "Sys.Date() - 1")
  expect_identical(fake_constraint_text("TODAY()"), "Sys.Date()")
  expect_identical(fake_constraint_text(""), NA_character_)
})

test_that("the depth of a chain of dates is how far it runs", {
  variables <- c("onset", "seen", "closed")
  bounds <- c(NA_character_, "onset", "seen")

  expect_identical(dependency_depth(variables, bounds), c(0L, 1L, 2L))
})

# The reading ---------------------------------------------------------------

test_that("a setup's own headers are read under one shape", {
  expect_identical(
    clean_setup_names(c("Variable.Name", " Control Details ", "Min")),
    c("variable_name", "control_details", "min")
  )
})

test_that("a sheet the setup does not carry is named in the error", {
  expect_error(
    read_setup_sheet(setup_workbook(), "Nowhere"),
    "Could not read"
  )
})

test_that("the dictionary of a setup is read under the names the engine uses", {
  dict <- read_setup_sheet(setup_workbook(), FAKE_DICTIONARY_SHEET)

  expect_true(all(FAKE_DICTIONARY_COLUMNS %in% names(dict)))
  expect_true(all(vapply(dict, is.character, logical(1))))
})

test_that("a file missing a column the records are read from is refused", {
  expect_error(
    check_setup_columns(
      data.frame(variable_name = "a"),
      wanted = c("variable_name", "control"),
      sheet = "Dictionary",
      path = "/tmp/setup.xlsb",
      call = rlang::caller_env()
    ),
    "missing 1 column"
  )
})

test_that("drawing reads a single value as a value, not as a count", {
  expect_identical(pick(7L, 3), c(7L, 7L, 7L))
  expect_identical(pick(integer(), 3), integer())
  expect_identical(pick(1:5, 0), integer())
})
