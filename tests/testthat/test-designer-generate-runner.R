test_that("the run hands the designer every value it reads", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_identical(driver$calls, 1L)
  expect_identical(basename(driver$seen$designer), "designer_dev-latest.xlsb")
  expect_identical(basename(driver$seen$setup), "setup.xlsb")
  expect_identical(driver$seen$name, "measles-2026")
  expect_identical(driver$seen$setup_language, "English")
  expect_identical(driver$seen$form_language, "ENG")
})

test_that("the linelist is written into the linelist folder", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_identical(driver$seen$folder, obt_paths(recipe)$linelist)
})

test_that("the run answers the files it produced", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  answer <- run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_named(answer$produced, c("linelist", "summary", "log"))
  expect_true(all(file.exists(answer$produced)))
  expect_identical(basename(answer$produced[["linelist"]]), "measles-2026.xlsb")
})

test_that("the counts of the run are kept for the record", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  answer <- run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_identical(
    answer$state$counts,
    c(sheets = "12", variables = "240", built = "12", failed = "0")
  )
})

test_that("a run with no designer stops and names the verb", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder, designer = FALSE)
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "obt_designer_add"
  )
  expect_identical(driver$calls, 0L)
})

test_that("a designer already in the folder is used", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder, channel = "main")
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_identical(basename(driver$seen$designer), "designer_main-latest.xlsb")
})

test_that("the designer the recipe added wins over the one on disk", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  chosen <- file.path(folder, "obt", "dev", "designer_dev-2.1.0.xlsb")
  writeLines("designer", chosen)

  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state(designer = chosen)
  )

  expect_identical(driver$seen$designer, absolute_path(chosen))
})

test_that("two designers on disk stop the run", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  generation_folder(folder, channel = "main", setup = FALSE)

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "holds 2 designers"
  )
})

test_that("a run with no setup stops and says where it looked", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder, setup = FALSE)

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "no setup"
  )
})

test_that("a run with no dictionary language stops and names the verb", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state(dict = NULL)
    ),
    "obt_designer_languages"
  )
})

test_that("a run with no interface language stops", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state(form = NULL)
    ),
    "both languages"
  )
})

test_that("a linelist already there stops the run", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  ensure_folder(obt_paths(recipe)$linelist)
  writeLines("old", linelist_path(recipe, "measles-2026"))

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "already there"
  )
})

test_that("overwrite lets the run write over a linelist", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  ensure_folder(obt_paths(recipe)$linelist)
  writeLines("old", linelist_path(recipe, "measles-2026"))

  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  answer <- run_designer_generate(
    recipe,
    generation_operation(overwrite = TRUE),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_identical(driver$calls, 1L)
  expect_identical(
    readLines(answer$produced[["linelist"]], warn = FALSE),
    "linelist"
  )
})

test_that("a run with no geobase and no ribbon hands neither over", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_true(is.na(driver$seen$geo))
  expect_true(is.na(driver$seen$ribbon))
})

test_that("a geobase and a ribbon the recipe holds are handed over", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder, ribbon = TRUE)
  ribbon <- file.path(
    folder,
    "obt",
    "dev",
    "_ribbontemplate_dev-latest.xlsb"
  )

  geobase <- file.path(folder, "geo", "geobase.xlsb")
  ensure_folder(dirname(geobase))
  writeLines("geobase", geobase)

  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state(geobase = geobase, ribbon = ribbon)
  )

  expect_identical(driver$seen$geo, absolute_path(geobase))
  expect_identical(driver$seen$ribbon, absolute_path(ribbon))
})

test_that("a summary naming another linelist stops the run", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation(values = c(linelist = "something-else.xlsb"))
  local_mocked_bindings(driver_generate = driver$generate)

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "another name"
  )
})

test_that("a summary naming no linelist warns and carries on", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation(values = c(sheets = "12"))
  local_mocked_bindings(driver_generate = driver$generate)

  expect_warning(
    answer <- run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "named no linelist"
  )

  expect_true(file.exists(answer$produced[["linelist"]]))
})

test_that("a build that gave up on part of the work says so", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation(
    values = c(linelist = "measles-2026.xlsb", built = "10", failed = "2")
  )
  local_mocked_bindings(driver_generate = driver$generate)

  expect_warning(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "2 parts that did not"
  )
})

test_that("a build with nothing failed says nothing", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation()
  local_mocked_bindings(driver_generate = driver$generate)

  expect_no_warning(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    )
  )
})

test_that("a run that wrote no linelist stops the run", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation(writes = FALSE)
  local_mocked_bindings(driver_generate = driver$generate)

  expect_error(
    run_designer_generate(
      recipe,
      generation_operation(),
      stage = test_stage(folder, folder, staged = FALSE),
      state = generation_state()
    ),
    "was not written"
  )
})

test_that("a run refuses a waiting generation, and says what it needs", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_waiting_type(
    "designer-generate",
    needs = "silence, and a summary a script can read"
  )

  recipe <- obt(folder = folder) |>
    obt_designer_generate(name = "measles-2026")

  expect_error(obt_commit(recipe), "summary a script can read")
})

test_that("generation answers a runner all the same", {
  expect_true(is.function(operation_runner("designer-generate")))
})

test_that("a value is set only when it carries something", {
  expect_true(is_set("English"))
  expect_false(is_set(NULL))
  expect_false(is_set(NA_character_))
  expect_false(is_set(""))
  expect_false(is_set(c("a", "b")))
})

test_that("the counts of a summary are read in the order the keys hold", {
  values <- c(built = "3", linelist = "a.xlsb", sheets = "9")

  expect_identical(generation_counts(values), c(sheets = "9", built = "3"))
  expect_identical(generation_counts(character()), character())
})

test_that("one value of a summary is read by its key", {
  values <- c(linelist = "a.xlsb")

  expect_identical(summary_value(values, "linelist"), "a.xlsb")
  expect_true(is.na(summary_value(values, "log")))
  expect_true(is.na(summary_value(character(), "linelist")))
})

test_that("the build-in-place flag cannot be written yet", {
  reset_platform_state()
  withr::defer(reset_platform_state())
  local_mocked_bindings(os_name = function() "windows")

  expect_warning(
    expect_false(set_build_in_place("designer_dev-latest.xlsb")),
    "second Excel"
  )
})

test_that("the build-in-place word is said once per session", {
  reset_platform_state()
  withr::defer(reset_platform_state())
  local_mocked_bindings(os_name = function() "windows")

  expect_warning(set_build_in_place("designer_dev-latest.xlsb"))
  expect_silent(set_build_in_place("designer_dev-latest.xlsb"))
})

test_that("the build-in-place word names the designer the run drives", {
  reset_platform_state()
  withr::defer(reset_platform_state())
  local_mocked_bindings(os_name = function() "windows")

  expect_warning(
    set_build_in_place("/tmp/obt/dev/designer_dev-latest.xlsb"),
    "designer_dev-latest.xlsb"
  )
})

test_that("macOS says nothing about where the build happens", {
  reset_platform_state()
  withr::defer(reset_platform_state())
  local_mocked_bindings(os_name = function() "macos")

  expect_silent(expect_false(set_build_in_place("designer_dev-latest.xlsb")))
})

test_that("the run sets the flag on the designer it staged", {
  folder <- withr::local_tempdir()
  recipe <- generation_folder(folder)
  driver <- test_generation()
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(
    driver_generate = driver$generate,
    set_build_in_place = function(designer) {
      seen$designer <- designer
      invisible(FALSE)
    }
  )

  run_designer_generate(
    recipe,
    generation_operation(),
    stage = test_stage(folder, folder, staged = FALSE),
    state = generation_state()
  )

  expect_identical(seen$designer, driver$seen$designer)
})
