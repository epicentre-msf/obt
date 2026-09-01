test_that("a recipe with nothing in it is refused", {
  folder <- withr::local_tempdir()
  local_run_machine()

  expect_error(obt_commit(obt(folder = folder)), "holds no operation")
})

test_that("a value that is not a recipe is refused", {
  local_run_machine()

  expect_error(obt_commit("a folder path"), "must be an")
})

test_that("verbose takes TRUE or FALSE", {
  folder <- withr::local_tempdir()
  local_run_machine()

  expect_error(
    obt_commit(runnable_recipe(folder), verbose = "yes"),
    "must be"
  )
})

test_that("a run is refused on a system that cannot open Excel", {
  folder <- withr::local_tempdir()
  local_run_machine(os = "linux")

  expect_error(
    obt_commit(runnable_recipe(folder)),
    "macOS or Windows"
  )
})

test_that("a refused system leaves the working folder alone", {
  folder <- file.path(withr::local_tempdir(), "work")
  local_run_machine(os = "linux")

  expect_error(obt_commit(runnable_recipe(folder)))
  expect_false(dir.exists(folder))
})

test_that("an operation waiting on the workbooks stops the run", {
  folder <- file.path(withr::local_tempdir(), "work")
  local_run_machine()
  local_waiting_type("designer-generate")

  recipe <- obt(folder = folder) |>
    obt_designer_generate(name = "measles-2026")

  expect_error(obt_commit(recipe), "wait")
})

test_that("a waiting operation is turned down before anything is written", {
  folder <- file.path(withr::local_tempdir(), "work")
  local_run_machine()
  local_waiting_type("designer-generate")

  recipe <- obt(folder = folder) |>
    obt_designer_add(type = "dev") |>
    obt_designer_generate(name = "measles-2026")

  expect_error(obt_commit(recipe))
  expect_false(dir.exists(folder))
})

test_that("the refusal names every operation that is waiting", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_waiting_type("designer-generate")

  recipe <- obt(folder = folder) |>
    obt_designer_generate(name = "measles-2026")

  expect_error(obt_commit(recipe), "Generate the linelist")
})

test_that("a recipe runs end to end and says the run finished", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  ran <- printed_value(obt_commit(runnable_recipe(folder)))

  expect_true(is_obt(ran))
  expect_identical(ran$run$outcome, RUN_OK)
  expect_identical(ran$run$os, "macos")
  expect_identical(ran$run$excel_version, EXCEL_VERSION_PROVEN)
  expect_false(ran$run$staged)
})

test_that("every operation of a finished run carries its own record", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  ran <- printed_value(obt_commit(runnable_recipe(folder)))
  outcomes <- vapply(
    ran$operations,
    function(operation) operation$result$outcome,
    character(1)
  )

  expect_identical(outcomes, c(RUN_OK, RUN_OK))
  expect_true(all(vapply(
    ran$operations,
    function(operation) is.numeric(operation$result$seconds),
    logical(1)
  )))
})

test_that("the files a run produced are stored relative to the folder", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  ran <- printed_value(obt_commit(runnable_recipe(folder)))
  produced <- ran$operations[[1]]$result$produced

  expect_named(
    produced,
    c("zip", "designer", "empty_setup", "master_setup", "ribbon")
  )
  expect_true(all(startsWith(unname(produced), "obt/dev/")))
  expect_true(all(file.exists(file.path(folder, produced))))
})

test_that("a run builds the whole layout of the working folder", {
  folder <- file.path(withr::local_tempdir(), "work")
  local_run_machine()
  local_channel_download("dev")

  printed(obt_commit(runnable_recipe(folder)))

  expect_true(all(dir.exists(file.path(folder, OBT_FOLDERS))))
})

test_that("the run writes one log under the log folder", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  ran <- printed_value(obt_commit(runnable_recipe(folder)))

  expect_true(file.exists(file.path(folder, ran$run$log)))
  expect_true(startsWith(ran$run$log, "log/"))
})

test_that("the log carries the machine and the version of Excel", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  printed(obt_commit(runnable_recipe(folder)))
  lines <- run_log_of(folder)

  expect_true(any(grepl("^System: macos$", lines)))
  expect_true(any(grepl("^Excel version: ", lines)))
  expect_true(any(grepl("^Excel version proven: ", lines)))
  expect_true(any(grepl("^Staging folder: ", lines)))
})

test_that("the log carries every operation with its outcome", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  printed(obt_commit(runnable_recipe(folder)))
  lines <- run_log_of(folder)

  expect_true(any(grepl("1. Add the designer - ok - ", lines, fixed = TRUE)))
  expect_true(any(grepl("2. Set the languages - ok - ", lines, fixed = TRUE)))
  expect_true(any(grepl("^Outcome: ok$", lines)))
})

test_that("the log names the files an operation wrote", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  printed(obt_commit(runnable_recipe(folder)))
  lines <- run_log_of(folder)

  expect_true(any(grepl("designer: obt/dev/", lines, fixed = TRUE)))
})

test_that("a failure stops the run and names the operation", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_failed_download()

  expect_error(
    printed(obt_commit(runnable_recipe(folder))),
    "stopped at operation 1 of 2"
  )
})

test_that("a failure reports the reason it was given", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_failed_download("The network is down.")

  expect_error(
    printed(obt_commit(runnable_recipe(folder))),
    "The network is down"
  )
})

test_that("a failed run writes its log all the same", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_failed_download("The network is down.")

  expect_error(printed(obt_commit(runnable_recipe(folder))))
  lines <- run_log_of(folder)

  expect_true(any(grepl("^Outcome: failed$", lines)))
  expect_true(any(grepl(
    "Stopped at: 1. Add the designer",
    lines,
    fixed = TRUE
  )))
  expect_true(any(grepl("The network is down", lines)))
})

test_that("the operations after a failure never run", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_failed_download()

  expect_error(printed(obt_commit(runnable_recipe(folder))))
  lines <- run_log_of(folder)

  expect_true(any(grepl("1. Add the designer - failed", lines, fixed = TRUE)))
  expect_false(any(grepl("Set the languages", lines, fixed = TRUE)))
})

test_that("a run prints each operation as it starts and as it ends", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  said <- printed(obt_commit(runnable_recipe(folder)))

  expect_match(said, "1/2 Add the designer")
  expect_match(said, "2/2 Set the languages")
  expect_match(said, "Log:")
})

test_that("a quiet run prints nothing", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  said <- printed(obt_commit(runnable_recipe(folder), verbose = FALSE))

  expect_identical(said, "")
})

test_that("the run answers the recipe invisibly", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  answer <- withVisible(obt_commit(runnable_recipe(folder), verbose = FALSE))

  expect_false(answer$visible)
  expect_true(is_obt(answer$value))
})

test_that("the dictionary language is read again against the setup on disk", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  # The recipe is built while the folder holds no setup, so the verb records
  # the language unchecked. The run reads it against the file that is there.
  recipe <- obt(folder = folder) |>
    obt_designer_languages(dict = "Klingon")

  setup_in_folder(folder)

  expect_error(printed(obt_commit(recipe)), "language the setup file holds")
})

test_that("a dictionary language the setup holds passes the run", {
  folder <- withr::local_tempdir()
  local_run_machine()

  recipe <- obt(folder = folder) |>
    obt_designer_languages(dict = "English")

  setup_in_folder(folder)
  ran <- printed_value(obt_commit(recipe))

  expect_identical(ran$run$outcome, RUN_OK)
})

test_that("a second run writes a second log and leaves the first", {
  folder <- withr::local_tempdir()
  local_run_machine()
  downloads <- local_channel_download("dev")

  first <- printed_value(obt_commit(runnable_recipe(folder), verbose = FALSE))
  Sys.sleep(1.1)
  second <- printed_value(obt_commit(runnable_recipe(folder), verbose = FALSE))

  expect_false(identical(first$run$log, second$run$log))
  expect_true(file.exists(file.path(folder, first$run$log)))
  expect_identical(downloads$calls, 1L)
})

test_that("the summary of a recipe that ran says how the run ended", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  ran <- printed_value(obt_commit(runnable_recipe(folder), verbose = FALSE))
  said <- printed(obt_summary(ran))

  expect_match(said, "Ran on")
  expect_match(said, "Log:")
  expect_false(grepl("Every operation can run", said))
})

test_that("the description of a recipe that ran carries every outcome", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_channel_download("dev")

  ran <- printed_value(obt_commit(runnable_recipe(folder), verbose = FALSE))
  said <- printed(obt_describe(ran))

  expect_match(said, "Outcome")
  expect_match(said, "Took")
  expect_match(said, "Wrote designer")
})

test_that("a recipe that never ran prints no outcome", {
  folder <- withr::local_tempdir()
  said <- printed(obt_describe(runnable_recipe(folder)))

  expect_false(grepl("Outcome", said))
})

test_that("every type the recipe knows answers a runner", {
  has_runner <- vapply(
    names(OBT_OPERATIONS),
    function(type) !is.null(operation_runner(type)),
    logical(1)
  )

  expect_identical(names(has_runner)[!has_runner], character())
})

test_that("an operation with no runner is refused by name", {
  folder <- withr::local_tempdir()
  recipe <- obt(folder = folder)
  operation <- list(type = "nowhere", args = list(), waiting = TRUE)
  stage <- test_stage(folder, folder, staged = FALSE)

  expect_error(
    run_operation(recipe, operation, stage = stage, state = list()),
    "cannot run"
  )
})

test_that("a run with no operation reached ends as none", {
  expect_identical(run_outcome(list()), RUN_NONE)
})

test_that("seconds are written one way", {
  expect_identical(format_seconds(0), "0.0s")
  expect_identical(format_seconds(12.34), "12.3s")
  expect_identical(format_seconds(NA_real_), "unknown")
})

test_that("a failure reason is read back as one line", {
  condition <- tryCatch(
    cli::cli_abort(c("It broke.", "i" = "Try again.")),
    error = function(cnd) cnd
  )

  expect_match(failure_reason(condition), "It broke.")
  expect_false(grepl("\n", failure_reason(condition), fixed = TRUE))
})

test_that("the paths of a run are answered relative to the folder", {
  answer <- relative_paths(
    c(zip = "/work/obt/dev/OBT.zip", other = "/elsewhere/file.xlsb"),
    folder = "/work"
  )

  expect_identical(
    answer,
    c(zip = "obt/dev/OBT.zip", other = "/elsewhere/file.xlsb")
  )
})
