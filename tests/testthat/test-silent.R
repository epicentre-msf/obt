test_that("the switch is kept under the name the workbooks read", {
  expect_identical(SILENT_SWITCH_NAME, "__OBT__SILENT_OPERATIONS__")
  expect_identical(SILENT_ON, "Yes")
  expect_identical(SILENT_OFF, "No")
})

test_that("the quiet pair ships both halves and answers in one shape", {
  for (os in names(DRIVER_RUNNERS)) {
    file <- paste0("quiet.", DRIVER_RUNNERS[[os]]$extension)
    path <- system.file("scripts", file, package = "obt")
    skip_if(!nzchar(path), "the script is not installed")

    lines <- readLines(path, warn = FALSE)
    answers <- grep("\"(OK|ERROR )", lines, value = TRUE)

    expect_true(any(grepl("\"OK\"", answers, fixed = TRUE)), info = file)
    expect_true(any(grepl("\"ERROR ", answers, fixed = TRUE)), info = file)
  }
})

test_that("the macOS half of the quiet pair opens inside a timeout", {
  path <- system.file("scripts", "quiet.applescript", package = "obt")
  skip_if(!nzchar(path), "the script is not installed")

  lines <- readLines(path, warn = FALSE)

  expect_true(any(grepl("with timeout of", lines, fixed = TRUE)))
  expect_true(any(grepl("open workbook", lines, fixed = TRUE)))
})

test_that("both halves of the quiet pair turn the workbook events off", {
  windows <- readLines(driver_script("quiet", os = "windows"), warn = FALSE)
  macos <- readLines(driver_script("quiet", os = "macos"), warn = FALSE)

  expect_true(any(grepl("EnableEvents = False", windows, fixed = TRUE)))
  expect_true(any(grepl("set enable events to false", macos, fixed = TRUE)))
})

test_that("the quiet pair takes its five values in its own order", {
  args <- driver_args(
    "quiet",
    list(
      workbook = "/work/linelist/measles-2026.xlsb",
      switch_name = SILENT_SWITCH_NAME,
      action = QUIET_READ,
      value = NA_character_,
      summary = "/tmp/quiet/measles-2026-obt-summary.txt"
    )
  )

  expect_identical(
    args,
    c(
      "/work/linelist/measles-2026.xlsb",
      SILENT_SWITCH_NAME,
      "read",
      "",
      "/tmp/quiet/measles-2026-obt-summary.txt"
    )
  )
})

test_that("the quiet pair ships with the package", {
  expect_true("quiet" %in% names(DRIVER_PAIRS))
})

test_that("a read builds the command line the script is called with", {
  folder <- withr::local_tempdir()
  runs <- test_quiet_call(stored = SILENT_ON)

  local_mocked_bindings(driver_call = runs$call)

  ran <- driver_quiet(
    workbook = "/work/measles-2026.xlsb",
    action = QUIET_READ,
    summary = summary_path(folder, "measles-2026"),
    os = "macos"
  )

  expect_identical(runs$calls, 1L)
  expect_identical(runs$seen$command, "osascript")
  expect_match(runs$seen$args[[1]], "quiet\\.applescript$")
  expect_identical(runs$seen$args[[2]], "/work/measles-2026.xlsb")
  expect_identical(runs$seen$args[[3]], SILENT_SWITCH_NAME)
  expect_identical(runs$seen$args[[4]], "read")
  expect_identical(runs$seen$args[[5]], "")
  expect_identical(ran$pair, "quiet")
  expect_identical(ran$summary[["silent"]], SILENT_ON)
})

test_that("a write carries the value across to the script", {
  folder <- withr::local_tempdir()
  runs <- test_quiet_call(stored = SILENT_ON)

  local_mocked_bindings(driver_call = runs$call)

  driver_quiet(
    workbook = "C:/work/measles-2026.xlsb",
    action = QUIET_WRITE,
    value = SILENT_ON,
    summary = summary_path(folder, "measles-2026"),
    os = "windows"
  )

  # The Windows runner puts its own lead in front of the script, so every
  # value the pair takes sits one further along than it does on macOS. Every
  # path crosses to the script on the system's own separator.
  expect_identical(runs$seen$command, "cscript")
  expect_identical(runs$seen$args[[1]], "//nologo")
  expect_match(runs$seen$args[[2]], "quiet\\.vbs$")
  expect_identical(runs$seen$args[[3]], "C:\\work\\measles-2026.xlsb")
  expect_identical(runs$seen$args[[5]], "write")
  expect_identical(runs$seen$args[[6]], SILENT_ON)
})

test_that("a quiet run whose answer is lost is read off the summary", {
  folder <- withr::local_tempdir()
  runs <- test_quiet_call(
    stored = SILENT_ON,
    output = "Microsoft Excel got an error: AppleEvent timed out.",
    status = 1L
  )

  local_mocked_bindings(driver_call = runs$call)

  ran <- driver_quiet(
    workbook = "/work/measles-2026.xlsb",
    action = QUIET_READ,
    summary = summary_path(folder, "measles-2026"),
    os = "macos"
  )

  expect_identical(ran$answer, DRIVER_OK)
  expect_identical(ran$source, DRIVER_FROM_FILE)
  expect_identical(ran$summary[["silent"]], SILENT_ON)
})

test_that("a quiet run that answers nothing at all is a failed run", {
  folder <- withr::local_tempdir()
  runs <- test_quiet_call(
    output = "execution error: -609",
    status = 1L,
    writes = FALSE
  )

  local_mocked_bindings(driver_call = runs$call)

  expect_error(
    driver_quiet(
      workbook = "/work/measles-2026.xlsb",
      action = QUIET_READ,
      summary = summary_path(folder, "measles-2026"),
      os = "macos"
    ),
    "gave no answer"
  )
})

test_that("a quiet run Excel refused is reported with its number", {
  folder <- withr::local_tempdir()

  local_mocked_bindings(
    driver_call = test_quiet_call(output = "ERROR 1004: no such name.")$call
  )

  expect_error(
    driver_quiet(
      workbook = "/work/measles-2026.xlsb",
      action = QUIET_WRITE,
      value = SILENT_ON,
      summary = summary_path(folder, "measles-2026"),
      os = "macos"
    ),
    "1004"
  )
})

test_that("reading a workbook answers what the run found", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)
  runs <- test_quiet_run(stored = SILENT_ON)

  local_mocked_bindings(driver_quiet = runs$quiet)

  expect_identical(obt_silent_get(path), SILENT_ON)
  expect_identical(runs$calls, 1L)
  expect_identical(runs$seen$action, QUIET_READ)
  expect_identical(runs$seen$workbook, absolute_path(path))
  expect_identical(runs$seen$switch_name, SILENT_SWITCH_NAME)
  expect_true(is.na(runs$seen$value))
})

test_that("a workbook holding no switch reads as off", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)

  local_mocked_bindings(driver_quiet = test_quiet_run(stored = NA)$quiet)

  expect_identical(obt_silent_get(path), SILENT_OFF)
})

test_that("only Yes reads as silent, whatever case it was stored in", {
  expect_identical(silent_reading("Yes"), SILENT_ON)
  expect_identical(silent_reading("yes"), SILENT_ON)
  expect_identical(silent_reading("  YES  "), SILENT_ON)
  expect_identical(silent_reading("No"), SILENT_OFF)
  expect_identical(silent_reading(""), SILENT_OFF)
  expect_identical(silent_reading("maybe"), SILENT_OFF)
  expect_identical(silent_reading(NA_character_), SILENT_OFF)
  expect_identical(silent_reading(character()), SILENT_OFF)
})

test_that("writing a workbook carries the value and answers what it holds", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)
  runs <- test_quiet_run(stored = SILENT_ON)

  local_mocked_bindings(driver_quiet = runs$quiet)

  expect_identical(obt_silent_set(path, SILENT_ON), SILENT_ON)
  expect_identical(runs$seen$action, QUIET_WRITE)
  expect_identical(runs$seen$value, SILENT_ON)
})

test_that("writing answers invisibly", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)

  local_mocked_bindings(driver_quiet = test_quiet_run()$quiet)

  expect_invisible(obt_silent_set(path, SILENT_ON))
})

test_that("the switch is silent by default and No is asked for by name", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)
  runs <- test_quiet_run()

  local_mocked_bindings(driver_quiet = runs$quiet)

  obt_silent_set(path)
  expect_identical(runs$seen$value, SILENT_ON)

  obt_silent_set(path, SILENT_OFF)
  expect_identical(runs$seen$value, SILENT_OFF)
})

test_that("a value written in another case is stored in the workbooks' own", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)
  runs <- test_quiet_run()

  local_mocked_bindings(driver_quiet = runs$quiet)

  obt_silent_set(path, "yes")
  expect_identical(runs$seen$value, SILENT_ON)

  obt_silent_set(path, " no ")
  expect_identical(runs$seen$value, SILENT_OFF)
})

test_that("a value the switch does not take is refused", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)

  local_mocked_bindings(driver_quiet = test_quiet_run()$quiet)

  expect_error(obt_silent_set(path, "maybe"), "must be")
  expect_error(obt_silent_set(path, TRUE), "must be")
  expect_error(obt_silent_set(path, NA_character_), "must be")
  expect_error(obt_silent_set(path, character()), "must be")
  expect_error(obt_silent_set(path, c("Yes", "No")), "must be")
})

test_that("the run answers where it writes its own summary", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)
  runs <- test_quiet_run()

  local_mocked_bindings(driver_quiet = runs$quiet)

  obt_silent_get(path)

  expect_match(runs$seen$summary, "measles-2026-obt-summary\\.txt$")
  expect_match(basename(dirname(runs$seen$summary)), QUIET_ANSWER_LEAD)
})

test_that("the folder holding the answer is taken away again", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)
  runs <- test_quiet_run()

  local_mocked_bindings(driver_quiet = runs$quiet)

  obt_silent_get(path)

  expect_false(dir.exists(dirname(runs$seen$summary)))
})

test_that("the folder is taken away when the run fails too", {
  folder <- withr::local_tempdir()
  path <- test_workbook(folder)
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(
    driver_quiet = function(workbook, action, summary, ...) {
      seen$summary <- summary
      cli::cli_abort("The {.val quiet} run failed.")
    }
  )

  expect_error(obt_silent_get(path), "failed")
  expect_false(dir.exists(dirname(seen$summary)))
})

test_that("a file that is not a workbook is refused at the verb", {
  folder <- withr::local_tempdir()
  notes <- file.path(folder, "notes.txt")
  writeLines("notes", notes)

  expect_error(obt_silent_get(notes), "must name a workbook")
  expect_error(obt_silent_get(notes), "\\.txt")
  expect_error(obt_silent_set(notes, SILENT_ON), "must name a workbook")

  plain <- file.path(folder, "notes")
  writeLines("notes", plain)

  expect_error(obt_silent_get(plain), "no extension")
})

test_that("a workbook that is not there is refused at the verb", {
  folder <- withr::local_tempdir()

  expect_error(
    obt_silent_get(file.path(folder, "nowhere.xlsb")),
    "Nothing sits at"
  )
  expect_error(obt_silent_get(folder), "must name a workbook")
  expect_error(obt_silent_get(1), "single string")
  expect_error(obt_silent_get(c("a.xlsb", "b.xlsb")), "single string")
  expect_error(obt_silent_get(NA_character_), "single string")
})

test_that("the three workbooks the switch sits on are all read", {
  folder <- withr::local_tempdir()
  runs <- test_quiet_run()

  local_mocked_bindings(driver_quiet = runs$quiet)

  for (extension in SILENT_EXTENSIONS) {
    path <- test_workbook(folder, paste0("measles-2026.", extension))
    expect_identical(obt_silent_get(path), SILENT_ON)
  }

  expect_identical(runs$calls, length(SILENT_EXTENSIONS))
})

test_that("a system that cannot open Excel is refused before anything runs", {
  folder <- withr::local_tempdir()

  expect_error(
    driver_quiet(
      workbook = test_workbook(folder),
      action = QUIET_READ,
      summary = summary_path(folder, "measles-2026"),
      os = "linux"
    ),
    "macOS|Windows"
  )
})
