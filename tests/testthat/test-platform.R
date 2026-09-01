# The operating system ---------------------------------------------------

test_that("the operating system is one of the three shapes", {
  expect_true(
    os_name() %in%
      c(
        "macos",
        "windows",
        tolower(
          Sys.info()[["sysname"]]
        )
      )
  )
  expect_type(os_name(), "character")
  expect_length(os_name(), 1)
})

test_that("macOS and Windows are told apart", {
  local_mocked_bindings(os_name = function() "macos")
  expect_true(is_macos())
  expect_false(is_windows())
})

test_that("a system that can open Excel passes the guard", {
  expect_invisible(check_supported_os("macos"))
  expect_identical(check_supported_os("windows"), "windows")
})

test_that("any other system stops a run and names the two that work", {
  expect_error(check_supported_os("linux"), "macOS or Windows")
  expect_error(check_supported_os("linux"), "linux")
  expect_error(check_supported_os("sunos"), "builds and prints")
})

test_that("the guard reads the system when it is given none", {
  local_mocked_bindings(os_name = function() "linux")
  expect_error(check_supported_os(), "macOS or Windows")
})

# The record of this machine ---------------------------------------------

test_that("obt_platform answers every field of the record", {
  found <- obt_platform()

  expect_s3_class(found, "obt_platform")
  expect_named(
    found,
    c("os", "supported", "staging", "excel_version", "excel_proven", "app")
  )
  expect_length(found$os, 1)
  expect_type(found$supported, "logical")
  expect_type(found$staging, "character")
  expect_type(found$app, "character")
  expect_true(nzchar(found$app))
})

test_that("the record answers this machine", {
  found <- obt_platform()

  expect_identical(found$os, os_name())
  expect_identical(found$supported, os_name() %in% SUPPORTED_OS)
})

test_that("Excel is read on macOS alone", {
  local_mocked_bindings(os_name = function() "linux")

  found <- obt_platform()

  expect_true(is.na(found$excel_version))
  expect_false(found$excel_proven)
  expect_true(is.na(found$staging))
})

test_that("the record prints the machine in plain lines", {
  said <- printed(print(test_platform(staging = "/stage")))

  expect_match(said, "This machine")
  expect_match(said, "macos")
  expect_match(said, "/stage")
  expect_match(said, "16.111")
  expect_match(said, "Positron")
})

test_that("the record prints how to switch the staging folder on", {
  said <- printed(print(test_platform(staging = NA_character_)))

  expect_match(said, "Full Disk Access")
  expect_match(said, "your own folder")
})

test_that("a version that was never proven prints beside the proven one", {
  said <- printed(print(test_platform(
    staging = "/stage",
    excel_version = "16.104"
  )))

  expect_match(said, "16.104")
  expect_match(said, "16.111 is the proven one")
})

test_that("a version that could not be read prints as such", {
  said <- printed(print(test_platform(excel_version = NA_character_)))

  expect_match(said, "could not be read")
})

test_that("Windows prints no Excel version", {
  said <- printed(print(test_platform(os = "windows")))

  expect_match(said, "read on macOS only")
})

test_that("print answers the record unchanged and invisibly", {
  record <- test_platform(staging = "/stage")

  printed(expect_invisible(print(record)))
  expect_identical(printed(print(record)), printed(print(record)))
})

# What the log carries ---------------------------------------------------

test_that("the log carries the version whether it was proven or not", {
  proven <- platform_log_lines(test_platform(excel_version = "16.111"))
  other <- platform_log_lines(test_platform(excel_version = "16.104"))

  expect_true(any(grepl("Excel version: 16.111", proven, fixed = TRUE)))
  expect_true(any(grepl("Excel version: 16.104", other, fixed = TRUE)))
  expect_true(any(grepl("Excel version proven: 16.111", other, fixed = TRUE)))
})

test_that("the log names the folder a run staged itself in", {
  staged <- platform_log_lines(test_platform(staging = "/stage"))
  plain <- platform_log_lines(test_platform(staging = NA_character_))

  expect_true(any(grepl("Staging folder: /stage", staged, fixed = TRUE)))
  expect_true(any(grepl("working folder", plain)))
})

# The staging folder -----------------------------------------------------

test_that("the staging folder sits inside Excel's own folder", {
  local_mocked_bindings(os_name = function() "macos")

  root <- staging_root()

  expect_match(root, "com.microsoft.Excel")
  expect_identical(basename(root), STAGING_FOLDER)
})

test_that("Windows has no staging folder", {
  local_mocked_bindings(os_name = function() "windows")

  expect_true(is.na(staging_root()))
  expect_false(staging_available(staging_root()))
})

test_that("a folder that can be written answers as reachable", {
  container <- withr::local_tempdir()
  local_mocked_bindings(
    os_name = function() "macos",
    container_documents = function() container
  )

  expect_true(staging_available())
  expect_true(dir.exists(staging_root()))
})

test_that("the probe file is removed again", {
  container <- withr::local_tempdir()
  local_mocked_bindings(
    os_name = function() "macos",
    container_documents = function() container
  )

  staging_available()

  expect_false(file.exists(file.path(staging_root(), STAGING_PROBE)))
})

test_that("a folder that cannot be written falls back", {
  container <- withr::local_tempdir()
  file.create(file.path(container, STAGING_FOLDER))

  local_mocked_bindings(
    os_name = function() "macos",
    container_documents = function() container
  )

  expect_false(staging_available())
  expect_true(is.na(obt_platform()$staging))
})

test_that("a container that is not there falls back", {
  container <- file.path(withr::local_tempdir(), "no-excel-here")
  local_mocked_bindings(
    os_name = function() "macos",
    container_documents = function() container
  )

  expect_false(staging_available())
})

# What the user is told --------------------------------------------------

test_that("the fallback names the app and Full Disk Access, once", {
  reset_platform_state()
  withr::defer(reset_platform_state())

  expect_warning(
    warn_no_staging("/work", app = "Positron"),
    "Full Disk Access"
  )
  expect_no_warning(warn_no_staging("/work", app = "Positron"))
})

test_that("the fallback message says what to do", {
  reset_platform_state()
  withr::defer(reset_platform_state())

  said <- tryCatch(
    warn_no_staging("/work", app = "Positron"),
    warning = function(cnd) conditionMessage(cnd)
  )

  expect_match(said, "Privacy & Security")
  expect_match(said, "Positron")
  expect_match(said, "/work")
})

test_that("the proven version of Excel passes with nothing said", {
  expect_no_warning(warn_excel_version(test_platform()))
  expect_false(warn_excel_version(test_platform()))
})

test_that("any other version warns and names the proven one", {
  expect_warning(
    warn_excel_version(test_platform(excel_version = "16.104")),
    "16.111"
  )
  expect_warning(
    warn_excel_version(test_platform(excel_version = NA_character_)),
    "could not be read"
  )
})

test_that("Windows is never warned about a version", {
  expect_no_warning(
    warn_excel_version(test_platform(os = "windows", excel_version = NA))
  )
})

test_that("the guard refuses a system that cannot open Excel", {
  local_mocked_bindings(os_name = function() "linux")

  expect_error(
    platform_guard(obt(folder = tempdir())),
    "macOS or Windows"
  )
})

test_that("the guard refuses anything that is not a recipe", {
  expect_error(platform_guard("/work"), "must be an")
})

# Reading the version of Excel -------------------------------------------

test_that("the version is read off the application bundle", {
  skip_if_not_macos()

  app <- test_excel_app(withr::local_tempdir(), "16.111")

  expect_identical(excel_version(app), "16.111")
})

test_that("another version is read as it stands", {
  skip_if_not_macos()

  app <- test_excel_app(withr::local_tempdir(), "16.104")

  expect_identical(excel_version(app), "16.104")
  expect_false(identical(excel_version(app), EXCEL_VERSION_PROVEN))
})

test_that("a bundle with no plist reads as unknown", {
  app <- file.path(withr::local_tempdir(), "Microsoft Excel.app")
  ensure_folder(app)

  expect_true(is.na(excel_version(app)))
})

test_that("no Excel at all reads as unknown", {
  expect_true(is.na(excel_version(NA_character_)))
  expect_true(is.na(excel_version(character())))
})

test_that("a machine with Excel on it is read as supported", {
  skip_if_no_excel()

  found <- obt_platform()

  expect_true(found$supported)
  expect_true(found$os %in% SUPPORTED_OS)
})

test_that("the version on this machine reads as a version", {
  skip_if_no_excel()
  skip_if_not_macos()

  expect_match(obt_platform()$excel_version, "^[0-9]+\\.[0-9]+")
})

test_that("a command that is missing answers nothing", {
  expect_identical(run_quietly("no-such-command-anywhere"), character())
})

# The app running the R session ------------------------------------------

test_that("the outer bundle of a helper process is the one named", {
  helper <- paste0(
    "/Applications/Positron.app/Contents/Frameworks/",
    "Positron Helper (Plugin).app/Contents/MacOS/Positron Helper"
  )

  expect_identical(app_bundle_name(helper), "Positron")
  expect_identical(
    app_bundle_name("/Applications/Utilities/Terminal.app/Contents/MacOS/x"),
    "Terminal"
  )
  expect_true(is.na(app_bundle_name("/usr/local/bin/R")))
})

test_that("the environment names the app when the tree gives nothing", {
  withr::local_envvar(
    POSITRON = "",
    RSTUDIO = "",
    TERM_PROGRAM = "Apple_Terminal"
  )

  expect_identical(app_from_environment(), "Terminal")

  withr::local_envvar(TERM_PROGRAM = "", RSTUDIO = "1")

  expect_identical(app_from_environment(), "RStudio")
})

test_that("an app nothing names still answers a string", {
  local_mocked_bindings(
    app_from_process_tree = function(...) NA_character_,
    app_from_environment = function() NA_character_
  )

  expect_identical(session_app(), UNKNOWN_APP)
})

test_that("the app is always a single string", {
  expect_type(session_app(), "character")
  expect_length(session_app(), 1)
  expect_true(nzchar(session_app()))
})

# Staging a run ----------------------------------------------------------

test_that("a machine with no staging folder works in the user's folder", {
  work <- withr::local_tempdir()
  stage <- stage_open(obt(folder = work), platform = test_platform())

  expect_false(stage$staged)
  expect_identical(stage$root, absolute_path(work))
  expect_identical(stage$folder, absolute_path(work))
})

test_that("a staged run gets a folder of its own, stamped", {
  work <- withr::local_tempdir()
  root <- withr::local_tempdir()
  when <- as.POSIXct("2026-08-31 14:05:09", tz = "UTC")

  stage <- stage_open(
    obt(folder = work),
    platform = test_platform(staging = root),
    when = when
  )

  expect_true(stage$staged)
  expect_true(dir.exists(stage$root))
  expect_match(basename(stage$root), "^run-20260831-140509-[0-9]+$")
  expect_identical(dirname(stage$root), absolute_path(root))
})

test_that("a file under the working folder keeps its place in the stage", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))

  expect_identical(
    stage_path(stage, file.path(work, "geo", "regions.xlsx")),
    file.path(stage$root, "geo", "regions.xlsx")
  )
})

test_that("a file from outside the working folder is staged under in", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))

  expect_identical(
    stage_path(stage, "/home/me/regions.xlsx"),
    file.path(stage$root, STAGE_OUTSIDE, "regions.xlsx")
  )
})

test_that("a run that stages nothing answers the path it was given", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, work, staged = FALSE)
  here <- file.path(work, "geo", "regions.xlsx")

  expect_identical(stage_path(stage, here), absolute_path(here))
})

test_that("staging a file copies it and leaves the original alone", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))
  source <- file.path(work, "geo", "regions.xlsx")

  ensure_folder(dirname(source))
  writeLines("regions", source)

  staged <- stage_in(stage, source)

  expect_identical(staged, file.path(stage$root, "geo", "regions.xlsx"))
  expect_true(file.exists(staged))
  expect_true(file.exists(source))
  expect_identical(readLines(staged), "regions")
})

test_that("staging a file that is not there is reported", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))

  expect_error(
    stage_in(stage, file.path(work, "geo", "gone.xlsx")),
    "is not there"
  )
})

test_that("a run that stages nothing copies nothing", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, work, staged = FALSE)
  source <- file.path(work, "regions.xlsx")

  writeLines("regions", source)

  expect_identical(stage_in(stage, source), absolute_path(source))
})

test_that("a file the run produced is moved back out", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))
  made <- file.path(stage$root, "linelist", "measles-2026.xlsb")

  ensure_folder(dirname(made))
  writeLines("linelist", made)

  target <- file.path(work, "linelist", "measles-2026.xlsb")
  answered <- stage_out(stage, target)

  expect_identical(answered, absolute_path(target))
  expect_true(file.exists(target))
  expect_false(file.exists(made))
  expect_identical(readLines(target), "linelist")
})

test_that("a file the run never produced stops the run", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))

  expect_error(
    stage_out(stage, file.path(work, "linelist", "measles-2026.xlsb")),
    "was not written"
  )
})

test_that("a file the run may have skipped answers NA", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))

  expect_true(is.na(
    stage_out(stage, file.path(work, "obt-summary.txt"), required = FALSE)
  ))
})

test_that("closing moves what is left back and clears the folder", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))
  changed <- file.path(stage$root, "setup", "setup.xlsb")

  ensure_folder(dirname(changed))
  writeLines("changed by Excel", changed)

  stage_close(stage)

  expect_false(dir.exists(stage$root))
  expect_true(file.exists(file.path(work, "setup", "setup.xlsb")))
  expect_identical(
    readLines(file.path(work, "setup", "setup.xlsb")),
    "changed by Excel"
  )
})

test_that("a file staged from outside stays out of the working folder", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))
  outside <- file.path(stage$root, STAGE_OUTSIDE, "regions.xlsx")

  ensure_folder(dirname(outside))
  writeLines("regions", outside)

  stage_close(stage)

  expect_false(dir.exists(stage$root))
  expect_false(file.exists(file.path(work, STAGE_OUTSIDE, "regions.xlsx")))
})

test_that("closing a run that staged nothing removes nothing", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, work, staged = FALSE)

  writeLines("kept", file.path(work, "kept.txt"))
  stage_close(stage)

  expect_true(dir.exists(work))
  expect_true(file.exists(file.path(work, "kept.txt")))
})

test_that("closing answers the record invisibly", {
  work <- withr::local_tempdir()
  stage <- test_stage(work, file.path(withr::local_tempdir(), "run-1"))

  expect_invisible(stage_close(stage))
})

# Clearing what a run left behind ----------------------------------------

test_that("a run folder left behind is swept once it is old enough", {
  root <- withr::local_tempdir()
  old <- file.path(root, "run-20260101-090000-11")
  fresh <- file.path(root, "run-20260831-140509-22")

  ensure_folder(old)
  ensure_folder(fresh)
  Sys.setFileTime(old, Sys.time() - 60 * 60 * 48)

  swept <- stage_sweep(root)

  expect_identical(swept, old)
  expect_false(dir.exists(old))
  expect_true(dir.exists(fresh))
})

test_that("a folder that is not ours is left alone", {
  root <- withr::local_tempdir()
  theirs <- file.path(root, "OBTHome")

  ensure_folder(theirs)
  Sys.setFileTime(theirs, Sys.time() - 60 * 60 * 48)

  stage_sweep(root)

  expect_true(dir.exists(theirs))
})

test_that("a staging folder that is not there sweeps nothing", {
  expect_identical(stage_sweep(NA_character_), character())
  expect_identical(
    stage_sweep(file.path(withr::local_tempdir(), "gone")),
    character()
  )
})

test_that("opening a run sweeps what an older run left", {
  work <- withr::local_tempdir()
  root <- withr::local_tempdir()
  old <- file.path(root, "run-20260101-090000-11")

  ensure_folder(old)
  Sys.setFileTime(old, Sys.time() - 60 * 60 * 48)

  stage_open(obt(folder = work), platform = test_platform(staging = root))

  expect_false(dir.exists(old))
})

test_that("obt_staging_clean answers nothing where there is no staging", {
  local_mocked_bindings(os_name = function() "windows")

  expect_identical(obt_staging_clean(), character())
})

test_that("obt_staging_clean empties the folder and takes it away", {
  container <- withr::local_tempdir()
  local_mocked_bindings(
    os_name = function() "macos",
    container_documents = function() container
  )

  root <- staging_root()
  left <- file.path(root, "run-20260831-140509-22")
  ensure_folder(left)

  removed <- obt_staging_clean()

  expect_identical(removed, left)
  expect_false(dir.exists(left))
  expect_false(dir.exists(root))
})

# Moving and copying -----------------------------------------------------

test_that("a copy that cannot be made is reported", {
  work <- withr::local_tempdir()

  expect_error(
    copy_file(
      file.path(work, "gone.txt"),
      file.path(work, "copy.txt")
    ),
    "could not be copied"
  )
})

test_that("a move writes over what is already at the destination", {
  work <- withr::local_tempdir()
  from <- file.path(work, "new.txt")
  to <- file.path(work, "old.txt")

  writeLines("new", from)
  writeLines("old", to)

  move_file(from, to)

  expect_identical(readLines(to), "new")
  expect_false(file.exists(from))
})

# The rule about reaching the system -------------------------------------

test_that("the package reaches the system through system2 alone", {
  source_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(source_dir), "package sources are not there")

  lines <- unlist(lapply(
    list.files(source_dir, pattern = "\\.R$", full.names = TRUE),
    readLines,
    warn = FALSE
  ))

  expect_false(any(grepl("\\bshell\\s*\\(", sub("#.*$", "", lines))))
})
