test_that("every pair the package ships carries both halves", {
  for (pair in names(DRIVER_PAIRS)) {
    for (os in names(DRIVER_RUNNERS)) {
      file <- paste0(pair, ".", DRIVER_RUNNERS[[os]]$extension)
      expect_true(
        nzchar(system.file("scripts", file, package = "obt")),
        info = file
      )
    }
  }
})

test_that("no script sits in the package without its pair", {
  folder <- system.file("scripts", package = "obt")
  skip_if(!nzchar(folder), "the scripts folder is not installed")

  extensions <- vapply(
    DRIVER_RUNNERS,
    function(runner) runner$extension,
    character(1)
  )

  found <- list.files(folder)
  pairs <- unique(tools::file_path_sans_ext(found))

  expect_setequal(pairs, names(DRIVER_PAIRS))
  expect_setequal(unique(tools::file_ext(found)), unname(extensions))
})

test_that("the macOS half wraps its macro call in a timeout", {
  path <- system.file(
    "scripts",
    "designer-generate.applescript",
    package = "obt"
  )
  skip_if(!nzchar(path), "the script is not installed")

  lines <- readLines(path, warn = FALSE)

  expect_true(any(grepl("with timeout of 3600 seconds", lines, fixed = TRUE)))
  expect_true(any(grepl("run VB macro", lines, fixed = TRUE)))
})

test_that("both halves answer in the one shape the seam reads", {
  for (os in names(DRIVER_RUNNERS)) {
    file <- paste0("designer-generate.", DRIVER_RUNNERS[[os]]$extension)
    path <- system.file("scripts", file, package = "obt")
    skip_if(!nzchar(path), "the script is not installed")

    lines <- readLines(path, warn = FALSE)
    answers <- grep("\"(OK|ERROR )", lines, value = TRUE)

    expect_true(any(grepl("\"OK\"", answers, fixed = TRUE)), info = file)
    expect_true(any(grepl("\"ERROR ", answers, fixed = TRUE)), info = file)
  }
})

test_that("a pair resolves to the script of the system it is asked for", {
  windows <- driver_script("designer-generate", os = "windows")
  macos <- driver_script("designer-generate", os = "macos")

  expect_match(windows, "designer-generate\\.vbs$")
  expect_match(macos, "designer-generate\\.applescript$")
  expect_true(file.exists(windows))
  expect_true(file.exists(macos))
})

test_that("an unknown pair is refused", {
  expect_error(driver_script("setup-translate"), "not a script pair")
  expect_error(driver_script(1), "single string")
  expect_error(
    driver_script(c("designer-generate", "designer-generate")),
    "single string"
  )
})

test_that("every pair ships both of its halves", {
  for (pair in names(DRIVER_PAIRS)) {
    for (os in names(DRIVER_RUNNERS)) {
      file <- paste0(pair, ".", DRIVER_RUNNERS[[os]]$extension)
      path <- system.file("scripts", file, package = "obt")

      expect_true(nzchar(path), info = file)
      expect_true(file.exists(path), info = file)
    }
  }
})

test_that("every half names the count of arguments its pair takes", {
  for (pair in names(DRIVER_PAIRS)) {
    wanted <- length(DRIVER_PAIRS[[pair]]$arguments)

    for (os in names(DRIVER_RUNNERS)) {
      file <- paste0(pair, ".", DRIVER_RUNNERS[[os]]$extension)
      path <- system.file("scripts", file, package = "obt")
      skip_if(!nzchar(path), "the script is not installed")

      lines <- readLines(path, warn = FALSE)
      guard <- grep("takes [0-9]+ arguments", lines, value = TRUE)

      expect_length(guard, 1)
      expect_match(guard, paste("takes", wanted, "arguments"), info = file)
    }
  }
})

test_that("the Windows command line runs cscript with the script quoted", {
  command <- driver_command("C:/obt/generate.vbs", c("a b", ""), "windows")

  expect_identical(command$command, "cscript")
  expect_identical(
    command$args,
    c("\"//nologo\"", "\"C:\\obt\\generate.vbs\"", "\"a b\"", "\"\"")
  )
})

test_that("Windows hands every path over on its own separator", {
  command <- driver_command(
    "C:/obt/generate.vbs",
    c("D:/work/designer.xlsb", "measles-2026", ""),
    "windows"
  )

  expect_identical(
    command$args,
    c(
      "\"//nologo\"",
      "\"C:\\obt\\generate.vbs\"",
      "\"D:\\work\\designer.xlsb\"",
      "\"measles-2026\"",
      "\"\""
    )
  )
})

test_that("the macOS command line runs osascript with the script quoted", {
  command <- driver_command(
    "/obt/designer-generate.applescript",
    c("a b", ""),
    "macos"
  )

  expect_identical(command$command, "osascript")
  expect_identical(
    command$args,
    c("'/obt/designer-generate.applescript'", "'a b'", "''")
  )
})

test_that("the arguments of a pair come out in the pair's own order", {
  values <- test_generate_values()
  args <- driver_args("designer-generate", values[rev(names(values))])

  expect_identical(args, unlist(values, use.names = FALSE))
  expect_length(args, length(DRIVER_PAIRS[["designer-generate"]]$arguments))
})

test_that("a value the run does not have goes across as an empty string", {
  values <- test_generate_values()
  values$geo <- NA_character_
  values["ribbon"] <- list(NULL)

  args <- driver_args("designer-generate", values)

  expect_identical(args[[2]], "")
  expect_identical(args[[8]], "")
})

test_that("an argument that is left out or unknown is refused", {
  values <- test_generate_values()

  expect_error(driver_args("designer-generate", values[-1]), "left out")

  values$extra <- "x"
  expect_error(driver_args("designer-generate", values), "not one of them")
})

test_that("a run that answers OK is read from the answer", {
  folder <- withr::local_tempdir()

  local_mocked_bindings(
    driver_call = test_writing_call(summary_path(folder, "measles-2026"))
  )

  result <- driver_generate(
    designer = "designer.xlsb",
    setup = "setup.xlsb",
    folder = folder,
    name = "measles-2026",
    setup_language = "English",
    form_language = "ENG",
    os = "macos"
  )

  expect_identical(result$pair, "designer-generate")
  expect_identical(result$answer, "OK")
  expect_identical(result$source, "answer")
  expect_identical(result$summary[["linelist"]], "measles-2026.xlsb")
  expect_identical(result$summary[["sheets"]], "12")
  expect_identical(
    result$report,
    c("The build finished.", "Nothing was skipped.")
  )
})

test_that("a lost answer is read off the summary file", {
  folder <- withr::local_tempdir()

  local_mocked_bindings(
    driver_call = test_writing_call(
      summary_path(folder, "measles-2026"),
      output = c("Microsoft Excel got an error: AppleEvent timed out.", ""),
      status = 1L
    )
  )

  result <- driver_generate(
    designer = "designer.xlsb",
    setup = "setup.xlsb",
    folder = folder,
    name = "measles-2026",
    setup_language = "English",
    form_language = "ENG",
    os = "macos"
  )

  expect_identical(result$answer, "OK")
  expect_identical(result$source, "file")
  expect_identical(result$summary[["linelist"]], "measles-2026.xlsb")
})

test_that("a lost answer on a run the summary calls refused is a failure", {
  folder <- withr::local_tempdir()

  local_mocked_bindings(
    driver_call = test_writing_call(
      summary_path(folder, "measles-2026"),
      values = c(outcome = "refused", imported = "no"),
      report = c("The file records no language.", "Nothing was imported."),
      output = "execution error: -609",
      status = 1L
    )
  )

  expect_error(
    driver_generate(
      designer = "designer.xlsb",
      setup = "setup.xlsb",
      folder = folder,
      name = "measles-2026",
      setup_language = "English",
      form_language = "ENG",
      os = "macos"
    ),
    "was refused"
  )
})

test_that("a lost answer on a run the summary calls OK is a finished run", {
  folder <- withr::local_tempdir()

  local_mocked_bindings(
    driver_call = test_writing_call(
      summary_path(folder, "measles-2026"),
      values = c(outcome = "OK", export = "setup.xlsx"),
      output = "execution error: -1712",
      status = 1L
    )
  )

  result <- driver_generate(
    designer = "designer.xlsb",
    setup = "setup.xlsb",
    folder = folder,
    name = "measles-2026",
    setup_language = "English",
    form_language = "ENG",
    os = "macos"
  )

  expect_identical(result$source, "file")
  expect_identical(result$summary[["export"]], "setup.xlsx")
})

test_that("a summary says whether the run finished, or says nothing", {
  finished <- list(
    found = TRUE,
    values = c(outcome = "OK"),
    report = character()
  )
  refused <- list(
    found = TRUE,
    values = c(outcome = "refused"),
    report = character()
  )
  quiet <- list(
    found = TRUE,
    values = c(export = "x.xlsx"),
    report = character()
  )
  absent <- list(found = FALSE, values = character(), report = character())

  expect_true(summary_finished(finished))
  expect_false(summary_finished(refused))
  expect_true(is.na(summary_finished(quiet)))
  expect_true(is.na(summary_finished(absent)))

  expect_identical(summary_outcome(refused), "refused")
  expect_identical(summary_outcome(quiet), NA_character_)
})

test_that("the outcome key is read whatever case it was written in", {
  lower <- list(
    found = TRUE,
    values = c(outcome = " ok "),
    report = character()
  )

  expect_true(summary_finished(lower))
})

test_that("a summary left by an earlier run is cleared before the script runs", {
  folder <- withr::local_tempdir()
  stale <- test_summary_file(
    folder,
    values = c(outcome = "OK", linelist = "measles-2026.xlsb")
  )
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(
    driver_call = function(command, args) {
      seen$stale_there <- file.exists(stale)
      list(output = "execution error: -1712", status = 1L)
    }
  )

  expect_error(
    driver_generate(
      designer = "designer.xlsb",
      setup = "setup.xlsb",
      folder = folder,
      name = "measles-2026",
      setup_language = "English",
      form_language = "ENG",
      os = "macos"
    ),
    "gave no answer"
  )
  expect_false(seen$stale_there)
})

test_that("a run that writes no summary has nothing to clear", {
  local_mocked_bindings(driver_call = test_driver_call("OK"))

  result <- driver_generate(
    designer = "designer.xlsb",
    setup = "setup.xlsb",
    folder = withr::local_tempdir(),
    name = "measles-2026",
    setup_language = "English",
    form_language = "ENG",
    summary = NA_character_,
    os = "macos"
  )

  expect_identical(result$answer, "OK")
  expect_identical(forget_summary(NA_character_), NULL)
  expect_identical(forget_summary(character()), NULL)
})

test_that("a lost answer with no summary file is a failed run", {
  folder <- withr::local_tempdir()

  local_mocked_bindings(
    driver_call = test_driver_call("execution error: -1712", status = 1L)
  )

  expect_error(
    driver_generate(
      designer = "designer.xlsb",
      setup = "setup.xlsb",
      folder = folder,
      name = "measles-2026",
      setup_language = "English",
      form_language = "ENG",
      os = "macos"
    ),
    "gave no answer"
  )
})

test_that("a failed run carries the number and the text Excel gave", {
  folder <- withr::local_tempdir()
  test_summary_file(folder)

  local_mocked_bindings(
    driver_call = test_driver_call("ERROR 1004: Range RNG_LLDir is missing.")
  )

  expect_error(
    driver_generate(
      designer = "designer.xlsb",
      setup = "setup.xlsb",
      folder = folder,
      name = "measles-2026",
      setup_language = "English",
      form_language = "ENG",
      os = "macos"
    ),
    "RNG_LLDir is missing"
  )
})

test_that("a failure numbered zero is still a failure", {
  # The setup export refuses quietly when it has no folder to write into, and
  # the wrapper reports that with no error number of its own. A zero is a
  # number like any other here.
  folder <- withr::local_tempdir()

  local_mocked_bindings(
    driver_call = test_driver_call(
      "ERROR 0: the export wrote no file into /work/export."
    )
  )

  expect_error(
    driver_generate(
      designer = "designer.xlsb",
      setup = "setup.xlsb",
      folder = folder,
      name = "measles-2026",
      setup_language = "English",
      form_language = "ENG",
      os = "macos"
    ),
    "wrote no file"
  )

  expect_identical(
    driver_answer("ERROR 0: nothing was written"),
    "ERROR 0: nothing was written"
  )
  expect_identical(driver_failure("ERROR 0: nothing was written")$number, "0")
})

test_that("a run on a system that cannot open Excel is refused", {
  local_mocked_bindings(driver_call = test_driver_call("OK"))

  expect_error(
    driver_generate(
      designer = "designer.xlsb",
      setup = "setup.xlsb",
      folder = tempdir(),
      name = "measles-2026",
      setup_language = "English",
      form_language = "ENG",
      os = "linux"
    ),
    "macOS or Windows"
  )
})

test_that("the answer line is picked out of what Excel printed around it", {
  expect_identical(driver_answer(c("loading", "OK")), "OK")
  expect_identical(driver_answer(c("  OK  ", "")), "OK")
  expect_identical(
    driver_answer(c("OK", "ERROR 9: too late")),
    "ERROR 9: too late"
  )
  expect_identical(driver_answer(c("OKAY", "almost OK")), NA_character_)
  expect_identical(driver_answer(character()), NA_character_)
})

test_that("a failed answer is read into its number and its text", {
  failure <- driver_failure("ERROR -1712: AppleEvent timed out.")

  expect_identical(failure$number, "-1712")
  expect_identical(failure$text, "AppleEvent timed out.")
})

test_that("a summary file is read as values, then free text", {
  folder <- withr::local_tempdir()
  path <- test_summary_file(folder)

  read <- read_summary(path)

  expect_true(read$found)
  expect_identical(names(read$values), c("linelist", "sheets"))
  expect_identical(
    read$report,
    c("The build finished.", "Nothing was skipped.")
  )
})

test_that("a summary in the Windows codepage is read whole", {
  folder <- withr::local_tempdir()
  path <- summary_path(folder, "measles-2026")

  # The bullet Excel writes in its own error text: byte 0x95 in the ANSI
  # codepage, an invalid byte when the file is read as UTF-8.
  con <- file(path, open = "wb")
  writeLines(
    c(
      "outcome=ERROR 1004: cannot access the file 'OBTApp_'.",
      "",
      "\x95 The file name or path does not exist.",
      SUMMARY_MARKER,
      "\x95 The build gave up."
    ),
    con,
    useBytes = TRUE
  )
  close(con)

  read <- read_summary(path)

  expect_true(read$found)
  expect_match(read$values[["outcome"]], "^ERROR 1004")
  expect_length(read$report, 1)
  expect_true(all(validUTF8(read$values)))
  expect_true(all(validUTF8(read$report)))
})

test_that("a summary already in UTF-8 keeps its text", {
  folder <- withr::local_tempdir()
  path <- summary_path(folder, "measles-2026")
  writeLines(
    c("linelist=déjà.xlsb", SUMMARY_MARKER, "déjà vu"),
    path
  )

  read <- read_summary(path)

  expect_identical(read$values[["linelist"]], "déjà.xlsb")
  expect_identical(read$report, "déjà vu")
})

test_that("a summary file with no marker is all values", {
  folder <- withr::local_tempdir()
  path <- summary_path(folder, "measles-2026")
  writeLines(c("export=out.xlsx", "imported=Yes"), path)

  read <- read_summary(path)

  expect_true(read$found)
  expect_identical(read$values[["export"]], "out.xlsx")
  expect_identical(read$values[["imported"]], "Yes")
  expect_identical(read$report, character())
})

test_that("a summary file keeps the whole of a value that holds a separator", {
  folder <- withr::local_tempdir()
  path <- summary_path(folder, "measles-2026")
  writeLines(c("linelist=C:/work/a=b.xlsb", "", "=lost", SUMMARY_MARKER), path)

  read <- read_summary(path)

  expect_identical(read$values[["linelist"]], "C:/work/a=b.xlsb")
  expect_length(read$values, 1)
  expect_identical(read$report, character())
})

test_that("a summary file that is missing reads as nothing found", {
  read <- read_summary(file.path(tempdir(), "nowhere-obt-summary.txt"))

  expect_false(read$found)
  expect_identical(read$values, character())
  expect_identical(read$report, character())

  expect_false(read_summary(NA_character_)$found)
  expect_false(read_summary(character())$found)
})

test_that("a summary file sits beside the output, under its name", {
  path <- summary_path("/work/linelist", "measles-2026")

  expect_identical(path, "/work/linelist/measles-2026-obt-summary.txt")
})

test_that("every half calls the entry point its operation names", {
  called <- c(
    `setup-export` = "RunSetupExport",
    `setup-import` = "RunSetupImportFile",
    `setup-tags` = "RunSetupTags",
    `linelist-geobase` = "RunImportGeobase",
    `linelist-import` = "RunImportData",
    `linelist-export` = "RunExport"
  )

  for (pair in names(called)) {
    entry <- operation_spec(pair)$entry_point
    expect_identical(entry, unname(called[[pair]]), info = pair)

    for (os in names(DRIVER_RUNNERS)) {
      file <- paste0(pair, ".", DRIVER_RUNNERS[[os]]$extension)
      path <- system.file("scripts", file, package = "obt")
      skip_if(!nzchar(path), "the script is not installed")

      lines <- readLines(path, warn = FALSE)

      expect_true(any(grepl(entry, lines, fixed = TRUE)), info = file)
    }
  }
})

test_that("every entry point a recipe names is called by a shipped half", {
  entries <- vapply(
    names(OBT_OPERATIONS),
    function(type) operation_spec(type)$entry_point,
    character(1)
  )

  entries <- unique(entries[!is.na(entries)])

  shipped <- unlist(lapply(names(DRIVER_PAIRS), function(pair) {
    path <- system.file(
      "scripts",
      paste0(pair, ".vbs"),
      package = "obt"
    )

    if (!nzchar(path)) {
      return(character())
    }

    readLines(path, warn = FALSE)
  }))

  skip_if(length(shipped) == 0, "the scripts are not installed")

  for (entry in entries) {
    expect_true(any(grepl(entry, shipped, fixed = TRUE)), info = entry)
  }
})

test_that("every pair that opens a workbook takes its password beside it", {
  opening <- c(
    "quiet",
    "linelist-geobase",
    "linelist-import",
    "linelist-export"
  )

  for (pair in opening) {
    arguments <- DRIVER_PAIRS[[pair]]$arguments
    expect_identical(arguments[[2]], "password", info = pair)
  }

  expect_identical(
    DRIVER_PAIRS[["linelist-export"]]$arguments,
    c("linelist", "password", "name", "folder", "other_password", "other")
  )
})

test_that("the generate pair takes the two passwords of the linelist last", {
  arguments <- DRIVER_PAIRS[["designer-generate"]]$arguments

  expect_identical(
    utils::tail(arguments, 2),
    c("password", "debug_password")
  )
})

test_that("both halves of a workbook pair open the file with its password", {
  opening <- c(
    "quiet",
    "linelist-geobase",
    "linelist-import",
    "linelist-export"
  )

  for (pair in opening) {
    windows <- readLines(driver_script(pair, os = "windows"), warn = FALSE)
    macos <- readLines(driver_script(pair, os = "macos"), warn = FALSE)

    expect_true(
      any(grepl("Workbooks\\.Open\\(.*[Pp]assword", windows)),
      info = pair
    )
    expect_true(any(grepl("open workbook .*password", macos)), info = pair)
  }
})

test_that("both halves of the generate pair write the two password ranges", {
  for (os in names(DRIVER_RUNNERS)) {
    lines <- readLines(
      driver_script("designer-generate", os = os),
      warn = FALSE
    )

    expect_true(any(grepl("RNG_LLPwdOpen", lines, fixed = TRUE)), info = os)
    expect_true(any(grepl("RNG_LLPassword", lines, fixed = TRUE)), info = os)
  }
})

test_that("an optional value crosses as NA when the recipe recorded none", {
  expect_true(is.na(optional_text(NULL)))
  expect_identical(optional_text("pw"), "pw")
  expect_identical(optional_text(" two words "), " two words ")
})
