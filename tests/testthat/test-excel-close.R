# A recorder for every command the close reaches the system with.
#
# `driver_call()` is the seam that runs a command, so a test mocks it with
# this and Excel is never touched. `state$commands` and `state$args` keep
# each call in order.
recording_calls <- function(status = 0L, output = character()) {
  state <- new.env(parent = emptyenv())
  state$commands <- character()
  state$args <- list()

  state$call <- function(command, args) {
    state$commands <- c(state$commands, command)
    state$args <- c(state$args, list(as.character(args)))

    list(output = as.character(output), status = as.integer(status))
  }

  state
}

# A listing that answers one value per call, then the last one for good.
listing_in_turn <- function(...) {
  answers <- list(...)
  state <- new.env(parent = emptyenv())
  state$turn <- 0L

  function(os = os_name()) {
    state$turn <- min(state$turn + 1L, length(answers))
    answers[[state$turn]]
  }
}

# Reading the processes -------------------------------------------------

test_that("the Windows listing is read off the CSV rows of tasklist", {
  lines <- c(
    "\"EXCEL.EXE\",\"4120\",\"Console\",\"1\",\"132,456 K\"",
    "\"EXCEL.EXE\",\"9876\",\"Console\",\"1\",\"98,000 K\""
  )

  expect_identical(excel_pids_from_tasklist(lines), c(4120L, 9876L))
})

test_that("the Windows listing ignores the line tasklist prints for none", {
  lines <- "INFO: No tasks are running which match the specified criteria."

  expect_identical(excel_pids_from_tasklist(lines), integer())
  expect_identical(excel_pids_from_tasklist(character()), integer())
})

test_that("the Windows listing keeps Excel alone and reads any case", {
  lines <- c(
    "\"excel.exe\",\"4120\",\"Console\",\"1\",\"132,456 K\"",
    "\"WINWORD.EXE\",\"5000\",\"Console\",\"1\",\"50,000 K\"",
    "\"EXCEL.EXE\",\"4120\",\"Console\",\"1\",\"132,456 K\""
  )

  expect_identical(excel_pids_from_tasklist(lines), 4120L)
})

test_that("the macOS listing is read off the lines of pgrep", {
  expect_identical(excel_pids_from_lines(c("512", " 640 ", "")), c(512L, 640L))
  expect_identical(excel_pids_from_lines(character()), integer())
  expect_identical(excel_pids_from_lines("no such process"), integer())
})

test_that("the listing asks each system its own tool", {
  asked <- new.env(parent = emptyenv())
  local_mocked_bindings(
    run_quietly = function(command, args = character()) {
      asked$command <- command
      asked$args <- args
      character()
    }
  )

  excel_pids("windows")
  expect_identical(asked$command, "tasklist")
  expect_true(any(grepl("EXCEL.EXE", asked$args, fixed = TRUE)))

  excel_pids("macos")
  expect_identical(asked$command, "pgrep")
  expect_true(any(grepl("Microsoft Excel", asked$args, fixed = TRUE)))
})

test_that("the listing on this machine answers whole numbers", {
  skip_if_no_run()

  found <- excel_pids()

  expect_type(found, "integer")
  expect_false(anyNA(found))
})

# The commands each system closes Excel with ----------------------------

test_that("Windows closes every process it found by number, by force", {
  command <- close_excel_command(c(4120L, 9876L), "windows")

  expect_identical(command$command, "taskkill")
  expect_identical(
    command$args,
    c("/F", "/PID", "4120", "/PID", "9876")
  )
})

test_that("macOS asks Excel to quit and save nothing, through AppleScript", {
  command <- close_excel_command(c(512L, 640L), "macos")

  expect_identical(command$command, "osascript")
  expect_identical(command$args[[1]], "-e")
  expect_match(command$args[[2]], "Microsoft Excel", fixed = TRUE)
  expect_match(command$args[[2]], "quit saving no", fixed = TRUE)
  expect_match(command$args[[2]], "with timeout of", fixed = TRUE)
})

# Closing -----------------------------------------------------------------

test_that("closing on Windows runs taskkill once and answers the numbers", {
  calls <- recording_calls()
  local_mocked_bindings(driver_call = calls$call)

  closed <- close_excel(c(4120L, 9876L), "windows")

  expect_identical(closed, c(4120L, 9876L))
  expect_identical(calls$commands, "taskkill")
  expect_identical(calls$args[[1]], c("/F", "/PID", "4120", "/PID", "9876"))
})

test_that("a taskkill that fails is reported with what it printed", {
  calls <- recording_calls(status = 1L, output = "ERROR: Access is denied.")
  local_mocked_bindings(driver_call = calls$call)

  expect_error(close_excel(4120L, "windows"), "could not be closed")
  expect_error(close_excel(4120L, "windows"), "Access is denied")
})

test_that("closing on macOS quits through AppleScript and waits it out", {
  calls <- recording_calls()
  local_mocked_bindings(
    driver_call = calls$call,
    excel_pids = listing_in_turn(integer())
  )

  closed <- close_excel(c(512L, 640L), "macos", wait = 0)

  expect_identical(closed, c(512L, 640L))
  expect_identical(calls$commands, "osascript")
})

test_that("a process still there after the quit is killed by number", {
  calls <- recording_calls()
  local_mocked_bindings(
    driver_call = calls$call,
    excel_pids = listing_in_turn(640L, integer())
  )

  closed <- close_excel(c(512L, 640L), "macos", wait = 0)

  expect_identical(closed, c(512L, 640L))
  expect_identical(calls$commands, c("osascript", "kill"))
  expect_identical(calls$args[[2]], c("-9", "640"))
})

test_that("a process that survives the kill is reported by number", {
  calls <- recording_calls()
  local_mocked_bindings(
    driver_call = calls$call,
    excel_pids = listing_in_turn(640L)
  )

  expect_error(close_excel(c(512L, 640L), "macos", wait = 0), "640")
  expect_error(
    close_excel(c(512L, 640L), "macos", wait = 0),
    "could not be closed"
  )
})

# The verb ----------------------------------------------------------------

test_that("a system with no Excel on it is turned down", {
  local_mocked_bindings(os_name = function() "linux")

  expect_error(obt_close_excel(), "macOS or Windows")
  expect_error(obt_close_excel(), "linux")
})

test_that("ask takes TRUE or FALSE", {
  expect_error(obt_close_excel(ask = "yes"), "TRUE.*FALSE")
  expect_error(obt_close_excel(ask = NA), "TRUE.*FALSE")
})

test_that("no Excel running is said, and nothing is asked or closed", {
  calls <- recording_calls()
  local_mocked_bindings(
    os_name = function() "windows",
    excel_pids = function(os = os_name()) integer(),
    ask_yes = function(question) stop("nobody should be asked"),
    driver_call = calls$call
  )

  said <- printed(closed <- obt_close_excel())

  expect_match(said, "No Excel is running")
  expect_identical(closed, integer())
  expect_length(calls$commands, 0)
})

test_that("the question names how many instances would close", {
  asked <- new.env(parent = emptyenv())
  local_mocked_bindings(
    os_name = function() "windows",
    session_is_interactive = function() TRUE,
    excel_pids = function(os = os_name()) c(4120L, 9876L),
    ask_yes = function(question) {
      asked$question <- question
      FALSE
    }
  )

  printed(obt_close_excel())

  expect_match(asked$question, "2 Excel instances")
  expect_match(asked$question, "unsaved", ignore.case = TRUE)
})

test_that("the question for one instance reads in the singular", {
  asked <- new.env(parent = emptyenv())
  local_mocked_bindings(
    os_name = function() "windows",
    session_is_interactive = function() TRUE,
    excel_pids = function(os = os_name()) 4120L,
    ask_yes = function(question) {
      asked$question <- question
      FALSE
    }
  )

  printed(obt_close_excel())

  expect_match(asked$question, "1 Excel instance ")
})

test_that("a no leaves every instance running", {
  calls <- recording_calls()
  local_mocked_bindings(
    os_name = function() "windows",
    session_is_interactive = function() TRUE,
    excel_pids = function(os = os_name()) c(4120L, 9876L),
    ask_yes = function(question) FALSE,
    driver_call = calls$call
  )

  said <- printed(closed <- obt_close_excel())

  expect_match(said, "Nothing was closed")
  expect_identical(closed, integer())
  expect_length(calls$commands, 0)
})

test_that("a yes closes every instance and answers their numbers", {
  calls <- recording_calls()
  local_mocked_bindings(
    os_name = function() "windows",
    session_is_interactive = function() TRUE,
    excel_pids = function(os = os_name()) c(4120L, 9876L),
    ask_yes = function(question) TRUE,
    driver_call = calls$call
  )

  said <- printed(closed <- obt_close_excel())

  expect_match(said, "Closed 2 Excel instances")
  expect_identical(closed, c(4120L, 9876L))
  expect_identical(calls$commands, "taskkill")
})

test_that("ask = FALSE closes with no question", {
  calls <- recording_calls()
  local_mocked_bindings(
    os_name = function() "windows",
    session_is_interactive = function() FALSE,
    excel_pids = function(os = os_name()) 4120L,
    ask_yes = function(question) stop("nobody should be asked"),
    driver_call = calls$call
  )

  said <- printed(closed <- obt_close_excel(ask = FALSE))

  expect_match(said, "Closed 1 Excel instance")
  expect_identical(closed, 4120L)
})

test_that("a session nobody can answer in is told to pass ask = FALSE", {
  calls <- recording_calls()
  local_mocked_bindings(
    os_name = function() "windows",
    session_is_interactive = function() FALSE,
    excel_pids = function(os = os_name()) 4120L,
    driver_call = calls$call
  )

  expect_error(obt_close_excel(), "ask = FALSE")
  expect_length(calls$commands, 0)
})

test_that("the verb answers invisibly", {
  local_mocked_bindings(
    os_name = function() "windows",
    excel_pids = function(os = os_name()) integer()
  )

  printed(expect_invisible(obt_close_excel()))
})

test_that("the macOS route runs from the verb too", {
  calls <- recording_calls()
  local_mocked_bindings(
    os_name = function() "macos",
    session_is_interactive = function() TRUE,
    excel_pids = listing_in_turn(512L, integer()),
    ask_yes = function(question) TRUE,
    driver_call = calls$call
  )

  said <- printed(closed <- obt_close_excel())

  expect_match(said, "Closed 1 Excel instance")
  expect_identical(closed, 512L)
  expect_identical(calls$commands, "osascript")
})

test_that("whether the session can answer is read off rlang", {
  withr::local_options(rlang_interactive = FALSE)
  expect_false(session_is_interactive())

  withr::local_options(rlang_interactive = TRUE)
  expect_true(session_is_interactive())
})
