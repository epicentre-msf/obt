# Closing every Excel that is running.
#
# A run that fails can leave Excel open behind it: a designer stuck in a
# macro, a linelist waiting on a box nobody can click. The next run then
# opens a second Excel beside it, and the two fight over the same files. The
# verb here reads what is running and closes all of it, so the machine is
# clean before the next run.
#
# Closing this way throws unsaved work away. That is what makes it useful on
# a hung Excel, and what makes it dangerous on one the user is working in, so
# the verb asks before it acts. It closes through what each system already
# has: `taskkill` on Windows, and AppleScript on macOS, with a plain `kill`
# for a process that ignores the AppleScript.

# What the Excel process is called on each system.
EXCEL_IMAGE_WINDOWS <- "EXCEL.EXE"
EXCEL_PROCESS_MACOS <- "Microsoft Excel"

# How long macOS is given to quit after it is asked, and how often it is
# checked while it does.
EXCEL_QUIT_WAIT_SECONDS <- 10
EXCEL_QUIT_POLL_SECONDS <- 0.5

# How long the AppleScript itself waits on Excel before it gives up. A hung
# Excel answers nothing, and the plain `kill` that follows is the answer to
# that.
EXCEL_QUIT_TIMEOUT_SECONDS <- 15

#' Close every Excel running
#'
#' @description
#' Finds every Excel process on this machine and closes all of them. Unsaved
#' work in any of them is lost, so it asks a confirmation from the user first in
#' interactive session.
#'
#' Use it when a run failed and left Excel open behind it: a designer stuck
#' in a macro, or a linelist waiting on a message box.
#'
#' @details
#' On Windows the processes are read with `tasklist` and closed with
#' `taskkill`, by force. On macOS they are read with `pgrep`.
#'
#' @param ask Whether to ask confirmation before closing. Default is `TRUE`.
#'
#' @return The process numbers that were closed, as an integer vector,
#'   invisibly. Empty when no Excel was running or the answer was no.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' obt_close_excel()
#' obt_close_excel(ask = FALSE)
#' }
obt_close_excel <- function(ask = TRUE) {
  ask <- check_flag(ask)
  os <- os_name()

  check_excel_os(os)

  pids <- excel_pids(os)

  if (length(pids) == 0) {
    cli::cli_alert_info("No Excel is running.")
    return(invisible(integer()))
  }

  if (isTRUE(ask) && !confirm_close(pids)) {
    cli::cli_alert_info(
      "Nothing was closed. {length(pids)} Excel instance{?s} still run{?s/}."
    )
    return(invisible(integer()))
  }

  closed <- close_excel(pids, os)

  cli::cli_alert_success(
    "Closed {length(closed)} Excel instance{?s}. Unsaved work in {?it/them} is gone."
  )

  invisible(closed)
}

#' Fail on a system Excel does not run on
#'
#' @param os The operating system.
#' @param call The environment to blame in the error.
#'
#' @return The system, invisibly.
#' @noRd
check_excel_os <- function(os = os_name(), call = rlang::caller_env()) {
  if (os %in% SUPPORTED_OS) {
    return(invisible(os))
  }

  cli::cli_abort(
    c(
      "Excel runs on macOS or Windows, so there is none to close here.",
      "x" = "This R session runs on {.val {os}}."
    ),
    call = call
  )
}

#' Ask before closing, and refuse to guess where nobody can answer
#'
#' @param pids The processes that would close.
#' @param call The environment to blame in the error.
#'
#' @return `TRUE` on a yes, `FALSE` on a no.
#' @noRd
confirm_close <- function(pids, call = rlang::caller_env()) {
  if (!session_is_interactive()) {
    cli::cli_abort(
      c(
        "Closing Excel throws unsaved work away, so the verb asks first.",
        "x" = "Nobody can answer in this session.",
        "i" = "Call {.code obt_close_excel(ask = FALSE)} to close
               {length(pids)} Excel instance{?s} with no question."
      ),
      call = call
    )
  }

  n <- length(pids)
  question <- paste0(
    n,
    " Excel instance",
    if (n == 1) " is" else "s are",
    " running. Close ",
    if (n == 1) "it" else "them all",
    " and throw unsaved work away?"
  )

  isTRUE(ask_yes(question))
}

#' Put one yes-or-no question to the user
#'
#' The one place the package asks anything, so a test mocks this and the
#' suite never waits on a keyboard.
#'
#' @param question What is asked.
#'
#' @return `TRUE` on a yes, `FALSE` on anything else.
#' @noRd
ask_yes <- function(question) {
  identical(utils::menu(c("Yes", "No"), title = question), 1L)
}

#' Whether anyone can answer a question in this session
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
session_is_interactive <- function() {
  rlang::is_interactive()
}

#' The Excel processes running on this machine
#'
#' @param os The operating system.
#'
#' @return The process numbers, as an integer vector. Empty when none runs.
#' @noRd
excel_pids <- function(os = os_name()) {
  if (identical(os, "windows")) {
    lines <- run_quietly(
      "tasklist",
      c(
        "/FI",
        shQuote(paste("IMAGENAME eq", EXCEL_IMAGE_WINDOWS), type = "cmd"),
        "/FO",
        "CSV",
        "/NH"
      )
    )

    return(excel_pids_from_tasklist(lines))
  }

  lines <- run_quietly("pgrep", c("-x", shQuote(EXCEL_PROCESS_MACOS)))

  excel_pids_from_lines(lines)
}

#' Read the process numbers out of what tasklist printed
#'
#' Each process is one CSV row led by the image name. The line tasklist
#' prints when it finds none is worded in the language of the machine, and
#' it carries no row, so it falls out with everything else.
#'
#' @param lines The lines tasklist printed.
#'
#' @return The process numbers, as an integer vector.
#' @noRd
excel_pids_from_tasklist <- function(lines) {
  lines <- trimws(as.character(lines))
  lead <- paste0("^\"", EXCEL_IMAGE_WINDOWS, "\",\"([0-9]+)\"")
  rows <- lines[grepl(lead, lines, ignore.case = TRUE)]

  excel_pids_from_lines(sub(
    paste0(lead, ".*$"),
    "\\1",
    rows,
    ignore.case = TRUE
  ))
}

#' Read process numbers out of lines that carry one each
#'
#' @param lines The lines to read.
#'
#' @return The process numbers, as an integer vector, each once.
#' @noRd
excel_pids_from_lines <- function(lines) {
  lines <- trimws(as.character(lines))
  lines <- lines[grepl("^[0-9]+$", lines)]

  unique(as.integer(lines))
}

#' The command one system closes Excel with
#'
#' Windows closes by process number, by force. macOS asks Excel itself, so a
#' copy that still listens quits the way it does from the menu, and saves
#' nothing on the way out. The AppleScript sits inside a timeout, because a
#' hung Excel answers nothing and the call would otherwise wait on it.
#'
#' @param pids The processes to close.
#' @param os The operating system.
#'
#' @return A list with `command` and `args`.
#' @noRd
close_excel_command <- function(pids, os) {
  if (identical(os, "windows")) {
    return(list(
      command = "taskkill",
      args = c("/F", as.vector(rbind("/PID", as.character(pids))))
    ))
  }

  script <- paste0(
    "with timeout of ",
    EXCEL_QUIT_TIMEOUT_SECONDS,
    " seconds\n",
    "tell application \"",
    EXCEL_PROCESS_MACOS,
    "\" to quit saving no\n",
    "end timeout"
  )

  list(command = "osascript", args = c("-e", shQuote(script)))
}

#' Close the Excel processes that were found
#'
#' On macOS the AppleScript is given a moment to work. What is still running
#' after that is closed by number, the way Windows does it from the start.
#'
#' @param pids The processes to close.
#' @param os The operating system.
#' @param wait How long macOS is given to quit before what is left is killed.
#' @param call The environment to blame in the error.
#'
#' @return The process numbers that were closed.
#' @noRd
close_excel <- function(
  pids,
  os,
  wait = EXCEL_QUIT_WAIT_SECONDS,
  call = rlang::caller_env()
) {
  command <- close_excel_command(pids, os)
  ran <- driver_call(command$command, command$args)

  if (identical(os, "windows")) {
    if (!identical(ran$status, 0L)) {
      cli::cli_abort(
        c(
          "Excel could not be closed.",
          "x" = "{.code taskkill} printed: {.val {driver_echo(ran$output)}}"
        ),
        call = call
      )
    }

    return(pids)
  }

  left <- wait_for_excel_to_quit(os, wait = wait)

  if (length(left) > 0) {
    driver_call("kill", c("-9", as.character(left)))
    left <- excel_pids(os)
  }

  if (length(left) > 0) {
    cli::cli_abort(
      c(
        "Excel could not be closed.",
        "x" = "Still running: {.val {left}}.",
        "i" = "Force Quit Excel from the Apple menu."
      ),
      call = call
    )
  }

  pids
}

#' Check on Excel until it is gone or the wait is over
#'
#' @param os The operating system.
#' @param wait How long to wait, in seconds.
#'
#' @return The process numbers still running when the wait is over.
#' @noRd
wait_for_excel_to_quit <- function(os, wait = EXCEL_QUIT_WAIT_SECONDS) {
  deadline <- Sys.time() + wait
  left <- excel_pids(os)

  while (length(left) > 0 && Sys.time() < deadline) {
    Sys.sleep(EXCEL_QUIT_POLL_SECONDS)
    left <- excel_pids(os)
  }

  left
}
