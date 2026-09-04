# Small argument checks the verbs share.
#
# Each one names the argument it was given and says what it accepts, so the
# message tells the user what to change. They run in the verb, before any
# operation is recorded, because an argument caught here is caught minutes
# before Excel would have opened.

#' Check that a value is a single string
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The value, trimmed.
#' @noRd
check_string <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  if (!is.character(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single string.",
        "x" = "You supplied {.obj_type_friendly {value}}."
      ),
      call = call
    )
  }

  value <- trimws(value)

  if (!nzchar(value)) {
    cli::cli_abort("{.arg {arg}} must carry a value.", call = call)
  }

  value
}

#' Check that a value is a single TRUE or FALSE
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The value.
#' @noRd
check_flag <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  if (!is.logical(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
        "x" = "You supplied {.obj_type_friendly {value}}."
      ),
      call = call
    )
  }

  value
}

#' Check that a file the run needs is there
#'
#' A verb checks a path when it is called, and the run checks it again. A
#' recipe is built minutes before it is run, and a file can be moved in
#' between.
#'
#' @param path The file to look for.
#' @param what What the file is, in a few words, for the message.
#' @param call The environment to blame in the error.
#'
#' @return The path, invisibly.
#' @noRd
check_file_there <- function(path, what, call = rlang::caller_env()) {
  if (file.exists(path) && !dir.exists(path)) {
    return(invisible(path))
  }

  if (dir.exists(path)) {
    cli::cli_abort(
      c(
        "The run needs {what}.",
        "x" = "A folder sits at {.file {path}}."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "The run needs {what}.",
      "x" = "Nothing sits at {.file {path}}."
    ),
    call = call
  )
}

#' Check a password a verb was given
#'
#' A password is kept as it stands. `check_string()` trims what it is given,
#' and a password can open or close on a space. `NULL` is no password, and it
#' is what every verb takes to begin with.
#'
#' @param password The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The password, or `NULL`.
#' @noRd
check_password <- function(
  password,
  arg = rlang::caller_arg(password),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(password)) {
    return(NULL)
  }

  if (!is.character(password) || length(password) != 1 || is.na(password)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single string.",
        "x" = "You supplied {.obj_type_friendly {password}}."
      ),
      call = call
    )
  }

  if (!nzchar(password)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must carry a value.",
        "i" = "Leave it out where the workbook has no password."
      ),
      call = call
    )
  }

  password
}
