# The switch that quiets a workbook while it opens.
#
# A scripted run cannot answer a message box. Two things turn the boxes off,
# and they cover different moments. The wrapper of an operation arms its own
# silence for the length of its call, inside the Excel it runs in, and writes
# nothing to the file. That covers everything an operation does. It cannot
# cover the boxes a workbook shows while it is opening: those fire before any
# call can reach a wrapper, so the only thing that can quiet them is a value
# the file itself carries.
#
# That value is the switch here. The package writes it once per file, on the
# copy it works with, and leaves it. It never flips it around an operation.
#
# The value bites on a workbook whose OutbreakTools reads it. A workbook built
# before the switch existed carries no such name, reads as off, and behaves
# exactly as it always has.

# The name the three workbooks keep the switch under. One constant per side:
# this one, and the switch name inside the workbooks.
SILENT_SWITCH_NAME <- "__OBT__SILENT_OPERATIONS__"

# What the switch holds. `Yes` is silent. Every other value, and no value at
# all, leaves the workbook as it is.
SILENT_ON <- "Yes"
SILENT_OFF <- "No"
SILENT_VALUES <- c(SILENT_ON, SILENT_OFF)

# The workbooks the switch sits on: the setup, the designer, and a linelist
# the designer generated.
SILENT_EXTENSIONS <- c("xlsb", "xlsm", "xlsx")

# What the folder holding the answer of one run is called.
QUIET_ANSWER_LEAD <- "obt-quiet-"

#' Read the open-time switch of a workbook
#'
#' @description
#' Answers what a workbook does with the message boxes it shows while it is
#' opening: `"Yes"` when it shows none, `"No"` when it behaves as it always
#' has.
#'
#' `"No"` is the answer for a workbook that carries no switch at all, which is
#' every workbook built before the switch existed.
#'
#' This opens Excel, so it takes macOS or Windows.
#'
#' @param path The workbook to read. A setup, a designer, or a generated
#'   linelist.
#'
#' @return `"Yes"` or `"No"`.
#'
#' @seealso [obt_silent_set()] to write it.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' obt_silent_get("~/measles/linelist/measles-2026.xlsb")
#' }
obt_silent_get <- function(path) {
  path <- check_workbook_path(path)

  silent_answer(run_quiet(path, action = QUIET_READ))
}

#' Write the open-time switch of a workbook
#'
#' @description
#' Tells a workbook whether to show the message boxes it shows while it is
#' opening. `"Yes"` quiets them; `"No"` leaves the workbook as it is, and is
#' what every workbook carries to begin with.
#'
#' @details
#' The value is written once, on the copy the package works with, and it
#' stays. Nothing flips it around an operation: an operation run from R arms
#' its own silence inside Excel, for the length of its own call.
#'
#' The value is read by the OutbreakTools code inside the workbook, so it
#' quiets a workbook whose version knows the switch. On an older one the value
#' is written and nothing reads it, and the boxes come up as before.
#'
#' This opens Excel, so it takes macOS or Windows.
#'
#' @param path The workbook to write. A setup, a designer, or a generated
#'   linelist.
#' @param value `"Yes"` or `"No"`.
#'
#' @return The value the workbook holds once the run is over, invisibly.
#'
#' @seealso [obt_silent_get()] to read it.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' obt_silent_set("~/measles/linelist/measles-2026.xlsb", "Yes")
#' }
obt_silent_set <- function(path, value = SILENT_ON) {
  path <- check_workbook_path(path)
  value <- check_silent_value(value)

  ran <- run_quiet(path, action = QUIET_WRITE, value = value)

  invisible(silent_answer(ran))
}

#' Run the quiet pair over one workbook
#'
#' The run answers through a summary file, the way every pair does. This one
#' has no output folder to write it beside, so the package makes a folder of
#' its own for the length of the call and takes it away afterwards.
#'
#' @param path The workbook, already checked.
#' @param action `"read"` or `"write"`.
#' @param value The value a write stores.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
run_quiet <- function(
  path,
  action,
  value = NA_character_,
  call = rlang::caller_env()
) {
  folder <- tempfile(QUIET_ANSWER_LEAD)
  ensure_folder(folder, call = call)

  on.exit(unlink(folder, recursive = TRUE, force = TRUE), add = TRUE)

  driver_quiet(
    workbook = path,
    action = action,
    value = value,
    summary = summary_path(folder, tools::file_path_sans_ext(basename(path))),
    call = call
  )
}

#' What a quiet run says the workbook holds
#'
#' @param ran The run record.
#'
#' @return `"Yes"` or `"No"`.
#' @noRd
silent_answer <- function(ran) {
  silent_reading(summary_value(ran$summary, QUIET_VALUE_KEY))
}

#' Read one stored value the way the workbooks read it
#'
#' The workbook trims what it finds and calls it silent when it reads `Yes`,
#' whatever the case. Everything else is the switch being off, and so is a
#' name that is not there. The answer says what the workbook does, so a value
#' the workbook does not know reads as `"No"` here too.
#'
#' @param stored The value the run read back.
#'
#' @return `"Yes"` or `"No"`.
#' @noRd
silent_reading <- function(stored) {
  if (length(stored) != 1 || is.na(stored)) {
    return(SILENT_OFF)
  }

  same <- identical(toupper(trimws(stored)), toupper(SILENT_ON))

  if (same) SILENT_ON else SILENT_OFF
}

#' Check the workbook a switch is read off or written on
#'
#' @param path The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, absolute.
#' @noRd
check_workbook_path <- function(
  path,
  arg = rlang::caller_arg(path),
  call = rlang::caller_env()
) {
  force(arg)

  path <- check_string(path, arg = arg, call = call)
  extension <- tolower(tools::file_ext(path))

  if (!extension %in% SILENT_EXTENSIONS) {
    wanted <- paste0(".", SILENT_EXTENSIONS)
    found <- if (nzchar(extension)) {
      "{.file {path}} ends in {.val {paste0('.', extension)}}."
    } else {
      "{.file {path}} carries no extension."
    }

    cli::cli_abort(
      c(
        "{.arg {arg}} must name a workbook ending in {.val {wanted}}.",
        "x" = found,
        "i" = "The switch sits on the setup, the designer, and a generated
               linelist."
      ),
      call = call
    )
  }

  check_file_there(path, what = "the workbook to open", call = call)

  absolute_path(path)
}

#' Check the value a switch is written with
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The value, in the case the workbooks store it in.
#' @noRd
check_silent_value <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.character(value) && length(value) == 1 && !is.na(value)) {
    found <- SILENT_VALUES[toupper(SILENT_VALUES) == toupper(trimws(value))]

    if (length(found) == 1) {
      return(found)
    }
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} must be {.val {SILENT_ON}} or {.val {SILENT_OFF}}.",
      "x" = "You supplied {.obj_type_friendly {value}}."
    ),
    call = call
  )
}
