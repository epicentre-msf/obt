# The wrapper of the quiet pair.
#
# One wrapper per script pair, and the wrapper is the only thing a caller
# reaches Excel through. It names the values the pair takes, puts them in the
# pair's own order, and says where the run writes its answer. Everything below
# it is the seam in driver.R.
#
# The pair runs on any of the three workbooks and belongs to no operation, so
# `obt_silent_get()` and `obt_silent_set()` are what call it.

# The two actions the pair takes. The script does the one it is handed.
QUIET_READ <- "read"
QUIET_WRITE <- "write"

# What the summary of a quiet run calls the value it read back.
QUIET_VALUE_KEY <- "silent"

#' Read or write the open-time switch on one workbook
#'
#' The switch is a defined name the workbook carries, and the script reaches
#' it through the automation layer. Both actions read the value back and write
#' it to the summary, so a write answers what the file now holds.
#'
#' @param workbook The workbook the switch sits on.
#' @param action `"read"` or `"write"`.
#' @param value The value a write stores. A read passes `NA`, which reaches
#'   the script as an empty string.
#' @param switch_name The name the workbook keeps the switch under.
#' @param summary The file the run writes its answer to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_quiet <- function(
  workbook,
  action,
  value = NA_character_,
  switch_name = SILENT_SWITCH_NAME,
  summary,
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "quiet",
    args = driver_args(
      "quiet",
      list(
        workbook = workbook,
        switch_name = switch_name,
        action = action,
        value = value,
        summary = summary
      ),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}
