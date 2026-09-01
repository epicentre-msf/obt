# The wrappers of the three setup pairs.
#
# One wrapper per script pair, and the wrapper is the only thing an operation
# calls. It names the values the pair takes, puts them in the pair's own
# order, and says where the workbook writes its summary. Everything below it
# is the seam in driver.R.
#
# The setup wrappers leave the workbook open when they answer, so the answer
# of the call comes back whole. The script reads the summary through a second
# call and writes it where R asked, because the workbook's own copy is written
# under the name of the file it touched and R cannot always know that name.

#' Write the setup out as an `.xlsx` file
#'
#' @param setup The setup file.
#' @param to The folder the export is written to.
#' @param summary The file the script writes the run's summary to. It sits in
#'   the output folder, under the name of the setup, because the name of the
#'   file the export writes comes from the setup itself.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_setup_export <- function(
  setup,
  to,
  summary = summary_in(to, setup),
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "setup-export",
    args = driver_args(
      "setup-export",
      list(setup = setup, folder = to, summary = summary),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}

#' Read an `.xlsx` file into the working setup
#'
#' @param setup The setup file.
#' @param from The `.xlsx` file that is read.
#' @param summary The file the script writes the run's summary to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_setup_import <- function(
  setup,
  from,
  summary = summary_beside(from),
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "setup-import",
    args = driver_args(
      "setup-import",
      list(setup = setup, from = from, summary = summary),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}

#' Give every label a tag and make every tag unique
#'
#' @param setup The setup file.
#' @param summary The file the script writes the run's summary to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_setup_tags <- function(
  setup,
  summary = summary_beside(setup),
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "setup-tags",
    args = driver_args(
      "setup-tags",
      list(setup = setup, summary = summary),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}
