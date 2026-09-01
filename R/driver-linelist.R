# The wrappers of the three linelist pairs.
#
# One wrapper per script pair, and the wrapper is the only thing an operation
# calls. It names the values the pair takes, puts them in the pair's own
# order, and says where the workbook writes its summary. Everything below it
# is the seam in driver.R.
#
# All three run inside the linelist, and the linelist closes itself at the end
# of a run that worked. The answer of the call is lost with the workbook it
# was made in, so the summary the linelist wrote before it closed is what
# settles every one of these runs. Each wrapper says where that file sits.

#' Read a geobase into a generated linelist
#'
#' The linelist writes its summary beside the geobase it was handed.
#'
#' @param linelist The linelist workbook.
#' @param geo The geobase file.
#' @param summary The file the linelist writes its summary to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_linelist_geobase <- function(
  linelist,
  geo,
  summary = summary_beside(geo),
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "linelist-geobase",
    args = driver_args(
      "linelist-geobase",
      list(linelist = linelist, geo = geo),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}

#' Read a file another linelist wrote for migration
#'
#' The linelist writes its summary beside the file it was handed.
#'
#' @param linelist The linelist workbook.
#' @param from The migration file.
#' @param rule What happens to the rows the linelist already holds:
#'   `"append"` or `"replace"`.
#' @param force The string `"Yes"` where the run goes past a warning the
#'   linelist raised, `"No"` otherwise.
#' @param summary The file the linelist writes its summary to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_linelist_import <- function(
  linelist,
  from,
  rule = "append",
  force = FORCE_NO,
  summary = summary_beside(from),
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "linelist-import",
    args = driver_args(
      "linelist-import",
      list(linelist = linelist, from = from, rule = rule, force = force),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}

#' Write an export out of the linelist
#'
#' An export writes its files into a folder it is handed, and the linelist
#' writes its summary into that folder under its own name.
#'
#' @param linelist The linelist workbook.
#' @param name The name of the export. An empty name asks for the migration
#'   export.
#' @param to The folder the export is written to.
#' @param password The password the other linelist opens with.
#' @param other The linelist the export reads from. `NA` reads the linelist
#'   the call drives.
#' @param summary The file the linelist writes its summary to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_linelist_export <- function(
  linelist,
  name,
  to,
  password = NA_character_,
  other = NA_character_,
  summary = summary_in(to, linelist),
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "linelist-export",
    args = driver_args(
      "linelist-export",
      list(
        linelist = linelist,
        name = name,
        folder = to,
        password = password,
        other = other
      ),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}
