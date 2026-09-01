# The wrapper of the generate pair.
#
# One wrapper per script pair, and the wrapper is the only thing an operation
# calls. It names the values the pair takes, puts them in the pair's own
# order, and says where the workbook writes its summary. Everything below it
# is the seam in driver.R.

#' Generate a linelist
#'
#' Writes the seven entries the designer's `Main` sheet reads, then runs the
#' generation callback. The designer writes the linelist under `folder`, as
#' `<name>.xlsb`, and its summary beside it.
#'
#' @param designer The designer workbook.
#' @param setup The setup file the designer reads.
#' @param folder Where the linelist is written.
#' @param name The file name of the linelist, without an extension.
#' @param setup_language The column of the setup's translation table the
#'   dictionary, the choices and the analyses are read in.
#' @param form_language The code of the language the linelist interface is
#'   built in.
#' @param geo The geobase file. `NA` builds the linelist with no geobase.
#' @param ribbon The ribbon template. `NA` builds the buttons with no ribbon.
#' @param summary The file the designer writes its summary to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return The run record, as `run_driver()` answers it.
#' @noRd
driver_generate <- function(
  designer,
  setup,
  folder,
  name,
  setup_language,
  form_language,
  geo = NA_character_,
  ribbon = NA_character_,
  summary = summary_path(folder, name),
  os = os_name(),
  call = rlang::caller_env()
) {
  run_driver(
    "designer-generate",
    args = driver_args(
      "designer-generate",
      list(
        designer = designer,
        geo = geo,
        setup = setup,
        folder = folder,
        name = name,
        setup_language = setup_language,
        form_language = form_language,
        ribbon = ribbon
      ),
      call = call
    ),
    summary = summary,
    os = os,
    call = call
  )
}
