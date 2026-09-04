# Generating the linelist.
#
# This is the operation the whole package is built around: the designer reads
# the values on its Main sheet and writes a linelist. The verb records it and
# answers the recipe, the way every verb does. `obt_commit()` is what opens
# Excel.

# Characters Windows refuses in a file name. A linelist is written on both
# systems, so a name that works on one has to work on the other.
FORBIDDEN_NAME_CHARS <- "[\\\\/:*?\"<>|]"

# Extensions a linelist is written with. A name that already carries one was
# meant as a file name, and would come out doubled.
LINELIST_EXTENSIONS <- c("xlsb", "xlsx", "xlsm")

#' Generate the linelist
#'
#' @description
#' Records the generation run. At commit time the designer's `Main` sheet is
#' filled from the recipe and the linelist is built under `linelist/` in the
#' working folder, as `<name>.xlsb`.
#'
#' A linelist can be built with two passwords, and the designer takes both
#' off its `Main` sheet. `password` is what the file opens with, and every
#' verb that opens the linelist later takes it: [obt_linelist()],
#' [obt_silent_get()] and [obt_silent_set()]. `debug_password` is what the
#' sheets and the structure of the linelist are protected with; the linelist
#' keeps it and uses it on its own. Both are hidden wherever a recipe is
#' printed, and the default is a linelist with no password.
#'
#' Like every verb this one records and answers the recipe. Nothing is written
#' until `obt_commit()` runs.
#'
#' @param obtops An `obt` recipe.
#' @param name The file name of the linelist, without an extension.
#' @param overwrite Whether a linelist of that name may be replaced. With
#'   `FALSE`, the default, the run stops when the file is already there.
#' @param password The password the linelist opens with. Left out, the
#'   default, the linelist opens with none.
#' @param debug_password The password the sheets and the structure of the
#'   linelist are protected with, which the designer calls the debugging
#'   password. Left out, the default, the designer protects with an empty one.
#'
#' @return The recipe, with the generation operation added.
#'
#' @export
#'
#' @examples
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_generate(name = "measles-2026")
#'
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_generate(name = "measles-2026", password = "open-sesame")
obt_designer_generate <- function(
  obtops,
  name,
  overwrite = FALSE,
  password = NULL,
  debug_password = NULL
) {
  check_obt_plain(obtops)
  name <- check_linelist_name(name)
  check_flag(overwrite)
  password <- check_password(password)
  debug_password <- check_password(debug_password)

  add_operation(
    obtops,
    type = "designer-generate",
    args = list(
      name = name,
      overwrite = overwrite,
      password = password,
      debug_password = debug_password
    )
  )
}

#' Check the name a linelist will be written under
#'
#' @param name The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The name, trimmed.
#' @noRd
check_linelist_name <- function(
  name,
  arg = rlang::caller_arg(name),
  call = rlang::caller_env()
) {
  # The label is read before `name` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  name <- check_string(name, arg = arg, call = call)

  if (grepl(FORBIDDEN_NAME_CHARS, name)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a file name on its own.",
        "x" = "{.val {name}} carries a character a file name cannot hold.",
        "i" = "The linelist is written under {.file linelist/} in the
               working folder."
      ),
      call = call
    )
  }

  extension <- tolower(tools::file_ext(name))

  if (extension %in% LINELIST_EXTENSIONS) {
    bare <- tools::file_path_sans_ext(name)
    cli::cli_abort(
      c(
        "{.arg {arg}} must be given without an extension.",
        "i" = "Use {.val {bare}}. The designer adds the extension."
      ),
      call = call
    )
  }

  name
}
