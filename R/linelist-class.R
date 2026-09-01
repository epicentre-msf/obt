# The linelist recipe.
#
# A linelist operation runs inside a generated linelist workbook. Those
# operations have a class of their own, because a user can work on a linelist
# on its own: a linelist built weeks ago still reads a geobase, reads a
# migration file and writes an export. The recipe holds the linelist and the
# verbs that touch it, and that is the whole of what it carries.
#
# An `obt` recipe narrows to an `obt_linelist` one, because an `obt` holds
# linelist operations among the rest of what it carries. The way back is
# closed. An `obt_linelist` holds a linelist alone, and there is nothing in it
# to build an `obt` from.
#
# The class is a subset of `obt` and it is built as one, so every printer,
# every path and the run itself read it the way they read any recipe.

# The operations a linelist recipe holds. A narrowing keeps these and leaves
# the rest of what an `obt` carries behind.
#
# The generation stays out. It fills the designer's Main sheet, and the
# designer comes with an `obt`, which a narrowing leaves behind.
LINELIST_OPERATION_TYPES <- c(
  "linelist-add",
  "linelist-geobase",
  "linelist-import",
  "linelist-export"
)

#' Start a linelist recipe
#'
#' @description
#' Creates a recipe that works on one generated linelist, and holds that
#' linelist and the verbs that touch it. The linelist verbs
#' [obt_linelist_geobase()], [obt_linelist_import()] and
#' [obt_linelist_export()] all take a recipe of this class.
#'
#' `from` takes either of two things:
#'
#' * **A linelist workbook**, ending in `.xlsb`. The recipe records the copy
#'   of that file into `linelist/` in the working folder, and `folder` says
#'   where. The file itself is an input and stays as it is: the copy keeps its
#'   name and is what the verbs work on.
#' * **An `obt` recipe.** It narrows to a linelist one, keeping the working
#'   folder and the linelist operations it already holds. The recipe it came
#'   from is left as it was.
#'
#' A recipe narrowed from an `obt` names no linelist of its own. The run reads
#' the one file under `linelist/` in the working folder, so a linelist built
#' by an earlier run is picked up. A folder holding more than one is refused,
#' and naming the file with `from` is what settles it.
#'
#' Like every verb this one records and answers the recipe. Nothing is copied
#' until [obt_commit()] runs.
#'
#' @param from A linelist workbook, or an `obt` recipe to narrow.
#' @param folder Path to the working folder, as a single string. Give it with
#'   a linelist workbook. A narrowed recipe carries the folder it came with.
#' @param overwrite Whether a linelist of that name already in `linelist/` may
#'   be replaced. With `FALSE`, the default, the run stops when one is there,
#'   because that file carries whatever the linelist verbs have written into
#'   it.
#'
#' @return An object of class `obt_linelist`.
#'
#' @seealso [obt_linelist_geobase()], [obt_linelist_import()] and
#'   [obt_linelist_export()] for what a linelist recipe can do,
#'   [obt_designer_generate()] for building one in the first place.
#'
#' @export
#'
#' @examples
#' linelist <- file.path(tempdir(), "measles-2026.xlsb")
#' invisible(file.create(linelist))
#'
#' obt_linelist(from = linelist, folder = file.path(tempdir(), "measles"))
#'
#' # An `obt` recipe narrows to a linelist one.
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_add(type = "dev") |>
#'   obt_linelist()
obt_linelist <- function(from, folder = NULL, overwrite = FALSE) {
  if (is_obt(from)) {
    return(narrow_linelist(from, folder = folder, overwrite = overwrite))
  }

  from <- check_linelist_file(from)
  folder <- check_linelist_folder(folder)
  check_flag(overwrite)

  add_operation(
    new_obt_linelist(folder = folder, operations = list()),
    type = "linelist-add",
    args = list(
      from = path_relative(from, folder = folder),
      overwrite = overwrite
    )
  )
}

#' Test whether an object is a linelist recipe
#'
#' @param x An object.
#'
#' @return `TRUE` when `x` is an `obt_linelist` recipe, `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' is_obt_linelist(obt_linelist(obt(folder = tempdir())))
#' is_obt_linelist(obt(folder = tempdir()))
is_obt_linelist <- function(x) {
  inherits(x, "obt_linelist")
}

#' Build a linelist recipe from its parts
#'
#' The class carries `obt` under it, so the printers, the paths and the run
#' read a linelist recipe the way they read any other.
#'
#' @param folder The working folder, already checked.
#' @param operations The ordered list of operation records.
#'
#' @return An object of class `obt_linelist`.
#' @noRd
new_obt_linelist <- function(folder, operations) {
  structure(
    list(folder = folder, operations = operations),
    class = c("obt_linelist", "obt")
  )
}

#' Narrow a recipe to its linelist operations
#'
#' The folder comes with the recipe, and so do the linelist operations it
#' holds, in the order they were recorded.
#'
#' @param obtops The recipe to narrow.
#' @param folder What the caller passed as the folder.
#' @param overwrite What the caller passed as the overwrite flag.
#' @param call The environment to blame in the error.
#'
#' @return An object of class `obt_linelist`.
#' @noRd
narrow_linelist <- function(
  obtops,
  folder = NULL,
  overwrite = FALSE,
  call = rlang::caller_env()
) {
  kept <- narrow_operations(
    obtops,
    target = "obt_linelist",
    types = LINELIST_OPERATION_TYPES,
    folder = folder,
    overwrite = overwrite,
    what = "linelist",
    call = call
  )

  new_obt_linelist(folder = obtops$folder, operations = kept)
}

#' Copy the linelist into the working folder
#'
#' The copy keeps the name the user knows the file by, and it is what every
#' linelist verb works on, so the file the user handed over is never opened by
#' a run.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_linelist_add <- function(obtops, operation, stage, state) {
  paths <- obt_paths(obtops)
  from <- path_absolute(operation$args$from, folder = obtops$folder)

  check_file_there(from, what = "the linelist it was given")

  ensure_folder(paths$linelist)
  kept <- copy_path(obtops, "linelist", from)

  if (!identical(kept, from)) {
    check_output_free(kept, operation$args$overwrite)
    copy_file(from, kept)
  }

  list(produced = c(linelist = kept), state = list(linelist = kept))
}

#' Check the file a linelist recipe is built from
#'
#' @param from The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, trimmed.
#' @noRd
check_linelist_file <- function(
  from,
  arg = rlang::caller_arg(from),
  call = rlang::caller_env()
) {
  # The label is read before `from` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  from <- check_string(from, arg = arg, call = call)

  wanted <- paste0(".", LINELIST_EXTENSION)
  extension <- tolower(tools::file_ext(from))

  if (!identical(extension, LINELIST_EXTENSION)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a linelist workbook, ending in {.val {wanted}}.",
        "x" = "{.file {from}} ends in {.val {paste0('.', extension)}}.",
        "i" = "The designer writes a linelist as an {.val {wanted}} file."
      ),
      call = call
    )
  }

  check_file_there(from, what = "a linelist workbook", call = call)

  from
}

#' Check the folder a linelist recipe was given
#'
#' @param folder The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The folder path, with `~` expanded.
#' @noRd
check_linelist_folder <- function(
  folder,
  arg = rlang::caller_arg(folder),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(folder)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must name the working folder.",
        "i" = "The copy of the linelist and everything the verbs write go
               under it."
      ),
      call = call
    )
  }

  check_folder(folder, arg = arg, call = call)
}

#' Fail when a value is not a linelist recipe
#'
#' @param obtops The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The recipe.
#' @noRd
check_obt_linelist <- function(
  obtops,
  arg = rlang::caller_arg(obtops),
  call = rlang::caller_env()
) {
  if (is_obt_linelist(obtops)) {
    return(obtops)
  }

  if (is_obt(obtops)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an {.cls obt_linelist} recipe.",
        "x" = "You supplied an {.cls {class(obtops)[[1]]}} recipe.",
        "i" = "Narrow it with {.code obt_linelist()}."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} must be an {.cls obt_linelist} recipe.",
      "x" = "You supplied {.obj_type_friendly {obtops}}.",
      "i" = "Start one with {.code obt_linelist(from = )}."
    ),
    call = call
  )
}
