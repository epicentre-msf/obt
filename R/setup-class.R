# The setup recipe.
#
# A setup operation edits a setup workbook. Those operations have a class of
# their own, because a user can work on a setup on its own. The recipe holds
# the setup and the verbs that touch it, and that is the whole of what it
# carries. `obt_setup()` builds one from a setup file.
#
# An `obt` recipe narrows to an `obt_setup` one, because an `obt` holds a
# setup among the rest of what it carries. The way back is closed. An
# `obt_setup` holds a setup alone, and there is nothing in it to build an
# `obt` from.
#
# The class is a subset of `obt` and it is built as one, so every printer,
# every path and the run itself read it the way they read any recipe.

# The extension a setup workbook carries. The workbook holds the code the
# setup verbs run.
SETUP_EXTENSION <- "xlsb"

# The extension of the file a setup is written out to and read back from. It
# carries the content of a setup.
SETUP_EXCHANGE_EXTENSION <- "xlsx"

# The operations a setup recipe holds. A narrowing keeps these and leaves the
# rest of what an `obt` carries behind.
#
# The steps of a conversion stay out. A conversion fills a copy of the empty
# setup the OutbreakTools files carry, and those files come with an `obt`,
# which a narrowing leaves behind.
SETUP_OPERATION_TYPES <- c(
  "setup-add",
  "setup-export",
  "setup-import",
  "setup-tags",
  "setup-fake"
)

#' Start a setup recipe
#'
#' @description
#' Creates a recipe that works on one setup workbook, and holds that setup and
#' the verbs that touch it. The setup verbs [obt_setup_export()],
#' [obt_setup_import()] and [obt_setup_tags()] all take a recipe of this
#' class.
#'
#' `from` takes either of two things:
#'
#' * **A setup workbook**, ending in `.xlsb`. The recipe records the copy of
#'   that file into the working folder, and `folder` says where. The file
#'   itself is an input and stays as it is: the copy under `setup/source/`
#'   keeps its name, and the copy at `setup/setup.xlsb` is what the verbs
#'   work on.
#' * **An `obt` recipe.** It narrows to a setup one, keeping the working
#'   folder and the setup operations it already holds. The recipe it came
#'   from is left as it was.
#'
#' Like every verb this one records and answers the recipe. Nothing is copied
#' until [obt_commit()] runs.
#'
#' @param from A setup workbook, or an `obt` recipe to narrow.
#' @param folder Path to the working folder, as a single string. Give it with
#'   a setup workbook. A narrowed recipe carries the folder it came with.
#' @param overwrite Whether a working setup already in the folder may be
#'   replaced. With `FALSE`, the default, the run stops when one is there,
#'   because that file carries whatever the setup verbs have written into it.
#'
#' @return An object of class `obt_setup`.
#'
#' @seealso [obt_setup_export()], [obt_setup_import()] and
#'   [obt_setup_tags()] for what a setup recipe can do,
#'   [obt_setup_languages()] for the languages a setup file holds.
#'
#' @export
#'
#' @examples
#' setup <- system.file("extdata", "generic-test-setup.xlsb", package = "obt")
#'
#' obt_setup(from = setup, folder = file.path(tempdir(), "measles"))
#'
#' # An `obt` recipe narrows to a setup one.
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_add(type = "dev") |>
#'   obt_setup()
obt_setup <- function(from, folder = NULL, overwrite = FALSE) {
  if (is_obt(from)) {
    return(narrow_setup(from, folder = folder, overwrite = overwrite))
  }

  from <- check_setup_file(from)
  folder <- check_setup_folder(folder)
  check_flag(overwrite)

  add_operation(
    new_obt_setup(folder = folder, operations = list()),
    type = "setup-add",
    args = list(
      from = path_relative(from, folder = folder),
      overwrite = overwrite
    )
  )
}

#' Test whether an object is a setup recipe
#'
#' @param x An object.
#'
#' @return `TRUE` when `x` is an `obt_setup` recipe, `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' is_obt_setup(obt_setup(obt(folder = tempdir())))
#' is_obt_setup(obt(folder = tempdir()))
is_obt_setup <- function(x) {
  inherits(x, "obt_setup")
}

#' Build a setup recipe from its parts
#'
#' The class carries `obt` under it, so the printers, the paths and the run
#' read a setup recipe the way they read any other.
#'
#' @param folder The working folder, already checked.
#' @param operations The ordered list of operation records.
#'
#' @return An object of class `obt_setup`.
#' @noRd
new_obt_setup <- function(folder, operations) {
  structure(
    list(folder = folder, operations = operations),
    class = c("obt_setup", "obt")
  )
}

#' Narrow a recipe to its setup operations
#'
#' The folder comes with the recipe, and so do the setup operations it holds,
#' in the order they were recorded.
#'
#' @param obtops The recipe to narrow.
#' @param folder What the caller passed as the folder.
#' @param overwrite What the caller passed as the overwrite flag.
#' @param call The environment to blame in the error.
#'
#' @return An object of class `obt_setup`.
#' @noRd
narrow_setup <- function(
  obtops,
  folder = NULL,
  overwrite = FALSE,
  call = rlang::caller_env()
) {
  kept <- narrow_operations(
    obtops,
    target = "obt_setup",
    types = SETUP_OPERATION_TYPES,
    folder = folder,
    overwrite = overwrite,
    what = "setup",
    call = call
  )

  new_obt_setup(folder = obtops$folder, operations = kept)
}

#' Copy the setup into the working folder
#'
#' Two copies are made. The one under `setup/source/` keeps the name the user
#' knows the file by. The one at `setup/setup.xlsb` is what every setup verb
#' works on, so the file the user handed over is never opened by a run.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_setup_add <- function(obtops, operation, stage, state) {
  paths <- obt_paths(obtops)
  from <- path_absolute(operation$args$from, folder = obtops$folder)

  check_file_there(from, what = "the setup it was given")
  check_output_free(paths$setup_file, operation$args$overwrite)

  ensure_folder(paths$setup)
  ensure_folder(paths$setup_source)

  kept <- copy_path(obtops, "setup_source", from)
  copy_file(from, kept)
  copy_file(from, paths$setup_file)

  list(
    produced = c(source = kept, setup = paths$setup_file),
    state = list(setup = paths$setup_file)
  )
}

#' The setup file every verb of a setup recipe works on
#'
#' @param obtops The recipe.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
setup_working_file <- function(obtops, call = rlang::caller_env()) {
  path <- obt_paths(obtops)$setup_file

  if (!file.exists(path)) {
    cli::cli_abort(
      c(
        "The working folder holds no setup.",
        "x" = "Nothing sits at {.file {path}}.",
        "i" = "Put one there with {.code obt_setup(from = )}."
      ),
      call = call
    )
  }

  path
}

#' Check the file a setup recipe is built from
#'
#' @param from The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, trimmed.
#' @noRd
check_setup_file <- function(
  from,
  arg = rlang::caller_arg(from),
  call = rlang::caller_env()
) {
  # The label is read before `from` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  from <- check_string(from, arg = arg, call = call)

  wanted <- paste0(".", SETUP_EXTENSION)
  exchange <- paste0(".", SETUP_EXCHANGE_EXTENSION)
  extension <- tolower(tools::file_ext(from))

  if (identical(extension, SETUP_EXCHANGE_EXTENSION)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a setup workbook, ending in {.val {wanted}}.",
        "x" = "{.file {from}} ends in {.val {exchange}}.",
        "i" = "An {.val {exchange}} file carries the content of a setup. The
               workbook carries the code the setup verbs run."
      ),
      call = call
    )
  }

  if (!identical(extension, SETUP_EXTENSION)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a setup workbook, ending in {.val {wanted}}.",
        "x" = "{.file {from}} ends in {.val {paste0('.', extension)}}."
      ),
      call = call
    )
  }

  check_file_there(from, what = "a setup workbook", call = call)

  from
}

#' Check the folder a setup recipe was given
#'
#' @param folder The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The folder path, with `~` expanded.
#' @noRd
check_setup_folder <- function(
  folder,
  arg = rlang::caller_arg(folder),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(folder)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must name the working folder.",
        "i" = "The copy of the setup and everything the verbs write go
               under it."
      ),
      call = call
    )
  }

  check_folder(folder, arg = arg, call = call)
}

#' Fail when a value is not a setup recipe
#'
#' @param obtops The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The recipe.
#' @noRd
check_obt_setup <- function(
  obtops,
  arg = rlang::caller_arg(obtops),
  call = rlang::caller_env()
) {
  if (is_obt_setup(obtops)) {
    return(obtops)
  }

  if (is_obt(obtops)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an {.cls obt_setup} recipe.",
        "x" = "You supplied an {.cls {class(obtops)[[1]]}} recipe.",
        "i" = "Narrow it with {.code obt_setup()}."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} must be an {.cls obt_setup} recipe.",
      "x" = "You supplied {.obj_type_friendly {obtops}}.",
      "i" = "Start one with {.code obt_setup(from = )}."
    ),
    call = call
  )
}
