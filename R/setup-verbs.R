# What a setup recipe can do.
#
# Three verbs, and each one records an operation and answers the recipe the
# way every verb of the package does. `obt_commit()` is what opens Excel.
#
# All three run in the setup workbook, so all three wait on an entry point a
# script can call. Each one has a runner below it, and each runner resolves
# its values and calls one wrapper. The day a pair ships, that wrapper is the
# one function that changes.
#
# The three cover the whole of what a setup can be asked from R today. A
# fine-grained edit — one variable, one choice list, one analysis — is a verb
# of its own later, and the record it adds is the same shape as these.

#' Write the setup out as an `.xlsx` file
#'
#' @description
#' Records the export. At commit time the sheets of the working setup are
#' written into an `.xlsx` file, which can be read from R or edited outside
#' Excel.
#'
#' The layout of that file is the setup's own: it is what the setup writes and
#' what [obt_setup_import()] reads back.
#'
#' Like every verb this one records and answers the recipe. Nothing is written
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt_setup` recipe.
#' @param to The folder the file is written into. The default is `export/` in
#'   the working folder. A folder that is missing is created by the run.
#'
#' @return The recipe, with the export operation added.
#'
#' @seealso [obt_setup()] to start a setup recipe, [obt_setup_import()] to
#'   read a file back in.
#'
#' @export
#'
#' @examples
#' setup <- system.file("extdata", "generic-test-setup.xlsb", package = "obt")
#'
#' obt_setup(from = setup, folder = file.path(tempdir(), "measles")) |>
#'   obt_setup_export()
obt_setup_export <- function(obtops, to = NULL) {
  check_obt_setup(obtops)
  to <- check_export_folder(to, folder = obtops$folder)

  add_operation(obtops, type = "setup-export", args = list(to = to))
}

#' Read an `.xlsx` file into the setup
#'
#' @description
#' Records the import. At commit time the content of `from` is written into
#' the working setup. The file keeps the layout the setup's own export writes,
#' so a file [obt_setup_export()] produced is a file this verb reads.
#'
#' The file has to be there when the verb is called, so a path typed wrong is
#' caught while it still costs nothing.
#'
#' Like every verb this one records and answers the recipe. Nothing is read
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt_setup` recipe.
#' @param from The `.xlsx` file to read.
#'
#' @return The recipe, with the import operation added.
#'
#' @seealso [obt_setup()] to start a setup recipe, [obt_setup_export()] to
#'   write one out.
#'
#' @export
#'
#' @examples
#' setup <- system.file("extdata", "generic-test-setup.xlsb", package = "obt")
#'
#' content <- file.path(tempdir(), "setup-content.xlsx")
#' invisible(file.create(content))
#'
#' obt_setup(from = setup, folder = file.path(tempdir(), "measles")) |>
#'   obt_setup_import(from = content)
obt_setup_import <- function(obtops, from) {
  check_obt_setup(obtops)
  from <- check_exchange_file(from)

  add_operation(
    obtops,
    type = "setup-import",
    args = list(from = path_relative(from, folder = obtops$folder))
  )
}

#' Update the translation tags of the setup
#'
#' @description
#' Records the tag update. At commit time the setup goes over its own labels
#' and gives every one of them a tag, and makes every tag unique.
#'
#' Like every verb this one records and answers the recipe. Nothing is written
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt_setup` recipe.
#'
#' @return The recipe, with the tag operation added.
#'
#' @seealso [obt_setup()] to start a setup recipe,
#'   [obt_setup_languages()] for the languages the setup holds.
#'
#' @export
#'
#' @examples
#' setup <- system.file("extdata", "generic-test-setup.xlsb", package = "obt")
#'
#' obt_setup(from = setup, folder = file.path(tempdir(), "measles")) |>
#'   obt_setup_tags()
obt_setup_tags <- function(obtops) {
  check_obt_setup(obtops)

  add_operation(obtops, type = "setup-tags")
}

#' Write the setup out as an `.xlsx` file
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_setup_export <- function(obtops, operation, stage, state) {
  setup <- setup_working_file(obtops)
  to <- path_absolute(operation$args$to, folder = obtops$folder)

  ensure_folder(to)

  ran <- driver_setup_export(setup = setup, to = to)
  written <- summary_value(ran$summary, SETUP_EXPORT_KEY)

  if (!is_set(written)) {
    return(list(produced = character(), state = list()))
  }

  list(produced = c(export = written), state = list(setup_export = written))
}

#' Read an `.xlsx` file into the setup
#'
#' The file is looked for again here. A recipe is built minutes before it is
#' run, and a file can be moved in between.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_setup_import <- function(obtops, operation, stage, state) {
  setup <- setup_working_file(obtops)
  from <- path_absolute(operation$args$from, folder = obtops$folder)

  check_file_there(from, what = "the file to read into the setup")

  driver_setup_import(setup = setup, from = from)

  list(produced = character(), state = list(setup_import = from))
}

#' Update the translation tags of the setup
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_setup_tags <- function(obtops, operation, stage, state) {
  # The setup is resolved before the wrapper is called. R holds an argument
  # back until it is read, and a wrapper that answers without reading this one
  # would leave the check behind.
  setup <- setup_working_file(obtops)

  driver_setup_tags(setup = setup)

  list(produced = character(), state = list())
}

#' Check the folder an export is written into
#'
#' The default is the `export/` entry of the layout, recorded as the relative
#' path the recipe stores, so a folder that moves takes the export with it.
#'
#' @param to The value to check.
#' @param folder The working folder.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The folder, relative to the working folder where it sits under it.
#' @noRd
check_export_folder <- function(
  to,
  folder,
  arg = rlang::caller_arg(to),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(to)) {
    return(unname(OBT_FOLDERS[["export"]]))
  }

  to <- check_string(to, arg = arg, call = call)

  if (file.exists(to) && !dir.exists(to)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must name a folder.",
        "x" = "A file sits at {.file {to}}."
      ),
      call = call
    )
  }

  path_relative(to, folder = folder)
}

#' Check the file an import reads
#'
#' @param from The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, trimmed.
#' @noRd
check_exchange_file <- function(
  from,
  arg = rlang::caller_arg(from),
  call = rlang::caller_env()
) {
  force(arg)

  from <- check_string(from, arg = arg, call = call)

  wanted <- paste0(".", SETUP_EXCHANGE_EXTENSION)
  workbook <- paste0(".", SETUP_EXTENSION)
  extension <- tolower(tools::file_ext(from))

  if (identical(extension, SETUP_EXTENSION)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must end in {.val {wanted}}.",
        "x" = "{.file {from}} ends in {.val {workbook}}.",
        "i" = "The setup reads a {.val {workbook}} through a form, and a form
               takes a click. The {.val {wanted}} route is the one a script
               can drive.",
        "i" = "Write the workbook out with {.code obt_setup_export()} first."
      ),
      call = call
    )
  }

  if (!identical(extension, SETUP_EXCHANGE_EXTENSION)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must end in {.val {wanted}}.",
        "x" = "{.file {from}} ends in {.val {paste0('.', extension)}}."
      ),
      call = call
    )
  }

  check_file_there(from, what = "the file to read into the setup", call = call)

  from
}
