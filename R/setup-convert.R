# Converting an old setup into the one the designer expects.
#
# A user comes with a setup they filled in months ago, built with an older
# OutbreakTools. The designer they just downloaded reads the empty setup that
# came in the same zip, so the content of the old file has to move into a
# fresh copy of that empty one.
#
# The move goes through an `.xlsx` file. The setup workbook has two import
# paths, and the `.xlsx` one is the only one a script can drive: the other
# opens a modal form, and a form takes a click. The layout of that `.xlsx`
# belongs to the setup workbook. It is what the setup's own export writes and
# what its own import reads, so R hands the file over as it stands.
#
# One verb records three steps, and each step is an operation of its own, so
# `obt_operations()` shows the route and `obt_commit()` runs it a step at a
# time.

# The steps one conversion records. `obt_remove()` reads them to see whether
# a recipe still holds a conversion after a step came out.
CONVERT_OPERATION_TYPES <- c(
  "convert-add",
  "convert-export",
  "convert-import"
)

# What the setup's summary calls the file its export wrote.
SETUP_EXPORT_KEY <- "export"

#' Convert an old setup into the one the designer expects
#'
#' @description
#' Records the conversion of a filled setup, usually built with an older
#' OutbreakTools, into the empty setup that came with the files
#' [obt_designer_add()] downloads. That empty setup is the version the
#' downloaded designer reads.
#'
#' The verb records three steps, and the run does them in order:
#'
#' 1. Copy the file into `setup/source/`. The file the user handed over stays
#'    as it is and is never opened by the run.
#' 2. Run the old setup's own export on that copy, which writes an `.xlsx`
#'    into `export/`. A file that already ends in `.xlsx` is taken as it
#'    stands, and this step is skipped.
#' 3. Put a fresh copy of the empty setup at `setup/setup.xlsb`, and read the
#'    `.xlsx` into it. That file is what every later operation works on.
#'
#' The `.xlsx` stays in `export/` once the run is over, so a user can open it
#' and read what came across.
#'
#' The recipe has to hold [obt_designer_add()] already, because step 3 fills a
#' copy of the empty setup the OutbreakTools files carry. The verb says so
#' when the recipe holds none.
#'
#' Like every verb this one records and answers the recipe. Nothing is copied
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt` recipe.
#' @param from The setup to convert, ending in `.xlsb` or `.xlsx`. The file
#'   has to be there when the verb is called.
#' @param overwrite Whether a working setup already in the folder may be
#'   replaced. With `FALSE`, the default, the run stops when one is there,
#'   because that file carries whatever the setup verbs have written into it.
#'
#' @return The recipe, with the steps of the conversion added.
#'
#' @seealso [obt_designer_add()] for the files the conversion fills,
#'   [obt_setup()] to work on one setup on its own.
#'
#' @export
#'
#' @examples
#' old <- system.file("extdata", "generic-test-setup.xlsb", package = "obt")
#'
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_add(type = "dev") |>
#'   obt_setup_convert(from = old)
obt_setup_convert <- function(obtops, from, overwrite = FALSE) {
  check_convert_recipe(obtops)
  check_designer_recorded(obtops)
  from <- check_convert_file(from)
  check_flag(overwrite)

  group <- next_group(obtops)

  obtops <- add_operation(
    obtops,
    group = group,
    type = "convert-add",
    args = list(from = path_relative(from, folder = obtops$folder))
  )

  if (!is_exchange_file(from)) {
    obtops <- add_operation(
      obtops,
      group = group,
      type = "convert-export",
      args = list(to = unname(OBT_FOLDERS[["export"]]))
    )
  }

  add_operation(
    obtops,
    group = group,
    type = "convert-import",
    args = list(overwrite = overwrite)
  )
}

#' Copy the setup to convert into the working folder
#'
#' The copy keeps the name the user knows the file by. A file that is already
#' an `.xlsx` is what the import reads, so it is handed on as it stands.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_convert_add <- function(obtops, operation, stage, state) {
  paths <- obt_paths(obtops)
  from <- path_absolute(operation$args$from, folder = obtops$folder)

  check_file_there(from, what = "the setup to convert")
  ensure_folder(paths$setup_source)

  kept <- copy_path(obtops, "setup_source", from)
  copy_file(from, kept)

  carried <- list(convert_source = kept)

  if (is_exchange_file(kept)) {
    carried$converted <- kept
  }

  list(produced = c(source = kept), state = carried)
}

#' Write the old setup out as an `.xlsx` file
#'
#' The export runs on the copy under `setup/source/`, so the file the user
#' handed over is left alone. The name of the file it writes comes from the
#' setup workbook, which answers it in the summary of the run.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_convert_export <- function(obtops, operation, stage, state) {
  source <- convert_source_file(state)
  to <- path_absolute(operation$args$to, folder = obtops$folder)

  ensure_folder(to)

  ran <- driver_setup_export(setup = source, to = to)
  written <- exported_setup_file(ran)

  list(produced = c(exported = written), state = list(converted = written))
}

#' Read the `.xlsx` into a fresh copy of the empty setup
#'
#' The empty setup is copied over `setup/setup.xlsb` first, so the import
#' fills a file carrying the version the downloaded designer reads. An import
#' that fails leaves that fresh copy behind, and a second run of the same
#' recipe puts a fresh one back over it.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_convert_import <- function(obtops, operation, stage, state) {
  paths <- obt_paths(obtops)
  from <- convert_exchange_file(state)
  template <- convert_template(state)

  check_file_there(from, what = "the file to read into the new setup")
  check_output_free(paths$setup_file, operation$args$overwrite)

  ensure_folder(paths$setup)
  copy_file(template, paths$setup_file)

  driver_setup_import(setup = paths$setup_file, from = from)

  list(
    produced = c(setup = paths$setup_file),
    state = list(setup = paths$setup_file)
  )
}

#' The copy of the setup the conversion works on
#'
#' @param state What the operations before it left behind.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
convert_source_file <- function(state, call = rlang::caller_env()) {
  if (is_set(state$convert_source)) {
    return(state$convert_source)
  }

  cli::cli_abort(
    c(
      "The run found no setup to convert.",
      "i" = "Name one with {.code obt_setup_convert(from = )}."
    ),
    call = call
  )
}

#' The `.xlsx` file the new setup is filled from
#'
#' It is the file the user handed over when that file was already an `.xlsx`,
#' and the one the export wrote otherwise.
#'
#' @param state What the operations before it left behind.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
convert_exchange_file <- function(state, call = rlang::caller_env()) {
  if (is_set(state$converted)) {
    return(state$converted)
  }

  cli::cli_abort(
    c(
      "The run has no {.val .xlsx} file to read into the new setup.",
      "i" = "The step before this one writes it out of the old setup."
    ),
    call = call
  )
}

#' The empty setup the conversion fills
#'
#' @param state What the operations before it left behind.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
convert_template <- function(state, call = rlang::caller_env()) {
  if (is_set(state$empty_setup)) {
    return(state$empty_setup)
  }

  cli::cli_abort(
    c(
      "The run found no empty setup to fill.",
      "i" = "The OutbreakTools files carry it. Add them with
             {.code obt_designer_add()}."
    ),
    call = call
  )
}

#' The file the setup's export wrote
#'
#' The setup workbook names the file it writes and answers that name in its
#' summary. The run reads the path back off that answer.
#'
#' @param ran The run record, as the driver answers it.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
exported_setup_file <- function(ran, call = rlang::caller_env()) {
  written <- summary_value(ran$summary, SETUP_EXPORT_KEY)

  if (!is_set(written)) {
    cli::cli_abort(
      c(
        "The export of the old setup named no file.",
        "i" = "Its summary carries the path under {.val {SETUP_EXPORT_KEY}}."
      ),
      call = call
    )
  }

  written <- absolute_path(written)
  check_file_there(written, what = "the file the old setup wrote", call = call)

  written
}

#' Test whether a path carries the exchange extension
#'
#' @param path The path.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_exchange_file <- function(path) {
  identical(tolower(tools::file_ext(path)), SETUP_EXCHANGE_EXTENSION)
}

#' Fail when a conversion was handed the wrong kind of recipe
#'
#' A conversion fills a copy of the empty setup the OutbreakTools files carry,
#' and a full recipe is what holds those files.
#'
#' @param obtops The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The recipe.
#' @noRd
check_convert_recipe <- function(
  obtops,
  arg = rlang::caller_arg(obtops),
  call = rlang::caller_env()
) {
  if (is_obt_setup(obtops)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an {.cls obt} recipe.",
        "x" = "You supplied an {.cls obt_setup} recipe, which holds a setup
               on its own.",
        "i" = "A conversion fills a copy of the empty setup the
               OutbreakTools files carry, so it runs on a recipe holding
               {.code obt_designer_add()}."
      ),
      call = call
    )
  }

  check_obt(obtops, arg = arg, call = call)
}

#' Fail when the recipe has yet to record the OutbreakTools files
#'
#' @param obtops The recipe.
#' @param call The environment to blame in the error.
#'
#' @return The recipe, invisibly.
#' @noRd
check_designer_recorded <- function(obtops, call = rlang::caller_env()) {
  if (!is.null(last_operation(obtops, "designer-add"))) {
    return(invisible(obtops))
  }

  cli::cli_abort(
    c(
      "The recipe has to carry the OutbreakTools files first.",
      "x" = "It records no {.code obt_designer_add()}.",
      "i" = "The conversion fills a copy of the empty setup those files
             carry."
    ),
    call = call
  )
}

#' Check the file a conversion was given
#'
#' Both extensions are taken. An `.xlsb` goes through the export step first;
#' an `.xlsx` is read into the new setup as it stands.
#'
#' @param from The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, trimmed.
#' @noRd
check_convert_file <- function(
  from,
  arg = rlang::caller_arg(from),
  call = rlang::caller_env()
) {
  force(arg)

  from <- check_string(from, arg = arg, call = call)

  workbook <- paste0(".", SETUP_EXTENSION)
  exchange <- paste0(".", SETUP_EXCHANGE_EXTENSION)
  extension <- tolower(tools::file_ext(from))

  if (!extension %in% c(SETUP_EXTENSION, SETUP_EXCHANGE_EXTENSION)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must end in {.val {workbook}} or {.val {exchange}}.",
        "x" = "{.file {from}} ends in {.val {paste0('.', extension)}}.",
        "i" = "A {.val {workbook}} is a setup workbook. An {.val {exchange}}
               is the file a setup writes its content into."
      ),
      call = call
    )
  }

  check_file_there(from, what = "the setup to convert", call = call)

  from
}
