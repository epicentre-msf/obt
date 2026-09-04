# What a generated linelist can be asked to write out.
#
# One verb, and it covers every export the linelist offers. `type` says which
# kind: the migration file another linelist reads back, or one of the exports
# the setup defines. A new kind of export arrives as a new `type`.
#
# The verb records and answers the recipe the way every verb of the package
# does. `obt_commit()` is what opens Excel.
#
# The export runs inside the linelist workbook, so it waits on an entry point
# a script can call. The runner below resolves its values and calls one
# wrapper. The day the pair ships, that wrapper is the one function that
# changes.
#
# The linelist itself is never an argument. A recipe that generated one hands
# it on through the run state, and a working folder holding one answers it
# too.

# The kinds of export the verb takes.
EXPORT_MIGRATION <- "migration"
EXPORT_SPECIFIC <- "specific"
EXPORT_TYPES <- c(EXPORT_MIGRATION, EXPORT_SPECIFIC)

# The name the linelist reads as a request for the migration export. The
# entry point takes the name of the export, and an empty one means migration.
MIGRATION_EXPORT_NAME <- ""

# The exports that can read another linelist. The three write the migration
# files, and a second linelist is a file they read instead of this one. The
# analysis export and the numbered exports read the running linelist alone.
EXPORTS_TAKING_ANOTHER <- c("migration", "geo", "historic")

# What the linelist's summary calls the files it wrote. One export can write
# more than one file, and the linelist puts them on one line.
LINELIST_EXPORT_KEY <- "export"
LINELIST_EXPORT_SEPARATOR <- ", "

#' Write an export out of a generated linelist
#'
#' @description
#' Records the export. At commit time the linelist runs it and writes the file
#' into `export/` in the working folder, or into the folder `to` names.
#'
#' `type` says which export runs:
#'
#' * `"migration"` writes the file [obt_linelist_import()] reads back. The
#'   linelist names the file itself, so `name` is left out.
#' * `"specific"` runs one of the exports the setup defines, and `name` says
#'   which one.
#'
#' A migration export can read a second linelist instead of the one the run
#' drives. `other` names that file and `password` is what it opens with.
#' Clicking the button raises a prompt for the password, and a scripted run
#' has nobody to type into that prompt, so the verb carries it. It is hidden
#' wherever a recipe is printed.
#'
#' The exports that read a second linelist are `"migration"`, `"geo"` and
#' `"historic"`. The analysis export and the numbered exports read the
#' linelist the run drives, and they refuse `other`.
#'
#' Like every verb this one records and answers the recipe. Nothing is written
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt_linelist` recipe.
#' @param type The kind of export: `"migration"` or `"specific"`.
#' @param name The name of the export to run, read on a `"specific"` export.
#'   The linelist takes `"migration"`, `"geo"`, `"historic"`, `"analysis"`,
#'   and the number of an export the setup defines.
#' @param to The folder the file is written into. The default is `export/` in
#'   the working folder. A folder that is missing is created by the run.
#' @param other The linelist the export reads from. The default reads the
#'   linelist the run drives.
#' @param password The password `other` opens with. Left out where that file
#'   carries none.
#'
#' @return The recipe, with the export added.
#'
#' @seealso [obt_linelist_import()] for the verb that reads a migration file
#'   back, [obt_designer_generate()] for the linelist this one writes out of.
#'
#' @export
#'
#' @examples
#' # A recipe narrowed from an `obt` reads the linelist the folder holds.
#' obt_linelist(obt(folder = file.path(tempdir(), "measles"))) |>
#'   obt_linelist_export(type = "migration")
#'
#' obt_linelist(obt(folder = file.path(tempdir(), "measles"))) |>
#'   obt_linelist_export(type = "specific", name = "analysis")
obt_linelist_export <- function(
  obtops,
  type = "migration",
  name = NULL,
  to = NULL,
  other = NULL,
  password = NULL
) {
  check_obt_linelist(obtops)
  type <- check_export_type(type)
  name <- check_export_name(name, type = type)
  to <- check_export_folder(to, folder = obtops$folder)
  other <- check_export_other(other, type = type, name = name)
  password <- check_export_password(password, other = other)

  if (!is.null(other)) {
    other <- path_relative(other, folder = obtops$folder)
  }

  add_operation(
    obtops,
    type = "linelist-export",
    args = list(
      type = type,
      name = name,
      to = to,
      other = other,
      password = password
    )
  )
}

#' Write an export out of the linelist
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_linelist_export <- function(obtops, operation, stage, state) {
  linelist <- linelist_target(obtops, state)
  to <- path_absolute(operation$args$to, folder = obtops$folder)
  other <- export_other(obtops, operation$args$other)

  ensure_folder(to)

  ran <- driver_linelist_export(
    linelist = linelist,
    name = export_name(operation$args$type, operation$args$name),
    to = to,
    password = linelist_password(state),
    other = optional_text(other),
    other_password = optional_text(operation$args$password)
  )

  written <- export_written(ran, to = to)

  if (length(written) == 0) {
    return(list(produced = character(), state = list()))
  }

  names(written) <- rep("export", length(written))

  list(produced = written, state = list(export = written))
}

#' The second linelist an export reads, resolved for the run
#'
#' The file is looked for again here. A recipe is built minutes before it is
#' run, and a file can be moved in between.
#'
#' @param obtops The recipe.
#' @param other The path recorded on the operation, or `NULL`.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path, or `NULL`.
#' @noRd
export_other <- function(obtops, other, call = rlang::caller_env()) {
  if (is.null(other)) {
    return(NULL)
  }

  path <- path_absolute(other, folder = obtops$folder)

  check_file_there(path, what = "the linelist to export from", call = call)

  path
}

#' The name the entry point reads as the export to run
#'
#' The migration export is asked for by an empty name. Every other export is
#' asked for by the name the setup gave it.
#'
#' @param type The kind of export.
#' @param name The name recorded on the operation.
#'
#' @return A single string.
#' @noRd
export_name <- function(type, name) {
  if (identical(type, EXPORT_MIGRATION)) {
    return(MIGRATION_EXPORT_NAME)
  }

  name
}

#' The files an export run says it wrote
#'
#' The run answering at all says the linelist was driven. What it wrote is the
#' paths it names in its summary. One export can write more than one file, and
#' the linelist puts them on one line, so the line is split back apart here.
#'
#' A run that names nothing is taken as done and says so, because the caller
#' has a folder to look in.
#'
#' @param ran The run record, as `run_driver()` answers it.
#' @param to The folder the export was written into.
#'
#' @return The paths, or an empty character vector.
#' @noRd
export_written <- function(ran, to) {
  named <- summary_value(ran$summary, LINELIST_EXPORT_KEY)

  if (!is_set(named)) {
    cli::cli_warn(c(
      "The export run named no file it wrote.",
      "i" = "It answered, so the run is taken as done.",
      "i" = "Look under {.file {to}} for what it left."
    ))

    return(character())
  }

  paths <- trimws(strsplit(named, LINELIST_EXPORT_SEPARATOR, fixed = TRUE)[[1]])

  paths[nzchar(paths)]
}

#' Check the kind of export a verb was given
#'
#' @param type The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The kind, in lower case.
#' @noRd
check_export_type <- function(
  type,
  arg = rlang::caller_arg(type),
  call = rlang::caller_env()
) {
  # The label is read before `type` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  type <- tolower(check_string(type, arg = arg, call = call))

  if (!type %in% EXPORT_TYPES) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.val {EXPORT_TYPES}}.",
        "x" = "You supplied {.val {type}}.",
        "i" = "{.val {EXPORT_MIGRATION}} writes the file another linelist
               reads back. {.val {EXPORT_SPECIFIC}} runs one of the exports
               the setup defines."
      ),
      call = call
    )
  }

  type
}

#' Check the name of the export a verb was given
#'
#' A specific export is asked for by name, so the name is required there. The
#' migration export is named by the linelist itself, and a name handed to it
#' would be dropped in silence.
#'
#' @param name The value to check.
#' @param type The kind of export, already checked.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The name, trimmed, or `NULL`.
#' @noRd
check_export_name <- function(
  name,
  type,
  arg = rlang::caller_arg(name),
  call = rlang::caller_env()
) {
  force(arg)

  if (identical(type, EXPORT_SPECIFIC)) {
    return(check_string(name, arg = arg, call = call))
  }

  if (is.null(name)) {
    return(NULL)
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} is read on a {.val {EXPORT_SPECIFIC}} export.",
      "x" = "This one is a {.val {EXPORT_MIGRATION}} export, and the linelist
             names the file it writes.",
      "i" = "Set {.arg type} to {.val {EXPORT_SPECIFIC}} to run an export by
             name."
    ),
    call = call
  )
}

#' Check the second linelist an export reads
#'
#' The three exports that write the migration files can read a second
#' linelist. The analysis export and the numbered exports read the linelist
#' the run drives, and the workbook turns a second one down, so the verb turns
#' it down first.
#'
#' @param other The value to check.
#' @param type The kind of export, already checked.
#' @param name The name of the export, already checked.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, trimmed, or `NULL`.
#' @noRd
check_export_other <- function(
  other,
  type,
  name,
  arg = rlang::caller_arg(other),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(other)) {
    return(NULL)
  }

  word <- export_name(type, name)

  if (nzchar(word) && !tolower(word) %in% EXPORTS_TAKING_ANOTHER) {
    cli::cli_abort(
      c(
        "The {.val {word}} export reads the linelist the run drives.",
        "x" = "{.arg {arg}} names another one.",
        "i" = "The exports that read a second linelist are
               {.val {EXPORTS_TAKING_ANOTHER}}."
      ),
      call = call
    )
  }

  other <- check_string(other, arg = arg, call = call)
  check_file_there(other, what = "the linelist to export from", call = call)

  other
}

#' Check the password the second linelist opens with
#'
#' A password with no second linelist to open is turned down here. The value
#' itself is checked the way every password is, and kept as it stands.
#'
#' @param password The value to check.
#' @param other The second linelist, already checked.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The password, or `NULL`.
#' @noRd
check_export_password <- function(
  password,
  other,
  arg = rlang::caller_arg(password),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(password)) {
    return(NULL)
  }

  if (is.null(other)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} opens the linelist {.arg other} names.",
        "x" = "This export reads the linelist the run drives, which is
               already open.",
        "i" = "Name the file with {.arg other}, or leave {.arg {arg}} out."
      ),
      call = call
    )
  }

  check_password(password, arg = arg, call = call)
}
