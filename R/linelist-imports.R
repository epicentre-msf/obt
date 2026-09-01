# What a generated linelist can be asked to read in.
#
# Two verbs, and both act on a linelist the recipe already built. Each records
# an operation and answers the recipe the way every verb of the package does.
# `obt_commit()` is what opens Excel.
#
# Both run inside the linelist workbook, so both wait on an entry point a
# script can call. Each has a runner below it, and each runner resolves its
# values and calls one wrapper. The day a pair ships, that wrapper is the one
# function that changes.
#
# The linelist itself is never an argument. A recipe that generated one hands
# it on through the run state, and a working folder holding one answers it
# too, so a user names the file once and the verbs read it from there.

# The extension a migration file carries. It is what another linelist wrote
# with a migration export, and what this one reads back.
MIGRATION_EXTENSION <- "xlsx"

# What happens to the rows the linelist already holds. `append` puts the
# import under them; `replace` clears them and starts at the first row.
PASTING_RULES <- c("append", "replace")

# The word the linelist reads as an answer to a warning it raised.
FORCE_YES <- "Yes"
FORCE_NO <- "No"

# What the linelist's summary calls the files it read.
LINELIST_GEOBASE_KEY <- "geobase"
LINELIST_IMPORT_KEY <- "import"

#' Read a geobase into a generated linelist
#'
#' @description
#' Records the geobase import. At commit time the file is copied into `geo/`
#' in the working folder and read into the linelist the recipe generated.
#'
#' This is the other half of [obt_designer_geobase()]. That verb hands the geobase
#' to the designer, so the linelist is built with it. This one reads a geobase
#' into a linelist that is already built.
#'
#' The file has to be there when the verb is called, so a path typed wrong is
#' caught while it still costs nothing.
#'
#' Like every verb this one records and answers the recipe. Nothing is read
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt_linelist` recipe.
#' @param path The geobase file, as downloaded from the geobase web app.
#'
#' @return The recipe, with the geobase import added.
#'
#' @seealso [obt_designer_geobase()] to build a linelist with a geobase,
#'   [obt_designer_generate()] for the linelist this one reads into.
#'
#' @export
#'
#' @examples
#' geobase <- file.path(tempdir(), "geobase.xlsx")
#' invisible(file.create(geobase))
#'
#' obt_linelist(obt(folder = file.path(tempdir(), "measles"))) |>
#'   obt_linelist_geobase(path = geobase)
obt_linelist_geobase <- function(obtops, path) {
  check_obt_linelist(obtops)
  path <- check_geobase_file(path)

  add_operation(
    obtops,
    type = "linelist-geobase",
    args = list(path = path_relative(path, folder = obtops$folder))
  )
}

#' Read a migrated file into a generated linelist
#'
#' @description
#' Records the migration import. At commit time the file another linelist
#' wrote with a migration export is read into the linelist the recipe
#' generated.
#'
#' The linelist checks the file before it takes it, and warns on three things:
#' the file carries no metadata, it records no language, or it was written in
#' another language. A person clicking the button reads that warning and
#' decides. A scripted run has nobody to decide, so it stops and reports which
#' of the three stopped it.
#'
#' `force = TRUE` is what pushes past all three. Use it once you have looked
#' at the file yourself. [obt_describe()] names a recipe that forces, so it is
#' said out loud before Excel opens.
#'
#' Like every verb this one records and answers the recipe. Nothing is read
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt_linelist` recipe.
#' @param from The migration file, ending in `.xlsx`. The file has to be there
#'   when the verb is called.
#' @param rule What happens to the rows the linelist already holds.
#'   `"append"`, the default, puts the import under them. `"replace"` clears
#'   them and starts at the first row.
#' @param force Whether the import goes past a warning the linelist raised
#'   about the file.
#'
#' @return The recipe, with the import added.
#'
#' @seealso [obt_designer_generate()] for the linelist this one reads into.
#'
#' @export
#'
#' @examples
#' migrated <- file.path(tempdir(), "cases-from-the-field.xlsx")
#' invisible(file.create(migrated))
#'
#' obt_linelist(obt(folder = file.path(tempdir(), "measles"))) |>
#'   obt_linelist_import(from = migrated, rule = "append")
obt_linelist_import <- function(
  obtops,
  from,
  rule = "append",
  force = FALSE
) {
  check_obt_linelist(obtops)
  from <- check_migration_file(from)
  rule <- check_pasting_rule(rule)
  check_flag(force)

  add_operation(
    obtops,
    type = "linelist-import",
    args = list(
      from = path_relative(from, folder = obtops$folder),
      rule = rule,
      force = force
    )
  )
}

#' Read a geobase into the linelist
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_linelist_geobase <- function(obtops, operation, stage, state) {
  linelist <- linelist_target(obtops, state)
  geo <- take_geobase(obtops, operation$args$path)

  ran <- driver_linelist_geobase(linelist = linelist, geo = geo)

  check_geobase_read(ran, geo = geo)

  list(produced = c(geobase = geo), state = list(geobase = geo))
}

#' Read a migrated file into the linelist
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
run_linelist_import <- function(obtops, operation, stage, state) {
  linelist <- linelist_target(obtops, state)
  from <- path_absolute(operation$args$from, folder = obtops$folder)
  forced <- isTRUE(operation$args$force)

  check_file_there(from, what = "the file to read into the linelist")

  summary <- summary_beside(from)

  ran <- tryCatch(
    driver_linelist_import(
      linelist = linelist,
      from = from,
      rule = operation$args$rule,
      force = force_word(forced),
      summary = summary
    ),
    error = function(cnd) {
      stop_import_refused(cnd, summary = summary, forced = forced)
    }
  )

  check_import_read(ran, from = from)

  list(produced = character(), state = list(imported = from))
}

#' The linelist the two verbs read into
#'
#' A recipe that generated one answers it straight away. A folder holding one
#' from an earlier run answers it too, so a second recipe reads into the
#' linelist that is there.
#'
#' @param obtops The recipe.
#' @param state What the operations before it left behind.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
linelist_target <- function(obtops, state, call = rlang::caller_env()) {
  if (is_set(state$linelist)) {
    return(state$linelist)
  }

  found <- linelists_on_disk(obtops)

  if (length(found) == 1) {
    return(found)
  }

  if (length(found) == 0) {
    cli::cli_abort(
      c(
        "The run found no linelist to read into.",
        "i" = "Build one with {.code obt_designer_generate()} earlier in the
               recipe, or put one under {.file linelist/} in the working
               folder."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "The working folder holds {length(found)} linelists.",
      "x" = "They are {.file {basename(found)}}.",
      "i" = "Build the one you want with {.code obt_designer_generate()}, so
             the run knows which one to read into."
    ),
    call = call
  )
}

#' The linelists a working folder already holds
#'
#' @param obtops The recipe.
#'
#' @return The absolute paths of every linelist under `linelist/`.
#' @noRd
linelists_on_disk <- function(obtops) {
  folder <- obt_paths(obtops)$linelist

  if (!dir.exists(folder)) {
    return(character())
  }

  found <- list.files(folder, full.names = TRUE)
  found <- found[
    tolower(tools::file_ext(found)) == LINELIST_EXTENSION
  ]

  vapply(found, absolute_path, character(1), USE.NAMES = FALSE)
}

#' Read what the geobase run says it took in
#'
#' The run answering at all says the linelist was driven. What it read is the
#' path it names in its summary.
#'
#' @param ran The run record, as `run_driver()` answers it.
#' @param geo The geobase the run was handed.
#' @param call The environment to blame in the error.
#'
#' @return The run record, invisibly.
#' @noRd
check_geobase_read <- function(ran, geo, call = rlang::caller_env()) {
  check_file_read(
    ran,
    key = LINELIST_GEOBASE_KEY,
    path = geo,
    what = "geobase",
    call = call
  )
}

#' Read what the import run says it took in
#'
#' @param ran The run record, as `run_driver()` answers it.
#' @param from The file the run was handed.
#' @param call The environment to blame in the error.
#'
#' @return The run record, invisibly.
#' @noRd
check_import_read <- function(ran, from, call = rlang::caller_env()) {
  check_file_read(
    ran,
    key = LINELIST_IMPORT_KEY,
    path = from,
    what = "import",
    call = call
  )
}

#' Read the file a run says it took in, against the file it was handed
#'
#' The two are compared by name. On macOS the workbook is handed a copy
#' inside Excel's own folder, so it answers that path and the folders differ.
#'
#' @param ran The run record, as `run_driver()` answers it.
#' @param key The summary key that carries the path.
#' @param path The file the run was handed.
#' @param what What the file is, in one word, for the message.
#' @param call The environment to blame in the error.
#'
#' @return The run record, invisibly.
#' @noRd
check_file_read <- function(ran, key, path, what, call = rlang::caller_env()) {
  named <- summary_value(ran$summary, key)

  if (!is_set(named)) {
    cli::cli_warn(c(
      "The {what} run named no file it read.",
      "i" = "It was handed {.file {basename(path)}} and it answered, so the
             run is taken as done."
    ))

    return(invisible(ran))
  }

  if (!identical(basename(named), basename(path))) {
    cli::cli_abort(
      c(
        "The {what} run read another file.",
        "x" = "It named {.file {basename(named)}}.",
        "i" = "The recipe handed it {.file {basename(path)}}."
      ),
      call = call
    )
  }

  invisible(ran)
}

#' Say which warning stopped an import
#'
#' The linelist raises three warnings about a migration file it cannot vouch
#' for: no metadata, no language recorded, or written in another language. A
#' run with nobody to answer them stops, and the boxes it swallowed are in the
#' summary it wrote beside the file.
#'
#' A failure the summary says nothing about is raised as it came, so a run
#' that stopped for another reason keeps its own message.
#'
#' @param cnd The condition the wrapper raised.
#' @param summary The file the linelist writes its summary to.
#' @param forced Whether the run was already forcing.
#' @param call The environment to blame in the error.
#'
#' @return Nothing. It always fails.
#' @noRd
stop_import_refused <- function(
  cnd,
  summary,
  forced,
  call = rlang::caller_env()
) {
  read <- read_summary(summary)

  if (!isTRUE(read$found) || length(read$report) == 0) {
    stop(cnd)
  }

  said <- driver_echo(read$report)

  cli::cli_abort(
    c(
      "The linelist turned the file down.",
      "x" = "It said: {.val {said}}",
      if (!isTRUE(forced)) {
        c(
          "i" = "Read the file yourself, then use {.code force = TRUE} to
                 import it anyway."
        )
      }
    ),
    parent = cnd,
    call = call
  )
}

#' The word the linelist reads as an answer to its own warning
#'
#' @param forced Whether the run goes past a warning.
#'
#' @return `"Yes"` or `"No"`.
#' @noRd
force_word <- function(forced) {
  if (isTRUE(forced)) {
    return(FORCE_YES)
  }

  FORCE_NO
}

#' Check the migration file an import reads
#'
#' @param from The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, trimmed.
#' @noRd
check_migration_file <- function(
  from,
  arg = rlang::caller_arg(from),
  call = rlang::caller_env()
) {
  # The label is read before `from` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  from <- check_string(from, arg = arg, call = call)

  wanted <- paste0(".", MIGRATION_EXTENSION)
  extension <- tolower(tools::file_ext(from))

  if (!identical(extension, MIGRATION_EXTENSION)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must end in {.val {wanted}}.",
        "x" = "{.file {from}} ends in {.val {paste0('.', extension)}}.",
        "i" = "A migration export writes an {.val {wanted}} file, and that
               is the file this verb reads."
      ),
      call = call
    )
  }

  check_file_there(
    from,
    what = "the file to read into the linelist",
    call = call
  )

  from
}

#' Check the rule an import follows
#'
#' @param rule The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The rule, in lower case.
#' @noRd
check_pasting_rule <- function(
  rule,
  arg = rlang::caller_arg(rule),
  call = rlang::caller_env()
) {
  force(arg)

  rule <- tolower(check_string(rule, arg = arg, call = call))

  if (!rule %in% PASTING_RULES) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.val {PASTING_RULES}}.",
        "x" = "You supplied {.val {rule}}.",
        "i" = "{.val append} puts the import under the rows the linelist
               holds. {.val replace} clears them first."
      ),
      call = call
    )
  }

  rule
}
