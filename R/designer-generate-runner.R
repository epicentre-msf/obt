# Generating the linelist.
#
# This is the operation the package is built around, and the first one that
# opens Excel. The designer reads nine entries off its `Main` sheet and
# writes a linelist beside them. The run fills those entries, stages every
# file Excel has to open, fires the generation callback, and reads the run
# back off the summary the designer answers.
#
# Everything the run needs is resolved here, before Excel opens: the designer,
# the setup, the two languages, the geobase and the ribbon. A value that is
# missing stops the run with a message naming the verb that sets it, because a
# designer handed an empty language builds a linelist nobody can use.
#
# An answer of `OK` says the generation ran. What it built is read off the
# summary and off the file itself.

# What the designer's summary calls the file it wrote, and the keys that carry
# the counts of the run.
GENERATION_OUTPUT_KEY <- "linelist"
GENERATION_LOG_KEY <- "log"
GENERATION_COUNT_KEYS <- c("sheets", "variables", "built", "failed")

# The key that carries the parts of the build that did not finish.
GENERATION_FAILED_KEY <- "failed"

#' Generate the linelist
#'
#' Fills the nine entries of the designer's `Main` sheet, runs the generation
#' callback, and reads back what the run produced.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_designer_generate <- function(obtops, operation, stage, state) {
  paths <- obt_paths(obtops)
  name <- operation$args$name
  output <- linelist_path(obtops, name)
  summary <- summary_path(paths$linelist, name)

  check_output_free(output, operation$args$overwrite)

  designer <- generate_designer(obtops, state)
  setup <- generate_setup(paths)
  languages <- generate_languages(state)
  password <- optional_text(operation$args$password)

  folder <- stage_path(stage, paths$linelist)
  ensure_folder(folder)

  # The summary is the one file a run that stopped leaves behind to say why,
  # and the stage takes its folder away when the run closes. It is copied
  # back to the working folder on the way out, whatever stopped the run,
  # before the stage closes. A run that finished has moved it out already,
  # and the copy then finds nothing to do.
  on.exit(stage_keep(stage, summary), add = TRUE)

  staged_designer <- stage_in(stage, designer)
  set_build_in_place(staged_designer)

  ran <- driver_generate(
    designer = staged_designer,
    setup = stage_in(stage, setup),
    folder = folder,
    name = name,
    setup_language = languages$dict,
    form_language = languages$form,
    geo = stage_optional(stage, state$geobase),
    ribbon = stage_optional(stage, state$ribbon),
    password = password,
    debug_password = optional_text(operation$args$debug_password)
  )

  check_generation(ran, name = name)

  produced <- c(
    linelist = stage_out(stage, output),
    summary = stage_out(stage, summary, required = FALSE),
    log = stage_generation_log(stage, ran, folder = paths$linelist)
  )

  list(
    produced = produced[!is.na(produced)],
    state = list(
      linelist = unname(produced[[GENERATION_OUTPUT_KEY]]),
      linelist_password = password,
      counts = generation_counts(ran$summary)
    )
  )
}

#' The designer the run reads
#'
#' A recipe that added the designer answers it straight away. A folder that
#' already holds one from an earlier run answers it too, so a second recipe
#' downloads nothing.
#'
#' @param obtops The recipe.
#' @param state What the operations before it left behind.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
generate_designer <- function(obtops, state, call = rlang::caller_env()) {
  if (is_set(state$designer)) {
    return(state$designer)
  }

  found <- designer_on_disk(obtops)

  if (length(found) == 1) {
    return(found)
  }

  if (length(found) == 0) {
    cli::cli_abort(
      c(
        "The run found no designer to build with.",
        "i" = "Add one with {.code obt_designer_add()} before generating."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "The working folder holds {length(found)} designers.",
      "x" = "They are {.file {basename(found)}}.",
      "i" = "Add the channel you want with {.code obt_designer_add()}, so the
             run knows which one to build with."
    ),
    call = call
  )
}

#' The designers a working folder already holds
#'
#' @param obtops The recipe.
#'
#' @return The absolute paths of every designer under the channel folders.
#' @noRd
designer_on_disk <- function(obtops) {
  paths <- obt_paths(obtops)
  folders <- c(paths$obt_main, paths$obt_dev)
  folders <- folders[dir.exists(folders)]

  if (length(folders) == 0) {
    return(character())
  }

  found <- list.files(
    folders,
    pattern = CHANNEL_ROLES$designer$pattern,
    ignore.case = TRUE,
    full.names = TRUE
  )

  found <- found[
    tolower(tools::file_ext(found)) == CHANNEL_EXTENSION
  ]

  vapply(found, absolute_path, character(1), USE.NAMES = FALSE)
}

#' The setup the designer reads
#'
#' @param paths The paths of the working folder.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
generate_setup <- function(paths, call = rlang::caller_env()) {
  if (file.exists(paths$setup_file)) {
    return(paths$setup_file)
  }

  cli::cli_abort(
    c(
      "The run found no setup to build from.",
      "x" = "Nothing sits at {.file {paths$setup_file}}.",
      "i" = "The designer reads the dictionary, the choices and the analyses
             out of that file."
    ),
    call = call
  )
}

#' The two languages the run is built in
#'
#' The designer writes whatever it is handed into its `Main` sheet, so an
#' empty language blanks the value the workbook carried and builds a linelist
#' with no dictionary translation. Both are read here, and a missing one stops
#' the run.
#'
#' @param state What the operations before it left behind.
#' @param call The environment to blame in the error.
#'
#' @return A list with `dict` and `form`.
#' @noRd
generate_languages <- function(state, call = rlang::caller_env()) {
  absent <- c(
    if (!is_set(state$dict)) "dict",
    if (!is_set(state$form)) "form"
  )

  if (length(absent) == 0) {
    return(list(dict = state$dict, form = state$form))
  }

  cli::cli_abort(
    c(
      "The run needs both languages of the linelist.",
      "x" = "{.arg {absent}} {?was/were} never set.",
      "i" = "Set them with {.code obt_designer_languages()}.",
      "i" = "The designer takes the values it is handed, so an empty one
             builds a linelist nobody can read."
    ),
    call = call
  )
}

#' Stage a file the run uses only when it has one
#'
#' The geobase and the ribbon are both optional. A run without one hands the
#' script an empty value, and the designer builds without it.
#'
#' @param stage The staging record.
#' @param path The file, or `NULL` when the run has none.
#'
#' @return The path Excel is given, or `NA`.
#' @noRd
stage_optional <- function(stage, path) {
  if (!is_set(path)) {
    return(NA_character_)
  }

  stage_in(stage, path)
}

#' Move the designer's own run log out of the staging area
#'
#' The designer writes a log of the build and names it in the summary. It sits
#' beside the linelist, so it comes back with it.
#'
#' @param stage The staging record.
#' @param ran The run record, as `run_driver()` answers it.
#' @param folder Where the log belongs in the working folder.
#'
#' @return An absolute path, or `NA` when the run named no log.
#' @noRd
stage_generation_log <- function(stage, ran, folder) {
  named <- summary_value(ran$summary, GENERATION_LOG_KEY)

  if (!is_set(named)) {
    return(NA_character_)
  }

  stage_out(stage, file.path(folder, basename(named)), required = FALSE)
}

#' Read what the generation run says it built
#'
#' The run answering at all says the generation ran. It says nothing about
#' what came out of it, so the summary is read: the file it names has to be
#' the file that was asked for, and a build that gave up on part of the work
#' is said out loud.
#'
#' @param ran The run record, as `run_driver()` answers it.
#' @param name The name the linelist was asked for under.
#' @param call The environment to blame in the error.
#'
#' @return The run record, invisibly.
#' @noRd
check_generation <- function(ran, name, call = rlang::caller_env()) {
  built <- summary_value(ran$summary, GENERATION_OUTPUT_KEY)

  if (!is_set(built)) {
    cli::cli_warn(c(
      "The generation run named no linelist.",
      "i" = "The file it wrote is read off the working folder instead."
    ))

    return(invisible(ran))
  }

  asked <- tools::file_path_sans_ext(basename(built))

  if (!identical(asked, name)) {
    cli::cli_abort(
      c(
        "The generation run built a linelist under another name.",
        "x" = "It named {.file {basename(built)}}.",
        "i" = "The recipe asked for {.val {name}}."
      ),
      call = call
    )
  }

  warn_generation_failures(ran$summary)

  invisible(ran)
}

#' Say when a build gave up on part of the work
#'
#' @param values The summary, as `read_summary()` answers it.
#'
#' @return `TRUE` when the warning was said, `FALSE` otherwise. Invisibly.
#' @noRd
warn_generation_failures <- function(values) {
  failed <- suppressWarnings(
    as.integer(summary_value(values, GENERATION_FAILED_KEY))
  )

  if (length(failed) != 1 || is.na(failed) || failed <= 0) {
    return(invisible(FALSE))
  }

  cli::cli_warn(c(
    "The build finished with {failed} part{?s} that did not.",
    "i" = "The linelist is there and the log beside it says which."
  ))

  invisible(TRUE)
}

#' The counts a generation run answers
#'
#' @param values The summary, as `read_summary()` answers it.
#'
#' @return A named character vector, holding the count keys the summary
#'   carried.
#' @noRd
generation_counts <- function(values) {
  held <- intersect(GENERATION_COUNT_KEYS, names(values))

  if (length(held) == 0) {
    return(character())
  }

  values[held]
}

#' One value of a summary
#'
#' @param values The summary, as `read_summary()` answers it.
#' @param key The key to read.
#'
#' @return The value, or `NA` when the summary carries none.
#' @noRd
summary_value <- function(values, key) {
  if (length(values) == 0 || !key %in% names(values)) {
    return(NA_character_)
  }

  unname(values[[key]])
}

#' Set the designer's build-in-place flag
#'
#' `clickGenerate` reads a flag inside the designer to decide where it builds:
#' in the Excel the script drives, or in a second one it opens. The run sets
#' the flag, so two runs of the same recipe build the same way.
#'
#' The flag sits in the designer's hidden name store, and the one thing that
#' writes it today is a ribbon button a script cannot press. An entry point
#' taking the value as a string has been asked for, and this function is the
#' one place that changes on the day it lands.
#'
#' Mac Excel runs a single instance, so a build there is in place whatever the
#' flag says. Windows is where the flag decides, and that is where the run
#' says the word, once per session.
#'
#' @param designer The designer workbook the run drives.
#'
#' @return `TRUE` when the flag was written, `FALSE` while it cannot be.
#'   Invisibly.
#' @noRd
set_build_in_place <- function(designer) {
  if (!is_windows() || isTRUE(platform_state$warned_build_in_place)) {
    return(invisible(FALSE))
  }

  platform_state$warned_build_in_place <- TRUE

  cli::cli_warn(c(
    "The designer may open a second Excel to build the linelist.",
    "i" = "Where it builds is a flag inside {.file {basename(designer)}}, and
           the designer's own ribbon button is what writes it.",
    "i" = "The run reads the build back off the summary beside the linelist,
           so it works either way."
  ))

  invisible(FALSE)
}

#' Whether a value was set
#'
#' The run reads its values out of what the operations before it left behind,
#' and an absent one arrives as `NULL`, as `NA` or as an empty string
#' depending on where it came from. All three mean the same thing here.
#'
#' @param value The value.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_set <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value)) {
    return(FALSE)
  }

  nzchar(as.character(value))
}
