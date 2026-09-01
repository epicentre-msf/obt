# What a recipe can hold.
#
# Every operation a verb records is one of the types below. A type carries the
# label the printers show, the one line that describes what it does, the
# OutbreakTools entry point that runs it, and what it is still waiting for.
#
# `waits_on` is the whole of the readiness question. An entry with `NA` there
# can run today. An entry with text there records and prints, and the text
# says what a release has to carry before it can run. Clearing the field is
# the one edit an operation needs when its entry point ships.
#
# `verb` is the call that records the type. `obt_remove()` reads it to name
# where a step came from, and three types share one verb because a conversion
# records three steps in one call.

OBT_OPERATIONS <- list(
  `designer-add` = list(
    label = "Add the designer",
    blurb = "Download the OutbreakTools files and unzip them",
    verb = "obt_designer_add()",
    entry_point = NA_character_,
    waits_on = NA_character_
  ),
  `designer-geobase` = list(
    label = "Add a geobase",
    blurb = "Copy the geobase in and point the run at it",
    verb = "obt_designer_geobase()",
    entry_point = NA_character_,
    waits_on = NA_character_
  ),
  `designer-languages` = list(
    label = "Set the languages",
    blurb = "Carry the setup and interface languages into the run",
    verb = "obt_designer_languages()",
    entry_point = NA_character_,
    waits_on = NA_character_
  ),
  `designer-generate` = list(
    label = "Generate the linelist",
    blurb = "Fill the designer's Main sheet and generate the linelist",
    verb = "obt_designer_generate()",
    entry_point = "clickGenerate",
    waits_on = "silence, and a summary a script can read"
  ),
  `setup-add` = list(
    label = "Add the setup",
    blurb = "Copy the setup in and point the run at it",
    verb = "obt_setup()",
    entry_point = NA_character_,
    waits_on = NA_character_
  ),
  `setup-export` = list(
    label = "Export the setup",
    blurb = "Write the setup out as an .xlsx file",
    verb = "obt_setup_export()",
    entry_point = "RunSetupExport",
    waits_on = "an entry point a script can call"
  ),
  `setup-import` = list(
    label = "Import into the setup",
    blurb = "Read an .xlsx file into the working setup",
    verb = "obt_setup_import()",
    entry_point = "RunSetupImportFile",
    waits_on = "an entry point a script can call"
  ),
  `setup-fake` = list(
    label = "Make up records",
    blurb = "Fill every worksheet of the setup with made-up records",
    verb = "obt_fake()",
    entry_point = NA_character_,
    waits_on = NA_character_
  ),
  `setup-tags` = list(
    label = "Update the setup tags",
    blurb = "Give every label a tag and make every tag unique",
    verb = "obt_setup_tags()",
    entry_point = "RunSetupTags",
    waits_on = "an entry point a script can call"
  ),
  `convert-add` = list(
    label = "Add the setup to convert",
    blurb = "Copy the old setup in and leave the file as it is",
    verb = "obt_setup_convert()",
    entry_point = NA_character_,
    waits_on = NA_character_
  ),
  `convert-export` = list(
    label = "Write the old setup out",
    blurb = "Run the old setup's own export to get an .xlsx file",
    verb = "obt_setup_convert()",
    entry_point = "RunSetupExport",
    waits_on = "an entry point a script can call"
  ),
  `convert-import` = list(
    label = "Fill the new setup",
    blurb = "Read the .xlsx into a fresh copy of the empty setup",
    verb = "obt_setup_convert()",
    entry_point = "RunSetupImportFile",
    waits_on = "an entry point a script can call"
  ),
  `linelist-add` = list(
    label = "Add the linelist",
    blurb = "Copy the linelist in and point the run at it",
    verb = "obt_linelist()",
    entry_point = NA_character_,
    waits_on = NA_character_
  ),
  `linelist-geobase` = list(
    label = "Import a geobase",
    blurb = "Import a geobase into a generated linelist",
    verb = "obt_linelist_geobase()",
    entry_point = "RunImportGeobase",
    waits_on = "an entry point a script can call"
  ),
  `linelist-import` = list(
    label = "Import a migrated file",
    blurb = "Read a file another linelist wrote for migration",
    verb = "obt_linelist_import()",
    entry_point = "RunImportData",
    waits_on = "an entry point a script can call"
  ),
  `linelist-export` = list(
    label = "Run an export",
    blurb = "Write an export out of the linelist",
    verb = "obt_linelist_export()",
    entry_point = "RunExport",
    waits_on = "an entry point a script can call"
  )
)

#' Look up what a type is
#'
#' @param type The operation type name.
#'
#' @return The entry of `OBT_OPERATIONS` for that type.
#' @noRd
operation_spec <- function(type) {
  OBT_OPERATIONS[[type]]
}

#' Build one operation record
#'
#' @param type The operation type name, already checked.
#' @param args The arguments the verb was given, as a named list.
#' @param group The verb call the step came from. Steps sharing one are
#'   removed together, because one call recorded all of them.
#'
#' @return A list with the type, its arguments, the call it came from, and
#'   whether it is waiting on the workbooks.
#' @noRd
new_operation <- function(type, args = list(), group = NA_integer_) {
  list(
    type = type,
    args = args,
    group = group,
    waiting = !is.na(operation_spec(type)$waits_on)
  )
}

#' Record one operation on a recipe
#'
#' Every verb ends here. The record is appended, so the list keeps the order
#' the verbs were called in, which is the order `obt_commit()` runs them in.
#'
#' @param obtops The recipe.
#' @param type The operation type name.
#' @param args The arguments the verb was given, as a named list.
#' @param group The verb call the step came from. The default opens a new one,
#'   which is what a verb recording a single step wants. A verb recording
#'   several reads one with `next_group()` and hands it to every call.
#' @param arg The name of the recipe argument, for the error message.
#' @param call The environment to blame in the error.
#'
#' @return The recipe, with the operation added.
#' @noRd
add_operation <- function(
  obtops,
  type,
  args = list(),
  group = NULL,
  arg = rlang::caller_arg(obtops),
  call = rlang::caller_env()
) {
  check_obt(obtops, arg = arg, call = call)
  check_operation_type(type, call = call)
  check_operation_args(args, call = call)

  if (is.null(group)) {
    group <- next_group(obtops)
  }

  obtops$operations <- c(
    obtops$operations,
    list(new_operation(type, args, group = group))
  )

  obtops
}

#' The next verb call a recipe has room for
#'
#' A verb that records several steps reads one of these and hands it to every
#' call, so `obt_remove()` can take the steps out together.
#'
#' @param obtops The recipe.
#'
#' @return A single integer, above every group the recipe already holds.
#' @noRd
next_group <- function(obtops) {
  groups <- operation_groups(obtops)
  groups <- groups[!is.na(groups)]

  if (length(groups) == 0) {
    return(1L)
  }

  max(groups) + 1L
}

#' The verb call every step of a recipe came from
#'
#' A record built by hand carries none, and reads as `NA`.
#'
#' @param obtops The recipe.
#'
#' @return An integer vector, one entry per operation.
#' @noRd
operation_groups <- function(obtops) {
  vapply(
    obtops$operations,
    function(operation) {
      if (is.null(operation$group)) NA_integer_ else as.integer(operation$group)
    },
    integer(1)
  )
}

#' Fail when a type is unknown
#'
#' @param type The value to check.
#' @param call The environment to blame in the error.
#'
#' @return The type.
#' @noRd
check_operation_type <- function(type, call = rlang::caller_env()) {
  if (!is.character(type) || length(type) != 1 || is.na(type)) {
    cli::cli_abort(
      "An operation type must be a single string.",
      call = call
    )
  }

  if (!type %in% names(OBT_OPERATIONS)) {
    known <- names(OBT_OPERATIONS)
    cli::cli_abort(
      c(
        "{.val {type}} is not an operation this package knows.",
        "i" = "The types it knows are {.val {known}}."
      ),
      call = call
    )
  }

  type
}

#' Fail when the arguments of an operation are unusable
#'
#' A record is read back by the printers and by the run, both of which reach
#' arguments by name, so every argument carries one.
#'
#' @param args The value to check.
#' @param call The environment to blame in the error.
#'
#' @return The arguments.
#' @noRd
check_operation_args <- function(args, call = rlang::caller_env()) {
  if (!is.list(args)) {
    cli::cli_abort(
      "The arguments of an operation must be a list.",
      call = call
    )
  }

  if (length(args) == 0) {
    return(args)
  }

  named <- names(args)

  if (is.null(named) || any(is.na(named)) || !all(nzchar(named))) {
    cli::cli_abort(
      "Every argument of an operation must carry a name.",
      call = call
    )
  }

  if (anyDuplicated(named) > 0) {
    duplicated_names <- unique(named[duplicated(named)])
    cli::cli_abort(
      c(
        "Every argument of an operation must carry its own name.",
        "x" = "{.val {duplicated_names}} appear{?s} more than once."
      ),
      call = call
    )
  }

  args
}

#' How many operations of a recipe wait on the workbooks
#'
#' @param obtops The recipe.
#'
#' @return An integer.
#' @noRd
count_waiting <- function(obtops) {
  waiting <- vapply(
    obtops$operations,
    function(operation) operation$waiting,
    logical(1)
  )

  sum(waiting)
}

#' The last operation of a type a recipe holds
#'
#' A verb can be called twice; the value the run uses is the one recorded
#' last, so that is the one the printers show.
#'
#' @param obtops The recipe.
#' @param type The operation type name.
#'
#' @return The operation record, or `NULL` when the recipe holds none.
#' @noRd
last_operation <- function(obtops, type) {
  matches <- Filter(
    function(operation) identical(operation$type, type),
    obtops$operations
  )

  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[length(matches)]]
}
