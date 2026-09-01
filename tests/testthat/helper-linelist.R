# A geobase under a name the verbs accept.
#
# What the file holds is never read: the verbs check the path and the run
# hands the file to Excel.
geobase_file <- function(folder, name = "geobase.xlsx") {
  empty_file(folder, name)
}

# A migration file under a name the import accepts.
migration_file <- function(folder, name = "cases-from-the-field.xlsx") {
  empty_file(folder, name)
}

# A working folder holding one linelist, the way a generation run leaves it.
built_linelist <- function(folder, name = "measles-2026") {
  paths <- obt_paths(obt(folder = folder))

  ensure_folder(paths$linelist)
  path <- file.path(paths$linelist, paste0(name, ".", LINELIST_EXTENSION))
  writeLines("linelist", path)

  path
}

# What the linelist answers once it has read a file in. The path of that file
# comes back in the summary, under the key the workbook writes it to.
linelist_answer <- function(key, path = NULL, report = character()) {
  values <- character()

  if (!is.null(path)) {
    values <- stats::setNames(path, key)
  }

  list(
    pair = paste0("linelist-", key),
    answer = DRIVER_OK,
    source = DRIVER_FROM_FILE,
    summary = values,
    report = report
  )
}

# A driver wrapper that records what it was handed and answers a run record.
#
# `state$call` stands where the wrapper stands, `state$seen` is what it was
# given, and `state$calls` counts the times it ran.
test_linelist_driver <- function(answer = linelist_answer("import")) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L

  state$call <- function(..., call = rlang::caller_env()) {
    state$calls <- state$calls + 1L
    state$seen <- list(...)
    answer
  }

  state
}

# A driver wrapper that fails the way a refused run fails.
test_refused_driver <- function(reason = "the file was not imported") {
  function(..., call = rlang::caller_env()) {
    cli::cli_abort("The {.val linelist-import} run failed. {reason}")
  }
}

# The summary a refused run leaves beside the file it was handed.
write_refusal_summary <- function(path, report) {
  summary <- summary_beside(path)

  writeLines(
    c(
      paste0(SUMMARY_OUTCOME_KEY, "=ERROR 0: the file was not imported"),
      "import=",
      SUMMARY_MARKER,
      report
    ),
    summary
  )

  summary
}

# A linelist recipe on a working folder, narrowed from an empty `obt`.
#
# It names no linelist of its own, so the run reads the one file under
# `linelist/`. That is the shape most of these tests want: the verbs under
# test, and the folder to resolve the linelist from.
linelist_recipe <- function(folder) {
  obt_linelist(obt(folder = folder))
}
