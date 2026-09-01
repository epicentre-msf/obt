# A workbook the switch can be read off, which nothing ever opens.
test_workbook <- function(folder, name = "measles-2026.xlsb") {
  ensure_folder(folder)

  path <- file.path(folder, name)
  writeLines("workbook", path)

  path
}

# One argument as the script receives it, with the quoting of the system it
# was built for taken back off.
test_unquote <- function(value) {
  gsub("^['\"]|['\"]$", "", value)
}

# A quiet run the driver never makes.
#
# The mock stands where `driver_call()` stands, so everything above it runs:
# the command line, the answer line, and the summary the seam reads back. It
# writes the summary the script would have written, at the path it was handed.
# `state$seen` is what a test reads back.
test_quiet_call <- function(
  stored = SILENT_ON,
  output = DRIVER_OK,
  status = 0L,
  writes = TRUE
) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L

  state$call <- function(command, args) {
    state$calls <- state$calls + 1L

    values <- test_unquote(args)
    state$seen <- list(command = command, args = values)

    if (isTRUE(writes)) {
      writeLines(
        c("outcome=OK", paste0(QUIET_VALUE_KEY, "=", stored)),
        values[[length(values)]]
      )
    }

    list(output = as.character(output), status = as.integer(status))
  }

  state
}

# A quiet run the wrapper never makes.
#
# The mock stands where `driver_quiet()` stands, so a test of the two verbs
# never builds a command line. `stored` is what the run says the workbook
# holds, and `NA` is a summary that carries no value at all.
test_quiet_run <- function(stored = SILENT_ON) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L

  state$quiet <- function(
    workbook,
    action,
    value = NA_character_,
    switch_name = SILENT_SWITCH_NAME,
    summary,
    os = os_name(),
    call = rlang::caller_env()
  ) {
    state$calls <- state$calls + 1L
    state$seen <- list(
      workbook = workbook,
      action = action,
      value = value,
      switch_name = switch_name,
      summary = summary
    )

    list(
      pair = "quiet",
      answer = DRIVER_OK,
      source = DRIVER_FROM_ANSWER,
      summary = if (is.na(stored)) character() else c(silent = stored),
      report = character()
    )
  }

  state
}
