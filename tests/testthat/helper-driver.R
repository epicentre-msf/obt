# A summary file of the shape a workbook writes, in a folder a test owns.
test_summary_file <- function(
  folder,
  name = "measles-2026",
  # The default is the designer's shape, which carries no outcome key.
  values = c(linelist = "measles-2026.xlsb", sheets = "12"),
  report = c("The build finished.", "Nothing was skipped.")
) {
  ensure_folder(folder)

  path <- summary_path(folder, name)
  lines <- paste0(names(values), "=", values)

  if (length(report) > 0) {
    lines <- c(lines, SUMMARY_MARKER, report)
  }

  writeLines(lines, path)
  path
}

# What `driver_call()` answers, for a test that mocks it.
test_driver_call <- function(output = "OK", status = 0L) {
  function(command, args) {
    list(output = as.character(output), status = as.integer(status))
  }
}

# The same, recording what it was called with.
#
# `state$call` stands where `driver_call()` stands, `state$command` and
# `state$args` are the last command line it built, and `state$calls` counts
# the times it ran. A test that mocks this never reaches Excel.
test_recording_call <- function(output = DRIVER_OK, status = 0L) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L

  state$call <- function(command, args) {
    state$calls <- state$calls + 1L
    state$command <- command
    state$args <- args

    list(output = as.character(output), status = as.integer(status))
  }

  state
}

# The values the generate pair takes, all of them filled.
test_generate_values <- function() {
  list(
    designer = "/work/obt/main/designer.xlsb",
    geo = "/work/geo/geobase.xlsb",
    setup = "/work/setup/setup.xlsb",
    folder = "/work/linelist",
    name = "measles-2026",
    setup_language = "English",
    form_language = "ENG",
    ribbon = "/work/obt/main/ribbon.xlsm"
  )
}
