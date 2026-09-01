# The setup workbook the sources carry, as a file a verb can be given.
#
# It is the OutbreakTools test setup, so the file a verb accepts here is the
# shape of the file a user hands over.
setup_workbook <- function() {
  path <- system.file(
    "extdata",
    "generic-test-setup.xlsb",
    package = "obt"
  )
  skip_if(!nzchar(path), "fixture not installed: generic-test-setup")
  path
}

# A setup recipe on a folder the test owns.
setup_recipe <- function(folder, ...) {
  obt_setup(from = setup_workbook(), folder = folder, ...)
}

# An empty file under a name the setup verbs accept. What it holds is never
# read: the verbs check the name and the run hands the file to Excel.
empty_file <- function(folder, name) {
  ensure_folder(folder)
  path <- file.path(folder, name)
  file.create(path)
  path
}

# What a driver wrapper was handed, for a test that mocks it.
#
# `record` stands in for the wrapper and keeps the arguments it was called
# with. `seen` answers them.
driver_recorder <- function() {
  seen <- NULL

  list(
    record = function(...) {
      seen <<- list(...)
      list(produced = character(), state = list())
    },
    seen = function() seen
  )
}
