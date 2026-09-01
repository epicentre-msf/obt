# The whole setup the sources carry, put where a recipe reads its own.
#
# The dictionary check reads the working setup of the folder, so a test that
# wants the check to run has to put a setup there first. This is the
# OutbreakTools test setup, so the languages the check refuses are the
# languages a real file refuses.
#
# `folder` is the working folder, usually a `withr::local_tempdir()`.
setup_in_folder <- function(folder) {
  source_file <- system.file(
    "extdata",
    "generic-test-setup.xlsb",
    package = "obt"
  )
  skip_if(!nzchar(source_file), "fixture not installed: generic-test-setup")

  recipe <- obt(folder = folder)
  paths <- obt_paths(recipe)

  ensure_folder(paths$setup)
  file.copy(source_file, paths$setup_file, overwrite = TRUE)

  recipe
}

# The languages that setup holds, as the reader answers them.
setup_fixture_languages <- function() {
  c("English", "Français", "Espanol", "Portugues", "Arabic")
}
