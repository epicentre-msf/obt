# The channel zips the tests carry, standing in for a real download. They sit
# under the tests and never ship with the package: a channel is downloaded,
# and a carried copy would go stale the day the release build changes a name.
#
#   main     a stable release, four workbooks, a version
#   dev      the development build, four workbooks, "latest"
#   legacy   the shape the stable channel shipped in 2024
channel_fixture <- function(shape = "main") {
  name <- paste0("obt-channel-", shape, ".zip")
  path <- test_path("fixtures", name)
  skip_if(!file.exists(path), paste0("fixture missing: ", name))
  path
}

# A download that copies a zip into place and counts how often it was asked.
# `state$download` is what a test hands to `local_mocked_bindings()`, and
# `state$calls` says whether a second run downloaded anything.
test_downloader <- function(zip) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L

  state$download <- function(url, path, call = rlang::caller_env()) {
    state$calls <- state$calls + 1L
    state$url <- url
    ensure_folder(dirname(path))
    file.copy(zip, path, overwrite = TRUE)
    invisible(path)
  }

  state
}

# The names a release build writes into a channel zip.
test_channel_names <- function(channel = "main", version = "2.0.0") {
  paste0(
    c("designer_", "setup_", "msetup_", "_ribbontemplate_"),
    channel,
    "-",
    version,
    ".xlsb"
  )
}

# The names the stable channel shipped in 2024: three workbooks, the setup
# with no channel in its name, and two helper scripts beside them.
test_legacy_names <- function() {
  c(
    "setup-2024-10-19.xlsb",
    "designer_main-2024-10-19.xlsb",
    "_ribbontemplate_main-2024-10-19.xlsb",
    "run_designer_on_windows.R",
    "rundesigner.vbs"
  )
}
