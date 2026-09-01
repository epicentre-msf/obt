# A machine a run can happen on, without reading the real one.
#
# `obt_commit()` reads the operating system and then the whole machine. Both
# are answered here, so the tests run the same on every system the package is
# checked on and no run touches Excel's own folder.
local_run_machine <- function(
  os = "macos",
  staging = NA_character_,
  excel_version = EXCEL_VERSION_PROVEN,
  .env = parent.frame()
) {
  platform <- test_platform(
    os = os,
    staging = staging,
    excel_version = excel_version
  )

  testthat::local_mocked_bindings(
    os_name = function() os,
    platform_guard = function(obtops, call = rlang::caller_env()) platform,
    .env = .env
  )

  platform
}

# A download that never reaches the network, for the runs that add the
# designer. The fixture is copied into the folder the run asked for.
local_channel_download <- function(shape = "dev", .env = parent.frame()) {
  state <- test_downloader(channel_fixture(shape))

  testthat::local_mocked_bindings(
    download_zip = state$download,
    .env = .env
  )

  state
}

# A download that fails, for the runs that have to stop.
local_failed_download <- function(
  reason = "The network is down.",
  .env = parent.frame()
) {
  testthat::local_mocked_bindings(
    download_zip = function(url, path, call = rlang::caller_env()) {
      cli::cli_abort(reason, call = call)
    },
    .env = .env
  )

  reason
}

# A recipe of the two operations the package runs today, on a folder the test
# owns.
runnable_recipe <- function(folder) {
  obt(folder = folder) |>
    obt_designer_add(type = "dev") |>
    obt_designer_languages(form = "ENG")
}

# The lines of the one log a run wrote.
run_log_of <- function(folder) {
  logs <- list.files(
    obt_paths(obt(folder = folder))$log,
    full.names = TRUE
  )

  if (length(logs) != 1) {
    return(character())
  }

  readLines(logs[[1]], warn = FALSE)
}
