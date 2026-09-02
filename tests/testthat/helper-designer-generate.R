# A working folder holding what a generation run reads.
#
# Neither file is ever opened: the run resolves paths, hands them to the
# driver, and the driver is mocked. What matters is that each one is where the
# run looks for it, under the name the channel zip gives it.
generation_folder <- function(
  folder,
  channel = "dev",
  designer = TRUE,
  setup = TRUE,
  ribbon = FALSE
) {
  recipe <- obt(folder = folder)
  paths <- obt_paths(recipe)
  channel_folder <- paths[[channel_spec(channel)$entry]]

  ensure_folder(channel_folder)
  ensure_folder(paths$setup)

  if (isTRUE(designer)) {
    writeLines(
      "designer",
      file.path(channel_folder, paste0("designer_", channel, "-latest.xlsb"))
    )
  }

  if (isTRUE(ribbon)) {
    writeLines(
      "ribbon",
      file.path(
        channel_folder,
        paste0("_ribbontemplate_", channel, "-latest.xlsb")
      )
    )
  }

  if (isTRUE(setup)) {
    writeLines("setup", paths$setup_file)
  }

  recipe
}

# A generation the driver never runs.
#
# The mock stands where `driver_generate()` stands. It records everything it
# was handed, writes the files a real run would leave behind, and answers the
# run record the seam answers. `state$seen` is what a test reads back.
test_generation <- function(
  values = c(
    linelist = "measles-2026.xlsb",
    log = "measles-2026-log.txt",
    sheets = "12",
    variables = "240",
    built = "12",
    failed = "0"
  ),
  report = c("The build finished."),
  writes = TRUE
) {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L

  state$generate <- function(
    designer,
    setup,
    folder,
    name,
    setup_language,
    form_language,
    geo = NA_character_,
    ribbon = NA_character_,
    summary = summary_path(folder, name),
    os = os_name(),
    call = rlang::caller_env()
  ) {
    state$calls <- state$calls + 1L
    state$seen <- list(
      designer = designer,
      setup = setup,
      folder = folder,
      name = name,
      setup_language = setup_language,
      form_language = form_language,
      geo = geo,
      ribbon = ribbon,
      summary = summary
    )

    if (isTRUE(writes)) {
      ensure_folder(folder)
      writeLines("linelist", file.path(folder, paste0(name, ".xlsb")))
      writeLines("summary", summary)

      if ("log" %in% names(values)) {
        writeLines(
          "run log",
          file.path(folder, basename(values[["log"]]))
        )
      }
    }

    list(
      pair = "designer-generate",
      answer = DRIVER_OK,
      source = DRIVER_FROM_ANSWER,
      summary = values,
      report = report
    )
  }

  state
}

# The generation record a recipe carries, with the run's own arguments.
generation_operation <- function(name = "measles-2026", overwrite = FALSE) {
  new_operation("designer-generate", list(name = name, overwrite = overwrite))
}

# What the operations before the generation left behind.
generation_state <- function(dict = "English", form = "ENG", ...) {
  c(list(dict = dict, form = form), list(...))
}

# Keeping the build-in-place word quiet for the length of a test.
#
# `set_build_in_place()` says the word once per session, and it says it on
# Windows alone. A test that drives a generation on Windows therefore carries
# a warning about a flag it never looks at, and the same test on macOS carries
# nothing. Marking the word as already said makes every generation test read
# the same on both systems.
#
# The word itself is covered by the tests that call `set_build_in_place()` on
# its own, and the call the run makes is covered by the test that mocks it.
local_build_in_place_said <- function(.env = parent.frame()) {
  withr::defer(reset_platform_state(), envir = .env)
  platform_state$warned_build_in_place <- TRUE

  invisible(TRUE)
}
