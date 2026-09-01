# Build the small channel zips the download reader is tested against.
#
# A real channel zip holds four Excel workbooks of several megabytes, which is
# too heavy to carry in the sources. These copy the only part the reader looks
# at: the file names, flat, with no folder above them. Every name carries the
# channel and the version of the build, the way a release writes it.
#
# Three shapes are kept, because the two channels ship different ones:
#
#   obt-channel-main.zip     a stable release, four workbooks, a version
#   obt-channel-dev.zip      the development build, four workbooks, "latest"
#   obt-channel-legacy.zip   the shape the stable channel shipped in 2024:
#                            three workbooks, the setup named with no channel,
#                            and two helper scripts beside them
#
# The files inside are one line of text each. Nothing in the tests opens them.
#
# They live under the tests and never ship with the package. A channel is
# downloaded, and a copy carried in the sources would go stale the day the
# release build changes a name.
#
# Run with: Rscript data-raw/make-designer-fixture.R

fixtures_folder <- file.path("tests", "testthat", "fixtures")
dir.create(fixtures_folder, recursive = TRUE, showWarnings = FALSE)

release_names <- function(channel, version) {
  paste0(
    c("designer_", "setup_", "msetup_", "_ribbontemplate_"),
    channel,
    "-",
    version,
    ".xlsb"
  )
}

fixtures <- list(
  "obt-channel-main.zip" = release_names("main", "2.0.0"),
  "obt-channel-dev.zip" = release_names("dev", "latest"),
  "obt-channel-legacy.zip" = c(
    "setup-2024-10-19.xlsb",
    "designer_main-2024-10-19.xlsb",
    "_ribbontemplate_main-2024-10-19.xlsb",
    "run_designer_on_windows.R",
    "rundesigner.vbs"
  )
)

for (fixture in names(fixtures)) {
  members <- fixtures[[fixture]]

  staging <- tempfile("obt-channel")
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)

  for (member in members) {
    writeLines(paste("A stand-in for", member), file.path(staging, member))
  }

  # The zip is written from inside the staging folder, so its own path has to
  # be absolute before the working directory moves.
  zip_file <- file.path(normalizePath(fixtures_folder), fixture)
  unlink(zip_file, force = TRUE)

  # `-j` writes the archive flat, the way the release build writes it.
  withr::with_dir(staging, utils::zip(zip_file, members, flags = "-j -q"))

  unlink(staging, recursive = TRUE, force = TRUE)

  cat("\n", fixture, "\n", sep = "")
  print(utils::unzip(zip_file, list = TRUE)[, c("Name", "Length")])
}
