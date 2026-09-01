# The verb

test_that("the verb records the channel and answers the recipe", {
  recipe <- obt(folder = file.path(tempdir(), "measles")) |>
    obt_designer_add(type = "dev")

  expect_s3_class(recipe, "obt")
  expect_length(recipe$operations, 1)
  expect_identical(recipe$operations[[1]]$type, "designer-add")
  expect_identical(
    recipe$operations[[1]]$args,
    list(type = "dev", force = FALSE)
  )
})

test_that("the channel is main when the verb is given none", {
  recipe <- obt_designer_add(obt(folder = tempdir()))

  expect_identical(recipe$operations[[1]]$args$type, "main")
})

test_that("the operation can run today", {
  recipe <- obt_designer_add(obt(folder = tempdir()))

  expect_false(recipe$operations[[1]]$waiting)
})

test_that("the verb writes nothing to disk", {
  folder <- file.path(withr::local_tempdir(), "measles")

  obt(folder = folder) |>
    obt_designer_add(type = "main", force = TRUE)

  expect_false(dir.exists(folder))
})

test_that("the verb refuses a channel it does not know", {
  expect_error(
    obt_designer_add(obt(folder = tempdir()), type = "nightly"),
    "must be one of"
  )
})

test_that("the message names the argument the user wrote", {
  expect_error(
    obt_designer_add(obt(folder = tempdir()), type = "nightly"),
    "`type`"
  )
})

test_that("a channel is read whatever case it is written in", {
  recipe <- obt_designer_add(obt(folder = tempdir()), type = "DEV")

  expect_identical(recipe$operations[[1]]$args$type, "dev")
})

test_that("the verb refuses a force that is not TRUE or FALSE", {
  expect_error(
    obt_designer_add(obt(folder = tempdir()), force = "yes"),
    "force"
  )
})

test_that("the verb refuses anything that is not a recipe", {
  expect_error(obt_designer_add("a folder"), "obt")
})

test_that("the description shows the channel the run will use", {
  recipe <- obt(folder = "/work") |>
    obt_designer_add(type = "main") |>
    obt_designer_add(type = "dev")

  expect_match(printed(obt_describe(recipe)), "Channel.*dev")
})

# The channel table

test_that("both channels resolve to their own folder", {
  recipe <- obt(folder = "/work")

  expect_identical(channel_path(recipe, "main"), "/work/obt/main")
  expect_identical(channel_path(recipe, "dev"), "/work/obt/dev")
})

test_that("the zip takes the name of the address it comes from", {
  recipe <- obt(folder = "/work")

  expect_identical(
    channel_zip_path(recipe, "main"),
    "/work/obt/main/OBT-main-latest.zip"
  )
  expect_identical(
    channel_zip_path(recipe, "dev"),
    "/work/obt/dev/OBT-dev-latest.zip"
  )
})

test_that("every channel names a folder of the layout", {
  entries <- vapply(OBT_CHANNELS, function(spec) spec$entry, character(1))

  expect_true(all(entries %in% names(OBT_FOLDERS)))
})

# Reading the names out of a zip

test_that("every workbook of a channel zip is found by its name", {
  files <- channel_files(test_channel_names())

  expect_named(files, names(CHANNEL_ROLES), ignore.order = FALSE)
  expect_identical(files$designer, "designer_main-2.0.0.xlsb")
  expect_identical(files$setup, "setup_main-2.0.0.xlsb")
  expect_identical(files$msetup, "msetup_main-2.0.0.xlsb")
  expect_identical(files$ribbon, "_ribbontemplate_main-2.0.0.xlsb")
})

test_that("the names of the dev channel are read the same way", {
  files <- channel_files(test_channel_names(channel = "dev"))

  expect_identical(files$designer, "designer_dev-2.0.0.xlsb")
  expect_identical(files$setup, "setup_dev-2.0.0.xlsb")
})

test_that("the setup and the master setup are told apart", {
  files <- channel_files(test_channel_names())

  expect_false(identical(files$setup, files$msetup))
})

test_that("a zip with no master setup still resolves", {
  names <- test_channel_names()
  files <- channel_files(names[!startsWith(names, "msetup")])

  expect_identical(files$msetup, NA_character_)
  expect_identical(files$designer, "designer_main-2.0.0.xlsb")
})

test_that("a zip with no designer is refused", {
  names <- test_channel_names()

  expect_error(
    channel_files(names[!startsWith(names, "designer")]),
    "the designer"
  )
})

test_that("a zip with no ribbon template is refused", {
  names <- test_channel_names()

  expect_error(
    channel_files(names[!startsWith(names, "_ribbontemplate")]),
    "the ribbon template"
  )
})

test_that("a zip holding two designers is refused", {
  names <- c(test_channel_names(), "designer_main-2.1.0.xlsb")

  expect_error(channel_files(names), "the designer")
})

test_that("the shape the stable channel ships today resolves", {
  # The stable channel still ships the older zip: the setup name carries no
  # channel, there is no master setup, and two helper scripts sit beside the
  # workbooks.
  files <- channel_files(test_legacy_names())

  expect_identical(files$designer, "designer_main-2024-10-19.xlsb")
  expect_identical(files$setup, "setup-2024-10-19.xlsb")
  expect_identical(files$ribbon, "_ribbontemplate_main-2024-10-19.xlsb")
  expect_identical(files$msetup, NA_character_)
})

test_that("a file of another kind is left out", {
  names <- c(test_channel_names(), "README.md", "notes.txt")

  expect_identical(
    channel_files(names)$designer,
    "designer_main-2.0.0.xlsb"
  )
})

test_that("an empty zip is refused and says so", {
  expect_error(channel_files(character()), "no workbook")
})

test_that("a workbook stored under a folder keeps its whole entry", {
  files <- channel_files(paste0("OBT-main/", test_channel_names()))

  expect_identical(files$designer, "OBT-main/designer_main-2.0.0.xlsb")
})

test_that("the message names the zip when it is given one", {
  expect_error(
    channel_files(character(), zip = "/work/obt/main/OBT-main-latest.zip"),
    "OBT-main-latest.zip"
  )
})

test_that("the entries of a zip are read off the file", {
  expect_setequal(zip_entries(channel_fixture()), test_channel_names())
})

test_that("a file that is not a zip is refused", {
  path <- file.path(withr::local_tempdir(), "OBT-main-latest.zip")
  writeLines("this is text", path)

  expect_error(zip_entries(path), "zip")
})

# The download, unzipped

test_that("a channel is downloaded, unzipped, and its paths answered", {
  folder <- withr::local_tempdir()
  downloads <- test_downloader(channel_fixture())
  local_mocked_bindings(download_zip = downloads$download)

  result <- designer_fetch(obt(folder = folder), type = "main")

  expect_identical(result$channel, "main")
  expect_identical(downloads$calls, 1L)
  expect_true(file.exists(result$zip))
  expect_true(file.exists(result$designer))
  expect_true(file.exists(result$setup))
  expect_true(file.exists(result$msetup))
  expect_true(file.exists(result$ribbon))
})

test_that("the file names come from the zip", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture())$download
  )

  result <- designer_fetch(obt(folder = folder), type = "main")

  expect_identical(basename(result$designer), "designer_main-2.0.0.xlsb")
  expect_identical(basename(result$setup), "setup_main-2.0.0.xlsb")
  expect_identical(
    basename(result$ribbon),
    "_ribbontemplate_main-2.0.0.xlsb"
  )
})

test_that("everything lands in the folder of its own channel", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture("dev"))$download
  )

  result <- designer_fetch(obt(folder = folder), type = "dev")
  channel <- channel_path(obt(folder = folder), "dev")

  expect_identical(dirname(result$designer), channel)
  expect_identical(dirname(result$zip), channel)
  expect_identical(basename(result$zip), "OBT-dev-latest.zip")
})

test_that("a zip already there is used again", {
  folder <- withr::local_tempdir()
  downloads <- test_downloader(channel_fixture())
  local_mocked_bindings(download_zip = downloads$download)

  recipe <- obt(folder = folder)
  designer_fetch(recipe, type = "main")
  second <- designer_fetch(recipe, type = "main")

  expect_identical(downloads$calls, 1L)
  expect_true(second$reused)
})

test_that("force downloads the zip again", {
  folder <- withr::local_tempdir()
  downloads <- test_downloader(channel_fixture())
  local_mocked_bindings(download_zip = downloads$download)

  recipe <- obt(folder = folder)
  designer_fetch(recipe, type = "main")
  second <- designer_fetch(recipe, type = "main", force = TRUE)

  expect_identical(downloads$calls, 2L)
  expect_false(second$reused)
})

test_that("force writes the workbooks again", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture())$download
  )

  recipe <- obt(folder = folder)
  first <- designer_fetch(recipe, type = "main")
  writeLines("edited", first$designer)

  designer_fetch(recipe, type = "main", force = TRUE)

  expect_false(identical(readLines(first$designer), "edited"))
})

test_that("a workbook already unzipped is left where it is", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture())$download
  )

  recipe <- obt(folder = folder)
  first <- designer_fetch(recipe, type = "main")
  writeLines("edited", first$designer)

  designer_fetch(recipe, type = "main")

  expect_identical(readLines(first$designer), "edited")
})

test_that("the fetch refuses a channel it does not know", {
  expect_error(
    designer_fetch(obt(folder = tempdir()), type = "nightly"),
    "must be one of"
  )
})

# The download itself

test_that("a download that fails names the address and writes nothing", {
  folder <- withr::local_tempdir()
  path <- file.path(folder, "OBT-main-latest.zip")

  local_mocked_bindings(
    download.file = function(...) stop("no network here"),
    .package = "utils"
  )

  expect_error(download_zip("https://example.org/a.zip", path), "example.org")
  expect_false(file.exists(path))
})

test_that("a download that arrives empty is a failure", {
  folder <- withr::local_tempdir()
  path <- file.path(folder, "OBT-main-latest.zip")

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      file.create(destfile)
      0L
    },
    .package = "utils"
  )

  expect_error(download_zip("https://example.org/a.zip", path), "downloaded")
  expect_false(file.exists(path))
})

test_that("a download that arrives is moved into place", {
  folder <- withr::local_tempdir()
  path <- file.path(folder, "OBT-main-latest.zip")
  source <- channel_fixture()

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      file.copy(source, destfile, overwrite = TRUE)
      0L
    },
    .package = "utils"
  )

  download_zip("https://example.org/a.zip", path)

  expect_true(file.exists(path))
  expect_setequal(zip_entries(path), test_channel_names())
})

# The version a channel carries

test_that("the version is read out of the designer name", {
  expect_identical(channel_version("designer_main-2.0.0.xlsb"), "2.0.0")
  expect_identical(channel_version("designer_dev-latest.xlsb"), "latest")
  expect_identical(
    channel_version("designer_main-2024-10-19.xlsb"),
    "2024-10-19"
  )
})

test_that("a name carrying no version answers NA", {
  expect_identical(channel_version("designer.xlsb"), NA_character_)
  expect_identical(channel_version(NA_character_), NA_character_)
})

test_that("the version is read off a whole path too", {
  expect_identical(
    channel_version("/work/obt/main/designer_main-2.0.0.xlsb"),
    "2.0.0"
  )
})

test_that("a release at the minimum or above is taken", {
  expect_silent(check_channel_version("2.0.0", "main"))
  expect_silent(check_channel_version("2.1.0", "main"))
  expect_silent(check_channel_version("3.0", "main"))
  expect_silent(check_channel_version("10.0.0", "main"))
})

test_that("a release below the minimum is refused", {
  expect_error(check_channel_version("1.2.0", "main"), "older than")
  expect_error(check_channel_version("1.2.0", "main"), "1.2.0")
  expect_error(check_channel_version("0.9", "main"), "older than")
})

test_that("the refusal names the version this package needs", {
  expect_error(check_channel_version("1.2.0", "main"), OBT_MINIMUM_VERSION)
})

test_that("a date in place of a version is refused", {
  expect_error(check_channel_version("2024-10-19", "main"), "older than")
  expect_error(check_channel_version("2026-06-11", "main"), "older than")
})

test_that("a designer with no version in its name is refused", {
  expect_error(check_channel_version(NA_character_, "main"), "no version")
})

test_that("the development build is taken as current", {
  expect_silent(check_channel_version("latest", "dev"))
  expect_silent(check_channel_version("LATEST", "dev"))
})

test_that("the stable channel has to carry a real version", {
  expect_error(check_channel_version("latest", "main"), "older than")
})

test_that("the refusal names the zip when it is given one", {
  expect_error(
    check_channel_version(
      "1.2.0",
      "main",
      zip = "/work/obt/main/OBT-main-latest.zip"
    ),
    "OBT-main-latest.zip"
  )
})

# The version check, on a download

test_that("a download answers the version it carries", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture("main"))$download
  )

  expect_identical(
    designer_fetch(obt(folder = folder), type = "main")$version,
    "2.0.0"
  )
})

test_that("the development build answers latest", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture("dev"))$download
  )

  expect_identical(
    designer_fetch(obt(folder = folder), type = "dev")$version,
    "latest"
  )
})

test_that("a channel older than this package can drive is refused", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture("legacy"))$download
  )

  expect_error(
    designer_fetch(obt(folder = folder), type = "main"),
    "older than"
  )
})

test_that("a refused channel leaves no workbook in the folder", {
  folder <- withr::local_tempdir()
  local_mocked_bindings(
    download_zip = test_downloader(channel_fixture("legacy"))$download
  )

  recipe <- obt(folder = folder)
  expect_error(designer_fetch(recipe, type = "main"))

  left <- list.files(channel_path(recipe, "main"))

  expect_identical(left, "OBT-main-latest.zip")
})

test_that("the refusal happens again on a zip already there", {
  folder <- withr::local_tempdir()
  downloads <- test_downloader(channel_fixture("legacy"))
  local_mocked_bindings(download_zip = downloads$download)

  recipe <- obt(folder = folder)
  expect_error(designer_fetch(recipe, type = "main"))
  expect_error(designer_fetch(recipe, type = "main"), "older than")
  expect_identical(downloads$calls, 1L)
})
