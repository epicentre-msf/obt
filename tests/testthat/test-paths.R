test_that("obt_paths answers every entry of the layout", {
  paths <- obt_paths(obt(folder = file.path(tempdir(), "measles")))

  expect_named(
    paths,
    c("folder", names(OBT_FOLDERS), names(OBT_FILES)),
    ignore.order = FALSE
  )
  expect_true(all(vapply(paths, is.character, logical(1))))
  expect_true(all(lengths(paths) == 1))
})

test_that("every path sits under the working folder", {
  folder <- file.path(tempdir(), "measles")
  paths <- obt_paths(obt(folder = folder))
  under <- paths[names(paths) != "folder"]

  expect_true(all(startsWith(unlist(under), paste0(paths$folder, "/"))))
})

test_that("the layout is the one the documentation shows", {
  paths <- obt_paths(obt(folder = "/work"))

  expect_identical(paths$folder, "/work")
  expect_identical(paths$obt, "/work/obt")
  expect_identical(paths$obt_main, "/work/obt/main")
  expect_identical(paths$obt_dev, "/work/obt/dev")
  expect_identical(paths$setup, "/work/setup")
  expect_identical(paths$setup_source, "/work/setup/source")
  expect_identical(paths$geo, "/work/geo")
  expect_identical(paths$linelist, "/work/linelist")
  expect_identical(paths$export, "/work/export")
  expect_identical(paths$log, "/work/log")
  expect_identical(paths$setup_file, "/work/setup/setup.xlsb")
})

test_that("obt_paths refuses anything that is not a recipe", {
  expect_error(obt_paths("/work"), "must be an")
})

test_that("reading the paths creates nothing", {
  folder <- file.path(withr::local_tempdir(), "measles")

  obt_paths(obt(folder = folder))

  expect_false(dir.exists(folder))
})

test_that("a folder given relative to the session resolves", {
  withr::local_dir(withr::local_tempdir())

  # `getwd()` is read back inside the test rather than the folder handed to
  # `local_dir()`: on macOS the temporary folder is reached through a symbolic
  # link and the session reports the path behind it.
  here <- absolute_path(getwd())
  paths <- obt_paths(obt(folder = "measles"))

  expect_identical(paths$folder, paste0(here, "/measles"))
  expect_identical(paths$geo, paste0(here, "/measles/geo"))
})

test_that("repeated separators are squeezed down to one", {
  expect_identical(absolute_path("/work//geo"), "/work/geo")
  expect_identical(absolute_path("/work///geo//"), "/work/geo")
  expect_identical(absolute_path("//server//share"), "//server/share")
  expect_identical(absolute_path("/work/./geo"), "/work/geo")
  expect_identical(
    path_relative("/work//geo/regions.xlsx", "/work/"),
    "geo/regions.xlsx"
  )
})

test_that("the layout is built from an empty directory", {
  work <- withr::local_tempdir()
  folder <- file.path(work, "measles")
  recipe <- obt(folder = folder)

  paths <- create_obt_folder(recipe)

  expect_true(dir.exists(folder))
  for (entry in names(OBT_FOLDERS)) {
    expect_true(dir.exists(paths[[entry]]))
  }
})

test_that("create_obt_folder answers the paths invisibly", {
  recipe <- obt(folder = file.path(withr::local_tempdir(), "measles"))

  expect_invisible(create_obt_folder(recipe))
  expect_identical(create_obt_folder(recipe), obt_paths(recipe))
})

test_that("building the layout twice keeps what is already there", {
  recipe <- obt(folder = file.path(withr::local_tempdir(), "measles"))
  paths <- create_obt_folder(recipe)

  file.create(file.path(paths$geo, "regions.xlsx"))
  create_obt_folder(recipe)

  expect_true(file.exists(file.path(paths$geo, "regions.xlsx")))
})

test_that("a file where a folder belongs is reported", {
  work <- withr::local_tempdir()
  taken <- file.path(work, "measles")
  file.create(taken)

  expect_error(ensure_folder(taken), "A file sits there")
})

test_that("a folder that cannot be created is reported", {
  work <- withr::local_tempdir()
  blocked <- file.path(work, "wall")
  file.create(blocked)

  expect_error(
    ensure_folder(file.path(blocked, "geo")),
    "could not be created"
  )
})

test_that("an output that is already there stops the run", {
  work <- withr::local_tempdir()
  output <- file.path(work, "measles-2026.xlsb")
  file.create(output)

  expect_error(check_output_free(output, overwrite = FALSE), "already there")
  expect_error(check_output_free(output, overwrite = FALSE), "overwrite = TRUE")
})

test_that("an output that is already there is written over when asked", {
  work <- withr::local_tempdir()
  output <- file.path(work, "measles-2026.xlsb")
  file.create(output)

  expect_invisible(check_output_free(output, overwrite = TRUE))
  expect_identical(check_output_free(output, overwrite = TRUE), output)
})

test_that("an output that is not there passes either way", {
  output <- file.path(withr::local_tempdir(), "measles-2026.xlsb")

  expect_identical(check_output_free(output, overwrite = FALSE), output)
  expect_identical(check_output_free(output, overwrite = TRUE), output)
})

test_that("the output guard names the argument it was given", {
  work <- withr::local_tempdir()
  output <- file.path(work, "measles-2026.xlsb")
  file.create(output)

  expect_error(
    check_output_free(output, overwrite = FALSE, arg = "force"),
    "force = TRUE"
  )
  expect_error(
    check_output_free(output, overwrite = "yes"),
    "must be"
  )
})

test_that("a linelist is written under linelist, with its extension", {
  recipe <- obt(folder = "/work")

  expect_identical(
    linelist_path(recipe, "measles-2026"),
    "/work/linelist/measles-2026.xlsb"
  )
})

test_that("a run log is stamped with the date and the time", {
  recipe <- obt(folder = "/work")
  when <- as.POSIXct("2026-08-31 14:05:09", tz = "UTC")

  expect_identical(
    log_path(recipe, when = when),
    "/work/log/20260831-140509.txt"
  )
})

test_that("a copy keeps the name the user gave it", {
  recipe <- obt(folder = "/work")

  expect_identical(
    copy_path(recipe, "geo", "/home/me/files/regions 2026.xlsx"),
    "/work/geo/regions 2026.xlsx"
  )
  expect_identical(
    copy_path(recipe, "setup_source", "/home/me/setup.xlsb"),
    "/work/setup/source/setup.xlsb"
  )
})

test_that("a copy into an unknown entry is refused", {
  recipe <- obt(folder = "/work")

  expect_error(copy_path(recipe, "somewhere", "/home/me/a.xlsx"), "layout")
  expect_error(copy_path(recipe, 1, "/home/me/a.xlsx"), "single string")
})

test_that("a path under the folder is stored relative to it", {
  expect_identical(
    path_relative("/work/geo/regions.xlsx", "/work"),
    "geo/regions.xlsx"
  )
  expect_identical(path_relative("/work/linelist", "/work"), "linelist")
})

test_that("a path outside the folder is stored as it stands", {
  expect_identical(
    path_relative("/home/me/regions.xlsx", "/work"),
    "/home/me/regions.xlsx"
  )
  expect_identical(
    path_relative("/workshop/a.xlsx", "/work"),
    "/workshop/a.xlsx"
  )
})

test_that("a stored path resolves under the folder it is read with", {
  expect_identical(
    path_absolute("geo/regions.xlsx", "/work"),
    "/work/geo/regions.xlsx"
  )
  expect_identical(
    path_absolute("/home/me/regions.xlsx", "/work"),
    "/home/me/regions.xlsx"
  )
})

test_that("a folder that moves carries its recipe with it", {
  stored <- path_relative("/work/geo/regions.xlsx", "/work")

  expect_identical(
    path_absolute(stored, "/somewhere/else"),
    "/somewhere/else/geo/regions.xlsx"
  )
})

test_that("a path is written with one shape of separator", {
  expect_identical(absolute_path("/work/geo/"), "/work/geo")
  expect_identical(absolute_path("/work/geo/."), "/work/geo")
  expect_identical(absolute_path("/"), "/")
  expect_identical(absolute_path("~"), path.expand("~"))
})

test_that("the three shapes of an absolute path are known", {
  expect_true(is_absolute_path("/work"))
  expect_true(is_absolute_path("~/work"))
  expect_true(is_absolute_path("C:/work"))
  expect_true(is_absolute_path("C:\\work"))
  expect_true(is_absolute_path("\\\\server\\share"))
  expect_false(is_absolute_path("work"))
  expect_false(is_absolute_path("./work"))
  expect_false(is_absolute_path("../work"))
})
