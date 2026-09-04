test_that("the geobase import records the file it will read", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)

  recipe <- linelist_recipe(folder) |> obt_linelist_geobase(path = geo)
  operation <- recipe$operations[[1]]

  expect_identical(operation$type, "linelist-geobase")
  expect_identical(operation$args$path, "geobase.xlsx")
  expect_false(operation$waiting)
})

test_that("a geobase the verb cannot find is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    linelist_recipe(folder) |>
      obt_linelist_geobase(path = file.path(folder, "missing.xlsx")),
    "Nothing sits at"
  )
})

test_that("the migration import records the file, the rule and the flag", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  recipe <- linelist_recipe(folder) |> obt_linelist_import(from = from)
  operation <- recipe$operations[[1]]

  expect_identical(operation$type, "linelist-import")
  expect_identical(operation$args$from, "cases-from-the-field.xlsx")
  expect_identical(operation$args$rule, "append")
  expect_false(operation$args$force)
  expect_false(operation$waiting)
})

test_that("append is the rule a recipe takes when none is named", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  recipe <- linelist_recipe(folder) |> obt_linelist_import(from = from)

  expect_identical(recipe$operations[[1]]$args$rule, "append")
})

test_that("replace is recorded as it stands", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  recipe <- linelist_recipe(folder) |>
    obt_linelist_import(from = from, rule = "replace")

  expect_identical(recipe$operations[[1]]$args$rule, "replace")
})

test_that("a rule is read whatever case it is written in", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  recipe <- linelist_recipe(folder) |>
    obt_linelist_import(from = from, rule = "Replace")

  expect_identical(recipe$operations[[1]]$args$rule, "replace")
})

test_that("any other rule is refused, and both words are named", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  expect_error(
    linelist_recipe(folder) |> obt_linelist_import(from = from, rule = "top"),
    "append"
  )
  expect_error(
    linelist_recipe(folder) |> obt_linelist_import(from = from, rule = "top"),
    "replace"
  )
})

test_that("the force flag is recorded when it is on", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  recipe <- linelist_recipe(folder) |>
    obt_linelist_import(from = from, force = TRUE)

  expect_true(recipe$operations[[1]]$args$force)
})

test_that("anything other than a .xlsx is refused by name", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder, "cases.xlsb")

  expect_error(
    linelist_recipe(folder) |> obt_linelist_import(from = from),
    "must end in"
  )
  expect_error(
    linelist_recipe(folder) |> obt_linelist_import(from = from),
    "xlsb"
  )
})

test_that("a migration file the verb cannot find is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    linelist_recipe(folder) |>
      obt_linelist_import(from = file.path(folder, "missing.xlsx")),
    "Nothing sits at"
  )
})

test_that("the three arguments are checked at the verb", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  expect_error(
    linelist_recipe(folder) |> obt_linelist_import(from = 1),
    "must be a single string"
  )
  expect_error(
    linelist_recipe(folder) |> obt_linelist_import(from = from, rule = 1),
    "must be a single string"
  )
  expect_error(
    linelist_recipe(folder) |> obt_linelist_import(from = from, force = "Yes"),
    "must be"
  )
})

test_that("both verbs are refused anything other than a recipe", {
  expect_error(obt_linelist_geobase("recipe", path = "geo.xlsx"), "must be an")
  expect_error(obt_linelist_import("recipe", from = "in.xlsx"), "must be an")
})

test_that("neither verb creates anything", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()

  linelist_recipe(folder) |>
    obt_linelist_geobase(path = geobase_file(elsewhere)) |>
    obt_linelist_import(from = migration_file(elsewhere))

  expect_identical(list.files(folder), character())
})

test_that("the two imports record in the order the verbs were called", {
  folder <- withr::local_tempdir()

  recipe <- linelist_recipe(folder) |>
    obt_linelist_geobase(path = geobase_file(folder)) |>
    obt_linelist_import(from = migration_file(folder))

  types <- vapply(recipe$operations, function(op) op$type, character(1))

  expect_identical(types, c("linelist-geobase", "linelist-import"))
})

test_that("a forced recipe says so before Excel opens", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  said <- printed(
    linelist_recipe(folder) |>
      obt_linelist_import(from = from, force = TRUE) |>
      obt_describe()
  )

  expect_match(said, "force = TRUE")
  expect_match(said, "1 import")
})

test_that("a recipe that forces nothing says nothing about it", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)

  said <- printed(
    linelist_recipe(folder) |>
      obt_linelist_import(from = from) |>
      obt_describe()
  )

  expect_false(grepl("force = TRUE", said, fixed = TRUE))
})

test_that("the designer's own force is left out of that line", {
  folder <- withr::local_tempdir()

  said <- printed(
    obt(folder = folder) |>
      obt_designer_add(type = "dev", force = TRUE) |>
      obt_describe()
  )

  expect_false(grepl("force = TRUE", said, fixed = TRUE))
})

test_that("every forced import is counted", {
  folder <- withr::local_tempdir()
  first <- migration_file(folder, "one.xlsx")
  second <- migration_file(folder, "two.xlsx")

  said <- printed(
    linelist_recipe(folder) |>
      obt_linelist_import(from = first, force = TRUE) |>
      obt_linelist_import(from = second, force = TRUE) |>
      obt_describe()
  )

  expect_match(said, "2 imports")
})

test_that("the geobase run reads into the linelist the recipe built", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)
  recipe <- linelist_recipe(folder)
  linelist <- built_linelist(folder)
  driver <- test_linelist_driver(linelist_answer("geobase", geo))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_geobase = driver$call)

  answer <- run_linelist_geobase(
    recipe,
    new_operation("linelist-geobase", list(path = "geobase.xlsx")),
    stage = stage,
    state = list(linelist = linelist)
  )

  expect_identical(driver$seen$linelist, linelist)
  expect_identical(answer$state$geobase, driver$seen$geo)
})

test_that("the geobase run copies the file into geo/ first", {
  folder <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()
  geo <- geobase_file(elsewhere)
  recipe <- linelist_recipe(folder)
  kept <- file.path(obt_paths(recipe)$geo, "geobase.xlsx")
  driver <- test_linelist_driver(linelist_answer("geobase", kept))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_geobase = driver$call)

  run_linelist_geobase(
    recipe,
    new_operation("linelist-geobase", list(path = geo)),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_identical(driver$seen$geo, kept)
  expect_true(file.exists(kept))
})

test_that("a linelist already in the folder is the one a run reads into", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  linelist <- built_linelist(folder, "measles-2026")

  expect_identical(linelist_target(recipe, list()), linelist)
})

test_that("the linelist the run built wins over the folder", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  built_linelist(folder, "old")
  fresh <- built_linelist(folder, "new")

  expect_identical(linelist_target(recipe, list(linelist = fresh)), fresh)
})

test_that("a folder with no linelist stops the run and names the verb", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)

  expect_error(linelist_target(recipe, list()), "no linelist")
  expect_error(linelist_target(recipe, list()), "obt_designer_generate")
})

test_that("a folder holding two linelists stops the run and names both", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  built_linelist(folder, "measles-2026")
  built_linelist(folder, "measles-2025")

  expect_error(linelist_target(recipe, list()), "2 linelists")
  expect_error(linelist_target(recipe, list()), "measles-2025")
})

test_that("a file in linelist/ that is no workbook is left out", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  linelist <- built_linelist(folder)
  writeLines("notes", file.path(obt_paths(recipe)$linelist, "notes.txt"))

  expect_identical(linelist_target(recipe, list()), linelist)
})

test_that("the import hands the driver the file, the rule and the word", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  linelist <- built_linelist(folder)
  driver <- test_linelist_driver(linelist_answer("import", from))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_import = driver$call)

  run_linelist_import(
    recipe,
    new_operation(
      "linelist-import",
      list(from = "cases-from-the-field.xlsx", rule = "replace", force = TRUE)
    ),
    stage = stage,
    state = list(linelist = linelist)
  )

  expect_identical(driver$seen$linelist, linelist)
  expect_identical(driver$seen$from, absolute_path(from))
  expect_identical(driver$seen$rule, "replace")
  expect_identical(driver$seen$force, "Yes")
})

test_that("an unforced import reaches the workbook as No", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("import", from))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_import = driver$call)

  run_linelist_import(
    recipe,
    new_operation(
      "linelist-import",
      list(from = from, rule = "append", force = FALSE)
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_identical(driver$seen$force, "No")
})

test_that("the summary the import reads sits beside the file it was handed", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("import", from))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_import = driver$call)

  run_linelist_import(
    recipe,
    new_operation(
      "linelist-import",
      list(from = from, rule = "append", force = FALSE)
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_identical(
    driver$seen$summary,
    summary_beside(from)
  )
})

test_that("a file moved between the verb and the run stops the run", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  expect_error(
    run_linelist_import(
      recipe,
      new_operation(
        "linelist-import",
        list(from = "gone.xlsx", rule = "append", force = FALSE)
      ),
      stage = stage,
      state = list(linelist = built_linelist(folder))
    ),
    "the file to read into the linelist"
  )
})

test_that("the import hands the file it read on to the operations after it", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("import", from))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_import = driver$call)

  answer <- run_linelist_import(
    recipe,
    new_operation(
      "linelist-import",
      list(from = from, rule = "append", force = FALSE)
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_identical(answer$state$imported, absolute_path(from))
})

test_that("a refused import says which warning stopped it", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  write_refusal_summary(from, "The file carries no metadata.")

  local_mocked_bindings(driver_linelist_import = test_refused_driver())

  expect_error(
    run_linelist_import(
      recipe,
      new_operation(
        "linelist-import",
        list(from = from, rule = "append", force = FALSE)
      ),
      stage = stage,
      state = list(linelist = built_linelist(folder))
    ),
    "carries no metadata"
  )
})

test_that("a refused import points at the flag that pushes past it", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  write_refusal_summary(from, "The file records no language.")

  local_mocked_bindings(driver_linelist_import = test_refused_driver())

  expect_error(
    run_linelist_import(
      recipe,
      new_operation(
        "linelist-import",
        list(from = from, rule = "append", force = FALSE)
      ),
      stage = stage,
      state = list(linelist = built_linelist(folder))
    ),
    "force = TRUE"
  )
})

test_that("a run that was already forcing is not told to force", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  write_refusal_summary(from, "The file is in another language.")

  local_mocked_bindings(driver_linelist_import = test_refused_driver())

  said <- tryCatch(
    run_linelist_import(
      recipe,
      new_operation(
        "linelist-import",
        list(from = from, rule = "append", force = TRUE)
      ),
      stage = stage,
      state = list(linelist = built_linelist(folder))
    ),
    error = function(cnd) cli::ansi_strip(conditionMessage(cnd))
  )

  expect_false(grepl("force = TRUE", said, fixed = TRUE))
})

test_that("a failure the summary says nothing about is raised as it came", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(
    driver_linelist_import = test_refused_driver("Excel never opened.")
  )

  expect_error(
    run_linelist_import(
      recipe,
      new_operation(
        "linelist-import",
        list(from = from, rule = "append", force = FALSE)
      ),
      stage = stage,
      state = list(linelist = built_linelist(folder))
    ),
    "Excel never opened"
  )
})

test_that("a run naming another file stops the operation", {
  ran <- linelist_answer("import", "/somewhere/other-file.xlsx")

  expect_error(
    check_import_read(ran, from = "/work/cases.xlsx"),
    "read another file"
  )
})

test_that("a run naming the same file under another folder is taken", {
  ran <- linelist_answer("import", "/staged/cases.xlsx")

  expect_silent(check_import_read(ran, from = "/work/cases.xlsx"))
})

test_that("a run naming no file warns and carries on", {
  ran <- linelist_answer("import")

  expect_warning(
    check_import_read(ran, from = "/work/cases.xlsx"),
    "named no file"
  )
})

test_that("the geobase read is checked the same way", {
  ran <- linelist_answer("geobase", "/staged/geobase.xlsx")

  expect_silent(check_geobase_read(ran, geo = "/work/geo/geobase.xlsx"))
  expect_error(
    check_geobase_read(ran, geo = "/work/geo/other.xlsx"),
    "read another file"
  )
})

test_that("the force word is the one the linelist reads", {
  expect_identical(force_word(TRUE), "Yes")
  expect_identical(force_word(FALSE), "No")
  expect_identical(force_word(NA), "No")
})

test_that("both imports build the command line the script is called with", {
  runs <- test_recording_call()

  local_mocked_bindings(driver_call = runs$call, os_name = function() "macos")

  driver_linelist_geobase(linelist = "/work/ll.xlsb", geo = "/work/geo.xlsx")

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("linelist-geobase"),
        "/work/ll.xlsb",
        "",
        "/work/geo.xlsx"
      ),
      type = "sh"
    )
  )

  driver_linelist_import(
    linelist = "/work/ll.xlsb",
    from = "/work/in.xlsx",
    rule = "replace",
    force = FORCE_YES
  )

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("linelist-import"),
        "/work/ll.xlsb",
        "",
        "/work/in.xlsx",
        "replace",
        "Yes"
      ),
      type = "sh"
    )
  )
})

test_that("the export builds the command line the script is called with", {
  runs <- test_recording_call()

  local_mocked_bindings(driver_call = runs$call, os_name = function() "macos")

  driver_linelist_export(
    linelist = "/work/ll.xlsb",
    name = "",
    to = "/work/export",
    password = "own-pw",
    other = "/work/old.xlsb",
    other_password = "pw"
  )

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("linelist-export"),
        "/work/ll.xlsb",
        "own-pw",
        "",
        "/work/export",
        "pw",
        "/work/old.xlsb"
      ),
      type = "sh"
    )
  )
})

test_that("the geobase run opens a protected linelist with its password", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("geobase", geo))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_geobase = driver$call)

  run_linelist_geobase(
    recipe,
    new_operation("linelist-geobase", list(path = "geobase.xlsx")),
    stage = stage,
    state = list(
      linelist = built_linelist(folder),
      linelist_password = "open-pw"
    )
  )

  expect_identical(driver$seen$password, "open-pw")
})

test_that("the import opens a protected linelist with its password", {
  folder <- withr::local_tempdir()
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("import", from))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_import = driver$call)

  run_linelist_import(
    recipe,
    new_operation(
      "linelist-import",
      list(from = from, rule = "append", force = FALSE)
    ),
    stage = stage,
    state = list(
      linelist = built_linelist(folder),
      linelist_password = "open-pw"
    )
  )

  expect_identical(driver$seen$password, "open-pw")
})

test_that("a linelist with no password is opened with none", {
  folder <- withr::local_tempdir()
  geo <- geobase_file(folder)
  from <- migration_file(folder)
  recipe <- linelist_recipe(folder)
  linelist <- built_linelist(folder)
  geobase <- test_linelist_driver(linelist_answer("geobase", geo))
  import <- test_linelist_driver(linelist_answer("import", from))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(
    driver_linelist_geobase = geobase$call,
    driver_linelist_import = import$call
  )

  run_linelist_geobase(
    recipe,
    new_operation("linelist-geobase", list(path = "geobase.xlsx")),
    stage = stage,
    state = list(linelist = linelist)
  )
  run_linelist_import(
    recipe,
    new_operation(
      "linelist-import",
      list(from = from, rule = "append", force = FALSE)
    ),
    stage = stage,
    state = list(linelist = linelist)
  )

  expect_true(is.na(geobase$seen$password))
  expect_true(is.na(import$seen$password))
})

test_that("the password crosses to the script beside the linelist it opens", {
  runs <- test_recording_call()

  local_mocked_bindings(driver_call = runs$call, os_name = function() "macos")

  driver_linelist_geobase(
    linelist = "/work/ll.xlsb",
    geo = "/work/geo.xlsx",
    password = "open-pw"
  )

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("linelist-geobase"),
        "/work/ll.xlsb",
        "open-pw",
        "/work/geo.xlsx"
      ),
      type = "sh"
    )
  )

  driver_linelist_import(
    linelist = "/work/ll.xlsb",
    from = "/work/in.xlsx",
    password = "open-pw"
  )

  expect_identical(
    runs$args,
    shQuote(
      c(
        driver_script("linelist-import"),
        "/work/ll.xlsb",
        "open-pw",
        "/work/in.xlsx",
        "append",
        "No"
      ),
      type = "sh"
    )
  )
})
