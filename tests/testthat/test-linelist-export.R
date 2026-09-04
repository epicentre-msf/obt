test_that("a migration export records its kind and the folder it writes to", {
  folder <- withr::local_tempdir()

  recipe <- linelist_recipe(folder) |> obt_linelist_export()
  operation <- recipe$operations[[1]]

  expect_identical(operation$type, "linelist-export")
  expect_identical(operation$args$type, "migration")
  expect_null(operation$args$name)
  expect_identical(operation$args$to, "export")
  expect_false(operation$waiting)
})

test_that("migration is the kind a recipe takes when none is named", {
  folder <- withr::local_tempdir()

  recipe <- linelist_recipe(folder) |> obt_linelist_export()

  expect_identical(recipe$operations[[1]]$args$type, "migration")
})

test_that("a specific export records the name it will run", {
  folder <- withr::local_tempdir()

  recipe <- linelist_recipe(folder) |>
    obt_linelist_export(type = "specific", name = "weekly-report")
  operation <- recipe$operations[[1]]

  expect_identical(operation$args$type, "specific")
  expect_identical(operation$args$name, "weekly-report")
})

test_that("a kind is read whatever case it is written in", {
  folder <- withr::local_tempdir()

  recipe <- linelist_recipe(folder) |>
    obt_linelist_export(type = "Migration")

  expect_identical(recipe$operations[[1]]$args$type, "migration")
})

test_that("any other kind is refused, and both words are named", {
  folder <- withr::local_tempdir()

  expect_error(
    linelist_recipe(folder) |> obt_linelist_export(type = "weekly"),
    "migration"
  )
  expect_error(
    linelist_recipe(folder) |> obt_linelist_export(type = "weekly"),
    "specific"
  )
})

test_that("a specific export with no name is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    linelist_recipe(folder) |> obt_linelist_export(type = "specific"),
    "must be a single string"
  )
})

test_that("a name handed to a migration export is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    linelist_recipe(folder) |>
      obt_linelist_export(type = "migration", name = "weekly-report"),
    "specific"
  )
})

test_that("the second linelist and its password are recorded together", {
  folder <- withr::local_tempdir()
  other <- built_linelist(folder, "measles-2025")

  recipe <- linelist_recipe(folder) |>
    obt_linelist_export(other = other, password = "hunter2")
  operation <- recipe$operations[[1]]

  expect_identical(operation$args$other, "linelist/measles-2025.xlsb")
  expect_identical(operation$args$password, "hunter2")
})

test_that("a password is kept as it stands, spaces and all", {
  folder <- withr::local_tempdir()
  other <- built_linelist(folder, "measles-2025")

  recipe <- linelist_recipe(folder) |>
    obt_linelist_export(other = other, password = " two words ")

  expect_identical(recipe$operations[[1]]$args$password, " two words ")
})

test_that("a password with no second linelist is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    linelist_recipe(folder) |> obt_linelist_export(password = "pw"),
    "opens the linelist"
  )
})

test_that("an empty password is refused at the verb", {
  folder <- withr::local_tempdir()
  other <- built_linelist(folder, "measles-2025")

  expect_error(
    linelist_recipe(folder) |>
      obt_linelist_export(other = other, password = ""),
    "must carry a value"
  )
  expect_error(
    linelist_recipe(folder) |> obt_linelist_export(other = other, password = 1),
    "must be a single string"
  )
})

test_that("the exports that read this linelist alone refuse a second one", {
  folder <- withr::local_tempdir()
  other <- built_linelist(folder, "measles-2025")

  expect_error(
    linelist_recipe(folder) |>
      obt_linelist_export(type = "specific", name = "analysis", other = other),
    "reads the linelist the run drives"
  )
  expect_error(
    linelist_recipe(folder) |>
      obt_linelist_export(type = "specific", name = "3", other = other),
    "reads the linelist the run drives"
  )
})

test_that("the three migration exports take a second linelist", {
  folder <- withr::local_tempdir()
  other <- built_linelist(folder, "measles-2025")

  for (word in c("migration", "geo", "historic")) {
    recipe <- linelist_recipe(folder) |>
      obt_linelist_export(type = "specific", name = word, other = other)

    expect_identical(
      recipe$operations[[1]]$args$other,
      "linelist/measles-2025.xlsb"
    )
  }
})

test_that("a second linelist the verb cannot find is refused", {
  folder <- withr::local_tempdir()

  expect_error(
    linelist_recipe(folder) |>
      obt_linelist_export(other = file.path(folder, "gone.xlsb")),
    "Nothing sits at"
  )
})

test_that("a folder the verb was given is recorded relative to the folder", {
  folder <- withr::local_tempdir()

  recipe <- linelist_recipe(folder) |>
    obt_linelist_export(to = file.path(folder, "out"))

  expect_identical(recipe$operations[[1]]$args$to, "out")
})

test_that("the verb is refused anything other than a recipe", {
  expect_error(obt_linelist_export("recipe"), "must be an")
})

test_that("the verb creates nothing", {
  folder <- withr::local_tempdir()

  linelist_recipe(folder) |>
    obt_linelist_export() |>
    obt_linelist_export(type = "specific", name = "weekly")

  expect_identical(list.files(folder), character())
})

test_that("the export records after the operations called before it", {
  folder <- withr::local_tempdir()
  linelist <- built_linelist(folder)

  recipe <- obt_linelist(from = linelist, folder = folder) |>
    obt_linelist_export()

  types <- vapply(recipe$operations, function(op) op$type, character(1))

  expect_identical(types, c("linelist-add", "linelist-export"))
})

test_that("a password is hidden wherever a recipe is printed", {
  folder <- withr::local_tempdir()
  other <- built_linelist(folder, "measles-2025")

  said <- printed(
    linelist_recipe(folder) |>
      obt_linelist_export(other = other, password = "hunter2") |>
      obt_describe()
  )

  expect_false(grepl("hunter2", said, fixed = TRUE))
  expect_match(said, "<hidden>")
})

test_that("a waiting export prints what a release has to carry", {
  folder <- withr::local_tempdir()
  local_waiting_type("linelist-export")

  said <- printed(linelist_recipe(folder) |> obt_linelist_export() |> print())

  expect_match(said, "Waiting on the workbooks")
})

test_that("the export run hands the driver the linelist and the folder", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  linelist <- built_linelist(folder)
  written <- file.path(obt_paths(recipe)$export, "migration.xlsx")
  driver <- test_linelist_driver(linelist_answer("export", written))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  answer <- run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "migration",
        name = NULL,
        to = "export",
        other = NULL,
        password = NULL
      )
    ),
    stage = stage,
    state = list(linelist = linelist)
  )

  expect_identical(driver$seen$linelist, linelist)
  expect_identical(driver$seen$to, obt_paths(recipe)$export)
  expect_identical(unname(answer$produced), written)
  expect_identical(names(answer$produced), "export")
  expect_identical(unname(answer$state$export), written)
})

test_that("an export that wrote more than one file names them all", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  written <- c("/out/data.xlsx", "/out/geo.xlsx", "/out/historic.xlsx")
  driver <- test_linelist_driver(
    linelist_answer("export", paste(written, collapse = ", "))
  )
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  answer <- run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "migration",
        name = NULL,
        to = "export",
        other = NULL,
        password = NULL
      )
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_identical(unname(answer$produced), written)
  expect_identical(names(answer$produced), rep("export", 3))
})

test_that("a migration export reaches the workbook under an empty name", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("export", "out.xlsx"))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "migration",
        name = NULL,
        to = "export",
        other = NULL,
        password = NULL
      )
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_identical(driver$seen$name, "")
})

test_that("a specific export reaches the workbook under its own name", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("export", "out.xlsx"))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "specific",
        name = "weekly-report",
        to = "export",
        other = NULL,
        password = NULL
      )
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_identical(driver$seen$name, "weekly-report")
})

test_that("the second linelist and its password cross as recorded", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  other <- built_linelist(folder, "measles-2025")
  driver <- test_linelist_driver(linelist_answer("export", "out.xlsx"))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "migration",
        name = NULL,
        to = "export",
        other = "linelist/measles-2025.xlsb",
        password = "pw"
      )
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder, "measles-2026"))
  )

  expect_identical(driver$seen$other_password, "pw")
  expect_identical(driver$seen$other, other)
  expect_true(is.na(driver$seen$password))
})

test_that("a second linelist moved between the verb and the run stops it", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  expect_error(
    run_linelist_export(
      recipe,
      new_operation(
        "linelist-export",
        list(
          type = "migration",
          name = NULL,
          to = "export",
          other = "linelist/gone.xlsb",
          password = NULL
        )
      ),
      stage = stage,
      state = list(linelist = built_linelist(folder))
    ),
    "the linelist to export from"
  )
})

test_that("a run with no password hands the workbook nothing", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("export", "out.xlsx"))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "migration",
        name = NULL,
        to = "export",
        other = NULL,
        password = NULL
      )
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_true(is.na(driver$seen$password))
  expect_true(is.na(driver$seen$other_password))
  expect_true(is.na(driver$seen$other))
})

test_that("the summary sits in the output folder under the linelist's name", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  linelist <- built_linelist(folder, "measles-2026")

  expect_identical(
    summary_in(obt_paths(recipe)$export, linelist),
    summary_path(obt_paths(recipe)$export, "measles-2026")
  )
})

test_that("the folder the export writes into is there before the run", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("export", "out.xlsx"))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "migration",
        name = NULL,
        to = "out",
        other = NULL,
        password = NULL
      )
    ),
    stage = stage,
    state = list(linelist = built_linelist(folder))
  )

  expect_true(dir.exists(file.path(folder, "out")))
})

test_that("a run that names no file warns and produces nothing", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("export"))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  expect_warning(
    answer <- run_linelist_export(
      recipe,
      new_operation(
        "linelist-export",
        list(
          type = "migration",
          name = NULL,
          to = "export",
          other = NULL,
          password = NULL
        )
      ),
      stage = stage,
      state = list(linelist = built_linelist(folder))
    ),
    "named no file"
  )

  expect_identical(answer$produced, character())
  expect_identical(answer$state, list())
})

test_that("a run with no linelist to write out of stops and names the verb", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  stage <- test_stage(folder, folder, staged = FALSE)

  expect_error(
    run_linelist_export(
      recipe,
      new_operation(
        "linelist-export",
        list(
          type = "migration",
          name = NULL,
          to = "export",
          other = NULL,
          password = NULL
        )
      ),
      stage = stage,
      state = list()
    ),
    "no linelist"
  )
})

test_that("the export is refused before Excel opens while it is waiting", {
  folder <- withr::local_tempdir()
  local_run_machine()
  local_waiting_type("linelist-export")

  expect_error(
    linelist_recipe(folder) |> obt_linelist_export() |> obt_commit(),
    "on the workbooks"
  )
})

test_that("the export opens a protected linelist with its password", {
  folder <- withr::local_tempdir()
  recipe <- linelist_recipe(folder)
  driver <- test_linelist_driver(linelist_answer("export", "out.xlsx"))
  stage <- test_stage(folder, folder, staged = FALSE)

  local_mocked_bindings(driver_linelist_export = driver$call)

  run_linelist_export(
    recipe,
    new_operation(
      "linelist-export",
      list(
        type = "migration",
        name = NULL,
        to = "export",
        other = NULL,
        password = NULL
      )
    ),
    stage = stage,
    state = list(
      linelist = built_linelist(folder),
      linelist_password = "open-pw"
    )
  )

  expect_identical(driver$seen$password, "open-pw")
  expect_true(is.na(driver$seen$other_password))
})
