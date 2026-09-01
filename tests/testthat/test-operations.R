test_that("an operation record carries a type, its arguments and its state", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("designer-geobase", list(path = "/tmp/geo.xlsx"))

  operation <- recipe$operations[[1]]

  expect_identical(names(operation), c("type", "args", "group", "waiting"))
  expect_identical(operation$type, "designer-geobase")
  expect_identical(operation$args, list(path = "/tmp/geo.xlsx"))
  expect_identical(operation$group, 1L)
  expect_false(operation$waiting)
})

test_that("an operation the workbooks cannot run yet records as waiting", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("linelist-export", list(type = "migration"))

  expect_true(recipe$operations[[1]]$waiting)
})

test_that("an operation R does on its own records as ready", {
  ready <- c("designer-add", "designer-geobase", "designer-languages")

  for (type in ready) {
    recipe <- obt(folder = "/tmp/measles") |> add_operation(type)
    expect_false(recipe$operations[[1]]$waiting)
  }
})

test_that("operations keep the order the verbs were called in", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("designer-add", list(type = "main")) |>
    add_operation("designer-geobase", list(path = "/tmp/geo.xlsx")) |>
    add_operation("setup-tags")

  types <- vapply(recipe$operations, function(x) x$type, character(1))

  expect_identical(types, c("designer-add", "designer-geobase", "setup-tags"))
})

test_that("an unknown type is refused and the known ones are named", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(add_operation(recipe, "setup-translate"), "designer-add")
  expect_error(add_operation(recipe, "setup-translate"), "not an operation")
})

test_that("an argument with no name is refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    add_operation(recipe, "designer-geobase", list("/tmp/geo.xlsx")),
    "must carry a name"
  )
  expect_error(
    add_operation(recipe, "designer-geobase", "/tmp/geo.xlsx"),
    "must be a list"
  )
})

test_that("two arguments of one name are refused", {
  recipe <- obt(folder = "/tmp/measles")

  expect_error(
    add_operation(recipe, "designer-geobase", list(path = "a", path = "b")),
    "its own name"
  )
})

test_that("every operation type is declared the same way", {
  fields <- c("label", "blurb", "verb", "entry_point", "waits_on")

  for (type in names(OBT_OPERATIONS)) {
    spec <- OBT_OPERATIONS[[type]]
    expect_identical(names(spec), fields, info = type)

    for (field in fields) {
      expect_length(spec[[field]], 1)
      expect_type(spec[[field]], "character")
    }

    expect_true(nzchar(spec$label), info = type)
    expect_true(nzchar(spec$blurb), info = type)
  }
})

test_that("an operation with no entry point waits on nothing", {
  for (type in names(OBT_OPERATIONS)) {
    spec <- OBT_OPERATIONS[[type]]
    expect_identical(is.na(spec$entry_point), is.na(spec$waits_on), info = type)
  }
})

test_that("counting the waiting operations counts every one of them", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("designer-add") |>
    add_operation("setup-export") |>
    add_operation("linelist-import")

  expect_identical(count_waiting(recipe), 2L)
})

test_that("the last operation of a type is the one read back", {
  recipe <- obt(folder = "/tmp/measles") |>
    add_operation("designer-add", list(type = "main")) |>
    add_operation("designer-add", list(type = "dev"))

  expect_identical(last_operation(recipe, "designer-add")$args$type, "dev")
  expect_null(last_operation(recipe, "setup-tags"))
})
