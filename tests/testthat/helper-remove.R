# The types a recipe holds, in run order.
step_types <- function(obtops) {
  vapply(obtops$operations, function(operation) operation$type, character(1))
}

# Three steps, each from a verb call of its own.
three_step_recipe <- function(folder = "/tmp/measles") {
  obt(folder = folder) |>
    obt_designer_add(type = "dev") |>
    obt_designer_languages(form = "ENG") |>
    obt_designer_generate(name = "measles-2026")
}

# A recipe whose middle three steps came from one call.
conversion_recipe <- function() {
  obt(folder = tempdir()) |>
    obt_designer_add() |>
    obt_setup_convert(from = setup_workbook()) |>
    obt_designer_generate(name = "measles-2026")
}
