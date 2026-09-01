# What a printer wrote, as one string.
#
# cli sends some lines to standard output and others as messages, so both are
# collected. The options hold the width steady and take the colours off, so a
# test can match plain text.
printed <- function(expr) {
  old <- options(cli.width = 200, cli.unicode = FALSE, cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  said <- character()

  out <- capture.output(
    withCallingHandlers(
      expr,
      message = function(cnd) {
        said <<- c(said, conditionMessage(cnd))
        invokeRestart("muffleMessage")
      }
    )
  )

  paste(c(out, said), collapse = "\n")
}

# A recipe with one operation of each readiness, for the printers.
mixed_recipe <- function(folder = file.path(tempdir(), "recipe")) {
  obt(folder = folder) |>
    add_operation("designer-add", list(type = "dev")) |>
    add_operation("designer-languages", list(dict = "English", form = "ENG")) |>
    obt_designer_generate(name = "measles-2026")
}

# What a printer answered, with everything it printed thrown away.
printed_value <- function(expr) {
  value <- NULL
  printed(value <- expr)
  value
}
