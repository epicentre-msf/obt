# Reading a recipe before it runs.
#
# The three verbs here print and change nothing. Each answers the recipe
# unchanged and invisibly, so it can sit in the middle of a chain. A run takes
# minutes and opens Excel, so the point of all three is to let a user find a
# mistake while it still costs nothing.

# The values a reader wants at the top of a description. Each is read from the
# last operation of its type that the recipe holds. Add a row when a verb
# records a value worth seeing before a run.
RECIPE_HIGHLIGHTS <- list(
  list(label = "Channel", type = "designer-add", arg = "type"),
  list(label = "Setup", type = "setup-import", arg = "from"),
  list(label = "Setup to convert", type = "convert-add", arg = "from"),
  list(label = "Geobase", type = "designer-geobase", arg = "path"),
  list(label = "Setup language", type = "designer-languages", arg = "dict"),
  list(
    label = "Interface language",
    type = "designer-languages",
    arg = "form"
  ),
  list(label = "Output name", type = "designer-generate", arg = "name"),
  list(label = "Fake records", type = "setup-fake", arg = "name")
)

# The same, for a setup recipe. A setup recipe holds a setup and the verbs
# that work on it, so the rows an `obt` shows have nothing to read.
SETUP_HIGHLIGHTS <- list(
  list(label = "Setup", type = "setup-add", arg = "from"),
  list(label = "Read in from", type = "setup-import", arg = "from"),
  list(label = "Written out to", type = "setup-export", arg = "to"),
  list(label = "Fake records", type = "setup-fake", arg = "name")
)

# The same, for a linelist recipe. A recipe narrowed from an `obt` names no
# linelist of its own, and the row then reads as unset: the run picks up the
# one file under `linelist/`.
LINELIST_HIGHLIGHTS <- list(
  list(label = "Linelist", type = "linelist-add", arg = "from"),
  list(label = "Geobase", type = "linelist-geobase", arg = "path"),
  list(label = "Read in from", type = "linelist-import", arg = "from"),
  list(label = "Export", type = "linelist-export", arg = "type")
)

# Arguments whose value is hidden wherever a recipe is printed. A generation
# carries the two passwords of the linelist it builds, a linelist recipe the
# one its file opens with, and an export the other linelist's. A description
# a user reads on a shared screen has to keep every one of them.
SECRET_ARGS <- c("password", "debug_password")

# What a hidden value shows as.
SECRET_MASK <- "<hidden>"

# What a value that was never set shows as.
UNSET_MARK <- "(not set)"

# cli hangs a wrapped list item at the left margin, where it reads as a new
# item, and it indents an argument list one column short of the item text.
# Both are pulled in under the text of the item they belong to. The margin
# adds to the one cli already applies.
OPERATION_LIST_THEME <- list(
  "ol li" = list("text-exdent" = 3),
  "ol li dl" = list("margin-left" = 1)
)

#' Print the operations of a recipe in run order
#'
#' @description
#' `obt_operations()` prints the numbered list of operations, in the order
#' `obt_commit()` will run them, one line each.
#'
#' `obt_summary()` prints the recipe in a few lines: the working folder, what
#' it will do, and how much of it is waiting on the workbooks. It is what the
#' `print()` method shows.
#'
#' `obt_describe()` prints everything: the working folder, the values the run
#' will use, and every operation with its arguments.
#'
#' All three number the steps, and the two lists print the type of each one
#' beside it. The number and the type are what [obt_remove()] takes.
#'
#' @param obtops An `obt` recipe.
#' @param x An `obt` recipe.
#' @param ... Passed nowhere. Present for the `print()` method.
#'
#' @return The recipe, unchanged and invisibly.
#'
#' @name obt_look
#'
#' @examples
#' recipe <- obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_generate(name = "measles-2026")
#'
#' obt_operations(recipe)
#' obt_summary(recipe)
#' obt_describe(recipe)
NULL

#' @rdname obt_look
#' @export
obt_operations <- function(obtops) {
  check_obt(obtops)

  cli::cli_h2("Operations")

  if (length(obtops$operations) == 0) {
    cli::cli_text("The recipe holds none yet.")
    return(invisible(obtops))
  }

  layout <- cli::cli_div(theme = OPERATION_LIST_THEME)
  numbered <- cli::cli_ol()

  for (operation in obtops$operations) {
    blurb <- operation_spec(operation$type)$blurb
    handle <- operation$type

    if (operation$waiting) {
      cli::cli_li(
        "{blurb} {.code {handle}} {.emph (waiting on the
                   workbooks)}"
      )
    } else {
      cli::cli_li("{blurb} {.code {handle}}")
    }
  }

  cli::cli_end(numbered)
  cli::cli_end(layout)

  invisible(obtops)
}

#' @rdname obt_look
#' @export
obt_summary <- function(obtops) {
  check_obt(obtops)

  folder <- obtops$folder
  count <- length(obtops$operations)

  cli::cli_h1(recipe_title(obtops))
  cli::cli_text("Working folder: {.file {folder}}")

  if (count == 0) {
    cli::cli_text("No operation recorded yet. Add one with a verb.")
    return(invisible(obtops))
  }

  labels <- vapply(
    obtops$operations,
    function(operation) operation_spec(operation$type)$label,
    character(1)
  )

  cli::cli_text("{count} operation{?s}: {labels}.")

  if (has_run(obtops)) {
    say_run(obtops)
    return(invisible(obtops))
  }

  waiting <- count_waiting(obtops)

  if (waiting > 0) {
    cli::cli_alert_warning(
      "Waiting on the workbooks: {waiting} of {count}."
    )
  } else {
    cli::cli_alert_success(
      "Every operation can run. Use {.code obt_commit()}."
    )
  }

  invisible(obtops)
}

#' Say when a recipe pushes an import past the linelist's own warning
#'
#' The linelist checks a migration file before it takes it, and `force` is
#' what pushes past that check. It is the one setting that lets a run import a
#' file the linelist could not vouch for, so a description says it out loud
#' before Excel opens.
#'
#' `obt_designer_add()` carries a `force` of its own, which asks for the zip
#' to be downloaded again. Only the import is read here.
#'
#' @param obtops The recipe.
#'
#' @return The number of forced imports, invisibly.
#' @noRd
say_forced <- function(obtops) {
  forced <- Filter(
    function(operation) {
      identical(operation$type, "linelist-import") &&
        isTRUE(operation$args$force)
    },
    obtops$operations
  )

  count <- length(forced)

  if (count == 0) {
    return(invisible(0L))
  }

  cli::cli_alert_warning(
    "{count} import{?s} run{?s/} with {.code force = TRUE}."
  )
  cli::cli_alert_info(
    "The linelist warns before it takes a file carrying no metadata, no
     language, or another language. A forced run takes the file and says
     what the warning was."
  )

  invisible(count)
}

#' Whether a recipe has been run
#'
#' @param obtops The recipe.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
has_run <- function(obtops) {
  !is.null(obtops$run)
}

#' Print how the run of a recipe ended
#'
#' @param obtops The recipe.
#'
#' @return `NULL`, invisibly. Called for what it prints.
#' @noRd
say_run <- function(obtops) {
  run <- obtops$run
  when <- format_time(run$started)
  took <- format_seconds(run$seconds)

  if (identical(run$outcome, RUN_OK)) {
    cli::cli_alert_success("Ran on {when}, in {took}.")
  } else {
    cli::cli_alert_danger("Ran on {when} and stopped after {took}.")
  }

  cli::cli_alert_info("Log: {.file {cli_escape(run$log)}}")

  invisible(NULL)
}

#' @rdname obt_look
#' @export
obt_describe <- function(obtops) {
  check_obt(obtops)

  folder <- obtops$folder

  cli::cli_h1(recipe_title(obtops))

  cli::cli_h2("The run will use:")
  cli::cli_dl(c("Working folder" = cli_escape(folder), highlights(obtops)))

  say_forced(obtops)

  cli::cli_h2("Operations")

  if (length(obtops$operations) == 0) {
    cli::cli_text("The recipe holds none yet.")
    return(invisible(obtops))
  }

  layout <- cli::cli_div(theme = OPERATION_LIST_THEME)
  numbered <- cli::cli_ol()

  for (operation in obtops$operations) {
    spec <- operation_spec(operation$type)
    label <- spec$label
    handle <- operation$type
    shown <- format_args(operation$args)

    if (operation$waiting) {
      item <- cli::cli_li(
        "{label} {.code {handle}} {.emph (waiting on the
                           workbooks)}"
      )
      shown <- c(Needs = cli_escape(spec$waits_on), shown)
    } else {
      item <- cli::cli_li("{label} {.code {handle}}")
    }

    shown <- c(shown, format_result(operation$result))

    if (length(shown) > 0) {
      cli::cli_dl(shown)
    }

    cli::cli_end(item)
  }

  cli::cli_end(numbered)
  cli::cli_end(layout)

  invisible(obtops)
}

#' @rdname obt_look
#' @export
print.obt <- function(x, ...) {
  obt_summary(x)
  invisible(x)
}

#' The values a description shows at the top
#'
#' @param obtops The recipe.
#'
#' @return A named character vector, one entry per row of
#'   `RECIPE_HIGHLIGHTS`, ready for `cli::cli_dl()`.
#' @noRd
highlights <- function(obtops) {
  rows <- recipe_highlights(obtops)

  values <- vapply(
    rows,
    function(highlight) {
      operation <- last_operation(obtops, highlight$type)

      if (is.null(operation) || is.null(operation$args[[highlight$arg]])) {
        return(UNSET_MARK)
      }

      cli_escape(format_value(
        operation$args[[highlight$arg]],
        name = highlight$arg
      ))
    },
    character(1)
  )

  labels <- vapply(
    rows,
    function(highlight) highlight$label,
    character(1)
  )

  names(values) <- labels
  values
}

#' What a recipe of this class is called
#'
#' @param obtops The recipe.
#'
#' @return A single string.
#' @noRd
recipe_title <- function(obtops) {
  if (is_obt_setup(obtops)) {
    return("OBT setup recipe")
  }

  if (is_obt_linelist(obtops)) {
    return("OBT linelist recipe")
  }

  "OBT recipe"
}

#' The rows a description of this class shows at the top
#'
#' @param obtops The recipe.
#'
#' @return A list of rows, each with a label, a type and an argument.
#' @noRd
recipe_highlights <- function(obtops) {
  if (is_obt_setup(obtops)) {
    return(SETUP_HIGHLIGHTS)
  }

  if (is_obt_linelist(obtops)) {
    return(LINELIST_HIGHLIGHTS)
  }

  RECIPE_HIGHLIGHTS
}

#' Lay the arguments of an operation out for printing
#'
#' @param args The arguments of the operation record.
#'
#' @return A named character vector, ready for `cli::cli_dl()`.
#' @noRd
format_args <- function(args) {
  if (length(args) == 0) {
    return(character())
  }

  values <- vapply(
    names(args),
    function(name) cli_escape(format_value(args[[name]], name = name)),
    character(1)
  )

  names(values) <- names(args)
  values
}

#' Turn one argument value into the text a reader sees
#'
#' @param value The value.
#' @param name The name of the argument it was recorded under.
#'
#' @return A single string.
#' @noRd
format_value <- function(value, name = "") {
  if (is.null(value) || length(value) == 0) {
    return(UNSET_MARK)
  }

  if (name %in% SECRET_ARGS) {
    return(SECRET_MASK)
  }

  if (is.character(value)) {
    return(paste0("\"", value, "\"", collapse = ", "))
  }

  if (is.logical(value) || is.numeric(value)) {
    return(paste0(format(value), collapse = ", "))
  }

  paste0("<", class(value)[[1]], ">")
}

#' Lay the result of one operation out for printing
#'
#' A recipe that has been run carries a result beside every operation it
#' reached. A recipe that has never run carries none, and this answers
#' nothing.
#'
#' @param result The result of the operation, or `NULL`.
#'
#' @return A named character vector, ready for `cli::cli_dl()`.
#' @noRd
format_result <- function(result) {
  if (is.null(result)) {
    return(character())
  }

  shown <- c(
    Outcome = result$outcome,
    Took = format_seconds(result$seconds)
  )

  produced <- result$produced

  if (length(produced) > 0) {
    names(produced) <- paste("Wrote", names(produced))
    shown <- c(shown, produced)
  }

  vapply(shown, cli_escape, character(1))
}

#' Make a value safe to hand to cli as a literal
#'
#' `cli::cli_dl()` reads its values as glue templates, so a brace a user put in
#' a file name would be read as code. Doubling the braces makes cli print them
#' as they stand.
#'
#' @param text The text to escape.
#'
#' @return The text, with every brace doubled.
#' @noRd
cli_escape <- function(text) {
  text <- gsub("{", "{{", text, fixed = TRUE)
  gsub("}", "}}", text, fixed = TRUE)
}
