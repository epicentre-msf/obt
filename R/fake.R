# Fake records for a setup.
#
# A setup says what the linelist it builds will hold, and that is enough to
# make up a table of records for every worksheet of it. `obt_fake()` records
# that, and the run writes one `.xlsx` file with a sheet per worksheet.
#
# The verb is the one thing in the package that needs no OutbreakTools entry
# point and no Excel. The setup is read with the same reader the language
# verbs use, the records are made up in R, and the answer is written with
# `writexl`. A user on a machine that cannot open a workbook can still get a
# dataset shaped like the one their setup describes.
#
# The rules that make the records up live in `fake-generate.R`.

# The extension the file of fake records carries. One workbook, one sheet per
# worksheet of the setup.
FAKE_EXTENSION <- "xlsx"

# What the file is called when the verb is not told otherwise.
FAKE_DEFAULT_NAME <- "fake"

# The columns of the dictionary the generation reads. A file missing one of
# them is not a setup this package can read.
FAKE_DICTIONARY_COLUMNS <- c(
  "variable_name",
  "sheet_name",
  "sheet_type",
  "variable_type",
  "control",
  "control_details",
  "unique",
  "min",
  "max"
)

# The columns of the choices table the generation reads.
FAKE_CHOICES_COLUMNS <- c("list_name", "label")

# The sheets of a geobase that carry the admin levels, one per level.
FAKE_GEOBASE_SHEETS <- paste0("ADM", FAKE_ADM_LEVELS)

# What a geobase writes in a cell of a level a place does not reach.
FAKE_GEOBASE_BLANK <- "N/A"

#' Generate fake datasets for the setup
#'
#'
#' @param obtops An `obt` or `obt_setup` recipe. Either way the recipe has to
#'   carry a setup, because the setup is what the records are read out of.
#' @param n How many records a worksheet holds. A `vlist1D` worksheet holds one
#'   whatever this says.
#' @param to The folder the file is written into. The default is `export/` in
#'   the working folder. A folder that is missing is created by the run.
#' @param name What the file is called, without its extension.
#' @param overwrite Whether a file of that name already there may be replaced.
#' @param seed The seed the records are drawn under. Give one and two runs of
#'   the same recipe answer the same records. The default draws fresh ones and
#'   leaves the session's own stream as it found it.
#' @param id_prefix The lead every generated id carries.
#' @param id_width How wide the number of an id is, padded with zeros. The
#'   default is as wide as `n` needs.
#' @param range_int The range whole numbers are drawn from, where the setup
#'   sets no bound of its own.
#' @param range_num The range decimals are drawn from, where the setup sets no
#'   bound of its own.
#' @param range_date The range dates are drawn from, where the setup sets no
#'   bound of its own.
#' @param nchar_text How long a piece of text is, where the setup sets no bound
#'   of its own.
#' @param prop_na How much of every column is left missing, between 0 and 1.
#'
#' @return The recipe, with the generation added.
#'
#' @seealso [obt_setup()] to start a setup recipe,
#'   [obt_designer_geobase()] for the geobase the geo columns are drawn from.
#'
#' @export
#'
#' @examples
#' setup <- system.file("extdata", "generic-test-setup.xlsb", package = "obt")
#'
#' obt_setup(from = setup, folder = file.path(tempdir(), "measles")) |>
#'   obt_fake(n = 20, seed = 1)
obt_fake <- function(
  obtops,
  n = 50,
  to = NULL,
  name = FAKE_DEFAULT_NAME,
  overwrite = FALSE,
  seed = NULL,
  id_prefix = "P",
  id_width = NULL,
  range_int = c(0, 100),
  range_num = c(0, 100),
  range_date = c(Sys.Date() - 365, Sys.Date()),
  nchar_text = c(4, 12),
  prop_na = 0.1
) {
  check_obt_fake(obtops)

  n <- check_count(n)
  to <- check_export_folder(to, folder = obtops$folder)
  name <- check_output_name(name)
  check_flag(overwrite)

  add_operation(
    obtops,
    type = "setup-fake",
    args = list(
      n = n,
      to = to,
      name = name,
      overwrite = overwrite,
      seed = check_seed(seed),
      id_prefix = check_string(id_prefix),
      id_width = check_id_width(id_width, n = n),
      range_int = check_range(range_int),
      range_num = check_range(range_num),
      range_date = check_date_range(range_date),
      nchar_text = check_nchar_range(nchar_text),
      prop_na = check_share(prop_na)
    )
  )
}

#' Make the records up and write them out
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_setup_fake <- function(obtops, operation, stage, state) {
  setup <- setup_working_file(obtops)
  to <- path_absolute(operation$args$to, folder = obtops$folder)
  path <- file.path(to, paste0(operation$args$name, ".", FAKE_EXTENSION))

  check_output_free(path, operation$args$overwrite)
  ensure_folder(to)

  tables <- with_seed(
    operation$args$seed,
    fake_from_setup(
      setup = setup,
      geo = fake_geobase(obtops, state),
      args = operation$args
    )
  )

  writexl::write_xlsx(tables, path)

  list(produced = c(fake = path), state = list(fake = path))
}

#' The tables of records one setup describes
#'
#' @param setup Path to the setup workbook.
#' @param geo The geobase sampling frame, or `NULL`.
#' @param args The arguments of the operation record.
#'
#' @return A named list of data frames, one per worksheet.
#' @noRd
fake_from_setup <- function(setup, geo, args) {
  tables <- read_setup_tables(setup)

  fake_tables(
    dict = tables$dict,
    choices = tables$choices,
    geo = geo,
    n = args$n,
    settings = fake_settings(args)
  )
}

#' The settings the generation reads
#'
#' The recipe stores its dates as text, because a recipe is read back by a
#' printer that shows what it holds, and a date shows as a date only when it
#' is written as one.
#'
#' @param args The arguments of the operation record.
#'
#' @return A list of settings.
#' @noRd
fake_settings <- function(args) {
  list(
    id_prefix = args$id_prefix,
    id_width = args$id_width,
    range_int = args$range_int,
    range_num = args$range_num,
    range_date = as.Date(args$range_date),
    nchar_text = args$nchar_text,
    prop_na = args$prop_na
  )
}

#' Read the tables of a setup the generation works from
#'
#' @param path Path to the setup workbook.
#' @param call The environment to blame in the error.
#'
#' @return A list of `dict` and `choices`, with cleaned column names.
#' @noRd
read_setup_tables <- function(path, call = rlang::caller_env()) {
  dict <- read_setup_sheet(path, FAKE_DICTIONARY_SHEET, call = call)
  choices <- read_setup_sheet(path, FAKE_CHOICES_SHEET, call = call)

  check_setup_columns(
    dict,
    wanted = FAKE_DICTIONARY_COLUMNS,
    sheet = FAKE_DICTIONARY_SHEET,
    path = path,
    call = call
  )
  check_setup_columns(
    choices,
    wanted = FAKE_CHOICES_COLUMNS,
    sheet = FAKE_CHOICES_SHEET,
    path = path,
    call = call
  )

  list(dict = dict, choices = choices)
}

#' Read one sheet of a setup
#'
#' Every cell is read as text. What a column holds is said by the setup, in the
#' dictionary, and reading a constraint as a number would lose the shape the
#' user typed it in.
#'
#' @param path Path to the setup workbook.
#' @param sheet The sheet to read.
#' @param call The environment to blame in the error.
#'
#' @return A data frame, with cleaned column names.
#' @noRd
read_setup_sheet <- function(path, sheet, call = rlang::caller_env()) {
  read <- tryCatch(
    readxlsb::read_xlsb(
      path,
      sheet = sheet,
      skip = unname(FAKE_SHEET_SKIP[[sheet]])
    ),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Could not read {.val {sheet}} from {.file {path}}.",
          "x" = "{conditionMessage(cnd)}",
          "i" = "A setup workbook carries a {.val {sheet}} sheet."
        ),
        parent = cnd,
        call = call
      )
    }
  )

  names(read) <- clean_setup_names(names(read))

  for (column in names(read)) {
    read[[column]] <- as.character(read[[column]])
  }

  read
}

#' Write a setup's own column headers the way this package reads them
#'
#' A setup writes `Variable Name` and a reader answers `Variable.Name`. One
#' lower-case, underscored shape is what the generation reads.
#'
#' @param names The column names as the reader answered them.
#'
#' @return The names, lower-cased and underscored.
#' @noRd
clean_setup_names <- function(names) {
  cleaned <- tolower(trimws(names))
  cleaned <- gsub("[^a-z0-9]+", "_", cleaned)

  gsub("^_+|_+$", "", cleaned)
}

#' Fail when a sheet of the setup is missing a column the generation reads
#'
#' @param table The sheet, with cleaned names.
#' @param wanted The columns it has to carry.
#' @param sheet The name of the sheet, for the message.
#' @param path Path to the setup, for the message.
#' @param call The environment to blame in the error.
#'
#' @return The table, invisibly.
#' @noRd
check_setup_columns <- function(table, wanted, sheet, path, call) {
  missing <- setdiff(wanted, names(table))

  if (length(missing) == 0) {
    return(invisible(table))
  }

  cli::cli_abort(
    c(
      "{.val {sheet}} in {.file {path}} is missing {length(missing)}
       column{?s} the records are read from.",
      "x" = "{.val {missing}}",
      "i" = "This does not look like a setup this package can read."
    ),
    call = call
  )
}

#' The geobase the geo columns are drawn from
#'
#' A recipe that recorded one hands it on through the run state. A working
#' folder holding one from an earlier run answers it too. A folder holding
#' none answers nothing, and the geo columns are left empty.
#'
#' @param obtops The recipe.
#' @param state What the operations before it left behind.
#' @param call The environment to blame in the error.
#'
#' @return A sampling frame, or `NULL`.
#' @noRd
fake_geobase <- function(obtops, state, call = rlang::caller_env()) {
  if (is_set(state$geobase)) {
    return(read_geobase(state$geobase, call = call))
  }

  found <- geobases_on_disk(obtops)

  if (length(found) == 0) {
    return(NULL)
  }

  if (length(found) > 1) {
    cli::cli_abort(
      c(
        "The working folder holds {length(found)} geobases.",
        "x" = "They are {.file {basename(found)}}.",
        "i" = "Record the one you want with {.code obt_designer_geobase()},
               so the run knows which one to draw places from."
      ),
      call = call
    )
  }

  read_geobase(found, call = call)
}

#' The geobases a working folder already holds
#'
#' @param obtops The recipe.
#'
#' @return The absolute paths of every geobase under `geo/`.
#' @noRd
geobases_on_disk <- function(obtops) {
  folder <- obt_paths(obtops)$geo

  if (!dir.exists(folder)) {
    return(character())
  }

  found <- list.files(folder, full.names = TRUE)
  found <- found[tolower(tools::file_ext(found)) == FAKE_EXTENSION]

  vapply(found, absolute_path, character(1), USE.NAMES = FALSE)
}

#' Read a geobase into a frame of places to draw from
#'
#' A geobase carries one sheet per admin level, each naming the levels above
#' it, so a row of the deepest sheet is a whole place. Every level goes into
#' the frame, because a record can name a region without naming a village, and
#' each level is weighted so no level drowns the others out.
#'
#' @param path Path to the geobase.
#' @param call The environment to blame in the error.
#'
#' @return A data frame of `adm1_name` to `adm4_name`, the `level` each place
#'   reaches, and the `weight` it is drawn under.
#' @noRd
read_geobase <- function(path, call = rlang::caller_env()) {
  sheets <- tryCatch(
    readxl::excel_sheets(path),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Could not read the geobase at {.file {path}}.",
          "x" = "{conditionMessage(cnd)}"
        ),
        parent = cnd,
        call = call
      )
    }
  )

  places <- lapply(FAKE_ADM_LEVELS, function(level) {
    geobase_level(path, level = level, sheets = sheets)
  })

  places <- do.call(rbind, places)

  if (is.null(places) || nrow(places) == 0) {
    cli::cli_abort(
      c(
        "The geobase at {.file {path}} names no place.",
        "i" = "It carries {.val {FAKE_GEOBASE_SHEETS}} sheets, one per admin
               level, and the records are drawn from them."
      ),
      call = call
    )
  }

  counts <- table(places$level)
  places$weight <- 1 / as.numeric(counts[as.character(places$level)])

  places
}

#' The places one admin level of a geobase names
#'
#' @param path Path to the geobase.
#' @param level The admin level.
#' @param sheets The sheets the geobase holds.
#'
#' @return A data frame of the four admin columns and the level, or `NULL`
#'   when the geobase carries nothing at that level.
#' @noRd
geobase_level <- function(path, level, sheets) {
  sheet <- paste0("ADM", level)

  if (!sheet %in% sheets) {
    return(NULL)
  }

  read <- readxl::read_excel(
    path,
    sheet = sheet,
    col_types = "text",
    .name_repair = "minimal"
  )

  own <- paste0("adm", level, "_name")

  if (!own %in% names(read)) {
    return(NULL)
  }

  places <- data.frame(row.names = seq_len(nrow(read)))

  for (each in FAKE_ADM_LEVELS) {
    column <- paste0("adm", each, "_name")

    places[[column]] <- if (each <= level && column %in% names(read)) {
      read[[column]]
    } else {
      NA_character_
    }
  }

  # A geobase writes a row for the places that stop short of the deepest
  # level, and marks the levels they do not reach. Those rows are the level
  # above's, and they are already in the frame under it.
  kept <- is_filled(places[[own]]) & places[[own]] != FAKE_GEOBASE_BLANK

  places <- places[kept, , drop = FALSE]

  if (nrow(places) == 0) {
    return(NULL)
  }

  places$level <- level
  rownames(places) <- NULL

  places
}

#' Draw under a seed, and leave the session's own stream as it was
#'
#' A recipe run twice under one seed answers the same records. A recipe run
#' without one draws fresh records and changes nothing a caller was relying
#' on.
#'
#' @param seed The seed, or `NA` for none.
#' @param expr What to evaluate.
#'
#' @return What `expr` answers.
#' @noRd
with_seed <- function(seed, expr) {
  if (!is_set(seed)) {
    return(expr)
  }

  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  kept <- if (had) get(".Random.seed", envir = globalenv()) else NULL

  on.exit(
    {
      if (had) {
        assign(".Random.seed", kept, envir = globalenv())
      } else {
        rm(".Random.seed", envir = globalenv())
      }
    },
    add = TRUE
  )

  set.seed(seed)
  expr
}

#' Fail when a recipe cannot be asked for fake records
#'
#' The records are read out of the setup, so the recipe has to be one that
#' carries a setup. An `obt_linelist` holds a generated linelist alone, and a
#' generated linelist is what the records would be for.
#'
#' @param obtops The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The recipe.
#' @noRd
check_obt_fake <- function(
  obtops,
  arg = rlang::caller_arg(obtops),
  call = rlang::caller_env()
) {
  if (is_obt_linelist(obtops)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an {.cls obt} or an {.cls obt_setup} recipe.",
        "x" = "You supplied an {.cls obt_linelist} recipe, which holds a
               generated linelist and no setup.",
        "i" = "The records are read out of the setup."
      ),
      call = call
    )
  }

  if (is_obt(obtops)) {
    return(obtops)
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} must be an {.cls obt} or an {.cls obt_setup} recipe.",
      "x" = "You supplied {.obj_type_friendly {obtops}}.",
      "i" = "Start one with {.code obt_setup(from = )}."
    ),
    call = call
  )
}

#' Check a count of records
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The value, as an integer.
#' @noRd
check_count <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number.",
        "x" = "You supplied {.obj_type_friendly {value}}."
      ),
      call = call
    )
  }

  if (value < 0 || value != trunc(value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a whole number, and not below zero.",
        "x" = "You supplied {.val {value}}."
      ),
      call = call
    )
  }

  as.integer(value)
}

#' Check the name a file is written under
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The name, trimmed.
#' @noRd
check_output_name <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  force(arg)

  value <- check_string(value, arg = arg, call = call)

  if (grepl("[/\\\\]", value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a file name, not a path.",
        "x" = "{.val {value}} names a folder to sit in.",
        "i" = "Say where it goes with {.arg to}."
      ),
      call = call
    )
  }

  value
}

#' Check the seed a run draws under
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The seed as an integer, or `NA` when none was given.
#' @noRd
check_seed <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  if (is.null(value)) {
    return(NA_integer_)
  }

  if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number, or {.code NULL}.",
        "x" = "You supplied {.obj_type_friendly {value}}."
      ),
      call = call
    )
  }

  as.integer(value)
}

#' Check how wide the number of an id is
#'
#' @param value The value to check, or `NULL` for as wide as `n` needs.
#' @param n How many records the run makes.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The width, as an integer.
#' @noRd
check_id_width <- function(
  value,
  n,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(value)) {
    return(if (n == 0) 1L else as.integer(floor(log10(n)) + 1L))
  }

  width <- check_count(value, arg = arg, call = call)

  if (width == 0) {
    cli::cli_abort("{.arg {arg}} must be at least 1.", call = call)
  }

  width
}

#' Check a range of two numbers
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The range, lowest first.
#' @noRd
check_range <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  if (!is.numeric(value) || length(value) != 2 || anyNA(value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be two numbers, the lowest and the highest.",
        "x" = "You supplied {.obj_type_friendly {value}}."
      ),
      call = call
    )
  }

  sort(as.numeric(value))
}

#' Check a range of two whole numbers, both above zero
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The range, lowest first, as integers.
#' @noRd
check_nchar_range <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  force(arg)

  range <- check_range(value, arg = arg, call = call)

  if (any(range < 1) || any(range != trunc(range))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be two whole numbers, both at least 1.",
        "x" = "You supplied {.val {value}}."
      ),
      call = call
    )
  }

  as.integer(range)
}

#' Check a range of two dates
#'
#' The range is stored as text, so a recipe reads back the way it was written.
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The range, earliest first, as two strings.
#' @noRd
check_date_range <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  parsed <- suppressWarnings(
    tryCatch(as.Date(value), error = function(cnd) as.Date(NA_character_))
  )

  if (length(parsed) != 2 || anyNA(parsed)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be two dates, the earliest and the latest.",
        "x" = "You supplied {.obj_type_friendly {value}}."
      ),
      call = call
    )
  }

  as.character(sort(parsed))
}

#' Check a share, between nothing and everything
#'
#' @param value The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The share.
#' @noRd
check_share <- function(
  value,
  arg = rlang::caller_arg(value),
  call = rlang::caller_env()
) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number.",
        "x" = "You supplied {.obj_type_friendly {value}}."
      ),
      call = call
    )
  }

  if (value < 0 || value > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be between 0 and 1.",
        "x" = "You supplied {.val {value}}."
      ),
      call = call
    )
  }

  as.numeric(value)
}
