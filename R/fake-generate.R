# Fake records, read out of a setup.
#
# A setup names its worksheets, its variables and what each variable holds. On
# that alone a table of records can be made up, one that carries the columns
# the generated linelist would carry and values that pass the setup's own
# constraints. That is what this file does, and it does it in R: nothing here
# opens Excel, so a fake dataset is one of the few things the package can
# produce on any machine.
#
# The rules are ported from the OutbreakTools data package, which grew them
# against real setups. What is kept is the shape of the answer: one column per
# variable, geo variables spread over four admin levels, dates that satisfy the
# minimum and maximum the setup wrote for them, and a tenth of the values left
# missing so a reader sees the gaps a real linelist has.
#
# `fake_tables()` is the whole of the public surface here. `obt_fake()` in
# `fake.R` is what records the operation and writes the answer out.

# The sheets of a setup this file reads, and how many rows sit above the header
# of each. A setup workbook carries a title block above its tables.
FAKE_DICTIONARY_SHEET <- "Dictionary"
FAKE_CHOICES_SHEET <- "Choices"
FAKE_SHEET_SKIP <- c(Dictionary = 4L, Choices = 3L)

# The two kinds of worksheet a setup defines. A vertical one holds a single
# record read down the page; a horizontal one holds a record per row.
FAKE_VERTICAL_TYPE <- "vlist1D"

# The admin levels a geo variable spreads over.
FAKE_ADM_LEVELS <- 1:4

# The share of values left missing. A linelist has gaps and a fake one that
# has none reads as a table nobody filled in by hand.
FAKE_MISSING_SHARE <- 0.1

# How many rounds a chain of dates may be followed for. A setup can write a
# date whose minimum is another date, and that other date's minimum a third.
# The chain is walked until it ends, and a setup that points a date back at
# itself would walk it forever, so it stops here instead.
FAKE_DEPENDENCY_LIMIT <- 100L

# What a variable type is called. A setup writes the decimal type under two
# names across releases and both mean the same column.
FAKE_TYPE_DATE <- "date"
FAKE_TYPE_INTEGER <- "integer"
FAKE_TYPE_DECIMAL <- c("decimal", "decimal2")
FAKE_TYPE_TEXT <- "text"

# The controls that name a value the linelist works out for itself. A formula
# column is filled by the workbook, so a fake one is left empty.
FAKE_DERIVED_CONTROLS <- "geo|formula|case_when"

# The control that reads its values from a choice list.
FAKE_CHOICE_CONTROL <- "choice_manual"

# The control that names a health facility. It takes a prefix of its own in
# the generated linelist.
FAKE_HF_CONTROL <- "hf"

# The control that spreads over the admin levels.
FAKE_GEO_CONTROL <- "geo"

# What a setup writes in the `Unique` column of a variable holding an id.
FAKE_UNIQUE_MARK <- "yes"

# What a setup writes for today's date in a constraint.
FAKE_TODAY_CALL <- "today\\(\\)"

#' The tables of fake records a setup describes
#'
#' One table per worksheet the setup names. A horizontal worksheet answers
#' `n` rows, one record each. A vertical worksheet holds a single record, so
#' it answers that record laid out down the page, a row per variable, the way
#' the linelist writes it.
#'
#' @param dict The dictionary of the setup, with cleaned names.
#' @param choices The choice lists of the setup, with cleaned names.
#' @param geo The geobase sampling frame, or `NULL`.
#' @param n How many records a horizontal worksheet holds.
#' @param settings The generation settings, as `fake_settings()` answers them.
#'
#' @return A named list of data frames, one per worksheet, in the order the
#'   dictionary names them.
#' @noRd
fake_tables <- function(dict, choices, geo, n, settings) {
  sheets <- fake_sheets(dict)

  tables <- lapply(seq_len(nrow(sheets)), function(index) {
    vertical <- identical(sheets$sheet_type[[index]], FAKE_VERTICAL_TYPE)
    rows <- if (vertical) 1L else n

    table <- fake_sheet(
      dict[dict$sheet_name == sheets$sheet_name[[index]], , drop = FALSE],
      choices = choices,
      geo = geo,
      n = rows,
      settings = settings
    )

    if (vertical) fake_upright(table) else table
  })

  names(tables) <- sheets$sheet_name
  tables
}

#' The worksheets a setup names
#'
#' A dictionary row that names no worksheet describes nothing the linelist
#' holds, so it is left out.
#'
#' @param dict The dictionary of the setup.
#'
#' @return A data frame of `sheet_name` and `sheet_type`, one row per
#'   worksheet, in the order the dictionary names them.
#' @noRd
fake_sheets <- function(dict) {
  named <- is_filled(dict$sheet_name) & is_filled(dict$sheet_type)
  sheets <- dict[named, c("sheet_name", "sheet_type"), drop = FALSE]

  sheets <- sheets[!duplicated(sheets$sheet_name), , drop = FALSE]
  rownames(sheets) <- NULL

  sheets
}

#' One worksheet of fake records
#'
#' @param dict The dictionary rows of that worksheet.
#' @param choices The choice lists of the setup.
#' @param geo The geobase sampling frame, or `NULL`.
#' @param n How many records the worksheet holds.
#' @param settings The generation settings.
#'
#' @return A data frame of `n` rows, one column per variable.
#' @noRd
fake_sheet <- function(dict, choices, geo, n, settings) {
  variables <- fake_variables(dict, settings = settings)

  columns <- lapply(
    seq_len(nrow(variables)),
    function(index) {
      fake_column(
        variables[index, , drop = FALSE],
        choices = choices,
        n = n,
        settings = settings
      )
    }
  )

  names(columns) <- variables$variable_name
  table <- as.data.frame(
    columns,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # A worksheet holding no variable answers a frame of no columns, and that
  # frame carries no rows either. The row count is set here so an empty
  # worksheet still answers the shape the caller asked for.
  if (ncol(table) == 0) {
    return(data.frame(row.names = seq_len(n)))
  }

  table <- fake_geo(table, variables = variables, geo = geo, n = n)
  table <- fake_dates(table, variables = variables, settings = settings)

  table[, variables$variable_name, drop = FALSE]
}

#' The columns one worksheet holds, and what fills each of them
#'
#' A geo variable is four columns in the linelist, one per admin level, and a
#' health facility variable takes a prefix. Both are spread here, so the rest
#' of the generation reads a plain list of columns.
#'
#' @param dict The dictionary rows of one worksheet.
#' @param settings The generation settings.
#'
#' @return A data frame, one row per column of the worksheet.
#' @noRd
fake_variables <- function(dict, settings) {
  dict <- dict[is_filled(dict$variable_name), , drop = FALSE]

  levels <- ifelse(
    dict$control %in% FAKE_GEO_CONTROL,
    length(FAKE_ADM_LEVELS),
    1L
  )
  spread <- rep(seq_len(nrow(dict)), levels)
  adm <- unlist(
    lapply(levels, function(count) seq_len(count)),
    use.names = FALSE
  )

  variables <- data.frame(
    variable_name = dict$variable_name[spread],
    variable_type = tolower(trimws(dict$variable_type[spread])),
    control = trimws(dict$control[spread]),
    control_details = dict$control_details[spread],
    unique = tolower(trimws(dict$unique[spread])),
    min = fake_constraint_text(dict$min[spread]),
    max = fake_constraint_text(dict$max[spread]),
    adm_level = adm,
    stringsAsFactors = FALSE
  )

  is_geo <- variables$control %in% FAKE_GEO_CONTROL
  is_hf <- variables$control %in% FAKE_HF_CONTROL

  variables$variable_name[is_hf] <- paste0(
    FAKE_HF_CONTROL,
    "_",
    variables$variable_name[is_hf]
  )
  variables$variable_name[is_geo] <- paste0(
    "adm",
    variables$adm_level[is_geo],
    "_",
    variables$variable_name[is_geo]
  )

  variables$choices <- ifelse(
    variables$control %in% FAKE_CHOICE_CONTROL,
    variables$control_details,
    NA_character_
  )

  variables$min_value <- fake_bounds(
    fake_constraint_value(variables$min),
    types = variables$variable_type,
    settings = settings,
    edge = 1L
  )
  variables$max_value <- fake_bounds(
    fake_constraint_value(variables$max),
    types = variables$variable_type,
    settings = settings,
    edge = 2L
  )

  variables[!duplicated(variables$variable_name), , drop = FALSE]
}

#' One column of fake values
#'
#' @param variable The row of `fake_variables()` describing the column.
#' @param choices The choice lists of the setup.
#' @param n How many values the column holds.
#' @param settings The generation settings.
#'
#' @return A vector of `n` values.
#' @noRd
fake_column <- function(variable, choices, n, settings) {
  share <- settings$prop_na

  if (variable$control %in% FAKE_CHOICE_CONTROL) {
    return(sample_choices(
      n,
      variable$choices,
      choices = choices,
      share = share
    ))
  }

  # A derived column is filled by the linelist out of the columns beside it.
  # Making one up here would put a value in it that its own formula denies.
  if (grepl(FAKE_DERIVED_CONTROLS, variable$control)) {
    return(rep(NA_character_, n))
  }

  if (identical(variable$unique, FAKE_UNIQUE_MARK)) {
    return(sample_ids(
      n,
      prefix = settings$id_prefix,
      width = settings$id_width
    ))
  }

  type <- variable$variable_type
  low <- variable$min_value
  high <- variable$max_value

  # A date is a placeholder here. The real values are drawn once every column
  # is there, because a date can be bounded by another date of the worksheet.
  if (type %in% FAKE_TYPE_DATE) {
    return(as.Date(rep(NA_character_, n)))
  }

  if (type %in% FAKE_TYPE_INTEGER) {
    return(sample_integers(n, as.integer(low), as.integer(high), share = share))
  }

  if (type %in% FAKE_TYPE_DECIMAL) {
    return(sample_decimals(n, as.numeric(low), as.numeric(high), share = share))
  }

  if (type %in% FAKE_TYPE_TEXT) {
    return(sample_text(n, as.integer(low), as.integer(high), share = share))
  }

  rep(NA_character_, n)
}

#' Fill the geo columns from a geobase
#'
#' Every geo variable takes one place, and the four admin levels of that place
#' go into its four columns, so a record names a place that exists rather than
#' four unrelated names. Without a geobase the columns are left empty, which
#' is what a linelist generated without one holds.
#'
#' @param table The worksheet so far.
#' @param variables The columns of the worksheet.
#' @param geo The geobase sampling frame, or `NULL`.
#' @param n How many records the worksheet holds.
#'
#' @return The worksheet, with its geo columns filled.
#' @noRd
fake_geo <- function(table, variables, geo, n) {
  if (is.null(geo) || nrow(geo) == 0 || n == 0) {
    return(table)
  }

  is_geo <- variables$control %in% FAKE_GEO_CONTROL

  if (!any(is_geo)) {
    return(table)
  }

  for (variable in unique(variables$variable_name[
    is_geo & variables$adm_level == 1
  ])) {
    # The name the setup gave the variable is what sits under the `adm1_`
    # lead, and the other three columns carry the same tail.
    tail <- substring(variable, nchar("adm1_") + 1L)
    drawn <- geo[
      sample.int(nrow(geo), n, replace = TRUE, prob = geo$weight),
      ,
      drop = FALSE
    ]

    for (level in FAKE_ADM_LEVELS) {
      column <- paste0("adm", level, "_", tail)

      if (column %in% names(table)) {
        table[[column]] <- drawn[[paste0("adm", level, "_name")]]
      }
    }
  }

  table
}

#' Draw the dates of a worksheet, in the order their bounds allow
#'
#' A setup can bound a date by another date of the same worksheet, and that
#' one by a third. A date is drawn only once everything it leans on is there,
#' so the dates that lean on nothing go first.
#'
#' @param table The worksheet, with a placeholder column per date.
#' @param variables The columns of the worksheet.
#' @param settings The generation settings.
#'
#' @return The worksheet, with its date columns drawn and thinned.
#' @noRd
fake_dates <- function(table, variables, settings) {
  dates <- variables[
    variables$variable_type %in% FAKE_TYPE_DATE,
    ,
    drop = FALSE
  ]

  if (nrow(dates) == 0 || nrow(table) == 0) {
    return(table)
  }

  order <- fake_date_order(dates)

  for (index in order) {
    table[[dates$variable_name[[index]]]] <- sample_bounded_dates(
      low = dates$min[[index]],
      high = dates$max[[index]],
      low_default = settings$range_date[[1]],
      high_default = settings$range_date[[2]],
      table = table
    )
  }

  # The gaps go in once every date is drawn. A date thinned before the one
  # that leans on it is drawn would push that one onto its default bound.
  for (index in order) {
    column <- dates$variable_name[[index]]
    table[[column]] <- thin_values(
      table[[column]],
      share = settings$prop_na,
      empty = as.Date(NA_character_)
    )
  }

  table
}

#' The order the dates of a worksheet are drawn in
#'
#' A date is ranked by how long the chain of dates under it is. One that leans
#' on nothing is drawn first, then the ones leaning on it, and so on.
#'
#' @param dates The date columns of one worksheet.
#'
#' @return An integer vector of row positions, in drawing order.
#' @noRd
fake_date_order <- function(dates) {
  depth_min <- dependency_depth(dates$variable_name, dates$min)
  depth_max <- dependency_depth(dates$variable_name, dates$max)

  order(depth_min + depth_max > 0, depth_min, depth_max)
}

#' How deep the chain of dates under each bound runs
#'
#' @param variables The names of the date columns.
#' @param bounds The bound each of them was written with.
#'
#' @return An integer vector, one entry per bound.
#' @noRd
dependency_depth <- function(variables, bounds) {
  leans_on <- referenced_variable(bounds, variables)
  depth <- as.integer(!is.na(leans_on))
  rounds <- 0L

  while (!all(is.na(leans_on)) && rounds < FAKE_DEPENDENCY_LIMIT) {
    leans_on <- referenced_variable(
      bounds[match(leans_on, variables)],
      variables
    )
    depth <- depth + as.integer(!is.na(leans_on))
    rounds <- rounds + 1L
  }

  depth
}

#' Which column of the worksheet a bound leans on
#'
#' @param bounds The bounds, as the setup wrote them.
#' @param variables The names a bound may name.
#'
#' @return A character vector, `NA` where a bound names none of them.
#' @noRd
referenced_variable <- function(bounds, variables) {
  vapply(
    bounds,
    function(bound) {
      named <- intersect(expression_variables(bound), variables)

      if (length(named) == 0) NA_character_ else named[[1]]
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Lay a single record out down the page
#'
#' A vertical worksheet holds one record, a row per variable, and the linelist
#' writes it that way.
#'
#' @param table The record, as a single-row frame.
#'
#' @return A data frame of `variable` and `value`.
#' @noRd
fake_upright <- function(table) {
  data.frame(
    variable = names(table),
    value = vapply(
      table,
      function(column) as.character(column)[[1]],
      character(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

#' Read a bound the way R can evaluate it
#'
#' A setup writes today's date the way Excel does. An empty bound reads as no
#' bound at all.
#'
#' @param bounds The bounds, as the setup wrote them.
#'
#' @return The bounds, with today's call rewritten and the empty ones `NA`.
#' @noRd
fake_constraint_text <- function(bounds) {
  bounds <- gsub(FAKE_TODAY_CALL, "Sys.Date()", bounds, ignore.case = TRUE)
  bounds <- trimws(bounds)
  bounds[!nzchar(bounds)] <- NA_character_

  bounds
}

#' Work out the bounds that stand on their own
#'
#' A bound naming another column cannot be worked out before that column is
#' there, and answers nothing here. The dates are the only columns that lean
#' on one another, and they are drawn later.
#'
#' @param bounds The bounds, as `fake_constraint_text()` answers them.
#'
#' @return A character vector of the values, `NA` where a bound leans on a
#'   column.
#' @noRd
fake_constraint_value <- function(bounds) {
  vapply(bounds, constraint_value, character(1), USE.NAMES = FALSE)
}

#' Work out one bound
#'
#' @param bound The bound, as the setup wrote it.
#'
#' @return The value as a string, or `NA` when it cannot be worked out here.
#' @noRd
constraint_value <- function(bound) {
  if (is.na(bound)) {
    return(NA_character_)
  }

  # A bound is read as a date before it is read as code. `2026-08-16` parses
  # as a subtraction, and answering 2002 for it would be worse than useless.
  if (is_date_text(bound)) {
    return(as.character(as.Date(bound)))
  }

  if (length(expression_variables(bound)) > 0) {
    return(NA_character_)
  }

  worked <- tryCatch(
    eval(str2lang(bound), envir = baseenv()),
    error = function(cnd) NULL
  )

  if (is.null(worked) || length(worked) != 1) {
    return(NA_character_)
  }

  as.character(worked)
}

#' Fall back to the default range where a bound answered nothing
#'
#' @param values The bounds that were worked out.
#' @param types The variable type of each of them.
#' @param settings The generation settings.
#' @param edge Which end of the range to fall back on, 1 or 2.
#'
#' @return A character vector of bounds, with the gaps filled.
#' @noRd
fake_bounds <- function(values, types, settings, edge) {
  ranges <- list(
    date = as.character(settings$range_date),
    integer = as.character(settings$range_int),
    decimal = as.character(settings$range_num),
    text = as.character(settings$nchar_text)
  )

  missing <- is.na(values)

  values[missing & types %in% FAKE_TYPE_DATE] <- ranges$date[[edge]]
  values[missing & types %in% FAKE_TYPE_INTEGER] <- ranges$integer[[edge]]
  values[missing & types %in% FAKE_TYPE_DECIMAL] <- ranges$decimal[[edge]]
  values[missing & types %in% FAKE_TYPE_TEXT] <- ranges$text[[edge]]

  values
}

#' The names an expression reads
#'
#' @param text The expression, as the setup wrote it.
#'
#' @return A character vector of names, empty when the text names none or
#'   cannot be parsed at all.
#' @noRd
expression_variables <- function(text) {
  if (is.na(text)) {
    return(character())
  }

  parsed <- tryCatch(str2lang(text), error = function(cnd) NULL)

  if (is.null(parsed)) {
    return(character())
  }

  all.vars(parsed)
}

#' Test whether a bound was written as a date
#'
#' @param text The bound.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_date_text <- function(text) {
  if (is.na(text)) {
    return(FALSE)
  }

  parsed <- suppressWarnings(
    tryCatch(
      as.Date(text, tryFormats = c("%Y-%m-%d", "%Y/%m/%d")),
      error = function(cnd) as.Date(NA_character_)
    )
  )

  !is.na(parsed)
}

#' Ids, numbered from one
#'
#' @param n How many.
#' @param prefix The lead every id carries.
#' @param width How wide the number is, padded with zeros.
#'
#' @return A character vector.
#' @noRd
sample_ids <- function(n, prefix, width) {
  paste0(prefix, formatC(seq_len(n), width = width, flag = "0"))
}

#' Whole numbers in a range
#'
#' @param n How many.
#' @param low The lowest value.
#' @param high The highest value.
#' @param share How much of the column to leave missing.
#'
#' @return An integer vector, with a share of it missing.
#' @noRd
sample_integers <- function(n, low, high, share = FAKE_MISSING_SHARE) {
  thin_values(
    pick(seq(low, high, by = 1L), n),
    share = share,
    empty = NA_integer_
  )
}

#' Numbers in a range, to three places
#'
#' @param n How many.
#' @param low The lowest value.
#' @param high The highest value.
#' @param share How much of the column to leave missing.
#'
#' @return A numeric vector, with a share of it missing.
#' @noRd
sample_decimals <- function(n, low, high, share = FAKE_MISSING_SHARE) {
  thin_values(
    round(stats::runif(n, low, high), 3),
    share = share,
    empty = NA_real_
  )
}

#' Words of a length in a range
#'
#' @param n How many.
#' @param low The shortest.
#' @param high The longest.
#' @param share How much of the column to leave missing.
#'
#' @return A character vector, with a share of it missing.
#' @noRd
sample_text <- function(n, low, high, share = FAKE_MISSING_SHARE) {
  # A space is one letter in five, so the text reads as a few short words
  # rather than one run of letters.
  alphabet <- c(letters, rep(" ", 5))
  lengths <- pick(seq(low, high, by = 1L), n)

  words <- vapply(
    lengths,
    function(count) paste(pick(alphabet, count), collapse = ""),
    character(1)
  )

  thin_values(words, share = share, empty = NA_character_)
}

#' Labels drawn from one choice list
#'
#' @param n How many.
#' @param list_name The list the variable reads.
#' @param choices The choice lists of the setup.
#' @param share How much of the column to leave missing.
#'
#' @return A character vector, with a share of it missing.
#' @noRd
sample_choices <- function(n, list_name, choices, share = FAKE_MISSING_SHARE) {
  labels <- choices$label[choices$list_name %in% list_name]
  labels <- labels[is_filled(labels)]

  if (length(labels) == 0) {
    return(rep(NA_character_, n))
  }

  thin_values(pick(labels, n), share = share, empty = NA_character_)
}

#' Dates inside the bounds the setup wrote for them
#'
#' A bound naming another column is worked out against the records already
#' drawn, so every record gets a bound of its own.
#'
#' @param low The lower bound, as the setup wrote it.
#' @param high The upper bound, as the setup wrote it.
#' @param low_default What stands in where the lower bound answers nothing.
#' @param high_default What stands in where the upper bound answers nothing.
#' @param table The worksheet so far.
#'
#' @return A Date vector, one per record.
#' @noRd
sample_bounded_dates <- function(low, high, low_default, high_default, table) {
  n <- nrow(table)

  if (n == 0) {
    return(as.Date(character()))
  }

  lows <- bound_dates(low, default = low_default, n = n, table = table)
  highs <- bound_dates(high, default = high_default, n = n, table = table)

  # A bound worked out per record can land above the one above it, when the
  # column it names holds a late date and the other bound fell back on a
  # default. The record then sits on the day its lower bound names.
  crossed <- lows > highs
  highs[crossed] <- lows[crossed]

  drawn <- vapply(
    seq_len(n),
    function(index) {
      as.numeric(pick(seq(lows[[index]], highs[[index]], by = 1L), 1L))
    },
    numeric(1)
  )

  as.Date(drawn, origin = "1970-01-01")
}

#' Work one bound of a date out for every record
#'
#' @param bound The bound, as the setup wrote it.
#' @param default What stands in where the bound answers nothing.
#' @param n How many records.
#' @param table The worksheet so far.
#'
#' @return A Date vector of length `n`.
#' @noRd
bound_dates <- function(bound, default, n, table) {
  worked <- tryCatch(
    eval(str2lang(as.character(bound)), envir = table, enclos = baseenv()),
    error = function(cnd) NA
  )

  worked <- suppressWarnings(as.Date(worked, origin = "1970-01-01"))

  if (length(worked) == 1) {
    worked <- rep(worked, n)
  }

  if (length(worked) != n) {
    worked <- rep(as.Date(NA_character_), n)
  }

  worked[is.na(worked)] <- default
  worked
}

#' Leave a share of the values missing
#'
#' @param values The values.
#' @param share How much of it to leave out.
#' @param empty What a missing value reads as, of the right type.
#'
#' @return The values, thinned.
#' @noRd
thin_values <- function(values, share = FAKE_MISSING_SHARE, empty = NA) {
  if (share <= 0 || length(values) == 0) {
    return(values)
  }

  values[stats::runif(length(values)) < share] <- empty
  values
}

#' Draw with replacement, whatever the length of what is drawn from
#'
#' `sample()` reads a single number as a range to count up to, so a range of
#' one day or one value would be drawn from the wrong set. Drawing the
#' positions instead reads the same for every length.
#'
#' @param values What to draw from.
#' @param size How many to draw.
#'
#' @return `size` of the values.
#' @noRd
pick <- function(values, size) {
  if (length(values) == 0 || size == 0) {
    return(values[integer()])
  }

  values[sample.int(length(values), size, replace = TRUE)]
}

#' Test whether a value carries anything
#'
#' A setup writes an unused cell as an empty string through one reader and as
#' `NA` through another, and both mean the same thing.
#'
#' @param values The values.
#'
#' @return A logical vector.
#' @noRd
is_filled <- function(values) {
  !is.na(values) & nzchar(trimws(as.character(values)))
}
