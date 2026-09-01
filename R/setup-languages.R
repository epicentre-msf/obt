# The languages a setup file holds.
#
# The designer reads this list in EventsDesignerAdvanced.SetupLanguages: it
# prefers a hidden defined name on the Translations sheet and falls back to the
# header row of the first table there, dropping the internal tag columns. Only
# the fallback is reachable from R, because none of the R readers open a hidden
# defined name in a closed workbook. A list that disagrees with the designer's
# is a fault on this side, so the rules below copy that routine: trim each
# header, drop the empty ones, drop the ones leading with the tag mark.

# What marks a column as internal rather than a language. The setup writes its
# tag column as "__TagInternal__" today (SetupTranslationsTable), so the lead
# is what identifies it.
INTERNAL_TAG_LEAD <- "__"

# What the tag column was called before it took the double underscore. A setup
# built by an older OutbreakTools still carries this header, and it is a column
# of tags rather than a language. The designer's own fallback keeps it, because
# newer files never reach that fallback: they answer from a stored name the R
# readers cannot open. Reading it back as a language would offer the user a
# dictionary language that does not exist, so it goes.
LEGACY_TAG_HEADER <- "TranslationTag"

# The sheet every setup keeps its translations on.
TRANSLATIONS_SHEET <- "Translations"

# How many rows sit above the header, per format. A setup workbook carries a
# title block above its table; the .xlsx an export writes starts at the header.
TRANSLATIONS_SKIP <- c(xlsb = 3L, xlsx = 0L)

#' Read the languages of a setup
#'
#'
#' @param path Path to a setup file. Either an `.xlsb` setup or the `.xlsx` an
#'   export of one writes.
#'
#' @return A character vector of language names, in the order the file holds
#'   them.
#'
#' @export
#'
#' @examples
#' fixture <- system.file(
#'   "extdata", "setup-translations.xlsx",
#'   package = "obt"
#' )
#' if (nzchar(fixture)) {
#'   obt_setup_languages(fixture)
#' }
obt_setup_languages <- function(path) {
  extension <- check_setup_path(path)
  header <- read_translations_header(path, extension)

  # An empty header cell reads as NA from one reader and as "" from the other.
  # The designer drops both, so both are dropped here before anything is
  # compared.
  header <- header[!is.na(header)]
  header <- trimws(header)
  header <- header[nzchar(header)]
  header <- header[!startsWith(header, INTERNAL_TAG_LEAD)]
  languages <- header[header != LEGACY_TAG_HEADER]

  if (length(languages) == 0) {
    cli::cli_abort(c(
      "{.file {path}} holds no languages.",
      "i" = "The {.val {TRANSLATIONS_SHEET}} sheet has no column that is not
             an internal tag column."
    ))
  }

  languages
}

#' Check the path handed to a setup reader
#'
#' @param path The path to check.
#'
#' @return The lowercased file extension, as a string.
#' @noRd
check_setup_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    cli::cli_abort(c(
      "{.arg path} must be a single file path.",
      "x" = "You supplied {.obj_type_friendly {path}}."
    ))
  }

  if (!file.exists(path)) {
    cli::cli_abort("No file at {.file {path}}.")
  }

  extension <- tolower(tools::file_ext(path))

  if (!extension %in% names(TRANSLATIONS_SKIP)) {
    cli::cli_abort(c(
      "{.arg path} must name an {.val xlsb} or an {.val xlsx} file.",
      "x" = "{.file {path}} is an {.val {extension}} file."
    ))
  }

  extension
}

#' Read the header row of a setup's translations table
#'
#' @param path Path to the setup file.
#' @param extension The lowercased file extension, from `check_setup_path()`.
#'
#' @return A character vector of the header cells, untrimmed and unfiltered.
#' @noRd
read_translations_header <- function(path, extension) {
  skip <- unname(TRANSLATIONS_SKIP[[extension]])

  header <- switch(
    extension,
    xlsx = read_header_xlsx(path, skip),
    xlsb = read_header_xlsb(path, skip)
  )

  as.character(header)
}

#' Read a translations header out of an .xlsx file
#'
#' @param path Path to the file.
#' @param skip Rows above the header.
#'
#' @return A character vector of the header cells.
#' @noRd
read_header_xlsx <- function(path, skip) {
  sheets <- readxl::excel_sheets(path)
  check_translations_sheet(path, sheets)

  # The header is read as data rather than as names. readxl repairs names it
  # is asked to make, and a repaired `__Tag` no longer leads with the tag mark.
  row <- readxl::read_excel(
    path,
    sheet = TRANSLATIONS_SHEET,
    skip = skip,
    n_max = 1,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )

  unlist(row, use.names = FALSE)
}

#' Read a translations header out of an .xlsb file
#'
#' @param path Path to the file.
#' @param skip Rows above the header.
#'
#' @return A character vector of the header cells.
#' @noRd
read_header_xlsb <- function(path, skip) {
  # readxlsb answers its own fault when a sheet is missing, and the text of it
  # names neither the file nor the sheets the file does hold.
  row <- tryCatch(
    readxlsb::read_xlsb(
      path,
      sheet = TRANSLATIONS_SHEET,
      skip = skip,
      col_names = FALSE
    ),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Could not read {.val {TRANSLATIONS_SHEET}} from {.file {path}}.",
          "x" = "{conditionMessage(cnd)}",
          "i" = "A setup file carries a {.val {TRANSLATIONS_SHEET}} sheet."
        ),
        parent = cnd
      )
    }
  )

  if (nrow(row) == 0) {
    cli::cli_abort(
      "{.val {TRANSLATIONS_SHEET}} in {.file {path}} holds no header row."
    )
  }

  unlist(row[1, ], use.names = FALSE)
}

#' Fail when a file carries no translations sheet
#'
#' @param path Path to the file, for the message.
#' @param sheets The sheet names the file holds.
#'
#' @return `NULL`, invisibly. Called for the error it raises.
#' @noRd
check_translations_sheet <- function(path, sheets) {
  if (TRANSLATIONS_SHEET %in% sheets) {
    return(invisible(NULL))
  }

  cli::cli_abort(c(
    "{.file {path}} carries no {.val {TRANSLATIONS_SHEET}} sheet.",
    "i" = "It holds {.val {sheets}}.",
    "x" = "This does not look like a setup file."
  ))
}
