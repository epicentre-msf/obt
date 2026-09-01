# The two languages of a run.
#
# A run carries two languages and they are two different things. The
# dictionary language is a column of the setup's own translation table, and it
# picks the translation of the dictionary, the choices and the analyses the
# linelist is built from. The interface language is a code, and it picks the
# language of the buttons, the messages and the sheet names.
#
# The interface language is checked at the verb. The designer takes the code
# it is handed, finds no message column under it, and writes the raw tags into
# the linelist. The run ends green and the linelist is unusable, so a code the
# designer does not hold never reaches Excel.
#
# The dictionary language is checked against the setup file whenever the
# working folder already holds one, and again when the run starts.

# The interface languages the designer offers, and the code each one reaches
# the workbook as. The code is what the linelist's message tables are keyed
# on. The designer's own dropdown shows every name in its own language and
# offers the pair as "CODE-Name".
FORM_LANGUAGES <- c(
  ARA = "Arabic",
  ENG = "English",
  FRA = "French",
  POR = "Portuguese",
  SPA = "Spanish"
)

# Codes a user reaches for that the designer does not hold, and the code each
# one is meant to be. Spanish has already cost a build: "ESP" is the everyday
# code for the language, the message tables are keyed on "SPA", and a linelist
# built under "ESP" came out with every message showing its own tag.
FORM_LANGUAGE_MISTAKES <- c(ESP = "SPA")

# What separates the code from the name in the form the designer's dropdown
# offers, "ENG-English".
FORM_LANGUAGE_SEPARATOR <- "-"

#' Set the languages of the run
#'
#' @description
#' Records the two languages the linelist is built in. They are two different
#' things and the recipe keeps them apart.
#'
#' `dict` is the dictionary language: a column of the setup's own translation
#' table, such as `"English"`. It picks the translation of the dictionary, the
#' choices and the analyses. Its list is read from the setup file, so it is
#' checked against that file whenever the working folder already holds one,
#' and again when the run starts.
#'
#' `form` is the interface language: the language of the buttons, the messages
#' and the sheet names. It is one of `r toString(names(FORM_LANGUAGES))`.
#' The form `CODE-Name`, as the designer's own list offers it, is accepted
#' too, and only the code is kept. A code the designer does not hold is
#' refused.
#'
#' A recipe that generates a linelist needs both, and
#' the run says so when one is missing: the designer writes whatever it is
#' handed into its own sheet, so an empty language blanks the value the
#' workbook carried.
#'
#' Like every verb this one records and answers the recipe. Nothing is written
#' until `obt_commit()` runs.
#'
#' @param obtops An `obt` recipe.
#' @param dict The dictionary language, as a column name of the setup's
#'   translation table.
#' @param form The interface language, as a code or as `CODE-Name`.
#'
#' @return The recipe, with the language operation added.
#'
#' @seealso [obt_languages()] for both lists, [obt_setup_languages()] for the
#'   languages one setup file holds.
#'
#' @export
#'
#' @examples
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_languages(dict = "English", form = "ENG")
#'
#' # The designer's own list offers the code with its name. Only the code is
#' # kept.
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_languages(form = "FRA-French")
obt_designer_languages <- function(obtops, dict = NULL, form = NULL) {
  check_obt_plain(obtops)

  if (is.null(dict) && is.null(form)) {
    cli::cli_abort(c(
      "Give {.arg dict}, {.arg form}, or both.",
      "i" = "An operation that sets nothing prints as if it set something."
    ))
  }

  dict <- check_dict_language(dict, setup = dict_language_source(obtops))
  form <- check_form_language(form)

  add_operation(
    obtops,
    type = "designer-languages",
    args = list(dict = dict, form = form)
  )
}

#' Print the languages a run can be built in
#'
#' @description
#' Prints both lists, grouped: the dictionary languages the setup file holds,
#' and the interface languages the designer offers, showing the ones that
#' were picked.
#'
#'
#' @param obtops An `obt` recipe.
#'
#' @return The recipe, unchanged and invisibly.
#'
#' @seealso [obt_designer_languages()] to pick them, [obt_describe()] for the
#'   whole recipe.
#'
#' @export
#'
#' @examples
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_languages(dict = "English", form = "ENG") |>
#'   obt_languages()
obt_languages <- function(obtops) {
  check_obt(obtops)

  chosen <- last_operation(obtops, "designer-languages")

  cli::cli_h1("Languages")

  cli::cli_h2("Dictionary, from the setup file")
  say_dict_languages(obtops)
  say_chosen(chosen, "dict", "The dictionary is built in")

  cli::cli_h2("Interface, the designer's own list")
  cli::cli_dl(FORM_LANGUAGES)
  say_chosen(chosen, "form", "The interface is built in")

  invisible(obtops)
}

#' Print the dictionary languages of the working setup
#'
#' @param obtops The recipe.
#'
#' @return `NULL`, invisibly. Called for what it prints.
#' @noRd
say_dict_languages <- function(obtops) {
  path <- dict_language_source(obtops)

  if (is.na(path)) {
    cli::cli_alert_info(
      "The working folder holds no setup yet. The list is read from
       {.file {obt_paths(obtops)$setup_file}} once one is there."
    )
    return(invisible(NULL))
  }

  items <- cli::cli_ul(cli_escape(obt_setup_languages(path)))
  cli::cli_end(items)
  cli::cli_alert_info("Read from {.file {path}}.")

  invisible(NULL)
}

#' Print the language a recipe has picked, when it has picked one
#'
#' @param operation The language operation the recipe holds, or `NULL`.
#' @param arg The argument to read it from.
#' @param lead The words the value is printed after.
#'
#' @return `NULL`, invisibly. Called for what it prints.
#' @noRd
say_chosen <- function(operation, arg, lead) {
  if (is.null(operation)) {
    return(invisible(NULL))
  }

  value <- operation$args[[arg]]

  if (is.null(value)) {
    return(invisible(NULL))
  }

  cli::cli_alert_success("{lead} {.val {value}}.")

  invisible(NULL)
}

#' The setup file a dictionary language can be checked against
#'
#' A recipe can be built before anything is on disk, so there is often no
#' setup to check against yet. The run checks again when it starts, by which
#' time the setup is there.
#'
#' @param obtops The recipe.
#'
#' @return The absolute path of the working setup, or `NA` when the working
#'   folder holds none.
#' @noRd
dict_language_source <- function(obtops) {
  path <- obt_paths(obtops)$setup_file

  if (file.exists(path)) {
    return(path)
  }

  NA_character_
}

#' Check a dictionary language against the setup that holds the list
#'
#' The answer is the spelling the file itself carries, so the workbook is
#' handed the column name it holds.
#'
#' @param dict The value to check. `NULL` passes, because the argument is
#'   optional.
#' @param setup The setup file to check against, or `NA` to record the value
#'   unchecked.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The language, spelled as the setup file spells it.
#' @noRd
check_dict_language <- function(
  dict,
  setup,
  arg = rlang::caller_arg(dict),
  call = rlang::caller_env()
) {
  # The label is read before `dict` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  if (is.null(dict)) {
    return(NULL)
  }

  dict <- check_string(dict, arg = arg, call = call)

  if (is.na(setup)) {
    return(dict)
  }

  known <- obt_setup_languages(setup)
  found <- match(tolower(dict), tolower(known))

  if (is.na(found)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a language the setup file holds.",
        "x" = "You supplied {.val {dict}}.",
        "i" = "The file holds {.val {known}}.",
        "i" = "Read from {.file {setup}}."
      ),
      call = call
    )
  }

  known[[found]]
}

#' Check an interface language and keep the code
#'
#' @param form The value to check. `NULL` passes, because the argument is
#'   optional.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The code, in upper case.
#' @noRd
check_form_language <- function(
  form,
  arg = rlang::caller_arg(form),
  call = rlang::caller_env()
) {
  force(arg)

  if (is.null(form)) {
    return(NULL)
  }

  form <- check_string(form, arg = arg, call = call)
  code <- form_language_code(form)
  known <- names(FORM_LANGUAGES)

  if (code %in% known) {
    return(code)
  }

  meant <- unname(FORM_LANGUAGE_MISTAKES[code])

  message <- c(
    "{.arg {arg}} must be one of {.val {known}}.",
    "x" = "You supplied {.val {form}}."
  )

  if (!is.na(meant)) {
    message <- c(
      message,
      "i" = "{.val {code}} is the everyday code for
             {FORM_LANGUAGES[[meant]]}. The designer holds it as
             {.val {meant}}."
    )
  }

  cli::cli_abort(
    c(
      message,
      "i" = "The form {.val CODE-Name} is accepted too, and only the code is
             kept."
    ),
    call = call
  )
}

#' Read the code out of an interface language
#'
#' The designer's own list offers a code and a name joined by a dash, and it
#' splits the value it is given the same way.
#'
#' @param form The value the verb was given, already a trimmed string.
#'
#' @return The code, in upper case.
#' @noRd
form_language_code <- function(form) {
  parts <- strsplit(form, FORM_LANGUAGE_SEPARATOR, fixed = TRUE)[[1]]

  if (length(parts) == 0) {
    return("")
  }

  toupper(trimws(parts[[1]]))
}
