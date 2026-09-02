# How R reaches Excel.
#
# R never talks to Excel itself. It builds every value, then calls a small
# script that drives Excel through the automation layer the system already
# has: VBScript and COM on Windows, AppleScript on macOS. One pair of scripts
# covers one operation, both halves take the same arguments in the same order,
# and every decision is made here before either half runs.
#
# A script answers one line: `OK`, or `ERROR <number>: <text>`. The Apple
# Event transport loses that line on runs Excel finished green, so every
# workbook also writes its summary beside the file it touched. When the answer
# does not arrive the summary file settles it. A refused run writes a summary
# too, so the file being there says only that Excel got far enough to write
# it. What it says is the `outcome` key it leads with. A file with no such key
# reads as a run that finished, which is what a binary predating the key
# writes.
#
# An answer of `OK` says the operation ran. It says nothing about what the
# operation produced: an export answers `OK` for any run that wrote a file,
# and on a workbook holding nothing that file holds nothing. What was produced
# is read off the summary, and checking it belongs to the caller.

# Where the script pairs sit inside the installed package.
DRIVER_SCRIPT_FOLDER <- "scripts"

# What each system runs a script with, what goes in front of the script, and
# the extension its half of a pair carries.
DRIVER_RUNNERS <- list(
  windows = list(
    command = "cscript",
    lead = "//nologo",
    extension = "vbs"
  ),
  macos = list(
    command = "osascript",
    lead = character(),
    extension = "applescript"
  )
)

# The script pairs the package ships, and the arguments each takes in order.
# Both halves of a pair read the same list, so the two cannot drift. A pair is
# added here when its scripts are written.
DRIVER_PAIRS <- list(
  `designer-generate` = list(
    workbook = "designer",
    arguments = c(
      "designer",
      "geo",
      "setup",
      "folder",
      "name",
      "setup_language",
      "form_language",
      "ribbon"
    )
  ),
  quiet = list(
    workbook = "any",
    arguments = c(
      "workbook",
      "switch_name",
      "action",
      "value",
      "summary"
    )
  ),
  `setup-export` = list(
    workbook = "setup",
    arguments = c("setup", "folder", "summary")
  ),
  `setup-import` = list(
    workbook = "setup",
    arguments = c("setup", "from", "summary")
  ),
  `setup-tags` = list(
    workbook = "setup",
    arguments = c("setup", "summary")
  ),
  `linelist-geobase` = list(
    workbook = "linelist",
    arguments = c("linelist", "geo")
  ),
  `linelist-import` = list(
    workbook = "linelist",
    arguments = c("linelist", "from", "rule", "force")
  ),
  `linelist-export` = list(
    workbook = "linelist",
    arguments = c("linelist", "name", "folder", "password", "other")
  )
)

# The answer a script prints when the run finished.
DRIVER_OK <- "OK"

# The answer a script prints when it failed, and how the number and the text
# are read out of it.
DRIVER_ERROR_PATTERN <- "^ERROR[[:space:]]+(-?[0-9]+)[[:space:]]*:[[:space:]]*(.*)$"

# Where the answer of a run came from.
DRIVER_FROM_ANSWER <- "answer"
DRIVER_FROM_FILE <- "file"

# The line that separates the values of a summary from the text under them.
SUMMARY_MARKER <- "--report--"

# The key a summary leads with to say how the run ended. A workbook that
# writes one writes it whether the run finished or was refused.
SUMMARY_OUTCOME_KEY <- "outcome"

# What that key holds when the run finished.
SUMMARY_OUTCOME_OK <- "OK"

# How many lines of what Excel printed are carried into a failure message.
DRIVER_ECHO_LINES <- 5L

#' Run one script pair and answer what the run produced
#'
#' The one place a script is called. Every operation reaches Excel through a
#' wrapper, and every wrapper ends here.
#'
#' Answering at all means the operation ran. The caller checks what it
#' produced, against the summary and against the file it expected.
#'
#' @param pair The name of the script pair.
#' @param args The arguments of the pair, in its own order, as
#'   `driver_args()` builds them.
#' @param summary The file the workbook writes its summary to.
#' @param os The operating system to build the call for.
#' @param call The environment to blame in the error.
#'
#' @return A list with `pair`, `answer`, `source`, `summary` and `report`.
#' @noRd
run_driver <- function(
  pair,
  args,
  summary = NA_character_,
  os = os_name(),
  call = rlang::caller_env()
) {
  check_supported_os(os, call = call)

  script <- driver_script(pair, os = os, call = call)
  command <- driver_command(script, args, os = os)
  ran <- driver_call(command$command, command$args)

  answer <- driver_answer(ran$output)
  read <- read_summary(summary)

  if (identical(answer, DRIVER_OK)) {
    return(new_driver_result(pair, DRIVER_OK, DRIVER_FROM_ANSWER, read))
  }

  if (is.na(answer)) {
    if (isTRUE(read$found)) {
      if (isFALSE(summary_finished(read))) {
        cli::cli_abort(
          c(
            "The {.val {pair}} run was refused.",
            "x" = "{.file {summary}} says {.val {summary_outcome(read)}}.",
            "i" = "Excel wrote: {.val {driver_echo(read$report)}}"
          ),
          call = call
        )
      }

      return(new_driver_result(pair, DRIVER_OK, DRIVER_FROM_FILE, read))
    }

    cli::cli_abort(
      c(
        "The {.val {pair}} run gave no answer.",
        "x" = "Nothing was written at {.file {summary}}, so the run failed.",
        "i" = "Excel printed: {.val {driver_echo(ran$output)}}"
      ),
      call = call
    )
  }

  failure <- driver_failure(answer)

  cli::cli_abort(
    c(
      "The {.val {pair}} run failed.",
      "x" = "Excel answered {.val {failure$number}}: {failure$text}"
    ),
    call = call
  )
}

#' Build the record of one run
#'
#' @param pair The name of the script pair.
#' @param answer The answer line the run is credited with.
#' @param source Where that answer came from.
#' @param read The summary, as `read_summary()` answers it.
#'
#' @return A list with `pair`, `answer`, `source`, `summary` and `report`.
#' @noRd
new_driver_result <- function(pair, answer, source, read) {
  list(
    pair = pair,
    answer = answer,
    source = source,
    summary = read$values,
    report = read$report
  )
}

#' Where the half of a pair for one system sits
#'
#' @param pair The name of the script pair.
#' @param os The operating system.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
driver_script <- function(pair, os = os_name(), call = rlang::caller_env()) {
  check_driver_pair(pair, call = call)

  file <- paste0(pair, ".", DRIVER_RUNNERS[[os]]$extension)
  path <- system.file(DRIVER_SCRIPT_FOLDER, file, package = "obt")

  if (!nzchar(path)) {
    cli::cli_abort(
      c(
        "The script {.file {file}} is missing from this installation.",
        "i" = "Install the package again."
      ),
      call = call
    )
  }

  path
}

#' Build the command line one system runs a script with
#'
#' Every part is quoted for the system it is built for, so a path holding a
#' space reaches the script whole. The quoting follows the system named here
#' rather than the one running R, so a test can build either.
#'
#' @param script The script to run.
#' @param args The arguments of the pair, in its own order.
#' @param os The operating system.
#'
#' @return A list with `command` and `args`.
#' @noRd
driver_command <- function(script, args, os = os_name()) {
  runner <- DRIVER_RUNNERS[[os]]
  parts <- driver_separators(c(runner$lead, script, args), os = os)

  list(
    command = runner$command,
    args = driver_quote(parts, os = os)
  )
}

#' Turn the paths of one call onto the separator of the system reading them
#'
#' R keeps every path on forward slashes, and `absolute_path()` makes it so
#' on both systems. Excel on Windows joins whatever base it is given with
#' `\`, so a `/` path handed to the designer comes back mangled the first
#' time the workbook builds a path under it, and Excel refuses the result.
#' The values cross to the scripts here, so the separators turn native here.
#'
#' A path is known by its drive lead (`C:/`). Every path a script takes is
#' absolute, so the lead marks all of them, and names, languages, switches
#' and empty strings pass through as they are. macOS reads `/` natively and
#' gets every value as it is.
#'
#' @param value The values of one call.
#' @param os The operating system.
#'
#' @return The values, with every path on the system's own separator.
#' @noRd
driver_separators <- function(value, os) {
  if (!identical(os, "windows")) {
    return(value)
  }

  path <- grepl("^[A-Za-z]:/", value)
  value[path] <- gsub("/", "\\", value[path], fixed = TRUE)

  value
}

#' Quote a value for the command line of one system
#'
#' @param value The values to quote.
#' @param os The operating system.
#'
#' @return The values, quoted.
#' @noRd
driver_quote <- function(value, os) {
  type <- if (identical(os, "windows")) "cmd" else "sh"
  shQuote(value, type = type)
}

#' Call a script and answer what it printed
#'
#' The one place the package runs a script, so a test mocks this and Excel is
#' never opened. `system2()` is what runs it, on both systems.
#'
#' @param command The command to run.
#' @param args Its arguments, already quoted.
#'
#' @return A list with `output` and `status`.
#' @noRd
driver_call <- function(command, args) {
  output <- tryCatch(
    suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE)),
    error = function(cnd) structure(character(), status = 1L)
  )

  status <- attr(output, "status")

  list(
    output = as.character(output),
    status = if (is.null(status)) 0L else as.integer(status)
  )
}

#' The answer line out of what a script printed
#'
#' Excel and the automation layer both print chatter of their own, so the
#' answer is picked out by its shape. A script prints one, and the last one
#' is the one the run ended on.
#'
#' @param output The lines the script printed.
#'
#' @return The answer line, or `NA` when none arrived.
#' @noRd
driver_answer <- function(output) {
  lines <- trimws(as.character(output))
  lines <- lines[nzchar(lines)]
  lines <- lines[lines == DRIVER_OK | grepl(DRIVER_ERROR_PATTERN, lines)]

  if (length(lines) == 0) {
    return(NA_character_)
  }

  lines[[length(lines)]]
}

#' The number and the text of a failed answer
#'
#' @param answer The answer line.
#'
#' @return A list with `number` and `text`.
#' @noRd
driver_failure <- function(answer) {
  list(
    number = sub(DRIVER_ERROR_PATTERN, "\\1", answer),
    text = sub(DRIVER_ERROR_PATTERN, "\\2", answer)
  )
}

#' What a failure message carries of the run's own output
#'
#' @param output The lines the script printed.
#'
#' @return A character vector, at most `DRIVER_ECHO_LINES` long.
#' @noRd
driver_echo <- function(output) {
  lines <- trimws(as.character(output))
  lines <- lines[nzchar(lines)]

  if (length(lines) == 0) {
    return("nothing")
  }

  lines[seq_len(min(length(lines), DRIVER_ECHO_LINES))]
}

#' Lay the arguments of a pair out in its own order
#'
#' Both halves of a pair read this list, so the order they are given in is the
#' order they arrive in on either system. A value the run does not use goes
#' across as an empty string, and the script writes it as it stands.
#'
#' @param pair The name of the script pair.
#' @param values The values, as a named list.
#' @param call The environment to blame in the error.
#'
#' @return A character vector, in the order the pair takes.
#' @noRd
driver_args <- function(pair, values, call = rlang::caller_env()) {
  check_driver_pair(pair, call = call)

  wanted <- DRIVER_PAIRS[[pair]]$arguments
  given <- names(values)

  absent <- setdiff(wanted, given)

  if (length(absent) > 0) {
    cli::cli_abort(
      c(
        "The {.val {pair}} pair takes {.val {wanted}}.",
        "x" = "{.val {absent}} {?was/were} left out."
      ),
      call = call
    )
  }

  spare <- setdiff(given, wanted)

  if (length(spare) > 0) {
    cli::cli_abort(
      c(
        "The {.val {pair}} pair takes {.val {wanted}}.",
        "x" = "{.val {spare}} {?is/are} not one of them."
      ),
      call = call
    )
  }

  vapply(
    wanted,
    function(name) driver_value(values[[name]]),
    character(1),
    USE.NAMES = FALSE
  )
}

#' Turn one value into the string a script is handed
#'
#' @param value The value.
#'
#' @return A single string. Empty where the run has no value to give.
#' @noRd
driver_value <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value)) {
    return("")
  }

  as.character(value)
}

#' Fail when a script pair is unknown
#'
#' @param pair The value to check.
#' @param call The environment to blame in the error.
#'
#' @return The pair name.
#' @noRd
check_driver_pair <- function(pair, call = rlang::caller_env()) {
  if (!is.character(pair) || length(pair) != 1 || is.na(pair)) {
    cli::cli_abort("A script pair must be a single string.", call = call)
  }

  if (!pair %in% names(DRIVER_PAIRS)) {
    shipped <- names(DRIVER_PAIRS)
    cli::cli_abort(
      c(
        "{.val {pair}} is not a script pair this package ships.",
        "i" = "It ships {.val {shipped}}."
      ),
      call = call
    )
  }

  pair
}

#' Read the summary a workbook wrote beside the file it produced
#'
#' The file holds `key=value` lines, then a marker, then free text. It is what
#' a run is read back from when the answer of the script does not arrive.
#'
#' @param path The summary file.
#'
#' @return A list with `found`, `values` and `report`.
#' @noRd
read_summary <- function(path) {
  empty <- list(found = FALSE, values = character(), report = character())

  if (length(path) != 1 || is.na(path) || !file.exists(path)) {
    return(empty)
  }

  lines <- summary_lines(path)
  marker <- match(SUMMARY_MARKER, trimws(lines))
  cut <- if (is.na(marker)) length(lines) + 1L else marker

  values <- summary_values(lines[seq_len(cut - 1L)])

  report <- if (cut < length(lines)) {
    lines[(cut + 1L):length(lines)]
  } else {
    character()
  }

  list(found = TRUE, values = values, report = report)
}

#' What a summary says about how the run ended
#'
#' @param read The summary, as `read_summary()` answers it.
#'
#' @return The value of the outcome key, or `NA` where the summary carries
#'   none.
#' @noRd
summary_outcome <- function(read) {
  if (!isTRUE(read$found)) {
    return(NA_character_)
  }

  if (!SUMMARY_OUTCOME_KEY %in% names(read$values)) {
    return(NA_character_)
  }

  read$values[[SUMMARY_OUTCOME_KEY]]
}

#' Whether a summary says the run finished
#'
#' A workbook that records an outcome answers it here. A summary with no
#' outcome key answers `NA`, and the caller reads that as a run that finished:
#' it is what a binary predating the key writes, and the file being there is
#' all such a summary can say.
#'
#' @param read The summary, as `read_summary()` answers it.
#'
#' @return `TRUE`, `FALSE`, or `NA` where the summary carries no outcome.
#' @noRd
summary_finished <- function(read) {
  outcome <- summary_outcome(read)

  if (is.na(outcome)) {
    return(NA)
  }

  identical(toupper(trimws(outcome)), SUMMARY_OUTCOME_OK)
}

#' Read the lines of a summary in the encoding they were written
#'
#' Excel writes the summary in the system's own ANSI codepage on Windows and
#' in UTF-8 on macOS. A line that already reads as UTF-8 passes through as it
#' is. A line that does not is turned to UTF-8 from that codepage, and a byte
#' with no mapping crosses as its hex escape, so the text Excel wrote stays
#' readable either way.
#'
#' @param path The summary file.
#'
#' @return The lines, every one of them valid UTF-8.
#' @noRd
summary_lines <- function(path) {
  lines <- readLines(path, warn = FALSE)
  broken <- !validUTF8(lines)

  if (any(broken)) {
    lines[broken] <- iconv(
      lines[broken],
      from = summary_codepage(),
      to = "UTF-8",
      sub = "byte"
    )
  }

  lines
}

#' The codepage Excel writes the summary in
#'
#' Windows answers its ANSI codepage through `l10n_info()`. Everywhere else,
#' and on a Windows whose own codepage is already UTF-8, latin1 stands in:
#' it accepts every byte, so a summary always reads.
#'
#' @return An encoding name `iconv()` accepts.
#' @noRd
summary_codepage <- function() {
  info <- l10n_info()
  codepage <- info$system.codepage

  if (is.null(codepage)) {
    codepage <- info$codepage
  }

  if (is.null(codepage) || isTRUE(codepage == 65001)) {
    return("latin1")
  }

  paste0("CP", codepage)
}

#' Read the `key=value` lines of a summary
#'
#' @param lines The lines above the marker.
#'
#' @return A named character vector.
#' @noRd
summary_values <- function(lines) {
  lines <- lines[grepl("=", lines, fixed = TRUE)]

  keys <- trimws(sub("=.*$", "", lines))
  values <- trimws(sub("^[^=]*=", "", lines))

  keep <- nzchar(keys)
  values <- values[keep]
  names(values) <- keys[keep]

  values
}
