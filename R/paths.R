# The working folder, and every path under it.
#
# `obt()` names one folder. Everything the package downloads, copies or writes
# lives under it, in the layout below. `obt_paths()` resolves that layout and
# is the one place a path is built, so a recipe can store its paths relative to
# the folder and the folder can be moved.
#
# The package creates a folder when it needs it and removes nothing.

# The folders of the layout, as paths relative to the working folder.
OBT_FOLDERS <- c(
  obt = "obt",
  obt_main = "obt/main",
  obt_dev = "obt/dev",
  setup = "setup",
  setup_source = "setup/source",
  geo = "geo",
  linelist = "linelist",
  export = "export",
  log = "log"
)

# The files the layout names. The working setup is the empty template from the
# zip with the user's imports written on top of it.
OBT_FILES <- c(setup_file = "setup/setup.xlsb")

# The extension a generated linelist carries.
LINELIST_EXTENSION <- "xlsb"

# The extension a run log carries.
LOG_EXTENSION <- "txt"

# How a run log is stamped: the date, then the time.
LOG_STAMP_FORMAT <- "%Y%m%d-%H%M%S"

# What a workbook names the file it writes its summary to. It sits beside the
# file the run produced, and a driver reads it when the answer of the run does
# not arrive.
SUMMARY_SUFFIX <- "-obt-summary.txt"

#' The paths of a working folder
#'
#' @description
#' Resolves the layout of the working folder and shows every path in it as
#' an absolute path.
#'
#' The layout is fixed:
#'
#' ```
#' <folder>/
#'   obt/        the OutbreakTools files, one folder per channel
#'     main/
#'     dev/
#'   setup/      the setup the designer reads
#'     source/   copies of the files handed to the setup verbs
#'   geo/        copies of the geobase files
#'   linelist/   generated linelists
#'   export/     files written by the export verbs
#'   log/        one text log per run
#' ```
#'
#' Reading the paths creates nothing. A folder is created when the run that
#' needs it starts.
#'
#' @param obtops An `obt` recipe.
#'
#' @return A named list of absolute paths: `folder`, one entry per folder of
#'   the layout, and `setup_file` for the working setup.
#'
#' @export
#'
#' @examples
#' paths <- obt_paths(obt(folder = file.path(tempdir(), "measles")))
#' names(paths)
#' paths$linelist
obt_paths <- function(obtops) {
  check_obt(obtops)

  folder <- absolute_path(obtops$folder)
  entries <- c(OBT_FOLDERS, OBT_FILES)

  resolved <- as.list(file.path(folder, entries))
  names(resolved) <- names(entries)

  c(list(folder = folder), resolved)
}

#' Build the whole layout under the working folder
#'
#' Every folder of the layout is created, and nothing is removed. A run calls
#' this once, at its start, so an operation later on finds the folder it
#' writes into.
#'
#' @param obtops The recipe.
#' @param call The environment to blame in the error.
#'
#' @return The paths, as `obt_paths()` answers them, invisibly.
#' @noRd
create_obt_folder <- function(obtops, call = rlang::caller_env()) {
  paths <- obt_paths(obtops)

  ensure_folder(paths$folder, call = call)

  for (entry in names(OBT_FOLDERS)) {
    ensure_folder(paths[[entry]], call = call)
  }

  invisible(paths)
}

#' Create one folder, with everything above it
#'
#' @param path The folder to create.
#' @param call The environment to blame in the error.
#'
#' @return The path, invisibly.
#' @noRd
ensure_folder <- function(path, call = rlang::caller_env()) {
  if (dir.exists(path)) {
    return(invisible(path))
  }

  if (file.exists(path)) {
    cli::cli_abort(
      c(
        "The package needs a folder at {.file {path}}.",
        "x" = "A file sits there.",
        "i" = "Move the file, or name another working folder."
      ),
      call = call
    )
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(path)) {
    cli::cli_abort(
      c(
        "The folder {.file {path}} could not be created.",
        "i" = "Check that you can write in {.file {dirname(path)}}."
      ),
      call = call
    )
  }

  invisible(path)
}

#' Fail when a file the run writes is already there
#'
#' A run writes its output under a name the user chose, and a second run with
#' the same name would replace the first one's work. That takes the user
#' saying so.
#'
#' @param path The file the run would write.
#' @param overwrite Whether the file may be replaced.
#' @param arg The name of the argument that carries `overwrite`.
#' @param call The environment to blame in the error.
#'
#' @return The path, invisibly.
#' @noRd
check_output_free <- function(
  path,
  overwrite,
  arg = "overwrite",
  call = rlang::caller_env()
) {
  check_flag(overwrite, arg = arg, call = call)

  if (isTRUE(overwrite) || !file.exists(path)) {
    return(invisible(path))
  }

  cli::cli_abort(
    c(
      "{.file {path}} is already there.",
      "i" = "Use {.code {arg} = TRUE} to write over it."
    ),
    call = call
  )
}

#' The path a generated linelist is written to
#'
#' @param obtops The recipe.
#' @param name The name the linelist was recorded under.
#'
#' @return An absolute path.
#' @noRd
linelist_path <- function(obtops, name) {
  file.path(
    obt_paths(obtops)$linelist,
    paste0(name, ".", LINELIST_EXTENSION)
  )
}

#' The path the log of one run is written to
#'
#' @param obtops The recipe.
#' @param when The time the run started.
#'
#' @return An absolute path.
#' @noRd
log_path <- function(obtops, when = Sys.time()) {
  file.path(
    obt_paths(obtops)$log,
    paste0(format(when, LOG_STAMP_FORMAT), ".", LOG_EXTENSION)
  )
}

#' The folder one channel of the OutbreakTools files lives in
#'
#' @param obtops The recipe.
#' @param type The channel.
#'
#' @return An absolute path.
#' @noRd
channel_path <- function(obtops, type) {
  obt_paths(obtops)[[channel_spec(type)$entry]]
}

#' The path the zip of one channel is downloaded to
#'
#' The name of the zip is read from the address it is downloaded from, so the
#' two carry one name between them.
#'
#' @param obtops The recipe.
#' @param type The channel.
#'
#' @return An absolute path.
#' @noRd
channel_zip_path <- function(obtops, type) {
  file.path(channel_path(obtops, type), basename(channel_spec(type)$url))
}

#' The path a workbook writes its summary to
#'
#' The workbook writes it beside the file it produced, under the name of that
#' file. A driver reads it when the answer of the run does not arrive.
#'
#' @param folder The folder the run writes into.
#' @param name The name the output was recorded under.
#'
#' @return An absolute path.
#' @noRd
summary_path <- function(folder, name) {
  file.path(absolute_path(folder), paste0(name, SUMMARY_SUFFIX))
}

#' The path a workbook writes its summary to, beside a file it was handed
#'
#' An operation that reads a file writes its summary next to that file, under
#' the same name. An operation that writes one names its own output instead,
#' and goes through `summary_path()`.
#'
#' @param path The file the workbook was handed.
#'
#' @return An absolute path.
#' @noRd
summary_beside <- function(path) {
  full <- absolute_path(path)

  summary_path(dirname(full), tools::file_path_sans_ext(basename(full)))
}

#' The path a workbook writes its summary to, inside a folder it was handed
#'
#' An operation that writes into a folder of its own names that folder and the
#' workbook it drove. The summary sits in the folder, under the name of the
#' workbook, because the file the run produced is named by the workbook and R
#' cannot know that name before the run.
#'
#' @param folder The folder the run writes into.
#' @param path The workbook the run drove.
#'
#' @return An absolute path.
#' @noRd
summary_in <- function(folder, path) {
  summary_path(folder, tools::file_path_sans_ext(basename(path)))
}

#' The path a file takes when it is copied into the working folder
#'
#' The user's own file is an input and stays where it is. The copy keeps its
#' name, so a user opening the folder recognises what they handed over.
#'
#' @param obtops The recipe.
#' @param entry The layout entry the copy goes into.
#' @param path The file the user handed over.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path.
#' @noRd
copy_path <- function(obtops, entry, path, call = rlang::caller_env()) {
  check_layout_entry(entry, call = call)
  file.path(obt_paths(obtops)[[entry]], basename(path))
}

#' Fail when a layout entry is unknown
#'
#' @param entry The value to check.
#' @param call The environment to blame in the error.
#'
#' @return The entry.
#' @noRd
check_layout_entry <- function(entry, call = rlang::caller_env()) {
  known <- names(OBT_FOLDERS)

  if (!is.character(entry) || length(entry) != 1 || is.na(entry)) {
    cli::cli_abort("A layout entry must be a single string.", call = call)
  }

  if (!entry %in% known) {
    cli::cli_abort(
      c(
        "{.val {entry}} is not a folder of the layout.",
        "i" = "The layout holds {.val {known}}."
      ),
      call = call
    )
  }

  entry
}

#' Answer a path the way a recipe stores it
#'
#' A path under the working folder is stored relative to it, so the folder can
#' be moved and the recipe still reads. A path outside the folder is stored as
#' it stands.
#'
#' @param path The path.
#' @param folder The working folder.
#'
#' @return The path, relative to the folder where it sits under it.
#' @noRd
path_relative <- function(path, folder) {
  folder <- absolute_path(folder)
  full <- absolute_path(path)
  lead <- paste0(folder, "/")

  if (!startsWith(full, lead)) {
    return(path)
  }

  substring(full, nchar(lead) + 1L)
}

#' Resolve a path a recipe stored
#'
#' @param path The path, relative to the working folder or absolute.
#' @param folder The working folder.
#'
#' @return An absolute path.
#' @noRd
path_absolute <- function(path, folder) {
  if (is_absolute_path(path)) {
    return(absolute_path(path))
  }

  absolute_path(file.path(folder, path))
}

#' Make a path absolute, and write it with one shape of separator
#'
#' A relative path is resolved against the working directory of the session.
#' Symbolic links are left alone: the path a user handed over is the path they
#' see in their file browser, and the run reports it back to them.
#'
#' Repeated separators are collapsed, because `path_relative()` compares two
#' paths by their text and `/work//geo` has to match `/work`. The first two
#' characters are left alone, so a Windows network path keeps its lead.
#'
#' @param path The path.
#'
#' @return An absolute path, with one `/` between its parts.
#' @noRd
absolute_path <- function(path) {
  path <- gsub("\\", "/", path.expand(path), fixed = TRUE)

  if (!is_absolute_path(path)) {
    path <- paste0(gsub("\\", "/", getwd(), fixed = TRUE), "/", path)
  }

  path <- paste0(substring(path, 1L, 2L), collapse_slashes(substring(path, 3L)))
  path <- gsub("(?<=.)/\\.(?=/|$)", "", path, perl = TRUE)
  sub("(?<=.)/+$", "", path, perl = TRUE)
}

#' Squeeze repeated separators down to one
#'
#' @param path The tail of a path.
#'
#' @return The tail, with one `/` between its parts.
#' @noRd
collapse_slashes <- function(path) {
  gsub("/{2,}", "/", path)
}

#' Test whether a path is absolute
#'
#' Three shapes count: a leading `/`, a Windows drive letter, and a UNC path.
#'
#' @param path The path.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\|//)", path)
}
