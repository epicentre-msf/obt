# Whether a run can happen at all, and where it happens.
#
# Two things decide it. The operating system: opening Excel takes macOS or
# Windows, and everywhere else a recipe still builds, prints and tests. And,
# on macOS, whether the package can reach Excel's own folder. Excel for Mac
# opens a file inside its container with no question, so a run is copied in
# there and the results are moved back out, and Excel meets a path it can
# always open.
#
# Nothing here refuses a run over the staging folder. Where the folder cannot
# be written the run works in the user's own folder, Excel asks about each
# file it opens, and the user is told once what to switch on. The operating
# system is the one thing that stops a run.

# Where Excel for Mac keeps the folder it opens without asking.
EXCEL_CONTAINER_DOCUMENTS <-
  "~/Library/Containers/com.microsoft.Excel/Data/Documents"

# The folder the package owns inside it.
STAGING_FOLDER <- "OBTFromR"

# The file written to prove the staging folder can be reached. `dir.exists()`
# answers TRUE for a folder macOS refuses to open, so the write is what
# settles it.
STAGING_PROBE <- ".reachable"

# What the folder of one run inside the staging folder is called.
STAGE_RUN_LEAD <- "run-"

# Where a file from outside the working folder is staged. It has no home to
# go back to, so it stays here and goes with the run folder.
STAGE_OUTSIDE <- "in"

# How long a run folder left behind by an interrupted run is kept before the
# next run sweeps it away.
STAGE_STALE_HOURS <- 24

# Where Excel for Mac usually sits, and how to ask the system for it.
EXCEL_APP_PATH <- "/Applications/Microsoft Excel.app"
EXCEL_BUNDLE_ID <- "com.microsoft.Excel"

# The version of Excel for Mac this package was proven on. A few versions
# crash Excel while the analyses are built, and the ones that do are spread
# through the range rather than sitting in a block.
EXCEL_VERSION_PROVEN <- "16.111"

# The systems that can open Excel.
SUPPORTED_OS <- c("macos", "windows")

# How far up the process tree the app running this R session is looked for.
PROCESS_WALK_LIMIT <- 12L

# What the message says when the app running the R session cannot be read.
UNKNOWN_APP <- "the app running this R session"

# The environment variables that name the app running the R session, read
# when the process tree gives nothing.
SESSION_APP_HINTS <- list(
  list(name = "POSITRON", value = "1", app = "Positron"),
  list(name = "RSTUDIO", value = "1", app = "RStudio"),
  list(name = "TERM_PROGRAM", value = "Apple_Terminal", app = "Terminal"),
  list(name = "TERM_PROGRAM", value = "iTerm.app", app = "iTerm"),
  list(name = "TERM_PROGRAM", value = "vscode", app = "Visual Studio Code")
)

# What the package remembers for as long as the R session lives. The staging
# warning is said once: a run stages many files and would otherwise repeat it
# for every one of them.
platform_state <- new.env(parent = emptyenv())

#' Forget what the package remembers about this session
#'
#' @return `NULL`, invisibly.
#' @noRd
reset_platform_state <- function() {
  rm(list = ls(platform_state, all.names = TRUE), envir = platform_state)
  invisible(NULL)
}

#' What this machine can do
#'
#' @description
#' Reads the two things that decide whether a recipe can run here, and answers
#' them as a record you can print.
#'
#' **The operating system.** `obt_commit()` opens Excel, and that takes macOS
#' or Windows. On any other system a recipe still builds and prints, and only
#' the run itself is refused.
#'
#' **The staging folder, on macOS.** Excel for Mac opens a file inside its own
#' folder without asking, so the package copies a run in there and moves the
#' results back out. Reaching that folder takes Full Disk Access for the app
#' running your R session. Where it cannot be reached the run works in your own
#' folder and Excel asks about each file it opens.
#'
#' **The version of Excel, on macOS.** A few versions crash Excel while the
#' analyses are built. `"16.111"` is the one this package was proven on, and
#' every run records the version it found.
#'
#' Reading the staging folder writes a small file in it and removes it again,
#' because a folder macOS refuses to open still answers as present.
#'
#' `obt_staging_clean()` empties the staging folder. A finished run removes its
#' own folder; this clears what an interrupted run left behind. Run it when no
#' run is going.
#'
#' @param x An `obt_platform` record.
#' @param ... Passed nowhere. Present for the `print()` method.
#'
#' @return
#' `obt_platform()` answers an object of class `obt_platform`, with `os`,
#' `supported`, `staging`, `excel_version`, `excel_proven` and `app`.
#'
#' `obt_staging_clean()` answers the paths it removed, invisibly.
#'
#' @name obt_platform
#'
#' @examples
#' obt_platform()
NULL

#' @rdname obt_platform
#' @export
obt_platform <- function() {
  os <- os_name()
  root <- staging_root()
  version <- if (identical(os, "macos")) excel_version() else NA_character_

  new_platform(
    os = os,
    supported = os %in% SUPPORTED_OS,
    staging = if (staging_available(root)) root else NA_character_,
    excel_version = version,
    excel_proven = identical(version, EXCEL_VERSION_PROVEN),
    app = session_app()
  )
}

#' @rdname obt_platform
#' @export
obt_staging_clean <- function() {
  root <- staging_root()

  if (!staging_available(root)) {
    return(invisible(character()))
  }

  removed <- stage_sweep(root, hours = 0)

  if (length(list.files(root, all.files = TRUE, no.. = TRUE)) == 0) {
    unlink(root, recursive = TRUE, force = TRUE)
  }

  invisible(removed)
}

#' @rdname obt_platform
#' @export
print.obt_platform <- function(x, ...) {
  cli::cli_h1("This machine")

  cli::cli_dl(c(
    "System" = cli_escape(x$os),
    "Can run a recipe" = if (isTRUE(x$supported)) "yes" else "no",
    "Staging folder" = cli_escape(staging_label(x)),
    "Excel version" = cli_escape(excel_version_label(x)),
    "R session runs in" = cli_escape(x$app)
  ))

  invisible(x)
}

#' Build the record of what this machine can do
#'
#' @param os The operating system, as `os_name()` names it.
#' @param supported Whether a run can happen on it.
#' @param staging The staging folder, or `NA` when the run works in the
#'   user's own folder.
#' @param excel_version The version of Excel that was found.
#' @param excel_proven Whether that version is the one this was proven on.
#' @param app The app running the R session.
#'
#' @return An object of class `obt_platform`.
#' @noRd
new_platform <- function(
  os,
  supported,
  staging,
  excel_version,
  excel_proven,
  app
) {
  structure(
    list(
      os = os,
      supported = supported,
      staging = staging,
      excel_version = excel_version,
      excel_proven = excel_proven,
      app = app
    ),
    class = "obt_platform"
  )
}

#' What the staging line of a printed record says
#'
#' @param x The platform record.
#'
#' @return A single string.
#' @noRd
staging_label <- function(x) {
  if (!is.na(x$staging)) {
    return(x$staging)
  }

  if (identical(x$os, "macos")) {
    return("your own folder. Full Disk Access opens Excel's own folder")
  }

  "your own folder"
}

#' What the Excel line of a printed record says
#'
#' @param x The platform record.
#'
#' @return A single string.
#' @noRd
excel_version_label <- function(x) {
  if (!identical(x$os, "macos")) {
    return("read on macOS only")
  }

  if (is.na(x$excel_version)) {
    return("could not be read")
  }

  if (isTRUE(x$excel_proven)) {
    return(x$excel_version)
  }

  paste0(x$excel_version, ". ", EXCEL_VERSION_PROVEN, " is the proven one")
}

#' The lines a run log carries about this machine
#'
#' A crash with no version in the log is a crash nobody can place, so the
#' version goes in whether it was the proven one or not.
#'
#' @param platform The platform record.
#'
#' @return A character vector, one line each.
#' @noRd
platform_log_lines <- function(platform) {
  staging <- if (is.na(platform$staging)) {
    "none. The run works in the working folder"
  } else {
    platform$staging
  }

  excel <- if (is.na(platform$excel_version)) {
    "could not be read"
  } else {
    platform$excel_version
  }

  c(
    paste0("System: ", platform$os),
    paste0("Excel version: ", excel),
    paste0("Excel version proven: ", EXCEL_VERSION_PROVEN),
    paste0("Staging folder: ", staging),
    paste0("R session app: ", platform$app)
  )
}

#' Read this machine and say what a user has to act on
#'
#' The one thing that stops a run is the operating system. A missing staging
#' folder and an unproven version of Excel are said out loud and the run
#' carries on.
#'
#' @param obtops The recipe.
#' @param call The environment to blame in the error.
#'
#' @return The platform record.
#' @noRd
platform_guard <- function(obtops, call = rlang::caller_env()) {
  check_obt(obtops, call = call)
  check_supported_os(call = call)

  platform <- obt_platform()

  warn_excel_version(platform)

  if (identical(platform$os, "macos") && is.na(platform$staging)) {
    warn_no_staging(absolute_path(obtops$folder), app = platform$app)
  }

  platform
}

#' Which operating system this is
#'
#' @return One of `"macos"`, `"windows"`, or the system's own name in lower
#'   case.
#' @noRd
os_name <- function() {
  if (identical(.Platform$OS.type, "windows")) {
    return("windows")
  }

  sysname <- Sys.info()[["sysname"]]

  if (identical(sysname, "Darwin")) {
    return("macos")
  }

  tolower(sysname)
}

#' Whether this is macOS
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_macos <- function() {
  identical(os_name(), "macos")
}

#' Whether this is Windows
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_windows <- function() {
  identical(os_name(), "windows")
}

#' Fail on a system that cannot open Excel
#'
#' @param os The operating system.
#' @param call The environment to blame in the error.
#'
#' @return The system, invisibly.
#' @noRd
check_supported_os <- function(os = os_name(), call = rlang::caller_env()) {
  if (os %in% SUPPORTED_OS) {
    return(invisible(os))
  }

  cli::cli_abort(
    c(
      "Running a recipe opens Excel, and that takes macOS or Windows.",
      "x" = "This R session runs on {.val {os}}.",
      "i" = "A recipe builds and prints on any system."
    ),
    call = call
  )
}

#' Excel's own folder, as this session sees it
#'
#' @return An absolute path.
#' @noRd
container_documents <- function() {
  path.expand(EXCEL_CONTAINER_DOCUMENTS)
}

#' The staging folder the package owns
#'
#' @return An absolute path on macOS, `NA` anywhere else.
#' @noRd
staging_root <- function() {
  if (!is_macos()) {
    return(NA_character_)
  }

  file.path(container_documents(), STAGING_FOLDER)
}

#' Whether the staging folder can be written
#'
#' The folder is created and a small file is written in it, because macOS
#' answers a folder it will refuse to open as present. The file is removed
#' again; the folder stays, and it is where a run stages itself.
#'
#' @param root The staging folder.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
staging_available <- function(root = staging_root()) {
  if (length(root) != 1 || is.na(root) || !dir.exists(dirname(root))) {
    return(FALSE)
  }

  probe <- file.path(root, STAGING_PROBE)

  reached <- tryCatch(
    {
      dir.create(root, recursive = TRUE, showWarnings = FALSE)
      writeLines("ok", probe)
      identical(readLines(probe, warn = FALSE), "ok")
    },
    error = function(cnd) FALSE,
    warning = function(cnd) FALSE
  )

  unlink(probe, force = TRUE)

  isTRUE(reached)
}

#' Say once that the run works in the user's own folder
#'
#' The grant is on the app running the R session, so the message names that
#' app and where to switch it on.
#'
#' @param folder The folder the run works in.
#' @param app The app running the R session.
#'
#' @return `TRUE` when the warning was said, `FALSE` when it was said
#'   already. Invisibly.
#' @noRd
warn_no_staging <- function(folder, app = session_app()) {
  if (isTRUE(platform_state$warned_no_staging)) {
    return(invisible(FALSE))
  }

  platform_state$warned_no_staging <- TRUE

  cli::cli_warn(c(
    "Excel for Mac opens a file inside its own folder without asking.",
    "i" = "Full Disk Access is what lets this R session put your files
           there:",
    "*" = "System Settings > Privacy & Security > Full Disk Access",
    "*" = "add {app}, then quit it and open it again.",
    "i" = "This run works in {.file {folder}}, and Excel asks about each
           file it opens."
  ))

  invisible(TRUE)
}

#' Say when the version of Excel is one this was never proven on
#'
#' @param platform The platform record.
#'
#' @return `TRUE` when the warning was said, `FALSE` otherwise. Invisibly.
#' @noRd
warn_excel_version <- function(platform) {
  if (!identical(platform$os, "macos") || isTRUE(platform$excel_proven)) {
    return(invisible(FALSE))
  }

  proven <- EXCEL_VERSION_PROVEN

  if (is.na(platform$excel_version)) {
    cli::cli_warn(c(
      "The version of Excel could not be read.",
      "i" = "{.val {proven}} is the version this package was proven on.",
      "i" = "The run carries on, and the log records what was found."
    ))

    return(invisible(TRUE))
  }

  found <- platform$excel_version

  cli::cli_warn(c(
    "Excel {.val {found}} is installed here.",
    "i" = "{.val {proven}} is the version this package was proven on.",
    "i" = "A few versions crash Excel while the analyses are built. The run
           carries on, and the log records the version."
  ))

  invisible(TRUE)
}

#' Where Excel for Mac is installed
#'
#' @return An absolute path, or `NA` when Excel cannot be found.
#' @noRd
excel_app_path <- function() {
  if (dir.exists(EXCEL_APP_PATH)) {
    return(EXCEL_APP_PATH)
  }

  query <- paste0("kMDItemCFBundleIdentifier == '", EXCEL_BUNDLE_ID, "'")
  found <- run_quietly("mdfind", shQuote(query))
  found <- found[nzchar(found)]
  found <- found[dir.exists(found)]

  if (length(found) == 0) {
    return(NA_character_)
  }

  found[[1]]
}

#' The version of Excel that is installed
#'
#' @param app The path of the Excel application bundle.
#'
#' @return The version as a string, or `NA` when it cannot be read.
#' @noRd
excel_version <- function(app = excel_app_path()) {
  if (length(app) != 1 || is.na(app)) {
    return(NA_character_)
  }

  plist <- file.path(app, "Contents", "Info.plist")

  if (!file.exists(plist)) {
    return(NA_character_)
  }

  read <- run_quietly(
    "/usr/libexec/PlistBuddy",
    c("-c", shQuote("Print :CFBundleShortVersionString"), shQuote(plist))
  )

  read <- trimws(read)
  read <- read[nzchar(read)]

  if (length(read) == 0) {
    return(NA_character_)
  }

  read[[1]]
}

#' The app running this R session
#'
#' Full Disk Access is granted to an app, so the message that asks for it has
#' to name the right one. The process tree is walked first, because it answers
#' for any app; the environment variables are the fallback.
#'
#' @return A single string. `UNKNOWN_APP` when nothing answers.
#' @noRd
session_app <- function() {
  from_tree <- app_from_process_tree()

  if (!is.na(from_tree)) {
    return(from_tree)
  }

  from_env <- app_from_environment()

  if (!is.na(from_env)) {
    return(from_env)
  }

  UNKNOWN_APP
}

#' Walk up the process tree for an application bundle
#'
#' @param pid The process to start from.
#'
#' @return The name of the bundle, or `NA`.
#' @noRd
app_from_process_tree <- function(pid = Sys.getpid()) {
  for (step in seq_len(PROCESS_WALK_LIMIT)) {
    row <- run_quietly("ps", c("-o", "ppid=,comm=", "-p", pid))
    row <- row[nzchar(trimws(row))]

    if (length(row) == 0) {
      return(NA_character_)
    }

    app <- app_bundle_name(sub("^\\s*[0-9]+\\s+", "", row[[1]]))

    if (!is.na(app)) {
      return(app)
    }

    parent <- suppressWarnings(
      as.integer(sub("^\\s*([0-9]+).*$", "\\1", row[[1]]))
    )

    if (length(parent) != 1 || is.na(parent) || parent <= 1L) {
      return(NA_character_)
    }

    pid <- parent
  }

  NA_character_
}

#' The application bundle a command sits inside
#'
#' A helper process lives inside a bundle of its own, inside the app's bundle.
#' The first `.app` of the path is the outer one, and that is the app a user
#' adds to Full Disk Access.
#'
#' @param command The path of the running command.
#'
#' @return The bundle name without its extension, or `NA`.
#' @noRd
app_bundle_name <- function(command) {
  found <- regmatches(command, regexpr("/[^/]+\\.app/", command))

  if (length(found) == 0) {
    return(NA_character_)
  }

  sub("\\.app/$", "", sub("^/", "", found[[1]]))
}

#' The app the environment names
#'
#' @return The app name, or `NA`.
#' @noRd
app_from_environment <- function() {
  for (hint in SESSION_APP_HINTS) {
    if (identical(Sys.getenv(hint$name), hint$value)) {
      return(hint$app)
    }
  }

  NA_character_
}

#' Run a command and answer what it printed
#'
#' Everything read here is something the package can carry on without, so a
#' command that is missing or fails answers nothing. `system2()` is what runs
#' it, on both systems.
#'
#' @param command The command to run.
#' @param args Its arguments, already quoted.
#'
#' @return A character vector of the lines it printed, empty on failure.
#' @noRd
run_quietly <- function(command, args = character()) {
  reachable <- nzchar(Sys.which(command)[[1]]) || file.exists(command)

  if (!reachable) {
    return(character())
  }

  answer <- tryCatch(
    suppressWarnings(system2(command, args, stdout = TRUE, stderr = FALSE)),
    error = function(cnd) character()
  )

  if (!is.character(answer)) {
    return(character())
  }

  answer
}

#' Open the staging area for one run
#'
#' A run gets a folder of its own inside the staging folder, and the layout of
#' the working folder is mirrored inside it, so a file keeps the place it had.
#' Where the staging folder cannot be reached the run works in the user's own
#' folder and every staging call below becomes a no-op.
#'
#' @param obtops The recipe.
#' @param platform The platform record.
#' @param when The time the run started.
#' @param call The environment to blame in the error.
#'
#' @return An object of class `obt_stage`.
#' @noRd
stage_open <- function(
  obtops,
  platform = platform_guard(obtops),
  when = Sys.time(),
  call = rlang::caller_env()
) {
  folder <- absolute_path(obtops$folder)

  if (length(platform$staging) != 1 || is.na(platform$staging)) {
    return(new_stage(folder = folder, root = folder, staged = FALSE))
  }

  stage_sweep(platform$staging, now = when)

  root <- file.path(
    platform$staging,
    paste0(
      STAGE_RUN_LEAD,
      format(when, LOG_STAMP_FORMAT),
      "-",
      Sys.getpid()
    )
  )

  ensure_folder(root, call = call)

  new_stage(folder = folder, root = root, staged = TRUE)
}

#' Build the staging record of one run
#'
#' @param folder The working folder.
#' @param root Where the run stages itself.
#' @param staged Whether the run stages at all.
#'
#' @return An object of class `obt_stage`.
#' @noRd
new_stage <- function(folder, root, staged) {
  # Both are written in one shape here. `stage_path()` decides whether a file
  # sits under the working folder by comparing the text of two paths, and a
  # temporary folder carries a doubled separator often enough to matter.
  structure(
    list(
      folder = absolute_path(folder),
      root = absolute_path(root),
      staged = staged
    ),
    class = "obt_stage"
  )
}

#' Where a file sits while the run works on it
#'
#' A file under the working folder keeps its place inside the run folder. A
#' file from anywhere else lands in one folder of its own, under its own name.
#'
#' @param stage The staging record.
#' @param path The file.
#'
#' @return An absolute path.
#' @noRd
stage_path <- function(stage, path) {
  full <- absolute_path(path)

  if (!isTRUE(stage$staged)) {
    return(full)
  }

  lead <- paste0(stage$folder, "/")

  if (startsWith(full, lead)) {
    return(file.path(stage$root, substring(full, nchar(lead) + 1L)))
  }

  file.path(stage$root, STAGE_OUTSIDE, basename(full))
}

#' Copy a file into the staging area
#'
#' @param stage The staging record.
#' @param path The file the run needs.
#' @param call The environment to blame in the error.
#'
#' @return The path Excel is given.
#' @noRd
stage_in <- function(stage, path, call = rlang::caller_env()) {
  full <- absolute_path(path)

  if (!file.exists(full)) {
    cli::cli_abort(
      c(
        "{.file {full}} is not there.",
        "i" = "The run needs it before Excel opens."
      ),
      call = call
    )
  }

  target <- stage_path(stage, full)

  if (identical(target, full)) {
    return(full)
  }

  ensure_folder(dirname(target), call = call)
  copy_file(full, target, call = call)

  target
}

#' Move a file the run produced back out of the staging area
#'
#' @param stage The staging record.
#' @param path Where the file belongs in the working folder.
#' @param required Whether a missing file stops the run.
#' @param call The environment to blame in the error.
#'
#' @return The path in the working folder, or `NA` when the file is missing
#'   and `required` is `FALSE`.
#' @noRd
stage_out <- function(
  stage,
  path,
  required = TRUE,
  call = rlang::caller_env()
) {
  target <- absolute_path(path)
  source <- stage_path(stage, target)

  if (!file.exists(source)) {
    if (isTRUE(required)) {
      cli::cli_abort(
        c(
          "{.file {target}} was not written.",
          "i" = "Nothing was left at {.file {source}}."
        ),
        call = call
      )
    }

    return(NA_character_)
  }

  if (identical(source, target)) {
    return(target)
  }

  ensure_folder(dirname(target), call = call)
  move_file(source, target, call = call)

  target
}

#' Copy a file the run wrote back to the working folder, and leave the stage
#' as it is
#'
#' A run that stops leaves the file that says why inside the stage, and the
#' stage goes when the run closes. This copies such a file out first, and the
#' staged copy stays for the close to sweep. It is called on the way out of a
#' failed run, so a copy that cannot be made answers `NA` and says nothing:
#' the failure of the run itself is the one worth reporting.
#'
#' @param stage The staging record.
#' @param path Where the file belongs in the working folder.
#'
#' @return The path in the working folder, or `NA` when there is nothing to
#'   copy or the copy could not be made.
#' @noRd
stage_keep <- function(stage, path) {
  target <- absolute_path(path)
  source <- stage_path(stage, target)

  if (!file.exists(source)) {
    return(NA_character_)
  }

  if (identical(source, target)) {
    return(target)
  }

  kept <- tryCatch(
    {
      ensure_folder(dirname(target))
      isTRUE(suppressWarnings(
        file.copy(source, target, overwrite = TRUE, copy.date = TRUE)
      ))
    },
    error = function(cnd) FALSE
  )

  if (!kept) {
    return(NA_character_)
  }

  target
}

#' Close the staging area of one run
#'
#' Everything the run left inside is moved back to the working folder first,
#' so a file Excel wrote or changed is never dropped. Then the run folder is
#' removed, and Excel's own folder holds nothing of ours.
#'
#' @param stage The staging record.
#' @param rescue Whether what is left is moved back before the folder goes.
#' @param call The environment to blame in the error.
#'
#' @return The staging record, invisibly.
#' @noRd
stage_close <- function(stage, rescue = TRUE, call = rlang::caller_env()) {
  if (!isTRUE(stage$staged)) {
    return(invisible(stage))
  }

  if (isTRUE(rescue)) {
    stage_rescue(stage, call = call)
  }

  unlink(stage$root, recursive = TRUE, force = TRUE)

  invisible(stage)
}

#' Move what is left in the staging area back to the working folder
#'
#' The files staged from outside the working folder stay where they are: they
#' are copies of the user's own inputs and they have no place to go back to.
#'
#' @param stage The staging record.
#' @param call The environment to blame in the error.
#'
#' @return The paths that were moved, invisibly.
#' @noRd
stage_rescue <- function(stage, call = rlang::caller_env()) {
  left <- list.files(stage$root, recursive = TRUE, all.files = TRUE)
  left <- left[!startsWith(left, paste0(STAGE_OUTSIDE, "/"))]

  moved <- character()

  for (entry in left) {
    source <- file.path(stage$root, entry)
    target <- file.path(stage$folder, entry)

    ensure_folder(dirname(target), call = call)
    move_file(source, target, call = call)

    moved <- c(moved, target)
  }

  invisible(moved)
}

#' Remove the run folders an earlier run left behind
#'
#' A run that was interrupted leaves its folder inside Excel's own folder. A
#' later run sweeps it once it is old enough to belong to nobody.
#'
#' @param root The staging folder.
#' @param hours How old a run folder has to be before it goes.
#' @param now The time to measure against.
#'
#' @return The paths that were removed, invisibly.
#' @noRd
stage_sweep <- function(root, hours = STAGE_STALE_HOURS, now = Sys.time()) {
  if (length(root) != 1 || is.na(root) || !dir.exists(root)) {
    return(invisible(character()))
  }

  runs <- list.files(
    root,
    pattern = paste0("^", STAGE_RUN_LEAD),
    full.names = TRUE
  )
  runs <- runs[dir.exists(runs)]

  if (length(runs) == 0) {
    return(invisible(character()))
  }

  age <- as.numeric(difftime(now, file.mtime(runs), units = "hours"))
  stale <- runs[!is.na(age) & age >= hours]

  unlink(stale, recursive = TRUE, force = TRUE)

  invisible(stale)
}

#' Copy one file, and say so when it fails
#'
#' @param from The file.
#' @param to Where the copy goes.
#' @param call The environment to blame in the error.
#'
#' @return The destination, invisibly.
#' @noRd
copy_file <- function(from, to, call = rlang::caller_env()) {
  copied <- suppressWarnings(
    file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  )

  if (!isTRUE(copied)) {
    cli::cli_abort(
      c(
        "{.file {from}} could not be copied.",
        "x" = "The copy would have gone to {.file {to}}."
      ),
      call = call
    )
  }

  invisible(to)
}

#' Move one file, across volumes when it has to
#'
#' `file.rename()` fails between two volumes, and Excel's own folder can sit
#' on one of its own, so a failed rename falls back to a copy and a delete.
#'
#' @param from The file.
#' @param to Where it goes.
#' @param call The environment to blame in the error.
#'
#' @return The destination, invisibly.
#' @noRd
move_file <- function(from, to, call = rlang::caller_env()) {
  if (file.exists(to)) {
    unlink(to, force = TRUE)
  }

  moved <- suppressWarnings(file.rename(from, to))

  if (isTRUE(moved)) {
    return(invisible(to))
  }

  copy_file(from, to, call = call)
  unlink(from, force = TRUE)

  invisible(to)
}
