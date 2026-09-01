# The OutbreakTools files, downloaded.
#
# A recipe names a channel. The run downloads that channel's zip into
# `obt/<channel>/` under the working folder, unzips it there, and answers the
# paths of the workbooks it holds. The names of those workbooks are read from
# the zip, because a name carries the channel and the version of the build.
#
# The download is a network step and it runs at commit time, the way every
# other operation does.

# The two channels, where each is downloaded from, and the folder of the
# layout it lands in. `main` is the stable release; `dev` is the development
# build.
OBT_CHANNELS <- list(
  main = list(
    url = paste0(
      "https://github.com/epicentre-msf/outbreak-tools/releases/",
      "latest/download/OBT-main-latest.zip"
    ),
    entry = "obt_main"
  ),
  dev = list(
    url = paste0(
      "https://github.com/epicentre-msf/outbreak-tools/releases/",
      "download/dev-latest/OBT-dev-latest.zip"
    ),
    entry = "obt_dev"
  )
)

# The workbooks a channel zip holds, and how each one is known by its file
# name. The rest of a name carries the channel and the version, so only the
# lead is matched and the whole name comes from the zip.
#
# A release build writes four files. The master setup arrived after the three
# the layout names, so a zip without one still resolves.
CHANNEL_ROLES <- list(
  designer = list(
    label = "the designer",
    pattern = "^designer[-_]",
    required = TRUE
  ),
  setup = list(
    label = "the empty setup",
    pattern = "^setup[-_]",
    required = TRUE
  ),
  msetup = list(
    label = "the master setup",
    pattern = "^msetup[-_]",
    required = FALSE
  ),
  ribbon = list(
    label = "the ribbon template",
    pattern = "^_?ribbontemplate[-_]",
    required = TRUE
  )
)

# The extension every workbook of a channel zip carries.
CHANNEL_EXTENSION <- "xlsb"

# The oldest release this package can drive. A release below it carries no
# entry point a script can call, so a run against one would sit on a message
# box nobody can answer. Raise this one name when the entry points move.
OBT_MINIMUM_VERSION <- "2.0.0"

# What the development build carries in place of a version. It is published
# from the current source whenever the binaries change and carries no version
# of its own, so the dev channel is taken as current.
CHANNEL_MOVING_VERSION <- "latest"

# The shape of a version a release carries: two or three numbers, dots
# between them. A file name from before the scheme carries a date, which this
# turns down.
CHANNEL_VERSION_PATTERN <- "^[0-9]+(\\.[0-9]+){1,2}$"

#' Look up a channel
#'
#' @param type The channel name, already checked.
#'
#' @return The entry of `OBT_CHANNELS` for that channel.
#' @noRd
channel_spec <- function(type) {
  OBT_CHANNELS[[type]]
}

#' Check the channel a verb was given
#'
#' @param type The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The channel name in lower case.
#' @noRd
check_channel <- function(
  type,
  arg = rlang::caller_arg(type),
  call = rlang::caller_env()
) {
  # The label is read before `type` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  type <- check_string(type, arg = arg, call = call)
  chosen <- tolower(type)
  known <- names(OBT_CHANNELS)

  if (!chosen %in% known) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be one of {.val {known}}.",
        "x" = "You supplied {.val {type}}."
      ),
      call = call
    )
  }

  chosen
}

#' Download a channel and unzip it under the working folder
#'
#' The zip lands in `obt/<channel>/` and its workbooks are unzipped beside it.
#' A zip already there is used again; `force` downloads it once more and
#' writes every workbook over the one that is there.
#'
#' @param obtops The recipe.
#' @param type The channel.
#' @param force Whether the zip is downloaded again when it is already there.
#' @param call The environment to blame in the error.
#'
#' @return A named list: the `channel`, the `version` it carries, the `zip`,
#'   whether the zip was `reused`, and one absolute path per workbook of
#'   `CHANNEL_ROLES`. A workbook the zip does not hold answers `NA`.
#' @noRd
designer_fetch <- function(
  obtops,
  type = "main",
  force = FALSE,
  call = rlang::caller_env()
) {
  check_obt(obtops, call = call)
  type <- check_channel(type, call = call)
  check_flag(force, call = call)

  folder <- channel_path(obtops, type)
  ensure_folder(folder, call = call)

  zip <- channel_zip_path(obtops, type)
  reused <- file.exists(zip) && !isTRUE(force)

  if (!reused) {
    download_zip(channel_spec(type)$url, zip, call = call)
  }

  entries <- zip_entries(zip, call = call)
  files <- channel_files(entries, zip = zip, call = call)

  # The version is read before anything is unzipped, so a release this
  # package cannot drive leaves no workbook in the folder.
  version <- channel_version(files$designer)
  check_channel_version(version, type, zip = zip, call = call)

  paths <- unzip_channel(zip, folder, files, force = force, call = call)

  c(list(channel = type, version = version, zip = zip, reused = reused), paths)
}

#' Read the version a channel carries
#'
#' The release build writes the version into every file name it packs, after
#' the channel. The development build writes `latest` there.
#'
#' @param designer The name of the designer inside the zip.
#'
#' @return The version as it is written in the name, or `NA` when the name
#'   carries none.
#' @noRd
channel_version <- function(designer) {
  if (length(designer) != 1 || is.na(designer)) {
    return(NA_character_)
  }

  parts <- strsplit(
    tools::file_path_sans_ext(basename(designer)),
    "-",
    fixed = TRUE
  )[[1]]

  if (length(parts) < 2) {
    return(NA_character_)
  }

  paste(parts[-1], collapse = "-")
}

#' Fail when a channel is older than this package can drive
#'
#' Everything this package asks a workbook to do arrived in
#' `OBT_MINIMUM_VERSION`. A release below it opens, shows a message box, and
#' waits for a click that a script cannot give.
#'
#' The development build is taken as current. It is published from the source
#' as it stands and carries `latest` in place of a version, so a number to
#' compare never reaches here.
#'
#' @param version The version the channel carries.
#' @param type The channel.
#' @param zip The zip file, for the error message.
#' @param call The environment to blame in the error.
#'
#' @return The version, invisibly.
#' @noRd
check_channel_version <- function(
  version,
  type,
  zip = NULL,
  call = rlang::caller_env()
) {
  known <- !is.na(version)

  moving <- identical(type, "dev") &&
    known &&
    identical(tolower(version), CHANNEL_MOVING_VERSION)

  if (moving) {
    return(invisible(version))
  }

  if (known && grepl(CHANNEL_VERSION_PATTERN, version)) {
    if (numeric_version(version) >= numeric_version(OBT_MINIMUM_VERSION)) {
      return(invisible(version))
    }
  }

  said <- if (known) {
    "Its designer carries {.val {version}}."
  } else {
    "Its designer carries no version in its name."
  }

  where <- if (is.null(zip)) {
    character()
  } else {
    c("i" = "The zip it came from is at {.file {zip}}.")
  }

  cli::cli_abort(
    c(
      "The {.val {type}} channel is older than this package can drive.",
      "x" = said,
      "i" = "Version {.val {OBT_MINIMUM_VERSION}} or above is needed. The
             releases below it carry no entry point a script can call, so a
             run would stop on a message box.",
      where
    ),
    call = call
  )
}

#' Download one file
#'
#' The download goes to a temporary file and is moved into place once it has
#' arrived, so a download that broke halfway never sits in the folder waiting
#' to be used again as a whole zip.
#'
#' @param url Where the file is downloaded from.
#' @param path Where it is written.
#' @param call The environment to blame in the error.
#'
#' @return The path, invisibly.
#' @noRd
download_zip <- function(url, path, call = rlang::caller_env()) {
  ensure_folder(dirname(path), call = call)

  partial <- tempfile(fileext = ".zip")
  on.exit(unlink(partial, force = TRUE), add = TRUE)

  status <- tryCatch(
    utils::download.file(url, destfile = partial, mode = "wb", quiet = TRUE),
    error = function(cnd) cnd,
    warning = function(cnd) cnd
  )

  arrived <- isTRUE(
    is.numeric(status) &&
      identical(as.integer(status), 0L) &&
      file.exists(partial) &&
      file.size(partial) > 0
  )

  if (!arrived) {
    cli::cli_abort(
      c(
        "The OutbreakTools files could not be downloaded.",
        "x" = "The download was {.url {url}}.",
        "i" = "Check the network, then run the recipe again."
      ),
      call = call
    )
  }

  move_file(partial, path, call = call)

  invisible(path)
}

#' The files a zip holds
#'
#' @param zip The zip file.
#' @param call The environment to blame in the error.
#'
#' @return The entries of the zip, with the folders left out.
#' @noRd
zip_entries <- function(zip, call = rlang::caller_env()) {
  listing <- tryCatch(
    utils::unzip(zip, list = TRUE),
    error = function(cnd) cnd,
    warning = function(cnd) cnd
  )

  if (!is.data.frame(listing) || is.null(listing$Name)) {
    cli::cli_abort(
      c(
        "{.file {zip}} could not be read as a zip file.",
        "i" = "Add the designer again with {.code force = TRUE} to download
               it once more."
      ),
      call = call
    )
  }

  entries <- listing$Name
  entries[!grepl("/$", entries)]
}

#' Read the workbook of every role out of the entries of a zip
#'
#' @param entries The entries of the zip.
#' @param zip The zip file, for the error message.
#' @param call The environment to blame in the error.
#'
#' @return A named list, one entry per role of `CHANNEL_ROLES`, holding the
#'   name of the entry inside the zip. A role the zip does not hold and does
#'   not need answers `NA`.
#' @noRd
channel_files <- function(entries, zip = NULL, call = rlang::caller_env()) {
  workbooks <- entries[
    tolower(tools::file_ext(basename(entries))) == CHANNEL_EXTENSION
  ]
  named <- basename(workbooks)
  from <- if (is.null(zip)) "The zip" else "{.file {zip}}"

  found <- list()

  for (role in names(CHANNEL_ROLES)) {
    spec <- CHANNEL_ROLES[[role]]
    hits <- workbooks[grepl(spec$pattern, named, ignore.case = TRUE)]

    if (length(hits) == 1) {
      found[[role]] <- hits
      next
    }

    if (length(hits) > 1) {
      cli::cli_abort(
        c(
          paste(from, "holds {length(hits)} files for {spec$label}."),
          "x" = "They are {.file {basename(hits)}}.",
          "i" = "A channel zip holds one workbook of each kind."
        ),
        call = call
      )
    }

    if (isTRUE(spec$required)) {
      held <- if (length(named) == 0) {
        paste(from, "holds no workbook at all.")
      } else {
        paste(from, "holds {.file {named}}.")
      }

      cli::cli_abort(
        c(
          paste(from, "holds no file for {spec$label}."),
          "x" = held
        ),
        call = call
      )
    }

    found[role] <- list(NA_character_)
  }

  found
}

#' Unzip the workbooks of a channel into its folder
#'
#' A workbook already unzipped is left where it is, so a second run costs
#' nothing. `force` writes every one of them again.
#'
#' @param zip The zip file.
#' @param folder The folder of the channel.
#' @param files The entries of the zip, one per role, as `channel_files()`
#'   answers them.
#' @param force Whether a workbook already there is written again.
#' @param call The environment to blame in the error.
#'
#' @return A named list, one absolute path per role.
#' @noRd
unzip_channel <- function(
  zip,
  folder,
  files,
  force = FALSE,
  call = rlang::caller_env()
) {
  paths <- list()

  for (role in names(files)) {
    entry <- files[[role]]

    if (is.na(entry)) {
      paths[role] <- list(NA_character_)
      next
    }

    target <- file.path(folder, basename(entry))

    if (isTRUE(force) || !file.exists(target)) {
      extract_entry(zip, entry, folder, call = call)
    }

    paths[[role]] <- absolute_path(target)
  }

  paths
}

#' Unzip one entry into a folder
#'
#' The entry lands directly in the folder, whatever it was stored under
#' inside the zip, so the layout of the working folder holds.
#'
#' @param zip The zip file.
#' @param entry The entry inside it.
#' @param folder Where the entry is written.
#' @param call The environment to blame in the error.
#'
#' @return The path of the file, invisibly.
#' @noRd
extract_entry <- function(zip, entry, folder, call = rlang::caller_env()) {
  target <- file.path(folder, basename(entry))

  tryCatch(
    utils::unzip(
      zip,
      files = entry,
      exdir = folder,
      junkpaths = TRUE,
      overwrite = TRUE
    ),
    error = function(cnd) cnd,
    warning = function(cnd) cnd
  )

  if (!file.exists(target)) {
    cli::cli_abort(
      c(
        "{.file {basename(entry)}} could not be unzipped.",
        "x" = "It comes from {.file {zip}}.",
        "i" = "It would have gone to {.file {folder}}."
      ),
      call = call
    )
  }

  invisible(target)
}
