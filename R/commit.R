# Running a recipe.
#
# A verb records. Reads the machine,
# builds the working folder, opens the staging area, and runs the operations
# in the order they were added. Each step is timed and the outcome is written.
#
# The recipe is checked before running, and committed only if the all the checks
# are green.
#
# Every run writes one text log under `log/`, whether it finished or stopped.
# The log carries the version of Excel the machine holds, because a crash with
# no version in the log is a crash nobody can place.

# What an operation records once it has run.
RUN_OK <- "ok"
RUN_FAILED <- "failed"

# What a run that never reached its first operation records.
RUN_NONE <- "none"

# The line a log opens with.
LOG_TITLE <- "OutbreakTools run"

# How a time is written in a log.
LOG_TIME_FORMAT <- "%Y-%m-%d %H:%M:%S"

#' Run the recipe
#'
#' @description
#' Runs the operations of a recipe, in the order the verbs recorded them. Creates the working
#' folder, downloads what the recipe asked for, and opens Excel where an
#' operation needs it.
#'
#' The run stops at the first operation that fails with a message.
#' Every run writes one text log under `log/`
#' in the working folder, whether it finished or stopped.
#'
#' Running a recipe takes macOS or Windows. An operation whose OutbreakTools entry point has not
#' shipped yet is turned down before anything is written. Use [obt_describe()] to see which
#' operations of a recipe are waiting and what each one needs.
#'
#' @param obtops An `obt` recipe.
#' @param verbose Whether each operation is printed as it starts and as it
#'   ends.
#'
#' @return The recipe, invisibly, carrying what the run did: the outcome of
#'   every operation, how long it took, and the files it produced. The paths
#'   are relative to the working folder, so the folder can be moved.
#'
#' @seealso [obt_describe()] to read a recipe before running it,
#'   [obt_platform()] for what this machine can do.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' obt(folder = "measles") |>
#'   obt_designer_add(type = "dev") |>
#'   obt_designer_languages(dict = "English", form = "ENG") |>
#'   obt_commit()
#' }
obt_commit <- function(obtops, verbose = TRUE) {
  check_obt(obtops)
  check_flag(verbose)

  # The three refusals come before anything is written: the system that
  # cannot open Excel, the recipe with nothing in it, and the operation whose
  # entry point has not shipped.
  check_supported_os()
  check_run_ready(obtops)

  platform <- platform_guard(obtops)
  started <- Sys.time()
  log <- log_path(obtops, when = started)

  create_obt_folder(obtops)

  stage <- stage_open(obtops, platform = platform, when = started)
  on.exit(stage_close(stage), add = TRUE)

  run <- run_operations(obtops, stage = stage, verbose = verbose)

  write_run_log(
    obtops,
    records = run$records,
    platform = platform,
    started = started,
    finished = Sys.time(),
    failure = run$failure,
    path = log
  )

  if (!is.null(run$failure)) {
    stop_run(run$failure, total = length(obtops$operations), log = log)
  }

  finished <- Sys.time()

  if (isTRUE(verbose)) {
    say_run_finished(
      run$records,
      seconds = elapsed(started, finished),
      log = log
    )
  }

  invisible(record_run(
    obtops,
    records = run$records,
    platform = platform,
    started = started,
    finished = finished,
    log = log
  ))
}

#' Run every operation of a recipe in order
#'
#' The loop stops at the first failure.
#'
#' @param obtops The recipe.
#' @param stage The staging record of the run.
#' @param verbose Whether each operation is printed.
#'
#' @return A list with `records`, one per operation that was reached, and
#'   `failure`, which is `NULL` when every operation finished.
#' @noRd
run_operations <- function(obtops, stage, verbose = TRUE) {
  total <- length(obtops$operations)
  records <- list()
  state <- list()

  for (index in seq_len(total)) {
    operation <- obtops$operations[[index]]
    label <- operation_spec(operation$type)$label

    if (isTRUE(verbose)) {
      cli::cli_alert_info("{index}/{total} {label}...")
    }

    started <- Sys.time()

    answer <- tryCatch(
      run_operation(obtops, operation, stage = stage, state = state),
      error = function(cnd) cnd
    )

    finished <- Sys.time()

    if (rlang::is_condition(answer)) {
      records[[index]] <- new_run_record(
        operation,
        outcome = RUN_FAILED,
        started = started,
        finished = finished
      )

      return(list(
        records = records,
        failure = list(index = index, label = label, condition = answer)
      ))
    }

    state <- utils::modifyList(state, answer$state)

    records[[index]] <- new_run_record(
      operation,
      outcome = RUN_OK,
      started = started,
      finished = finished,
      produced = relative_paths(answer$produced, folder = obtops$folder)
    )

    if (isTRUE(verbose)) {
      cli::cli_alert_success(
        "{index}/{total} {label} ({format_seconds(elapsed(started, finished))})"
      )
    }
  }

  list(records = records, failure = NULL)
}

#' Run one operation
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced`, the files it wrote, and `state`, what it
#'   leaves for the operations after it.
#' @noRd
run_operation <- function(obtops, operation, stage, state) {
  runner <- operation_runner(operation$type)

  if (is.null(runner)) {
    cli::cli_abort("This package cannot run {.val {operation$type}} yet.")
  }

  answer <- runner(obtops, operation, stage = stage, state = state)

  produced <- answer$produced
  state <- answer$state

  list(
    produced = if (is.null(produced)) character() else produced,
    state = if (is.null(state)) list() else state
  )
}

#' What runs one type of operation
#'
#' The lookup is made here rather than held in a list, so a runner can be
#' written anywhere in the sources and read at run time.
#'
#' @param type The operation type name.
#'
#' @return The runner, or `NULL` when the package has none for that type.
#' @noRd
operation_runner <- function(type) {
  switch(
    type,
    `designer-add` = run_designer_add,
    `designer-geobase` = run_designer_geobase,
    `designer-languages` = run_designer_languages,
    `designer-generate` = run_designer_generate,
    `linelist-add` = run_linelist_add,
    `linelist-geobase` = run_linelist_geobase,
    `linelist-import` = run_linelist_import,
    `linelist-export` = run_linelist_export,
    `setup-add` = run_setup_add,
    `setup-export` = run_setup_export,
    `setup-import` = run_setup_import,
    `setup-tags` = run_setup_tags,
    `setup-fake` = run_setup_fake,
    `convert-add` = run_convert_add,
    `convert-export` = run_convert_export,
    `convert-import` = run_convert_import,
    NULL
  )
}

#' Download the OutbreakTools files and keep their paths
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_designer_add <- function(obtops, operation, stage, state) {
  fetched <- designer_fetch(
    obtops,
    type = operation$args$type,
    force = operation$args$force
  )

  produced <- c(
    zip = fetched$zip,
    designer = fetched$designer,
    empty_setup = fetched$setup,
    master_setup = fetched$msetup,
    ribbon = fetched$ribbon
  )

  list(
    produced = produced[!is.na(produced)],
    state = list(
      channel = fetched$channel,
      version = fetched$version,
      designer = fetched$designer,
      empty_setup = fetched$setup,
      master_setup = fetched$msetup,
      ribbon = fetched$ribbon
    )
  )
}

#' Carry the two languages into the run
#'
#' The dictionary language is read against the setup that is on disk. The
#' verb checked it against whatever was there when the recipe was built, and a
#' recipe is often built before a setup exists.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_designer_languages <- function(obtops, operation, stage, state) {
  dict <- check_dict_language(
    operation$args$dict,
    setup = dict_language_source(obtops),
    arg = "dict"
  )

  list(
    produced = character(),
    state = list(dict = dict, form = operation$args$form)
  )
}

#' Build the record of one operation that was reached
#'
#' @param operation The operation record.
#' @param outcome How it ended.
#' @param started When it started.
#' @param finished When it ended.
#' @param produced The files it wrote, relative to the working folder.
#'
#' @return A list with `type`, `label`, `outcome`, `started`, `finished`,
#'   `seconds` and `produced`.
#' @noRd
new_run_record <- function(
  operation,
  outcome,
  started,
  finished,
  produced = character()
) {
  list(
    type = operation$type,
    label = operation_spec(operation$type)$label,
    outcome = outcome,
    started = started,
    finished = finished,
    seconds = elapsed(started, finished),
    produced = produced
  )
}

#' Write what the run did onto the recipe it answers
#'
#' Each operation keeps its own record, so a caller reads the recipe the way
#' it read it before the run and finds what happened beside every step.
#'
#' @param obtops The recipe.
#' @param records The record of every operation that was reached.
#' @param platform The platform record.
#' @param started When the run started.
#' @param finished When it ended.
#' @param log The log it wrote.
#'
#' @return The recipe, carrying the run.
#' @noRd
record_run <- function(obtops, records, platform, started, finished, log) {
  for (index in seq_along(records)) {
    obtops$operations[[index]]$result <- records[[index]]
  }

  obtops$run <- list(
    outcome = run_outcome(records),
    started = started,
    finished = finished,
    seconds = elapsed(started, finished),
    log = path_relative(log, folder = obtops$folder),
    os = platform$os,
    excel_version = platform$excel_version,
    staged = !is.na(platform$staging)
  )

  obtops
}

#' How a whole run ended
#'
#' @param records The record of every operation that was reached.
#'
#' @return One of the run outcomes.
#' @noRd
run_outcome <- function(records) {
  if (length(records) == 0) {
    return(RUN_NONE)
  }

  outcomes <- vapply(records, function(record) record$outcome, character(1))

  if (any(outcomes == RUN_FAILED)) {
    return(RUN_FAILED)
  }

  RUN_OK
}

#' Stop the run and say where it stopped
#'
#' The reason is the failure itself, carried as the parent of this one, so the
#' user reads what the operation said under the line naming it.
#'
#' @param failure The failure, as `run_operations()` answers it.
#' @param total How many operations the recipe holds.
#' @param log The log the run wrote.
#' @param call The environment to blame in the error.
#'
#' @return Nothing. It always fails.
#' @noRd
stop_run <- function(failure, total, log, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "The run stopped at operation {failure$index} of {total}:
       {failure$label}.",
      "i" = "What the run did is at {.file {log}}."
    ),
    parent = failure$condition,
    call = call
  )
}

#' Say that the run finished
#'
#' @param records The record of every operation.
#' @param seconds How long the whole run took.
#' @param log The log it wrote.
#'
#' @return `NULL`, invisibly. Called for what it prints.
#' @noRd
say_run_finished <- function(records, seconds, log) {
  count <- length(records)

  cli::cli_alert_success(
    "{count} operation{?s} in {format_seconds(seconds)}."
  )
  cli::cli_alert_info("Log: {.file {log}}")

  invisible(NULL)
}

#' Fail when a recipe cannot be run as it stands
#'
#' Two things turn a recipe down: it holds nothing, or it holds an operation
#' whose OutbreakTools entry point has not shipped. Both are read off the
#' recipe alone, so neither costs a file.
#'
#' @param obtops The recipe.
#' @param call The environment to blame in the error.
#'
#' @return The recipe, invisibly.
#' @noRd
check_run_ready <- function(obtops, call = rlang::caller_env()) {
  if (length(obtops$operations) == 0) {
    cli::cli_abort(
      c(
        "The recipe holds no operation.",
        "i" = "Add one with a verb, then run it."
      ),
      call = call
    )
  }

  waiting <- Filter(
    function(operation) isTRUE(operation$waiting),
    obtops$operations
  )

  if (length(waiting) == 0) {
    return(invisible(obtops))
  }

  labels <- vapply(
    waiting,
    function(operation) operation_spec(operation$type)$label,
    character(1)
  )

  needs <- unique(vapply(
    waiting,
    function(operation) operation_spec(operation$type)$waits_on,
    character(1)
  ))

  cli::cli_abort(
    c(
      "{length(waiting)} operation{?s} of this recipe wait{?s/} on the
       workbooks.",
      "x" = "{.val {labels}}",
      "i" = "A released OutbreakTools has to carry {needs}.",
      "i" = "Use {.code obt_describe()} to read the whole recipe."
    ),
    call = call
  )
}

#' Write the log of one run
#'
#' One file per run, whether the run finished or stopped. It carries the
#' machine, then every operation that was reached, then how the run ended.
#'
#' @param obtops The recipe.
#' @param records The record of every operation that was reached.
#' @param platform The platform record.
#' @param started When the run started.
#' @param finished When it ended.
#' @param failure The failure, or `NULL` when the run finished.
#' @param path Where the log is written.
#'
#' @return The path, invisibly.
#' @noRd
write_run_log <- function(
  obtops,
  records,
  platform,
  started,
  finished,
  failure = NULL,
  path = log_path(obtops, when = started)
) {
  ensure_folder(dirname(path))
  writeLines(
    run_log_lines(
      obtops,
      records = records,
      platform = platform,
      started = started,
      finished = finished,
      failure = failure
    ),
    path
  )

  invisible(path)
}

#' The lines of a run log
#'
#' @param obtops The recipe.
#' @param records The record of every operation that was reached.
#' @param platform The platform record.
#' @param started When the run started.
#' @param finished When it ended.
#' @param failure The failure, or `NULL` when the run finished.
#'
#' @return A character vector, one line each.
#' @noRd
run_log_lines <- function(
  obtops,
  records,
  platform,
  started,
  finished,
  failure = NULL
) {
  head <- c(
    LOG_TITLE,
    paste0("Started: ", format_time(started)),
    paste0("Working folder: ", absolute_path(obtops$folder)),
    platform_log_lines(platform)
  )

  body <- unlist(
    lapply(seq_along(records), function(index) {
      log_operation_lines(records[[index]], index = index)
    }),
    use.names = FALSE
  )

  tail <- c(
    paste0("Finished: ", format_time(finished)),
    paste0("Took: ", format_seconds(elapsed(started, finished))),
    paste0("Outcome: ", run_outcome(records))
  )

  if (!is.null(failure)) {
    tail <- c(
      tail,
      paste0("Stopped at: ", failure$index, ". ", failure$label),
      paste0("Reason: ", failure_reason(failure$condition))
    )
  }

  if (length(body) == 0) {
    body <- "  none"
  }

  c(head, "", "Operations", body, "", tail)
}

#' The lines one operation takes in a log
#'
#' @param record The record of the operation.
#' @param index Its place in the recipe.
#'
#' @return A character vector, one line each.
#' @noRd
log_operation_lines <- function(record, index) {
  lead <- paste0(
    "  ",
    index,
    ". ",
    record$label,
    " - ",
    record$outcome,
    " - ",
    format_seconds(record$seconds)
  )

  produced <- record$produced

  if (length(produced) == 0) {
    return(lead)
  }

  c(lead, paste0("     ", names(produced), ": ", unname(produced)))
}

#' What a failure says, as one line
#'
#' A log is read in a plain text editor, so the bullets and the colours of a
#' cli message are taken off and the lines are joined.
#'
#' @param condition The condition the operation raised.
#'
#' @return A single string.
#' @noRd
failure_reason <- function(condition) {
  text <- tryCatch(
    cli::ansi_strip(conditionMessage(condition)),
    error = function(cnd) "the operation failed"
  )

  lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]

  if (length(lines) == 0) {
    return("the operation failed")
  }

  paste(lines, collapse = " ")
}

#' Answer the paths of a run the way a recipe stores them
#'
#' @param paths The paths the operation produced.
#' @param folder The working folder.
#'
#' @return The paths, relative to the folder where they sit under it.
#' @noRd
relative_paths <- function(paths, folder) {
  if (length(paths) == 0) {
    return(character())
  }

  answer <- vapply(
    paths,
    function(path) path_relative(path, folder = folder),
    character(1),
    USE.NAMES = FALSE
  )

  names(answer) <- names(paths)
  answer
}

#' How long something took, in seconds
#'
#' @param started When it started.
#' @param finished When it ended.
#'
#' @return A number of seconds.
#' @noRd
elapsed <- function(started, finished) {
  as.numeric(difftime(finished, started, units = "secs"))
}

#' Write a number of seconds the one way the package writes it
#'
#' @param seconds The number of seconds.
#'
#' @return A single string.
#' @noRd
format_seconds <- function(seconds) {
  if (length(seconds) != 1 || is.na(seconds)) {
    return("unknown")
  }

  paste0(formatC(seconds, format = "f", digits = 1), "s")
}

#' Write a time the one way the package writes it
#'
#' @param when The time.
#'
#' @return A single string.
#' @noRd
format_time <- function(when) {
  format(when, LOG_TIME_FORMAT)
}
