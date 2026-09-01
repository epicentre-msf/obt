# Taking a step back out of a recipe.
#
# A recipe is built to be read and corrected before Excel opens, and a verb
# that was called by mistake has to come back out. Both verbs here answer the
# recipe, so they sit in a chain the way every other verb does.
#
# A step recorded by a verb that recorded several comes out with the rest of
# them. `obt_setup_convert()` records three, and a recipe holding one or two
# of them describes a conversion nothing can carry out.
#
# A recipe that has already run is left alone. It carries the record of what
# happened, and taking a step out of that record would say the run did
# something it did not.

#' Take a step back out of a recipe
#'
#' @description
#' `obt_remove()` takes one step out. `step` names it, either as the number
#' [obt_describe()] prints beside it or as the type it prints after its label.
#'
#' `obt_pop()` takes the last step out, so a verb called by mistake comes
#' straight back off.
#'
#' A step recorded by a verb that recorded several comes out with the rest of
#' them, and the message says so. [obt_setup_convert()] records three steps in
#' one call, and a recipe holding part of a conversion describes a run that
#' cannot happen.
#'
#' A type the recipe holds more than once is refused, because there is no
#' saying which of them was meant. The message names the numbers, and a number
#' settles it.
#'
#' A recipe that has already run is left alone. It carries the record of what
#' the run did, and a step taken out of that record would say the run did
#' something it did not.
#'
#' @param obtops An `obt` recipe, of any of its classes.
#' @param step The step to take out: the number [obt_describe()] prints beside
#'   it, or the operation type it prints after the label.
#'
#' @return The recipe, with the step and anything recorded beside it removed.
#'
#' @seealso [obt_describe()] to read the steps and their types,
#'   [obt_operations()] for the same list in one line each.
#'
#' @name obt_remove
#'
#' @examples
#' recipe <- obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_add(type = "dev") |>
#'   obt_designer_languages(form = "ENG") |>
#'   obt_designer_generate(name = "measles-2026")
#'
#' # By the number the description prints.
#' obt_remove(recipe, step = 2)
#'
#' # By the type, which the description prints after the label.
#' obt_remove(recipe, step = "designer-languages")
#'
#' # The last one called comes straight back off.
#' obt_pop(recipe)
NULL

#' @rdname obt_remove
#' @export
obt_remove <- function(obtops, step) {
  check_obt(obtops)
  check_recipe_editable(obtops)

  at <- resolve_step(obtops, step)

  drop_step(obtops, at)
}

#' @rdname obt_remove
#' @export
obt_pop <- function(obtops) {
  check_obt(obtops)
  check_recipe_editable(obtops)
  check_recipe_filled(obtops)

  drop_step(obtops, length(obtops$operations))
}

#' Take one step and everything its verb call recorded out of a recipe
#'
#' @param obtops The recipe.
#' @param at The position of the step named.
#'
#' @return The recipe, with the step and its siblings removed.
#' @noRd
drop_step <- function(obtops, at) {
  going <- step_siblings(obtops, at)

  if (length(going) > 1) {
    verb <- operation_spec(obtops$operations[[at]]$type)$verb
    count <- length(going)
    numbers <- paste(going, collapse = ", ")

    cli::cli_inform(c(
      "i" = "{.code {verb}} recorded {count} steps in one call, so all of
             them are taken out together.",
      "*" = "Steps {numbers}."
    ))
  }

  obtops$operations <- obtops$operations[-going]

  warn_conversion_orphaned(obtops, going = going)

  obtops
}

#' Say when a conversion is left with no designer to fill
#'
#' A conversion fills a copy of the empty setup the OutbreakTools files carry,
#' and `obt_setup_convert()` refuses to record without them. Taking the
#' designer back out afterwards leaves the same recipe the verb turned down,
#' so it is said here rather than at the run.
#'
#' @param obtops The recipe, with the steps already removed.
#' @param going The positions that were taken out.
#'
#' @return `NULL`, invisibly.
#' @noRd
warn_conversion_orphaned <- function(obtops, going) {
  types <- vapply(
    obtops$operations,
    function(operation) operation$type,
    character(1)
  )

  if (!any(types %in% CONVERT_OPERATION_TYPES) || "designer-add" %in% types) {
    return(invisible(NULL))
  }

  cli::cli_warn(c(
    "The recipe still converts a setup, and it no longer adds the
     OutbreakTools files.",
    "x" = "A conversion fills a copy of the empty setup those files carry.",
    "i" = "Add them again with {.code obt_designer_add()}, or take the
           conversion out too."
  ))

  invisible(NULL)
}

#' The positions of every step one verb call recorded
#'
#' A record built with no group of its own stands alone.
#'
#' @param obtops The recipe.
#' @param at The position of the step named.
#'
#' @return An integer vector of positions, `at` among them, in order.
#' @noRd
step_siblings <- function(obtops, at) {
  groups <- operation_groups(obtops)
  mine <- groups[[at]]

  if (is.na(mine)) {
    return(at)
  }

  which(!is.na(groups) & groups == mine)
}

#' Read the step a caller named
#'
#' @param obtops The recipe.
#' @param step The value the caller gave.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The position of the step, as a single integer.
#' @noRd
resolve_step <- function(
  obtops,
  step,
  arg = rlang::caller_arg(step),
  call = rlang::caller_env()
) {
  force(arg)

  check_recipe_filled(obtops, call = call)

  if (is.character(step)) {
    return(step_from_type(obtops, step, arg = arg, call = call))
  }

  step_from_number(obtops, step, arg = arg, call = call)
}

#' Read a step named by its number
#'
#' @param obtops The recipe.
#' @param step The value the caller gave.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The position, as a single integer.
#' @noRd
step_from_number <- function(
  obtops,
  step,
  arg = "step",
  call = rlang::caller_env()
) {
  held <- length(obtops$operations)
  whole <- is.numeric(step) &&
    length(step) == 1 &&
    !is.na(step) &&
    step == trunc(step)

  if (!whole) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be the number of a step, or its type.",
        "x" = "You supplied {.obj_type_friendly {step}}.",
        "i" = "{.code obt_describe()} prints both beside every step."
      ),
      call = call
    )
  }

  step <- as.integer(step)

  if (step < 1 || step > held) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be between {.val {1L}} and {.val {held}}.",
        "x" = "You supplied {.val {step}}.",
        "i" = "The recipe holds {held} step{?s}."
      ),
      call = call
    )
  }

  step
}

#' Read a step named by its type
#'
#' @param obtops The recipe.
#' @param step The value the caller gave.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The position, as a single integer.
#' @noRd
step_from_type <- function(
  obtops,
  step,
  arg = "step",
  call = rlang::caller_env()
) {
  step <- check_string(step, arg = arg, call = call)
  held <- vapply(
    obtops$operations,
    function(operation) operation$type,
    character(1)
  )

  found <- which(held == step)

  if (length(found) == 1) {
    return(found)
  }

  if (length(found) == 0) {
    cli::cli_abort(
      c(
        "The recipe holds no {.val {step}} step.",
        "x" = "It holds {.val {unique(held)}}.",
        "i" = "{.code obt_describe()} prints the type beside every step."
      ),
      call = call
    )
  }

  numbers <- paste(found, collapse = ", ")

  cli::cli_abort(
    c(
      "The recipe holds {length(found)} {.val {step}} steps.",
      "x" = "They are steps {numbers}.",
      "i" = "Name the one you mean by its number."
    ),
    call = call
  )
}

#' Fail when a recipe holds nothing to take out
#'
#' @param obtops The recipe.
#' @param call The environment to blame in the error.
#'
#' @return The recipe, invisibly.
#' @noRd
check_recipe_filled <- function(obtops, call = rlang::caller_env()) {
  if (length(obtops$operations) > 0) {
    return(invisible(obtops))
  }

  cli::cli_abort(
    c(
      "The recipe holds no step to take out.",
      "i" = "Add one with a verb."
    ),
    call = call
  )
}

#' Fail when a recipe has already run
#'
#' @param obtops The recipe.
#' @param call The environment to blame in the error.
#'
#' @return The recipe, invisibly.
#' @noRd
check_recipe_editable <- function(obtops, call = rlang::caller_env()) {
  if (!has_run(obtops)) {
    return(invisible(obtops))
  }

  cli::cli_abort(
    c(
      "This recipe has already run.",
      "x" = "It carries what the run did, and a step taken out of that
             record would say the run did something it did not.",
      "i" = "Build the recipe you want next with the verbs."
    ),
    call = call
  )
}
