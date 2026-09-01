# The recipe object.
#
# A recipe is a working folder and an ordered list of operations. A verb
# records one operation and answers the recipe, so verbs chain with `|>`.
# Nothing here reaches the disk: a whole run can be built, read and corrected
# before Excel is ever opened. `obt_commit()` is what acts on it.

#' Start a recipe
#'
#' @description
#' Creates an empty recipe. Every verb adds one operation to the recipe.
#' Nothing is downloaded, copied or
#' written while a recipe is built; `obt_commit()` runs the operations in the
#' order they were added.
#'
#' `folder` is the working folder. Everything the package downloads or writes
#' later goes under it, in a fixed layout. The folder is recorded here and
#' read at commit time, so a recipe can be built on a machine where the folder
#' does not exist yet.
#'
#' @param folder Path to the working folder, as a single string.
#'
#' @return An object of class `obt`: the working folder and an empty
#'   operation list.
#'
#' @export
#'
#' @examples
#' obt(folder = file.path(tempdir(), "measles"))
obt <- function(folder) {
  new_obt(folder = check_folder(folder), operations = list())
}

#' Test whether an object is a recipe
#'
#' @param x An object.
#'
#' @return `TRUE` when `x` is an `obt` recipe, `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' is_obt(obt(folder = tempdir()))
#' is_obt("a folder path")
is_obt <- function(x) {
  inherits(x, "obt")
}

#' Build a recipe from its parts
#'
#' @param folder The working folder, already checked.
#' @param operations The ordered list of operation records.
#'
#' @return An object of class `obt`.
#' @noRd
new_obt <- function(folder, operations) {
  structure(
    list(folder = folder, operations = operations),
    class = "obt"
  )
}

#' Check the folder handed to `obt()`
#'
#' @param folder The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The folder path, with `~` expanded.
#' @noRd
check_folder <- function(
  folder,
  arg = rlang::caller_arg(folder),
  call = rlang::caller_env()
) {
  folder <- check_string(folder, arg = arg, call = call)

  # `~` is expanded here so the recipe carries one shape of the path from the
  # start, and two recipes on the same folder compare equal.
  path.expand(folder)
}

#' Fail when a value is not a recipe
#'
#' @param obtops The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The recipe.
#' @noRd
check_obt <- function(
  obtops,
  arg = rlang::caller_arg(obtops),
  call = rlang::caller_env()
) {
  if (!is_obt(obtops)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an {.cls obt} recipe.",
        "x" = "You supplied {.obj_type_friendly {obtops}}.",
        "i" = "Start one with {.code obt(folder = )}."
      ),
      call = call
    )
  }

  obtops
}

#' Fail when a recipe was narrowed to one workbook
#'
#' The designer verbs fill the designer's `Main` sheet, and the designer comes
#' with an `obt`. A narrowed recipe left it behind, so a designer verb on one
#' would record an operation the run can never carry out.
#'
#' @param obtops The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The recipe.
#' @noRd
check_obt_plain <- function(
  obtops,
  arg = rlang::caller_arg(obtops),
  call = rlang::caller_env()
) {
  check_obt(obtops, arg = arg, call = call)

  narrowed <- setdiff(class(obtops), "obt")

  if (length(narrowed) == 0) {
    return(obtops)
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} must be an {.cls obt} recipe.",
      "x" = "You supplied an {.cls {narrowed[[1]]}} recipe, which holds one
             workbook on its own.",
      "i" = "A designer verb runs on a recipe holding
             {.code obt_designer_add()}."
    ),
    call = call
  )
}

#' Keep the operations of one narrowed class
#'
#' Shared by the two narrowings. A narrowed recipe is built from the folder it
#' came with and the operations of its own class, in the order they were
#' recorded.
#'
#' @param obtops The recipe to narrow.
#' @param target The class the narrowing builds.
#' @param types The operation types the narrowed class holds.
#' @param folder What the caller passed as the folder.
#' @param overwrite What the caller passed as the overwrite flag.
#' @param what The workbook the narrowed class works on, in one word.
#' @param call The environment to blame in the error.
#'
#' @return The operation records kept, with any result of an earlier run
#'   dropped.
#' @noRd
narrow_operations <- function(
  obtops,
  target,
  types,
  folder,
  overwrite,
  what,
  call = rlang::caller_env()
) {
  narrowed <- setdiff(class(obtops), c("obt", target))

  if (length(narrowed) > 0) {
    cli::cli_abort(
      c(
        "A {.cls {narrowed[[1]]}} recipe does not narrow to a
         {.cls {target}} one.",
        "i" = "It holds one workbook on its own, and the {what} operations
               are not among what it carries."
      ),
      call = call
    )
  }

  given <- c(
    if (!is.null(folder)) "folder",
    if (!identical(overwrite, FALSE)) "overwrite"
  )

  if (length(given) > 0) {
    cli::cli_abort(
      c(
        "A recipe is narrowed on its own.",
        "x" = "{.arg {given}} {?was/were} given as well.",
        "i" = "The narrowed recipe carries the working folder and the
               {what} operations of the recipe it came from."
      ),
      call = call
    )
  }

  kept <- Filter(
    function(operation) operation$type %in% types,
    obtops$operations
  )

  # The result of a run belongs to the recipe that ran, and that recipe held
  # more than the operations kept here. The narrowed one has yet to run.
  lapply(kept, function(operation) {
    operation$result <- NULL
    operation
  })
}
