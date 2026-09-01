# The geobase a run is given.
#
# The geobase is downloaded by the user from the geobase web app, which sits
# behind a login. The package takes a path and downloads nothing.
#
# There are two halves to what a recipe does with one. `obt_designer_geobase()`
# points the generation run at a geobase, so the designer builds the linelist
# with it. `obt_linelist_geobase()`, in `linelist-imports.R`, reads a geobase into
# a linelist that was already generated. Both copy the file into `geo/` and
# work on the copy, and the file the user handed over is never opened by a
# run.
#
# A geobase is optional everywhere. A run without one is a normal run.

#' Point the run at a geobase
#'
#' @description
#' Records the geobase the generation run builds with. At commit time the file
#' is copied into `geo/` in the working folder, and the designer is handed the
#' copy.
#'
#'
#' A geobase is optional. A recipe without a geobase will still build.
#'
#' Like every verb this one records and answers the recipe. Nothing is copied
#' until [obt_commit()] runs.
#'
#' @param obtops An `obt` recipe.
#' @param path The geobase file, as downloaded from the geobase web app.
#'
#' @return The recipe, with the geobase operation added.
#'
#' @seealso [obt_linelist_geobase()] to read a geobase into a linelist that was
#'   already generated.
#'
#' @export
#'
#' @examples
#' geobase <- file.path(tempdir(), "geobase.xlsx")
#' invisible(file.create(geobase))
#'
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_geobase(path = geobase) |>
#'   obt_designer_generate(name = "measles-2026")
obt_designer_geobase <- function(obtops, path) {
  check_obt_plain(obtops)
  path <- check_geobase_file(path)

  add_operation(
    obtops,
    type = "designer-geobase",
    args = list(path = path_relative(path, folder = obtops$folder))
  )
}

#' Copy the geobase into the working folder
#'
#' The copy is what the generation run is handed, under `state$geobase`.
#'
#' @param obtops The recipe.
#' @param operation The operation record.
#' @param stage The staging record of the run.
#' @param state What the operations before it left behind.
#'
#' @return A list with `produced` and `state`.
#' @noRd
run_designer_geobase <- function(obtops, operation, stage, state) {
  kept <- take_geobase(obtops, operation$args$path)

  list(produced = c(geobase = kept), state = list(geobase = kept))
}

#' Take a copy of the geobase the verb was given
#'
#' The copy keeps the name the user knows the file by, so a user opening
#' `geo/` recognises what they handed over. A file already sitting there is
#' left as it is.
#'
#' The file is looked for again here. A recipe is built minutes before it is
#' run, and a file can be moved in between.
#'
#' @param obtops The recipe.
#' @param path The geobase, as the operation recorded it.
#' @param call The environment to blame in the error.
#'
#' @return An absolute path, inside `geo/`.
#' @noRd
take_geobase <- function(obtops, path, call = rlang::caller_env()) {
  from <- path_absolute(path, folder = obtops$folder)

  check_file_there(from, what = "the geobase it was given", call = call)

  ensure_folder(obt_paths(obtops)$geo, call = call)
  kept <- copy_path(obtops, "geo", from, call = call)

  if (identical(kept, from)) {
    return(kept)
  }

  copy_file(from, kept, call = call)

  kept
}

#' Check the geobase a verb was given
#'
#' The geobase web app writes the file and the linelist reads it, so its
#' layout belongs to neither this package nor the user. What is checked here
#' is that a file was named and that it is there.
#'
#' @param path The value to check.
#' @param arg The name of the argument it came from.
#' @param call The environment to blame in the error.
#'
#' @return The path, trimmed.
#' @noRd
check_geobase_file <- function(
  path,
  arg = rlang::caller_arg(path),
  call = rlang::caller_env()
) {
  # The label is read before `path` is written over. `caller_arg()` answers
  # the value of a name it finds bound, so a forced default reads the
  # argument and a lazy one reads the string the user typed.
  force(arg)

  path <- check_string(path, arg = arg, call = call)
  check_file_there(path, what = "a geobase file", call = call)

  path
}
