# Adding the OutbreakTools files to a recipe.
#
# The verb records the channel and answers the recipe. `obt_commit()` is what
# downloads the zip, unzips it and keeps the paths of the workbooks it holds.

#' Add the OutbreakTools files
#'
#' @description
#' Records the download of the OutbreakTools files. At commit time the zip of
#' the channel is downloaded into `obt/<channel>/` in the working folder and
#' unzipped there, and the run keeps the paths of the designer, the empty
#' setup and the ribbon template it holds.
#'
#' Every file name is read from the zip, because a name carries the channel
#' and the version of the build.
#'
#' A zip already in the folder is used again, replaced With `force = TRUE`.
#'
#' The run refuses a release below version `r OBT_MINIMUM_VERSION`.
#'
#' Like every verb this one records and answers the recipe. Nothing is
#' downloaded until `obt_commit()` runs.
#'
#' @param obtops An `obt` recipe.
#' @param type The channel: `"main"` for the stable release, `"dev"` for the
#'   development build.
#' @param force Whether the zip is downloaded again when the folder already
#'   holds it.
#'
#' @return The recipe, with the download operation added.
#'
#' @export
#'
#' @examples
#' obt(folder = file.path(tempdir(), "measles")) |>
#'   obt_designer_add(type = "dev")
obt_designer_add <- function(obtops, type = "main", force = FALSE) {
  check_obt_plain(obtops)
  type <- check_channel(type)
  check_flag(force)

  add_operation(
    obtops,
    type = "designer-add",
    args = list(type = type, force = force)
  )
}
