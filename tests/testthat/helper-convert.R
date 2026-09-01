# A recipe carrying the OutbreakTools files, which a conversion needs.
convert_recipe <- function(folder) {
  obt(folder = folder) |> obt_designer_add(type = "dev")
}

# A setup to convert, under a name the verb accepts. What the file holds is
# never read: the verb checks the name and the run hands the file to Excel.
old_setup <- function(folder, name = "old-setup.xlsb") {
  empty_file(folder, name)
}

# What the setup workbook answers once its export has written a file. The
# path of that file comes back in the summary, under the key the workbook
# writes it to.
export_answer <- function(path = NULL) {
  list(
    pair = "setup-export",
    answer = DRIVER_OK,
    source = DRIVER_FROM_ANSWER,
    summary = if (is.null(path)) character() else c(export = path),
    report = character()
  )
}

# The operation of a recipe at one place in the run order.
operation_at <- function(obtops, index) {
  obtops$operations[[index]]
}
