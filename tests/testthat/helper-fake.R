# A dictionary and a choice table, as `read_setup_tables()` answers them.
#
# The engine reads a setup through these two frames alone, so a test that
# builds them by hand can name the exact variable it is about.
fake_dict <- function(...) {
  rows <- list(...)

  columns <- c(
    "variable_name",
    "sheet_name",
    "sheet_type",
    "variable_type",
    "control",
    "control_details",
    "unique",
    "min",
    "max"
  )

  filled <- lapply(rows, function(row) {
    missing <- setdiff(columns, names(row))
    row[missing] <- ""
    row[columns]
  })

  as.data.frame(
    do.call(rbind, lapply(filled, as.data.frame, stringsAsFactors = FALSE)),
    stringsAsFactors = FALSE
  )
}

# One variable of a horizontal worksheet, with everything else left empty.
fake_var <- function(name, ...) {
  c(
    list(variable_name = name, sheet_name = "ll", sheet_type = "hlist2D"),
    list(...)
  )
}

# The choice lists of a setup.
fake_choices <- function(list_name = character(), label = character()) {
  data.frame(
    list_name = list_name,
    label = label,
    stringsAsFactors = FALSE
  )
}

# The settings the engine reads, with the defaults `obt_fake()` records.
fake_test_settings <- function(...) {
  defaults <- list(
    id_prefix = "P",
    id_width = 2L,
    range_int = c(0, 100),
    range_num = c(0, 100),
    range_date = c(Sys.Date() - 365, Sys.Date()),
    nchar_text = c(4L, 12L),
    prop_na = 0
  )

  utils::modifyList(defaults, list(...))
}

# One worksheet of records, built straight from a hand-written dictionary.
fake_test_sheet <- function(
  dict,
  choices = fake_choices(),
  geo = NULL,
  n = 10,
  ...
) {
  fake_sheet(
    dict,
    choices = choices,
    geo = geo,
    n = n,
    settings = fake_test_settings(...)
  )
}

# A geobase file, in the shape the geobase web app writes.
#
# Level 2 names a place under each region, and level 3 names none at all, so a
# test can see the marker rows left out and a whole level dropped.
fake_geobase_file <- function(folder, name = "geobase.xlsx") {
  ensure_folder(folder)
  path <- file.path(folder, name)

  writexl::write_xlsx(
    list(
      ADM1 = data.frame(
        adm1_name = c("TCD North", "TCD South"),
        stringsAsFactors = FALSE
      ),
      ADM2 = data.frame(
        adm1_name = c("N/A", "TCD North", "TCD South"),
        adm2_name = c("N/A", "Moussoro", "Bongor"),
        stringsAsFactors = FALSE
      ),
      ADM3 = data.frame(
        adm1_name = "N/A",
        adm2_name = "N/A",
        adm3_name = "N/A",
        stringsAsFactors = FALSE
      )
    ),
    path
  )

  path
}

# A setup recipe on a working folder that already holds its setup, so a fake
# run has something to read.
#
# The copy is made here and the recipe answered holds no step of its own, so a
# test can run it as many times as it needs.
fake_committed_setup <- function(folder) {
  invisible(obt_commit(
    obt_setup(from = setup_workbook(), folder = folder),
    verbose = FALSE
  ))

  obt_setup(obt(folder = folder))
}

# The sheets of a file the fake run wrote, read back as data frames.
fake_read_back <- function(path, sheet) {
  as.data.frame(
    readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
  )
}
