# Build the small setup files the language reader is tested against.
#
# These files copy the only part the reader looks at: the header row of the
# Translations sheet. The shapes come from a setup export written by the
# workbook itself, where the header sits on the first row and an internal tag
# column leads with a double underscore.
#
# The real setup beside them, inst/extdata/generic-test-setup.xlsb, is not
# built here. It is the OutbreakTools test setup, copied from that repo at
# src/tests/.input/package/generic-test-setup.xlsb. Refresh it by copying the
# file over again under the same name. It is the one file in the fixtures that
# holds a whole setup, so a reader is proved against a workbook the designer
# itself is tested with.
#
# inst/extdata/generic-test-setup.xlsm holds the same workbook saved as
# .xlsm. OutbreakTools ships its setups in that format, and the package reads
# it with readxl. To make it, open the .xlsb in Excel with AutomationSecurity
# set to 3 (force-disable), so none of the workbook's code runs, then SaveAs
# with FileFormat 52. Redo it each time the .xlsb is refreshed. The tests
# check that both files give the same languages and the same fake records.
#
# Run with: Rscript data-raw/make-setup-fixtures.R

library(writexl)

extdata <- file.path("inst", "extdata")
dir.create(extdata, recursive = TRUE, showWarnings = FALSE)

# A setup holding four languages. The tag column and the blank column are both
# there to be dropped: the designer skips an empty header and any header
# leading with the tag mark.
translations <- data.frame(
  English = c("0", "1"),
  `Français` = c(NA_character_, NA_character_),
  `__Tag` = c("T0", "T1"),
  ` ` = c(NA_character_, NA_character_),
  `Español` = c(NA_character_, NA_character_),
  Portugese = c(NA_character_, NA_character_),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

write_xlsx(
  list(Dictionary = data.frame(a = 1), Translations = translations),
  file.path(extdata, "setup-translations.xlsx")
)

# A workbook with no Translations sheet at all.
write_xlsx(
  list(Dictionary = data.frame(a = 1)),
  file.path(extdata, "setup-no-translations.xlsx")
)

# A Translations sheet whose every column is internal, so no language is left.
write_xlsx(
  list(
    Translations = data.frame(
      `__Tag` = "T0",
      `__Other` = "T1",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  ),
  file.path(extdata, "setup-tags-only.xlsx")
)

message("wrote fixtures to ", extdata)
