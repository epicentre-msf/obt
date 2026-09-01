# Whether this machine can open Excel, and the skip that reads it.
#
# The suite runs on macOS, Windows and Linux. A test that opens a workbook
# needs two things at once: an operating system the package supports, and a
# copy of Excel installed on it. Both are read while the test runs, because
# a machine with the right system can still be missing the application.

# Whether Excel is installed, on either supported system.
has_excel <- function() {
  if (is_macos()) {
    return(!is.na(excel_app_path()))
  }

  if (is_windows()) {
    return(windows_has_excel())
  }

  FALSE
}

# Windows registers the automation name when Excel is installed, so reading
# it back answers for every install location.
windows_has_excel <- function() {
  registered <- tryCatch(
    utils::readRegistry("Excel.Application\\CurVer", hive = "HCR"),
    error = function(cnd) NULL
  )

  !is.null(registered)
}

# The skip a test that reaches Excel opens with.
skip_if_no_excel <- function() {
  testthat::skip_on_os(c("linux", "solaris"))
  testthat::skip_if(!has_excel(), "Excel is not installed on this machine")
}
