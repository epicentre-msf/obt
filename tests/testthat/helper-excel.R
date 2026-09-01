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

# The skip a test that runs a recipe opens with. `obt_commit()` is turned down
# on a system that cannot open Excel, so a run cannot be reached there at all,
# even where the operations of that run never open a workbook.
skip_if_no_run <- function() {
  testthat::skip_on_os(c("linux", "solaris"))
}

# The skip a test of the macOS version reader opens with. The reader asks
# PlistBuddy for the version, and PlistBuddy ships with macOS alone.
skip_if_not_macos <- function() {
  testthat::skip_on_os(c("windows", "linux", "solaris"))
}
