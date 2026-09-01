# A staging record a test owns, with both folders made.
test_stage <- function(folder, root, staged = TRUE) {
  ensure_folder(folder)

  if (isTRUE(staged)) {
    ensure_folder(root)
  }

  new_stage(folder = folder, root = root, staged = staged)
}

# A platform record a test can hand to `stage_open()`.
test_platform <- function(
  staging = NA_character_,
  os = "macos",
  excel_version = EXCEL_VERSION_PROVEN
) {
  new_platform(
    os = os,
    supported = os %in% SUPPORTED_OS,
    staging = staging,
    excel_version = excel_version,
    excel_proven = identical(excel_version, EXCEL_VERSION_PROVEN),
    app = "Positron"
  )
}

# An application bundle carrying one version, for the version reader.
test_excel_app <- function(folder, version) {
  contents <- file.path(folder, "Microsoft Excel.app", "Contents")
  ensure_folder(contents)

  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<plist version=\"1.0\">",
      "<dict>",
      "  <key>CFBundleShortVersionString</key>",
      paste0("  <string>", version, "</string>"),
      "</dict>",
      "</plist>"
    ),
    file.path(contents, "Info.plist")
  )

  dirname(contents)
}
