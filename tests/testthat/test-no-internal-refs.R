# Scrub guard. No internal planning shorthand or personal handle may
# reach a committed source file. The forbidden tokens are assembled from
# fragments here so this guard never carries them itself, and the guard
# file is excluded from its own scan. The short session form matches
# only 1, 2, 5, 6 on purpose: a bare "S3" / "S4" reads as R's class
# systems, not a session id, so flagging them would be all noise. The
# two-digit form matches any S00-S99: a two-digit session id can never
# collide with R's single-digit S3 / S4, so it carries no such risk.

find_package_root <- function() {
  candidates <- c(file.path("..", ".."), file.path("..", "..", ".."), ".")
  for (dir in candidates) {
    has_source <- dir.exists(file.path(dir, "R")) &&
      file.exists(file.path(dir, "DESCRIPTION"))
    if (has_source) {
      return(normalizePath(dir))
    }
  }
  NA_character_
}

forbidden_patterns <- function() {
  list(
    list(
      name = "author name",
      regex = paste0("\\b[", "Yy", "]ves\\b"),
      ignore_case = FALSE
    ),
    list(
      name = "author handle",
      regex = paste0("\\bamevoi", "[n]\\b"),
      ignore_case = TRUE
    ),
    list(
      name = "local notes path (draft)",
      regex = paste0("\\.", "draft/"),
      ignore_case = TRUE
    ),
    list(
      name = "local notes path (agent)",
      regex = paste0("\\.", "r-", "agent/"),
      ignore_case = TRUE
    ),
    list(
      name = "plan reference",
      regex = paste0("\\bplan\\b", "[^a-z0-9]*[0-9]"),
      ignore_case = TRUE
    ),
    list(
      name = "session reference (word)",
      regex = paste0("\\bsession", "\\s*[0-9]"),
      ignore_case = TRUE
    ),
    list(
      name = "session reference (short)",
      regex = paste0("\\bS[", "1256", "]\\b"),
      ignore_case = FALSE
    ),
    list(
      name = "session reference (two-digit)",
      regex = paste0("\\bS[0-9]", "[0-9]\\b"),
      ignore_case = FALSE
    )
  )
}

scan_files <- function(root) {
  files <- c(
    list.files(
      file.path(root, "R"),
      pattern = "\\.R$",
      full.names = TRUE,
      recursive = TRUE
    ),
    list.files(
      file.path(root, "tests"),
      pattern = "\\.R$",
      full.names = TRUE,
      recursive = TRUE
    ),
    # A vignette or a data-raw script is committed prose too; a new one
    # must not carry an internal reference any more than the R sources.
    list.files(
      file.path(root, "vignettes"),
      pattern = "\\.(Rmd|orig)$",
      full.names = TRUE,
      recursive = TRUE
    ),
    list.files(
      file.path(root, "data-raw"),
      pattern = "\\.R$",
      full.names = TRUE,
      recursive = TRUE
    ),
    # The driver scripts ship with the package, so they are committed prose
    # under the same rule as the R sources.
    list.files(
      file.path(root, "inst", "scripts"),
      pattern = "\\.(vbs|applescript)$",
      full.names = TRUE,
      recursive = TRUE
    ),
    # The README and the news file are the first things a stranger reads, so
    # they are held to the same rule as the R sources.
    Filter(
      file.exists,
      file.path(root, c("README.Rmd", "README.md", "NEWS.md"))
    )
  )
  files[basename(files) != "test-no-internal-refs.R"]
}

find_offenders <- function(files, patterns) {
  offenders <- character()
  for (file in files) {
    lines <- enc2utf8(readLines(file, warn = FALSE))
    for (pat in patterns) {
      hits <- grep(
        pat$regex,
        lines,
        perl = TRUE,
        ignore.case = pat$ignore_case
      )
      for (line_no in hits) {
        offenders <- c(
          offenders,
          sprintf("%s:%d (%s)", basename(file), line_no, pat$name)
        )
      }
    }
  }
  offenders
}

test_that("committed source carries no internal planning references", {
  root <- find_package_root()
  skip_if(is.na(root), "package source tree not found")

  files <- scan_files(root)
  expect_gt(length(files), 0)

  offenders <- find_offenders(files, forbidden_patterns())
  expect_identical(
    offenders,
    character(),
    info = paste0(
      "Internal references found in committed source:\n",
      paste(offenders, collapse = "\n")
    )
  )
})
