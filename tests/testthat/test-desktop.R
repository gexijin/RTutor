# Desktop (Electron) build: behavior switched by RTUTOR_DESKTOP, and the package
# dependencies the bundled library is built from.
# Run:  testthat::test_file("tests/testthat/test-desktop.R")

r_src <- function(file) {
  paste(readLines(file.path(rprojroot::find_package_root_file(), "R", file), warn = FALSE), collapse = "\n")
}

test_that("library() is rewritten to pacman::p_load() off desktop but kept on desktop", {
  withr::local_envvar(RTUTOR_DESKTOP = "")
  expect_match(paste(clean_cmd("library(ggplot2)", "mpg"), collapse = "\n"), "pacman::p_load(ggplot2)", fixed = TRUE)

  withr::local_envvar(RTUTOR_DESKTOP = "1")
  out <- paste(clean_cmd("library(ggplot2)", "mpg"), collapse = "\n")
  expect_match(out, "library(ggplot2)", fixed = TRUE)
  expect_no_match(out, "p_load", fixed = TRUE)
})

test_that("a missing package gets a desktop-specific note only on desktop", {
  msg <- tryCatch(library(notARealPackage123), error = conditionMessage)

  withr::local_envvar(RTUTOR_DESKTOP = "")
  expect_identical(desktop_package_note(msg), msg)

  withr::local_envvar(RTUTOR_DESKTOP = "1")
  expect_match(desktop_package_note(msg), "notARealPackage123 isn't available in the UIUC RTutor desktop app", fixed = TRUE)
  expect_identical(desktop_package_note("some other error"), "some other error")
})

test_that("a bundled package that fails to load gets the note too, naming the right package", {
  withr::local_envvar(RTUTOR_DESKTOP = "1")
  # Exact R wording when summarytools' dependency tcltk can't load (macOS without Tcl/Tk)
  via_library <- paste0("package or namespace load failed for \u2018summarytools\u2019:\n",
                        " .onLoad failed in loadNamespace() for 'tcltk', details:\n",
                        "  call: dyn.load(file, DLLpath = DLLpath, ...)\n",
                        "  error: unable to load shared object '/x/library/tcltk/libs/tcltk.so'")
  via_colons <- sub("^[^\n]*\n ", "", via_library)  # pkg:: reports only the failing dependency
  expect_match(desktop_package_note(via_library), "Package summarytools isn't available", fixed = TRUE)
  expect_match(desktop_package_note(via_colons), "Package tcltk isn't available", fixed = TRUE)
})

test_that("a rejected key (401) tells students the version expired and links to the download page", {
  src <- r_src("mod_06_error_hist.R")
  block <- regmatches(src, regexpr('(?s)"401" = tagList.*?"403"', src, perl = TRUE))
  expect_match(block, "has expired", fixed = TRUE)
  expect_match(block, "desktop_download_url", fixed = TRUE)
})

test_that("every pkg:: used in R/ is declared in DESCRIPTION, so the desktop bundle installs it", {
  root <- rprojroot::find_package_root_file()
  desc <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Imports", "Depends"))
  declared <- trimws(sub("\\(.*", "", unlist(strsplit(paste(desc, collapse = ","), ","))))

  src <- unlist(lapply(list.files(file.path(root, "R"), full.names = TRUE), readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  used <- unique(sub("::$", "", unlist(regmatches(src, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*::", src)))))

  base_pkgs <- c("base", "methods", "tools", "utils", "stats", "graphics", "grDevices")
  not_packages <- c("pkg", "summary", "question", "text")  # matched inside strings, not package calls
  expect_setequal(setdiff(used, c(declared, base_pkgs, not_packages)), character(0))
})
