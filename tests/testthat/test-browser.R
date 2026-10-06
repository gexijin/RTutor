test_that("create_response stops instead of sending an empty OPENAI_API_KEY", {
  withr::local_envvar(OPENAI_API_KEY = "")
  expect_error(create_response("m", list()), "OPENAI_API_KEY is not set")
})

test_that("app.R loads the key file the site build writes", {
  src <- paste(readLines(file.path(rprojroot::find_package_root_file(), "app.R")), collapse = "\n")
  expect_match(src, 'Sys.setenv(OPENAI_API_KEY = readLines("openai_key.txt"', fixed = TRUE)
  build <- paste(readLines(file.path(rprojroot::find_package_root_file(), "dev", "build_shinylive.R")), collapse = "\n")
  expect_match(build, 'writeLines(key, file.path(app, "openai_key.txt"))', fixed = TRUE)
})

test_that("install_missing_packages finds packages named in student code", {
  asked <- NULL
  local_mocked_bindings(
    getExportedValue = function(ns, name) function(pkgs, ...) asked <<- pkgs,
    .package = "base"
  )
  install_missing_packages(c("library(fakepkgA)", "require(\"fakepkgB\")",
                             "fakepkgC::f(m)", "library(stats)"))
  expect_setequal(asked, c("fakepkgA", "fakepkgB", "fakepkgC"))
})

test_that("the loading popup is plain HTML (shinybusy's spinner doesn't render in the browser)", {
  html <- as.character(loading_modal("A joke"))
  expect_match(html, "Loading", fixed = TRUE)
  expect_match(html, "rtutor-loading-dots", fixed = TRUE)
  expect_match(html, "A joke", fixed = TRUE)

  root <- rprojroot::find_package_root_file()
  src <- function(f) paste(readLines(file.path(root, "R", f)), collapse = "\n")
  for (f in c("mod_05_llms.R", "mod_16_qa.R")) {
    expect_no_match(src(f), "shinybusy::", fixed = TRUE)
    expect_match(src(f), "show_loading_modal(", fixed = TRUE)
  }
  expect_match(src("app_ui.R"), "$('.rtutor-loading-dots').text(dots[i])", fixed = TRUE)
  # mod_01_styles.R pins all popups to the bottom; this one is positioned explicitly
  expect_match(src("app_ui.R"), ".modal-dialog:has(.rtutor-loading-dots)", fixed = TRUE)
})
