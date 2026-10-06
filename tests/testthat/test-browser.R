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
