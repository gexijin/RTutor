test_that("resolve_provider always uses OpenAI's base URL", {
  expect_null(resolve_provider(list(key = "sk-abc"))$endpoint)
  expect_equal(resolve_provider(list(key = "sk-abc"))$key, "sk-abc")
})

test_that("create_response asks for a key instead of sending an empty one", {
  expect_error(create_response("m", list(), key = ""), "API Key")
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
