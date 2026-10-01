test_that("resolve_provider routes pasted keys by prefix", {
  expect_null(resolve_provider(list(key = "sk-abc"))$endpoint)
  expect_equal(resolve_provider(list(key = "abc123"))$endpoint, azure_endpoint)
  expect_equal(resolve_provider(list(key = "abc123"))$key, "abc123")
  # nothing pasted: fall back to the server's Azure key
  withr::with_envvar(c(AZURE_OPENAI_API_KEY = "server-key"),
    expect_equal(resolve_provider(list(key = ""))$key, "server-key"))
})

test_that("create_response asks for a key instead of sending an empty one", {
  expect_error(create_response("m", list(), key = ""), "Settings tab")
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
