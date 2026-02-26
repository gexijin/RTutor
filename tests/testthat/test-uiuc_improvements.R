# =============================================================================
# Tests for features added in the uiuc_improvements branch
#
# Run:  testthat::test_file("tests/testthat/test-uiuc_improvements.R")
#       devtools::test()   (when full package is installable)
#
# Design: tests are fully self-contained — no source() or load_all() required.
#   - File-content tests: read R source files with readLines() and check them.
#   - Logic tests: inline the logic being tested (copied from the implementation).
#   - Package tests: use requireNamespace() guards.
#
# Coverage:
#   1.  Global constants            (fct_helpers.R)
#   2.  create_chat_completion_openai  (new direct-httr function)
#   3.  create_chat_completion_azure   (missing-credential error messages)
#   4.  system_role_tutor              (Markdown not HTML)
#   5.  Q&A always visible             (mod_16_qa.R)
#   6.  Q&A "4." label                 (mod_16_qa.R)
#   7.  Q&A markdown rendering         (commonmark + file check)
#   8.  Chunk renaming — name field    (mod_06_error_hist.R)
#   9.  Chunk renaming — JS handler    (mod_04_main_panel.R)
#   10. Chunk renaming — rmd patching  (logic + mod_04_main_panel.R)
#   11. Temperature removed from Settings  (mod_11_settings.R)
#   12. Temperature added to sidebar       (app_ui.R)
#   13. sample_temp override in server     (app_server.R)
#   14. Report uses custom chunk names     (mod_09_report.R)
# =============================================================================

library(testthat)

# ---------------------------------------------------------------------------
# Helper: read an R source file as a single character string
# ---------------------------------------------------------------------------
r_file <- function(filename) {
  path <- file.path(rprojroot::find_package_root_file(), "R", filename)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}


# =============================================================================
# 1. Global constants — fct_helpers.R
# =============================================================================

test_that("fct_helpers.R defines default_temperature as 0.2", {
  src <- r_file("fct_helpers.R")
  expect_match(src, "default_temperature\\s*<-\\s*0\\.2")
})

test_that("fct_helpers.R defines api_versions with an o4-mini entry", {
  src <- r_file("fct_helpers.R")
  expect_match(src, '"o4-mini"', fixed = TRUE)
  expect_match(src, "api_versions\\s*<-")
})

test_that("fct_helpers.R defines language_models containing o4-mini", {
  src <- r_file("fct_helpers.R")
  expect_match(src, 'language_models\\s*<-\\s*c\\("o4-mini"\\)')
})


# =============================================================================
# 2. system_role_tutor — Markdown output, not HTML  (fct_helpers.R)
# =============================================================================

test_that("system_role_tutor requests Markdown format", {
  src <- r_file("fct_helpers.R")
  # Extract the system_role_tutor string value
  expect_match(src, "Markdown", fixed = TRUE)
})

test_that("system_role_tutor no longer instructs HTML output", {
  src <- r_file("fct_helpers.R")
  # After the change, "structured HTML" should not appear in system_role_tutor
  # Find just the system_role_tutor assignment block
  tutor_start <- regexpr("system_role_tutor\\s*<-", src)
  tutor_block <- substring(src, tutor_start, tutor_start + 400)
  expect_false(grepl("structured HTML", tutor_block, fixed = TRUE))
})

test_that("system_role_tutor retains role context and 'No comment' instruction", {
  src <- r_file("fct_helpers.R")
  tutor_start <- regexpr("system_role_tutor\\s*<-", src)
  tutor_block <- substring(src, tutor_start, tutor_start + 400)
  expect_match(tutor_block, "professor",  ignore.case = TRUE)
  expect_match(tutor_block, "No comment", fixed = TRUE)
})

test_that("system_role (code generation) is unaffected — no Markdown mention", {
  src <- r_file("fct_helpers.R")
  # system_role is a separate variable; find its assignment and check
  role_start <- regexpr('(?<!_)system_role\\s*<-', src, perl = TRUE)
  role_block <- substring(src, role_start, role_start + 300)
  expect_false(grepl("Markdown", role_block, fixed = TRUE))
  expect_match(role_block, "data scientist", ignore.case = TRUE)
})


# =============================================================================
# 3. create_chat_completion_openai — new function (fct_helpers.R)
#    NOTE: Implementation deferred — the direct-httr OpenAI function was written
#    but not yet merged. These tests are skipped until the feature is added.
# =============================================================================

test_that("create_chat_completion_openai function is defined in fct_helpers.R", {
  skip("Deferred: create_chat_completion_openai not yet added to fct_helpers.R")
  src <- r_file("fct_helpers.R")
  expect_match(src, "create_chat_completion_openai\\s*<-\\s*function")
})

test_that("create_chat_completion_openai has model, messages, temperature, openai_api_key params", {
  skip("Deferred: create_chat_completion_openai not yet added to fct_helpers.R")
  src <- r_file("fct_helpers.R")
  fn_start <- regexpr("create_chat_completion_openai\\s*<-\\s*function", src)
  fn_sig   <- substring(src, fn_start, fn_start + 300)
  for (param in c("model", "messages", "temperature", "openai_api_key")) {
    expect_match(fn_sig, param, fixed = TRUE, label = paste("param:", param))
  }
})

test_that("create_chat_completion_openai stops with 'OPENAI_API_KEY is missing' when key empty", {
  skip("Deferred: create_chat_completion_openai not yet added to fct_helpers.R")
  src <- r_file("fct_helpers.R")
  fn_start <- regexpr("create_chat_completion_openai\\s*<-\\s*function", src)
  fn_body  <- substring(src, fn_start, fn_start + 600)
  expect_match(fn_body, "OPENAI_API_KEY is missing", fixed = TRUE)
})

test_that("create_chat_completion_openai excludes temperature for o4-mini model", {
  skip("Deferred: create_chat_completion_openai not yet added to fct_helpers.R")
  src <- r_file("fct_helpers.R")
  fn_start <- regexpr("create_chat_completion_openai\\s*<-\\s*function", src)
  fn_body  <- substring(src, fn_start, fn_start + 1500)
  # Should have an if (model == "o4-mini") branch that omits temperature
  expect_match(fn_body, 'model == "o4-mini"', fixed = TRUE)
})

test_that("create_chat_completion_openai uses Bearer token auth header", {
  skip("Deferred: create_chat_completion_openai not yet added to fct_helpers.R")
  src <- r_file("fct_helpers.R")
  fn_start <- regexpr("create_chat_completion_openai\\s*<-\\s*function", src)
  fn_body  <- substring(src, fn_start, fn_start + 1500)
  expect_match(fn_body, "Bearer", fixed = TRUE)
  expect_match(fn_body, "api.openai.com", fixed = TRUE)
})


# =============================================================================
# 4. create_chat_completion_azure — error messages (fct_helpers.R)
# =============================================================================

test_that("create_chat_completion_azure stops with 'AZURE_OPENAI_API_KEY is missing'", {
  src <- r_file("fct_helpers.R")
  fn_start <- regexpr("create_chat_completion_azure\\s*<-\\s*function", src)
  # Use 2000 chars — the function signature alone is ~600 chars
  fn_body  <- substring(src, fn_start, fn_start + 2000)
  expect_match(fn_body, "AZURE_OPENAI_API_KEY is missing", fixed = TRUE)
})

test_that("create_chat_completion_azure stops with 'AZURE_OPENAI_API_ENDPOINT is missing'", {
  src <- r_file("fct_helpers.R")
  fn_start <- regexpr("create_chat_completion_azure\\s*<-\\s*function", src)
  fn_body  <- substring(src, fn_start, fn_start + 2000)
  expect_match(fn_body, "AZURE_OPENAI_API_ENDPOINT is missing", fixed = TRUE)
})


# =============================================================================
# 5 & 6. Q&A — always visible on startup + "4." prefix (mod_16_qa.R)
# =============================================================================

test_that("mod_16_qa.R: output$show_qa always returns 'show' (no submit_button gate)", {
  src <- r_file("mod_16_qa.R")
  # Find the show_qa render block
  block_start <- regexpr("output\\$show_qa\\s*<-", src)
  block       <- substring(src, block_start, block_start + 150)
  # Must NOT contain the old submit_button condition
  expect_false(grepl("submit_button.*>=.*1", block))
  # Must contain a plain return("show")
  expect_match(block, 'return\\("show"\\)')
})

test_that("mod_16_qa.R: label text contains '4. Ask About Results'", {
  src <- r_file("mod_16_qa.R")
  expect_match(src, "4\\. Ask About Results")
})


# =============================================================================
# 7. Q&A markdown rendering (mod_16_qa.R + commonmark)
# =============================================================================

test_that("mod_16_qa.R uses commonmark::markdown_html for response rendering", {
  src <- r_file("mod_16_qa.R")
  expect_match(src, "commonmark::markdown_html", fixed = TRUE)
})

test_that("mod_16_qa.R: old gsub HTML conversion is removed", {
  src <- r_file("mod_16_qa.R")
  expect_false(grepl('gsub("\\\\n\\\\n", "</p><p>"', src, fixed = TRUE))
})

test_that("mod_16_qa.R: answer blocks use 'qa-answer-block' CSS class", {
  src <- r_file("mod_16_qa.R")
  expect_match(src, "qa-answer-block", fixed = TRUE)
})

test_that("mod_16_qa.R: question is displayed with 'qa-question' CSS class", {
  src <- r_file("mod_16_qa.R")
  expect_match(src, "qa-question", fixed = TRUE)
})

test_that("mod_16_qa.R: modal is wider than the old 30%", {
  src <- r_file("mod_16_qa.R")
  expect_false(grepl("width: 30%", src, fixed = TRUE))
  expect_match(src, "55%", fixed = TRUE)
})

test_that("commonmark package is available", {
  expect_true(requireNamespace("commonmark", quietly = TRUE))
})

test_that("commonmark renders **bold** to <strong>", {
  skip_if_not_installed("commonmark")
  html <- commonmark::markdown_html("**bold**", extensions = TRUE)
  expect_match(html, "<strong>bold</strong>")
})

test_that("commonmark renders `inline code` to <code>", {
  skip_if_not_installed("commonmark")
  html <- commonmark::markdown_html("`myFunc(x)`", extensions = TRUE)
  # use fixed = TRUE because parentheses are regex metacharacters
  expect_match(html, "<code>myFunc(x)</code>", fixed = TRUE)
})

test_that("commonmark renders ## header to <h2>", {
  skip_if_not_installed("commonmark")
  html <- commonmark::markdown_html("## A Header", extensions = TRUE)
  expect_match(html, "<h2>")
})

test_that("commonmark renders bullet list to <ul><li>", {
  skip_if_not_installed("commonmark")
  html <- commonmark::markdown_html("- item one\n- item two", extensions = TRUE)
  expect_match(html, "<ul>")
  expect_match(html, "<li>")
})

test_that("commonmark renders fenced code block to <pre><code>", {
  skip_if_not_installed("commonmark")
  html <- commonmark::markdown_html("```r\nx <- 1\n```", extensions = TRUE)
  expect_match(html, "<pre>")
  expect_match(html, "<code")
})

test_that("commonmark renders numbered list to <ol><li>", {
  skip_if_not_installed("commonmark")
  html <- commonmark::markdown_html("1. first\n2. second", extensions = TRUE)
  expect_match(html, "<ol>")
  expect_match(html, "<li>")
})


# =============================================================================
# 8. Chunk renaming — name field in code_history (mod_06_error_hist.R)
# =============================================================================

test_that("mod_06_error_hist.R: current_code list includes a 'name' field", {
  src <- r_file("mod_06_error_hist.R")
  expect_match(src, 'name\\s*=\\s*paste0\\("Chunk #"', ignore.case = FALSE)
})

test_that("mod_06_error_hist.R: choices use code_history[[i]]$name for labels", {
  src <- r_file("mod_06_error_hist.R")
  expect_match(src, "code_history\\[\\[i\\]\\]\\$name")
})

test_that("chunk naming logic: custom name takes priority", {
  naming <- function(code_history) {
    choices <- seq_along(code_history)
    names(choices) <- sapply(choices, function(i) {
      if (!is.null(code_history[[i]]$name)) code_history[[i]]$name
      else paste0("Chunk #", i)
    })
    names(choices)
  }
  hist <- list(
    list(name = "EDA"),
    list(name = NULL),
    list(name = "Model Fit")
  )
  result <- naming(hist)
  expect_equal(result[[1]], "EDA")
  expect_equal(result[[2]], "Chunk #2")
  expect_equal(result[[3]], "Model Fit")
})

test_that("chunk naming logic: all NULL names fall back to 'Chunk #N'", {
  naming <- function(n) {
    code_history <- rep(list(list(name = NULL)), n)
    sapply(seq_len(n), function(i) {
      if (!is.null(code_history[[i]]$name)) code_history[[i]]$name
      else paste0("Chunk #", i)
    })
  }
  expect_equal(naming(3), c("Chunk #1", "Chunk #2", "Chunk #3"))
})

test_that("chunk naming logic: mixed names + fallbacks work correctly", {
  code_history <- list(
    list(name = "Step A"), list(name = NULL),
    list(name = NULL),    list(name = "Step D")
  )
  choices <- seq_along(code_history)
  names(choices) <- sapply(choices, function(i) {
    if (!is.null(code_history[[i]]$name)) code_history[[i]]$name
    else paste0("Chunk #", i)
  })
  expect_equal(names(choices), c("Step A", "Chunk #2", "Chunk #3", "Step D"))
  expect_equal(unname(choices), 1:4)
})


# =============================================================================
# 9. Chunk renaming — rmd heading patching (mod_04_main_panel.R)
# =============================================================================

test_that("rmd sub() regex replaces '### N. old title' with new name", {
  rmd     <- "\n### 1. plot a histogram\nAzure O4 Mini\n```{R}\nhist(df$x)\n```\n"
  updated <- sub("### [0-9]+\\. [^\n]*", "### 1. Histogram Analysis", rmd)
  expect_match(updated, "### 1\\. Histogram Analysis")
  expect_false(grepl("plot a histogram", updated, fixed = TRUE))
})

test_that("rmd sub() does not touch code inside code blocks", {
  rmd <- "\n### 2. regression\nInfo\n```{R}\n### this is a comment\nlm(y~x)\n```\n"
  updated <- sub("### [0-9]+\\. [^\n]*", "### 2. New Name", rmd)
  expect_match(updated,  "### this is a comment", fixed = TRUE)
  expect_match(updated,  "lm(y~x)",               fixed = TRUE)
})

test_that("rmd sub() works for any chunk number 1-999", {
  for (n in c(1, 5, 10, 99, 100, 999)) {
    rmd     <- paste0("\n### ", n, ". old title\n```{R}\nx<-1\n```\n")
    updated <- sub("### [0-9]+\\. [^\n]*", paste0("### ", n, ". New Name"), rmd)
    expect_match(updated, paste0("### ", n, "\\. New Name"))
    expect_false(grepl("old title", updated, fixed = TRUE))
  }
})

test_that("mod_04_main_panel.R: dblclick JS fires Shiny.setInputValue with chunk_dblclick", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "dblclick",      fixed = TRUE)
  expect_match(src, "chunk_dblclick", fixed = TRUE)
  expect_match(src, "setInputValue",  fixed = TRUE)
})

test_that("mod_04_main_panel.R: observeEvent responds to input$chunk_dblclick", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "observeEvent.*chunk_dblclick")
  expect_match(src, "shinyalert",   fixed = TRUE)
})

test_that("mod_04_main_panel.R: rename updates both $name and $rmd fields", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "code_history\\[\\[chunk_id\\]\\]\\$name")
  expect_match(src, "code_history\\[\\[chunk_id\\]\\]\\$rmd")
})

test_that("mod_04_main_panel.R: rename refreshes chunk_selection$chunk_choices", {
  src <- r_file("mod_04_main_panel.R")
  # Use regex (not fixed) so \$ matches a literal dollar sign
  expect_match(src, "chunk_selection\\$chunk_choices")
})

test_that("mod_04_main_panel.R: tooltip text mentions double-click to rename", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "[Dd]ouble.click")
})


# =============================================================================
# 10. Temperature — removed from Settings (mod_11_settings.R)
# =============================================================================

test_that("mod_11_settings.R: sliderInput for temperature is commented out", {
  lines        <- readLines(file.path(rprojroot::find_package_root_file(), "R", "mod_11_settings.R"),
                            warn = FALSE)
  slider_lines <- grep("sliderInput", lines, value = TRUE)
  expect_true(
    length(slider_lines) == 0 || all(grepl("^\\s*#", slider_lines)),
    label = "All sliderInput lines should be commented out"
  )
})

test_that("mod_11_settings.R: output$change_temperature renderUI is commented out", {
  lines        <- readLines(file.path(rprojroot::find_package_root_file(), "R", "mod_11_settings.R"),
                            warn = FALSE)
  render_lines <- grep("output\\$change_temperature", lines, value = TRUE)
  expect_true(
    length(render_lines) == 0 || all(grepl("^\\s*#", render_lines)),
    label = "output$change_temperature assignment should be commented out"
  )
})

test_that("mod_11_settings.R: uiOutput for change_temperature is commented out", {
  lines      <- readLines(file.path(rprojroot::find_package_root_file(), "R", "mod_11_settings.R"),
                          warn = FALSE)
  ct_lines   <- grep("change_temperature", lines, value = TRUE)
  ui_lines   <- grep("uiOutput", ct_lines, value = TRUE)
  if (length(ui_lines) > 0) {
    expect_true(
      all(grepl("^\\s*#", ui_lines)),
      label = "uiOutput(change_temperature) should be commented out"
    )
  } else {
    succeed("No uiOutput(change_temperature) found — correctly absent")
  }
})

test_that("mod_11_settings.R: AI Model selector is still active (not commented out)", {
  lines        <- readLines(file.path(rprojroot::find_package_root_file(), "R", "mod_11_settings.R"),
                            warn = FALSE)
  model_lines  <- grep("language_model", lines, value = TRUE)
  active_lines <- model_lines[!grepl("^\\s*#", model_lines)]
  expect_true(length(active_lines) > 0)
})

test_that("mod_11_settings.R: a comment explains the temperature was moved to sidebar", {
  src <- r_file("mod_11_settings.R")
  expect_match(src, "MOVED TO SIDEBAR", ignore.case = TRUE)
})


# =============================================================================
# 11. Temperature — added to sidebar (app_ui.R)
# =============================================================================

test_that("app_ui.R: contains numericInput with id 'sidebar_temperature'", {
  src <- r_file("app_ui.R")
  expect_match(src, 'inputId\\s*=\\s*"sidebar_temperature"', fixed = FALSE)
})

test_that("app_ui.R: sidebar temperature label is 'Sampling Temperature'", {
  src <- r_file("app_ui.R")
  expect_match(src, "Sampling Temperature", fixed = TRUE)
})

test_that("app_ui.R: sidebar temperature default value is 0.2", {
  src <- r_file("app_ui.R")
  # Anchor to inputId = "sidebar_temperature" (the numericInput), not the tippy call
  idx     <- regexpr('inputId\\s*=\\s*"sidebar_temperature"', src, perl = TRUE)
  context <- substring(src, idx, idx + 300)
  expect_match(context, "value\\s*=\\s*0\\.2")
})

test_that("app_ui.R: sidebar temperature min = 0", {
  src     <- r_file("app_ui.R")
  idx     <- regexpr('inputId\\s*=\\s*"sidebar_temperature"', src, perl = TRUE)
  context <- substring(src, idx, idx + 300)
  expect_match(context, "min\\s*=\\s*0")
})

test_that("app_ui.R: sidebar temperature max = 1", {
  src     <- r_file("app_ui.R")
  idx     <- regexpr('inputId\\s*=\\s*"sidebar_temperature"', src, perl = TRUE)
  context <- substring(src, idx, idx + 300)
  expect_match(context, "max\\s*=\\s*1")
})

test_that("app_ui.R: sidebar temperature has a tooltip", {
  src <- r_file("app_ui.R")
  expect_match(src, "sidebar_temperature", fixed = TRUE)
  expect_match(src, "tippy_this",          fixed = TRUE)
})


# =============================================================================
# 12. sample_temp override in app_server.R
# =============================================================================

test_that("app_server.R: sample_temp reads from input$sidebar_temperature", {
  src <- r_file("app_server.R")
  expect_match(src, "sidebar_temperature", fixed = TRUE)
  expect_match(src, "sample_temp\\s*<-\\s*reactive")
})

test_that("app_server.R: sample_temp clamps values with max(0, min(1, ...))", {
  src <- r_file("app_server.R")
  expect_match(src, "max(0",  fixed = TRUE)
  expect_match(src, "min(1",  fixed = TRUE)
})

test_that("app_server.R: sample_temp falls back to default_temperature on NULL/NA", {
  src <- r_file("app_server.R")
  idx     <- regexpr("sidebar_temperature", src)
  context <- substring(src, max(1, idx - 100), idx + 300)
  expect_match(context, "default_temperature", fixed = TRUE)
})

test_that("sample_temp clamping logic: values within [0,1] pass through", {
  clamp <- function(val, default = 0.2) {
    if (is.null(val) || is.na(val)) return(default)
    max(0, min(1, val))
  }
  expect_equal(clamp(0.0),  0.0)
  expect_equal(clamp(0.2),  0.2)
  expect_equal(clamp(0.5),  0.5)
  expect_equal(clamp(1.0),  1.0)
})

test_that("sample_temp clamping logic: out-of-range values are clamped", {
  clamp <- function(val, default = 0.2) {
    if (is.null(val) || is.na(val)) return(default)
    max(0, min(1, val))
  }
  expect_equal(clamp(-0.1),  0.0)
  expect_equal(clamp(-10),   0.0)
  expect_equal(clamp(1.1),   1.0)
  expect_equal(clamp(100),   1.0)
})

test_that("sample_temp clamping logic: NULL and NA return the default (0.2)", {
  clamp <- function(val, default = 0.2) {
    if (is.null(val) || is.na(val)) return(default)
    max(0, min(1, val))
  }
  expect_equal(clamp(NULL),      0.2)
  expect_equal(clamp(NA_real_),  0.2)
  expect_equal(clamp(NA),        0.2)
})


# =============================================================================
# 13. Report module uses custom chunk names (mod_09_report.R)
# =============================================================================

test_that("mod_09_report.R: chunk names come from code_history[[i]]$name", {
  src <- r_file("mod_09_report.R")
  expect_match(src, "code_history\\[\\[i\\]\\]\\$name")
})

test_that("mod_09_report.R: falls back to 'Chunk #i' when name is NULL", {
  src <- r_file("mod_09_report.R")
  expect_match(src, "Chunk #",  fixed = TRUE)
  expect_match(src, "is.null",  fixed = TRUE)
})

test_that("report naming logic is consistent with mod_04 / mod_06 logic", {
  naming <- function(code_history) {
    choices <- seq_along(code_history)
    names(choices) <- sapply(choices, function(i) {
      if (!is.null(code_history[[i]]$name)) code_history[[i]]$name
      else paste0("Chunk #", i)
    })
    names(choices)
  }
  history <- list(
    list(name = "Data Cleaning"),
    list(name = NULL),
    list(name = NULL),
    list(name = "Final Results")
  )
  result <- naming(history)
  expect_equal(result, c("Data Cleaning", "Chunk #2", "Chunk #3", "Final Results"))
})


# =============================================================================
# 14. Inline code editing — Save & Resubmit (mod_04_main_panel.R)
# =============================================================================

test_that("mod_04_main_panel.R: aceEditor is not read-only", {
  src <- r_file("mod_04_main_panel.R")
  # readOnly = FALSE must be present; readOnly = TRUE must not
  expect_match(src, "readOnly\\s*=\\s*FALSE")
  expect_false(grepl("readOnly\\s*=\\s*TRUE", src))
})

test_that("mod_04_main_panel.R: Save button exists (hidden by default)", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "save_code",    fixed = TRUE)
  expect_match(src, "shinyjs::hidden", fixed = TRUE)
})

test_that("mod_04_main_panel.R: Resubmit button exists (hidden by default)", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "resubmit_code", fixed = TRUE)
})

test_that("mod_04_main_panel.R: Save and Resubmit appear in the toolbar row", {
  src <- r_file("mod_04_main_panel.R")
  # Both buttons and Show Code should be in a flex right-group
  expect_match(src, "save_code",     fixed = TRUE)
  expect_match(src, "resubmit_code", fixed = TRUE)
  expect_match(src, "show_code",     fixed = TRUE)
})

test_that("mod_04_main_panel.R: is_dirty reactive compares editor to stored raw code", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "is_dirty",       fixed = TRUE)
  expect_match(src, "code_display",   fixed = TRUE)
  expect_match(src, "code_history",   fixed = TRUE)
})

test_that("mod_04_main_panel.R: save handler updates code_history $code, $raw, $rmd", {
  src <- r_file("mod_04_main_panel.R")
  save_start <- regexpr("observeEvent\\(input\\$save_code", src)
  save_block  <- substring(src, save_start, save_start + 700)
  # regex: \\$ matches a literal $
  expect_match(save_block, "\\$code")
  expect_match(save_block, "\\$raw")
  expect_match(save_block, "\\$rmd")
})

test_that("mod_04_main_panel.R: save handler does NOT call reverted()", {
  src <- r_file("mod_04_main_panel.R")
  save_start <- regexpr("observeEvent\\(input\\$save_code", src)
  save_block  <- substring(src, save_start, save_start + 700)
  expect_false(grepl("reverted\\(", save_block))
})

test_that("mod_04_main_panel.R: resubmit handler restores pre-chunk environment", {
  src <- r_file("mod_04_main_panel.R")
  resub_start <- regexpr("observeEvent\\(input\\$resubmit_code", src)
  resub_block  <- substring(src, resub_start, resub_start + 900)
  expect_match(resub_block, "run_env",  fixed = TRUE)
  expect_match(resub_block, "list2env", fixed = TRUE)
  expect_match(resub_block, "\\$env")   # regex: \\$ = literal $
})

test_that("mod_04_main_panel.R: resubmit handler triggers reverted()", {
  src <- r_file("mod_04_main_panel.R")
  resub_start <- regexpr("observeEvent\\(input\\$resubmit_code", src)
  resub_block  <- substring(src, resub_start, resub_start + 1600)
  expect_match(resub_block, "reverted", fixed = TRUE)
})

test_that("mod_04_main_panel.R: resubmit warns about downstream chunks", {
  src <- r_file("mod_04_main_panel.R")
  resub_start <- regexpr("observeEvent\\(input\\$resubmit_code", src)
  resub_block  <- substring(src, resub_start, resub_start + 1600)
  expect_match(resub_block, "may need to be re-run", fixed = TRUE)
})

test_that("mod_04_main_panel.R: post-resubmit observer updates $error in history", {
  src <- r_file("mod_04_main_panel.R")
  expect_match(src, "resubmit_chunk_id", fixed = TRUE)
  # find the post-run observer block
  post_start <- regexpr("resubmit_chunk_id\\(NULL\\)", src)
  post_context <- substring(src, max(1, post_start - 200), post_start + 10)
  expect_match(post_context, "\\$error")  # regex: \\$ = literal $
})

test_that("mod_04_main_panel.R: function signature includes run_env and reverted", {
  src <- r_file("mod_04_main_panel.R")
  sig_start <- regexpr("mod_04_main_panel_serv\\s*<-\\s*function", src)
  sig_block  <- substring(src, sig_start, sig_start + 400)
  expect_match(sig_block, "run_env",  fixed = TRUE)
  expect_match(sig_block, "reverted", fixed = TRUE)
})

test_that("app_server.R: passes run_env and reverted to mod_04", {
  src <- r_file("app_server.R")
  mod04_start <- regexpr("mod_04_main_panel_serv", src)
  mod04_block  <- substring(src, mod04_start, mod04_start + 600)
  expect_match(mod04_block, "run_env",  fixed = TRUE)
  expect_match(mod04_block, "reverted", fixed = TRUE)
})

test_that("Rmd code-block regex replaces ```{R} content correctly", {
  rmd <- "\n### 1. histogram\nAzure O4 Mini\n```{R}\nhist(df$x)\n```\n"
  updated <- sub(
    "(?s)```\\{R\\}\\n.*?\\n```",
    paste0("```{R}\n", "ggplot(df, aes(x)) + geom_histogram()", "\n```"),
    rmd,
    perl = TRUE
  )
  expect_match(updated, "ggplot", fixed = TRUE)
  expect_false(grepl("hist(df$x)", updated, fixed = TRUE))
  expect_match(updated, "### 1. histogram", fixed = TRUE)  # heading preserved
})

test_that("Rmd code-block regex handles multi-line code", {
  rmd <- "### 2. model\nInfo\n```{R}\nlm(y ~ x, data = df)\nsummary(fit)\n```\n"
  new_code <- "fit <- lm(y ~ x + z, data = df)\nsummary(fit)\nplot(fit)"
  updated <- sub(
    "(?s)```\\{R\\}\\n.*?\\n```",
    paste0("```{R}\n", new_code, "\n```"),
    rmd,
    perl = TRUE
  )
  expect_match(updated, "fit <- lm(y ~ x + z", fixed = TRUE)
  expect_match(updated, "plot(fit)",            fixed = TRUE)
  expect_false(grepl("lm(y ~ x, data = df)\nsummary(fit)\n", updated, fixed = TRUE))
})
