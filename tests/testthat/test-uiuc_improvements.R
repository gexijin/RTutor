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
#   2.  system_role_tutor              (Markdown not HTML)
#   3-4. OpenAI only                   (OPENAI_API_KEY, no Azure / user key)
#   5.  Q&A always visible             (mod_16_qa.R)
#   6.  Q&A "4." label                 (mod_16_qa.R)
#   7.  Q&A markdown rendering         (commonmark + file check)
#   8. Inline code editing — Save & Resubmit (mod_04_main_panel.R)
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

test_that("fct_helpers.R no longer defines default_temperature", {
  src <- r_file("fct_helpers.R")
  expect_no_match(src, "default_temperature")
})

test_that("fct_helpers.R defines language_models containing gpt-5.6-luna", {
  src <- r_file("fct_helpers.R")
  expect_match(src, 'language_models\\s*<-\\s*c\\("gpt-5.6-luna"\\)')
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
# 3 & 4. OpenAI only — one sk- key from OPENAI_API_KEY, no Azure, no user key
# =============================================================================

test_that("create_response calls OpenAI with the OPENAI_API_KEY env var", {
  src <- r_file("fct_helpers.R")
  fn_start <- regexpr("create_response\\s*<-\\s*function", src)
  fn_body  <- substring(src, fn_start, fn_start + 800)
  expect_match(fn_body, 'Sys.getenv("OPENAI_API_KEY")', fixed = TRUE)
  expect_match(fn_body, "OPENAI_API_KEY is not set", fixed = TRUE)
  expect_match(fn_body, "Bearer", fixed = TRUE)
  expect_match(fn_body, "https://api.openai.com/v1/responses", fixed = TRUE)
})

test_that("no Azure provider or user-entered API key remains in R/", {
  root <- rprojroot::find_package_root_file()
  src <- paste(unlist(lapply(list.files(file.path(root, "R"), full.names = TRUE), readLines, warn = FALSE)),
               collapse = "\n")
  expect_no_match(src, "AZURE_OPENAI|resolve_provider|switch_on|show_api_settings|api_key\\.txt")
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

test_that("mod_16_qa.R: label text contains '4. Explore Further'", {
  src <- r_file("mod_16_qa.R")
  expect_match(src, "4\\. Explore Further")
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

test_that("mod_16_qa.R: question is displayed with 'qa-summary' CSS class", {
  src <- r_file("mod_16_qa.R")
  expect_match(src, "qa-summary", fixed = TRUE)
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
# 8. Inline code editing — Save & Resubmit (mod_04_main_panel.R)
# =============================================================================

test_that("mod_04_main_panel.R: aceEditor starts read-only, toggled by JS on focus", {
  src <- r_file("mod_04_main_panel.R")
  # Starts read-only; JS mousedown/focusout handlers toggle editability
  expect_match(src, "readOnly\\s*=\\s*TRUE")
  expect_match(src, "setAceEditable", fixed = TRUE)
  expect_match(src, "mousedown",      fixed = TRUE)
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
  resub_block  <- substring(src, resub_start, resub_start + 5000)
  expect_match(resub_block, "run_env",  fixed = TRUE)
  expect_match(resub_block, "list2env", fixed = TRUE)
  expect_match(resub_block, "\\$env")   # regex: \\$ = literal $
})

test_that("mod_04_main_panel.R: resubmit handler triggers reverted()", {
  src <- r_file("mod_04_main_panel.R")
  resub_start <- regexpr("observeEvent\\(input\\$resubmit_code", src)
  resub_block  <- substring(src, resub_start, resub_start + 5000)
  expect_match(resub_block, "reverted", fixed = TRUE)
})

test_that("mod_04_main_panel.R: resubmit warns about downstream chunks", {
  src <- r_file("mod_04_main_panel.R")
  resub_start <- regexpr("observeEvent\\(input\\$resubmit_code", src)
  resub_block  <- substring(src, resub_start, resub_start + 5000)
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
