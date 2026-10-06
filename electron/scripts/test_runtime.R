# test_runtime.R: CI pre-flight check of the bundled R runtime, run the way bootstrap.R
# runs it but without starting Shiny. Catches broken bundles in CI instead of on a
# student's laptop.
#
# Usage:  <bundled Rscript> --vanilla test_runtime.R <bundled library>
# Expects RSTUDIO_PANDOC to point at the bundled pandoc, as main.js sets it.

lib <- commandArgs(trailingOnly = TRUE)[1]
.libPaths(lib)
cat("[test] .libPaths() =", paste(.libPaths(), collapse = " | "), "\n")

# Fail if this is the runner's R rather than the bundled one.
expected <- normalizePath(dirname(lib), winslash = "/", mustWork = TRUE)
actual <- normalizePath(R.home(), winslash = "/", mustWork = TRUE)
if (!identical(actual, expected)) stop("[test] FAIL: ran the wrong R. expected ", expected, " got ", actual)
cat("[test] OK: running the bundled R", as.character(getRversion()), "at", actual, "\n")

# The app, plus a sample of what generated code relies on.
required <- c("RTutor", "shiny", "rmarkdown", "tidyverse", "plotly", "canvasXpress", "DT",
              "caret", "randomForest", "ggpubr", "lme4", "emmeans", "car", "gtsummary", "forecast")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("[test] FAIL: package '", pkg, "' does not load")
  cat("[test] OK: requireNamespace(", pkg, ")\n", sep = "")
}

# The app object builds in desktop mode.
Sys.setenv(RTUTOR_DESKTOP = "1", OPENAI_API_KEY = "sk-test-not-a-real-key")
if (!RTutor:::on_desktop()) stop("[test] FAIL: RTUTOR_DESKTOP not detected")
app <- RTutor::run_app()
if (!inherits(app, "shiny.appobj")) stop("[test] FAIL: RTutor::run_app() did not return a shiny.appobj")
cat("[test] OK: RTutor::run_app() returned a shiny.appobj\n")

# Reports need the bundled pandoc: render a small R Markdown file end to end.
if (!rmarkdown::pandoc_available()) stop("[test] FAIL: bundled pandoc not found (RSTUDIO_PANDOC)")
rmd <- tempfile(fileext = ".Rmd")
writeLines(c("---", "title: test", "output: html_document", "---", "", "```{r}", "summary(cars)", "```"), rmd)
html <- rmarkdown::render(rmd, quiet = TRUE)
if (!file.exists(html)) stop("[test] FAIL: R Markdown did not render")
cat("[test] OK: rendered R Markdown with pandoc", as.character(rmarkdown::pandoc_version()), "\n")

# summarytools imports tcltk, which on macOS needs CRAN's optional Tcl/Tk under /opt/R.
# Only the EDA tab uses it, and that tab is hidden in UIUC RTutor, so warn instead of failing.
if (!requireNamespace("summarytools", quietly = TRUE)) {
  cat("::warning::summarytools does not load in the bundled R (likely missing Tcl/Tk).",
      "Generated code that calls summarytools will fail on this platform.\n")
}

cat("[test] PASS: bundled R runtime is healthy\n")
