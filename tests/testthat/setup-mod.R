# tests/testthat/setup-mod.R  (auto-run by testthat)
options(shiny.testmode = TRUE, stringsAsFactors = FALSE)
set.seed(123)

tp <- if (utils::packageVersion("testthat") >= "3.0.0") testthat::test_path() else getwd()
project_root <- normalizePath(file.path(tp, "..", ".."), mustWork = TRUE)

# Ensure demo exists in BOTH places
inst_demo <- file.path(project_root, "inst", "app", "www", "demo_questions.csv")
app_demo  <- file.path(project_root, "app",  "www", "demo_questions.csv")

dir.create(dirname(inst_demo), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(app_demo),  recursive = TRUE, showWarnings = FALSE)

if (!file.exists(inst_demo)) {
  utils::write.csv(data.frame(question = "demo question"), inst_demo, row.names = FALSE)
}
if (!file.exists(app_demo)) {
  ok <- tryCatch(file.copy(inst_demo, app_demo, overwrite = TRUE),
                 error = function(e) FALSE)
  if (!ok) {
    utils::write.csv(data.frame(question = "demo question"), app_demo, row.names = FALSE)
  }
}

# Stub app_sys() to be lenient about arguments/ordering
if (!exists("app_sys", mode = "function")) {
  app_sys <- function(...) {
    parts <- unlist(list(...), use.names = FALSE)
    # Normalize case/whitespace
    parts <- trimws(parts)
    # If any element ends with demo_questions.csv, map to the known file
    if (any(grepl("demo_questions\\.csv$", parts, ignore.case = TRUE))) {
      return(inst_demo)
    }
    # Otherwise, assemble a path under the project root
    file.path(project_root, parts)
  }
  assign("app_sys", app_sys, envir = globalenv())
}

# Source helpers/modules (helpers first)
src <- function(fname) {
  f <- file.path(project_root, "R", fname)
  if (!file.exists(f)) stop("Cannot find: ", f)
  sys.source(f, envir = globalenv())
  message("Sourced: ", f)
}

src("fct_helpers.R")
src("mod_02_load_data.R")
src("mod_04_main_panel.R")
src("mod_05_llms.R")
src("mod_06_error_hist.R")
src("mod_07_run_code.R")
src("mod_09_report.R")
src("mod_10_eda.R")
