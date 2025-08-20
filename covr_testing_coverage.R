# from the project root
# install.packages("covr")        # if not installed
# install.packages("devtools")    # if not installed
library(devtools)
library(covr)


# Covr uses a different app_sys() than the main app, must copy demo_questions to new dir to work properly
if (file.exists("inst/app/www/demo_questions.csv") && !file.exists("app/www/demo_questions.csv")) {
  dir.create("app/www", recursive = TRUE, showWarnings = FALSE)
  file.copy("inst/app/www/demo_questions.csv", "app/www/demo_questions.csv")
  cat("Created app/www/demo_questions.csv for testing\n")
}


# Make sure your code is loaded into the session (no package install needed)
devtools::load_all(".")

# Point covr at the specific source file(s) and test file(s)
cov <- covr::file_coverage(
  source_files = c("R/mod_04_main_panel.R", "R/fct_helpers.R", "R/mod_02_load_data.R", "R/mod_05_llms.R", "R/mod_06_error_hist.R", "R/mod_07_run_code.R", "R/mod_09_report.R", "R/mod_10_eda.R"),
  test_files   = c("tests/testthat/test-mod2.R", "tests/testthat/test-mod4.R", "tests/testthat/test-mod5.R", "tests/testthat/test-mod6.R", "tests/testthat/test-mod7.R", "tests/testthat/test-mod9.R", "tests/testthat/test-mod10.R")
)

# View coverage summary & HTML report
cov

# Create timestamp for unique filenames
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Save to coverage_reports directory with timestamp (Option 3)
dir.create("coverage_reports", showWarnings = FALSE)
report_path <- file.path("coverage_reports", paste0("coverage_", timestamp, ".html"))
covr::report(cov, file = report_path)
cat("Coverage report saved to:", report_path, "\n")