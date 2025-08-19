# from the project root
# install.packages("covr")        # if not installed
# install.packages("devtools")    # if not installed
library(devtools)
library(covr)

# Make sure your code is loaded into the session (no package install needed)
devtools::load_all(".")

# Point covr at the specific source file(s) and test file(s)
cov <- covr::file_coverage(
  source_files = c("R/mod_04_main_panel.R", "R/fct_helpers.R", "R/mod_02_load_data.R", "R/mod_05_llms.R", "R/mod_06_error_hist.R", "R/mod_07_run_code.R", "R/mod_09_report.R", "R/mod_10_eda.R"),
  test_files   = c("tests/testthat/test-mod2.R", "tests/testthat/test-mod4.R", "tests/testthat/test-mod5.R", "tests/testthat/test-mod6.R", "tests/testthat/test-mod7.R", "tests/testthat/test-mod9.R", "tests/testthat/test-mod10.R")
)

# View coverage summary & HTML report
cov
covr::report(cov)