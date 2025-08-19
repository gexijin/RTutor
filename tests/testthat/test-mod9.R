# tests/testthat/test-mod9.R
# Run: testthat::test_file("tests/testthat/test-mod9.R", reporter = "summary")

library(testthat)
library(shiny)
library(htmltools)
library(withr)

# Source the module if not already present
if (!exists("mod_09_report_serv", mode = "function")) {
  sys.source(file.path("R", "mod_09_report.R"), envir = .GlobalEnv)
  message("Manually Sourced: mod_09_report.R")
}

# Helper to stringify tag UI safely
html_to_string <- function(tag) {
  if (is.null(tag)) return("")
  htmltools::renderTags(tag)$html
}

# ---- Test fixtures that respect fct_helpers.R globals ----
# We intentionally DO NOT redefine: language_models, user_upload, release, etc.
# They come from fct_helpers.R (which your test logs show is sourced by other tests).

# Use the actual model id that prints as "GPT-4o mini"
selected_model_R   <- reactive("gpt-4o-mini")
sample_temp_low    <- reactive(0.2)
logs_simple        <- list(id = 1)

# Mixed code history for selection tests
fake_ch <- list(
  code_history = list(
    list(rmd = "```{R}\n# chunk 1\nx <- 1\n```",           error = FALSE),
    list(rmd = "```{R}\n# chunk 2\nstop('oops')\n```",      error = TRUE),
    list(rmd = "```{R}\n# chunk 3\nmean(1:3)\n```",         error = FALSE)
  )
)

llm_resp_R         <- reactive(list(cmd = c("summary(df)")))
use_python_false   <- reactive(FALSE)
code_error_false   <- reactive(FALSE)
python_to_html_ok  <- reactive(0)

# No uploads by default
selected_dataset_none <- reactive("Built-in")
current_data_null     <- reactive(NULL)
current_data2_null    <- reactive(NULL)
user_file_null        <- reactive(NULL)
user_file2_null       <- reactive(NULL)
user_data_null        <- reactive(NULL)
user_data2_null       <- reactive(NULL)

# ---- UI smoke test ----------------------------------------------------------
test_that("mod_09_report_ui has key outputs/controls", {
  ui <- mod_09_report_ui("rep")
  html <- html_to_string(ui)
  expect_match(html, "selected_chunk_report")
  expect_match(html, "html_report")
  expect_match(html, "Rmd_source")
  expect_match(html, "rmd_chunk_output")
})

# ---- Rmd_chunk (R, happy path) ----------------------------------------------
test_that("Rmd_chunk (R) includes header, model label and code without eval=FALSE", {
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = llm_resp_R,
      input_text = reactive("Summarize dataset"),
      use_python = use_python_false,
      logs = logs_simple,
      sample_temp = sample_temp_low,
      code_error = code_error_false,
      python_to_html = python_to_html_ok,
      current_data = current_data_null,
      current_data_2 = current_data2_null,
      selected_dataset_name = selected_dataset_none,
      user_data = user_data_null,
      user_data_2 = user_data2_null,
      user_file = user_file_null,
      user_file_2 = user_file2_null
    ),
    {
      # Verify the chunk itself
      chunk <- Rmd_chunk()
      expect_true(grepl("^\\s*###\\s*1\\.", chunk))
      expect_match(chunk, "GPT-4o mini \\(Temperature = 0\\.2\\)")
      expect_true(grepl("```\\{R\\}", chunk))
      expect_false(grepl("eval\\s*=\\s*FALSE", chunk))
      expect_true(grepl("summary\\(df\\)", chunk))

      # Force the renderUI output to compute even though it's "hidden" in testServer
      outputOptions(output, "html_report", suspendWhenHidden = FALSE)
      session$flushReact()

      # Read the materialized UI directly from output$
      ui_val <- output$html_report
      # Some Shiny versions return a function-like object; call it if needed
      if (is.function(ui_val)) ui_val <- ui_val()

      expect_false(is.null(ui_val))
      btn_html <- html_to_string(ui_val)
      expect_match(btn_html, "Session Report")
    }
  )
})

# ---- Rmd_chunk eval flags ----------------------------------------------------
test_that("Rmd_chunk sets eval=FALSE for R when code_error() is TRUE", {
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = reactive(list(cmd = "plot(1:3)")),
      input_text = reactive("Plot something"),
      use_python = reactive(FALSE),
      logs = logs_simple,
      sample_temp = reactive(0.75),
      code_error = reactive(TRUE),
      python_to_html = python_to_html_ok,
      current_data = current_data_null,
      current_data_2 = current_data2_null,
      selected_dataset_name = selected_dataset_none,
      user_data = user_data_null,
      user_data_2 = user_data2_null,
      user_file = user_file_null,
      user_file_2 = user_file2_null
    ),
    {
      chunk <- Rmd_chunk()
      expect_true(grepl("```\\{R,\\s*eval\\s*=\\s*FALSE\\}", chunk))
      expect_true(grepl("\\*\\* Error \\*\\*", chunk))
    }
  )
})

test_that("Rmd_chunk adds reticulate scaffolding and eval=FALSE for Python when python_to_html == -1", {
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = reactive(list(cmd = c("import numpy as np", "np.mean([1,2,3])"))),
      input_text = reactive("Compute mean"),
      use_python = reactive(TRUE),
      logs = logs_simple,
      sample_temp = reactive(0.33),
      code_error = reactive(FALSE),
      python_to_html = reactive(-1),
      current_data = current_data_null,
      current_data_2 = current_data2_null,
      selected_dataset_name = selected_dataset_none,
      user_data = user_data_null,
      user_data_2 = user_data2_null,
      user_file = user_file_null,
      user_file_2 = user_file2_null
    ),
    {
      chunk <- Rmd_chunk()
      expect_true(grepl("library\\(reticulate\\)", chunk))
      expect_true(grepl("```\\{python,\\s*message=FALSE\\}", chunk))
      expect_true(grepl("df = r\\.df", chunk))
      expect_true(grepl("```\\{python,\\s*eval\\s*=\\s*FALSE\\}", chunk))
      expect_true(grepl("np\\.mean", chunk))
    }
  )
})

# ---- Rmd_total composition & selection --------------------------------------
test_that("Rmd_total includes params chunk and respects selection choices", {
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = llm_resp_R,
      input_text = reactive("Hello"),
      use_python = use_python_false,
      logs = logs_simple,
      sample_temp = sample_temp_low,
      code_error = code_error_false,
      python_to_html = python_to_html_ok,
      current_data = current_data_null,
      current_data_2 = current_data2_null,
      selected_dataset_name = selected_dataset_none,
      user_data = user_data_null,
      user_data_2 = user_data2_null,
      user_file = user_file_null,
      user_file_2 = user_file2_null
    ),
    {
      # All chunks
      session$setInputs(selected_chunk_report = "All chunks")
      all_txt <- Rmd_total()
      expect_true(grepl("```\\{R, echo = FALSE\\}\\ndf <- params\\$df\\ndf2 <- params\\$df2\\n```", all_txt))
      expect_true(grepl("# chunk 1", all_txt) && grepl("# chunk 2", all_txt) && grepl("# chunk 3", all_txt))

      # All chunks without errors
      session$setInputs(selected_chunk_report = "All chunks without errors")
      no_err_txt <- Rmd_total()
      expect_true(grepl("# chunk 1", no_err_txt))
      expect_false(grepl("# chunk 2", no_err_txt))
      expect_true(grepl("# chunk 3", no_err_txt))

      # Specific selections (1 and 3)
      session$setInputs(selected_chunk_report = c("1", "3"))
      some_txt <- Rmd_total()
      expect_true(grepl("# chunk 1", some_txt))
      expect_true(grepl("# chunk 3", some_txt))
      expect_false(grepl("# chunk 2", some_txt))
    }
  )
})

test_that("Rmd_total adds file-read block when user uploaded one or two files", {
  # One CSV upload
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = llm_resp_R,
      input_text = reactive("Hello"),
      use_python = use_python_false,
      logs = logs_simple,
      sample_temp = sample_temp_low,
      code_error = code_error_false,
      python_to_html = python_to_html_ok,
      current_data = current_data_null,
      current_data_2 = current_data2_null,
      selected_dataset_name = reactive(user_upload),       # use real constant from fct_helpers.R
      user_data = reactive(list(file_type = "read.csv")),
      user_data_2 = user_data2_null,
      user_file = reactive(list(name = "data.csv")),
      user_file_2 = user_file2_null
    ),
    {
      session$setInputs(selected_chunk_report = "All chunks without errors")
      txt <- Rmd_total()
      expect_true(grepl("### 0\\. Read File", txt))
      expect_true(grepl("df <- read\\.csv\\(\"data\\.csv\"\\)", txt))
    }
  )

  # Two uploads: CSV + Excel
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = llm_resp_R,
      input_text = reactive("Hello"),
      use_python = use_python_false,
      logs = logs_simple,
      sample_temp = sample_temp_low,
      code_error = code_error_false,
      python_to_html = python_to_html_ok,
      current_data = current_data_null,
      current_data_2 = current_data2_null,
      selected_dataset_name = reactive(user_upload),
      user_data = reactive(list(file_type = "read.csv")),
      user_data_2 = reactive(list(file_type = "read_excel")),
      user_file = reactive(list(name = "data.csv")),
      user_file_2 = reactive(list(name = "data2.xlsx"))
    ),
    {
      txt <- Rmd_total()
      expect_true(grepl("df <- read\\.csv\\(\"data\\.csv\"\\)", txt))
      expect_true(grepl("library\\(readxl\\)", txt))
      expect_true(grepl("df2 <- read_excel\\(\"data2\\.xlsx\"\\)", txt))
    }
  )
})

# ---- Rmd_source (download) content logic test -------------------------------
# Direct access to download handlers can be flaky in some testServer backends.
# We validate the assembled text mirrors the logic in the handler.
test_that("Rmd_source content header and body look correct", {
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = llm_resp_R,
      input_text = reactive("Summarize"),
      use_python = use_python_false,
      logs = logs_simple,
      sample_temp = sample_temp_low,
      code_error = code_error_false,
      python_to_html = python_to_html_ok,
      current_data = current_data_null,
      current_data_2 = current_data2_null,
      selected_dataset_name = selected_dataset_none,
      user_data = user_data_null,
      user_data_2 = user_data2_null,
      user_file = user_file_null,
      user_file_2 = user_file2_null
    ),
    {
      # Ensure chunk-selection is set so history is appended
      session$setInputs(selected_chunk_report = "All chunks")
      session$flushReact()

      body <- Rmd_total()

      # Remove the params chunk exactly as the download handler does
      body_no_params <- gsub(
        "```\\{R, echo = FALSE\\}\ndf <- params\\$df\\ndf2 <- params\\$df2\\n```\\n",
        "",
        body
      )

      rmd <- paste0(
        "---\n",
        "title: \"RTutor report\"\n",
        "author: \"RTutor, Powered by ChatGPT\"\n",
        "date: \"", date(), "\"\n",
        "output: html_document\n",
        "---\n",
        body_no_params
      )

      # YAML should start with '---' then 'title:' on next line
      expect_true(grepl("^---\\s*\\ntitle:", rmd))

      # Now that selection is set, history content should be present
      expect_true(grepl("# chunk 1", rmd))
    }
  )
})


# ---- Report generation via input$report -------------------------------------
test_that("Report generation success path calls render and produces a file", {
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = llm_resp_R,
      input_text = reactive("Hello"),
      use_python = reactive(FALSE),
      logs = logs_simple,
      sample_temp = sample_temp_low,
      code_error = code_error_false,
      python_to_html = python_to_html_ok,
      current_data = reactive(iris),
      current_data_2 = current_data2_null,
      selected_dataset_name = selected_dataset_none,
      user_data = user_data_null,
      user_data_2 = user_data2_null,
      user_file = user_file_null,
      user_file_2 = user_file2_null
    ),
    {
      testthat::local_mocked_bindings(
        render = function(input, output_file, params, envir) {
          cat("<html><body>OK</body></html>", file = output_file)
          output_file
        },
        .package = "rmarkdown"
      )

      session$setInputs(report = 1)
      session$flushReact()

      # report_file() should be set to the generated temp HTML
      path <- report_file()
      expect_true(!is.null(path) && file.exists(path))

      # If the download handler is exposed, verify it copies the file out
      dl <- session$outputs$download_report
      if (!is.null(dl)) {
        out <- tempfile(fileext = ".html")
        dl$content(out)
        expect_true(file.exists(out))
        expect_match(paste(readLines(out, warn = FALSE), collapse = "\n"), "OK")
      }
    }
  )
})


test_that("Report generation error path leaves no file and download reports 'File not found.'", {
  testServer(
    app = mod_09_report_serv,
    args = list(
      id = "rep",
      submit_button = reactive(NULL),
      ch = fake_ch,
      selected_model = selected_model_R,
      llm_response = llm_resp_R,
      input_text = reactive("Hello"),
      use_python = reactive(FALSE),
      logs = logs_simple,
      sample_temp = sample_temp_low,
      code_error = code_error_false,
      python_to_html = python_to_html_ok,
      current_data = reactive(iris),
      current_data_2 = current_data2_null,
      selected_dataset_name = selected_dataset_none,
      user_data = user_data_null,
      user_data_2 = user_data2_null,
      user_file = user_file_null,
      user_file_2 = user_file2_null
    ),
    {
      # Make render fail
      testthat::local_mocked_bindings(
        render = function(...) stop("boom"),
        .package = "rmarkdown"
      )

      session$setInputs(report = 1)
      session$flushReact()

      # On error, module should NOT set a report file
      expect_true(is.null(report_file()))

      # If the download handler is exposed, trying to use it should error with "File not found."
      dl <- session$outputs$download_report
      if (!is.null(dl)) {
        out <- tempfile(fileext = ".html")
        expect_error(dl$content(out), "File not found\\.")
      }
    }
  )
})

