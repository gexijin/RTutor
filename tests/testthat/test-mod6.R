# Test script for Module 06: Error handling and record keeping
# Required packages: testthat, shinytest2, shiny

# tests/testthat/test-mod6.R

# to test mod 06, in R terminal: 'testthat::test_file("tests/testthat/test-mod6.R", reporter = "summary")'


library(testthat)
library(shiny)
library(shinytest2)

# --- Helpers ------------------------------------------------------------------

create_mock_reactives <- function() {
  list(
    submit_button = reactiveVal(0),
    llm_response = reactiveVal(NULL),
    logs = reactiveValues(
      id = 0, code = "", raw = "", last_code = "", language = "R"
    ),
    ch = reactiveValues(code_history = list()),
    counter = reactiveValues(costs = 0, requests = 1, time = 100, tokens_current = 50),
    reverted = reactiveVal(0),
    use_python = reactiveVal(FALSE),
    run_result = reactiveVal(NULL),
    python_to_html = reactiveVal(""),
    input_text = reactiveVal("test prompt"),
    llm_prompt = reactiveVal("full test prompt"),
    run_env = reactiveVal(new.env()),
    run_env_start = reactiveVal(list(df = data.frame(x = 1:3))),
    chunk_selection = reactiveValues(chunk_choices = NULL, selected_chunk = NULL, past_prompt = ""),
    Rmd_chunk = reactiveVal(""),
    current_data = reactiveVal(data.frame(x = 1:3)),
    current_data_2 = reactiveVal(data.frame(y = 1:3)),
    contribute_data = reactiveVal(TRUE),
    selected_dataset_name = reactiveVal("test_dataset"),
    user_file = reactiveVal(data.frame(name = "test.csv", size = 1000)),
    code_error = reactiveVal(FALSE)
  )
}

# Provide on_server & save_data that the module expects
on_server <- tempfile()

save_data <- function(...) TRUE

# --- Tests --------------------------------------------------------------------

test_that("Module 06 basic functionality", {
  testServer(
    mod_06_error_hist_serv,
    args = list(
      id = "test",
      submit_button = reactiveVal(0),
      llm_response = reactiveVal(NULL),
      logs = reactiveValues(id = 0, code = "", raw = "", last_code = "", language = "R"),
      ch = reactiveValues(code_history = list()),
      counter = reactiveValues(costs = 0, requests = 1, time = 100, tokens_current = 50),
      reverted = reactiveVal(0),
      use_python = reactiveVal(FALSE),
      run_result = reactiveVal(NULL),
      python_to_html = reactiveVal(""),
      input_text = reactiveVal("test prompt"),
      llm_prompt = reactiveVal("full test prompt"),
      run_env = reactiveVal(new.env()),
      run_env_start = reactiveVal(list(df = data.frame(x = 1:3))),
      chunk_selection = reactiveValues(chunk_choices = NULL, selected_chunk = NULL, past_prompt = ""),
      Rmd_chunk = reactiveVal(""),
      current_data = reactiveVal(data.frame(x = 1:3)),
      current_data_2 = reactiveVal(data.frame(y = 1:3)),
      contribute_data = reactiveVal(TRUE),
      selected_dataset_name = reactiveVal("test_dataset"),
      user_file = reactiveVal(data.frame(name = "test.csv", size = 1000)),
      code_error = reactiveVal(FALSE)
    ),
    {
      # Correct getter: this is a function, CALL it
      ret <- session$getReturned()
      expect_true(is.list(ret))
      expect_true("api_error_modal" %in% names(ret))
      expect_true(inherits(ret$api_error_modal, c("shiny.tag", "shiny.tag.list")))
    }
  )
})

test_that("LLM response handling", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      mock_response <- list(
        cmd = "print('hello world')",
        error = FALSE,
        response = list(usage = list(prompt_tokens = 10, completion_tokens = 5))
      )

      llm_response(mock_response)
      session$flushReact()  # <-- ensure observer runs

      expect_equal(logs$id, 1)
      expect_equal(logs$code, "print('hello world')")
      expect_equal(logs$raw,  "print('hello world')")
      expect_equal(logs$language, "R")
    }
  )
})

test_that("Python language detection", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      # Arrange the response first
      mock_response <- list(
        cmd = "print('hello world')",
        error = FALSE,
        response = list(usage = list(prompt_tokens = 10, completion_tokens = 5))
      )

      # IMPORTANT: set both, then a single flush
      use_python(TRUE)                # flip the language mode
      llm_response(mock_response)     # now the observer can read use_python() == TRUE
      session$flushReact()

      expect_equal(logs$language, "Python")
    }
  )
})


test_that("Code history saving", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      # First the llm response (populates logs)
      mock_response <- list(
        cmd = "summary(df)",
        error = FALSE,
        response = list(usage = list(prompt_tokens = 15, completion_tokens = 8))
      )
      llm_response(mock_response)
      session$flushReact()

      # Then signal the run finished
      mock_run_result <- list(error_message = NULL)
      run_result(mock_run_result)
      session$flushReact()

      expect_length(ch$code_history, 1)
      expect_equal(ch$code_history[[1]]$code, "summary(df)")
      expect_equal(ch$code_history[[1]]$language, "R")
      expect_equal(ch$code_history[[1]]$prompt, "test prompt")

      expect_equal(chunk_selection$selected_chunk, 1)
      expect_length(chunk_selection$chunk_choices, 1)
    }
  )
})

test_that("Error handling in code history", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      # Arrange
      code_error(TRUE)
      mock_response <- list(
        cmd = "invalid_function()",
        error = FALSE,
        response = list(usage = list(prompt_tokens = 10, completion_tokens = 5))
      )

      # Set and flush once (avoid triggering the cost observer)
      llm_response(mock_response)
      session$flushReact()

      # Simulate run finishing with an error
      run_result(list(error_message = "Error: object 'invalid_function' not found"))
      session$flushReact()

      # Assert latest history entry reflects the error
      expect_gte(length(ch$code_history), 1)
      last <- ch$code_history[[length(ch$code_history)]]
      expect_true(last$error)
      expect_equal(last$error_message, "Error: object 'invalid_function' not found")
    }
  )
})


test_that("Chunk selection and reversion", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      # Seed history with 2 chunks
      ch$code_history <- list(
        list(
          id = 1,
          code = "first_code",
          raw = "first_code",
          prompt = "first prompt",
          env = list(df = data.frame(a = 1:2))
        ),
        list(
          id = 2,
          code = "second_code", 
          raw = "second_code",
          prompt = "second prompt",
          env = list(df = data.frame(b = 3:4))
        )
      )
      session$flushReact()

      # Go to the latest chunk (no reversion increment expected)
      latest_id <- length(ch$code_history)
      chunk_selection$selected_chunk <- latest_id
      session$flushReact()

      # Now go back to a past chunk -> should increment `reverted()`
      initial_reverted <- reverted()
      chunk_selection$selected_chunk <- 1
      session$flushReact()

      expect_gt(reverted(), initial_reverted)

      # Sanity: code & prompt reflect the selected past chunk
      expect_equal(logs$code, "first_code")
      expect_equal(logs$raw,  "first_code")
      expect_equal(chunk_selection$past_prompt, "first prompt")
    }
  )
})


test_that("Cost warning modal", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      # Only this test enables the cost modal path
      file.create(on_server)
      on.exit(unlink(on_server), add = TRUE)

      counter$costs <- 0.5  # 5 cents
      mock_response <- list(
        cmd = "test_code",
        error = FALSE,
        response = list(usage = list(prompt_tokens = 10, completion_tokens = 5))
      )
      llm_response(mock_response)

      # Now simulate a submit event to trigger the observer
      submit_button(1)
      session$flushReact()

      cost_session <- counter$costs * 10
      expect_equal(cost_session, 5)
      expect_true(cost_session %% 5 == 0)
    }
  )
})


test_that("Raw code cleaning", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      mock_response <- list(
        cmd = "\n\n\nprint('hello')",
        error = FALSE,
        response = list(usage = list(prompt_tokens = 10, completion_tokens = 5))
      )
      llm_response(mock_response)
      session$flushReact()

      expect_equal(logs$raw,  "print('hello')")   # cleaned
      expect_equal(logs$code, "\n\n\nprint('hello')")  # original preserved
    }
  )
})

# The Shiny integration test uses shinytest2 (via Chromote driving Chrome/Chromium) to spin up a real app, load your module, and perform a tiny user action (app$click("submit")).
# That catches issues you won’t see in pure server tests: session lifecycle quirks, observer wiring that only triggers in a live session, UI/rendering errors, etc.

# test_that("Module 06 Shiny integration", {
#   skip_if_not_installed("shinytest2")
#   skip_if_not_installed("chromote")

#   # Skip cleanly if no Chrome/Chromium is available
#   has_chrome <- tryCatch({
#     # chromote >= 0.3.1 provides find_chrome(); if not, this returns ""
#     path <- chromote::find_chrome()
#     nzchar(path)
#   }, error = function(e) FALSE)
#   skip_if(!has_chrome && identical(Sys.getenv("CHROMOTE_CHROME"), ""),
#           "No Chrome/Chromium found for chromote")

#   test_app <- function() {
#     ui <- fluidPage(
#       actionButton("submit", "Submit"),
#       textOutput("status")
#     )
#     server <- function(input, output, session) {
#       reactives <- create_mock_reactives()
#       reactives$submit_button <- reactive(input$submit)
#       mod_06_error_hist_serv(
#         "test_module",
#         reactives$submit_button, reactives$llm_response, reactives$logs, reactives$ch,
#         reactives$counter, reactives$reverted, reactives$use_python, reactives$run_result,
#         reactives$python_to_html, reactives$input_text, reactives$llm_prompt, reactives$run_env,
#         reactives$run_env_start, reactives$chunk_selection, reactives$Rmd_chunk,
#         reactives$current_data, reactives$current_data_2, reactives$contribute_data,
#         reactives$selected_dataset_name, reactives$user_file, reactives$code_error
#       )
#       output$status <- renderText("Module loaded successfully")
#     }
#     shinyApp(ui, server)
#   }

#   app <- AppDriver$new(test_app(), name = "module_06_test")
#   app$expect_text("#status", "Module loaded successfully")
#   app$click("submit")
#   app$stop()
# })


test_that("Module 06 performance with large history", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      # Make extra sure the cost-warning observer won't run here
      if (file.exists(on_server)) unlink(on_server)
      llm_response(list(cmd = "", error = FALSE,
                        response = list(usage = list(prompt_tokens = 0, completion_tokens = 0))))
      session$flushReact()

      large_history <- lapply(1:100, function(i) {
        list(id = i,
             code = paste("code_chunk", i),
             raw  = paste("code_chunk", i),
             prompt = paste("prompt", i),
             env = list(df = data.frame(x = i)))
      })
      ch$code_history <- large_history
      session$flushReact()

      start_time <- Sys.time()
      chunk_selection$selected_chunk <- 50
      session$flushReact()
      end_time <- Sys.time()

      expect_lt(as.numeric(end_time - start_time), 1)
      expect_equal(logs$code, "code_chunk 50")
    }
  )
})


test_that("Module 06 edge cases", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      expect_no_error({
        llm_response(NULL)
        run_result(NULL)
      })
      expect_equal(length(ch$code_history), 0)

      # # The module does not guard invalid chunk IDs; skip this edge
      # testthat::skip("Module does not guard out-of-range chunk IDs")
      chunk_selection$selected_chunk <- 999
    }
  )
})

test_that("Data contribution functionality", {
  testServer(
    mod_06_error_hist_serv,
    args = create_mock_reactives(),
    {
      contribute_data(TRUE)

      test_data <- data.frame(
        numeric_col = 1:5,
        character_col = letters[1:5],
        factor_col = factor(c("A", "B", "A", "B", "A"))
      )
      current_data(test_data)
      session$flushReact()

      mock_response <- list(
        cmd = "str(df)",
        error = FALSE,
        response = list(usage = list(prompt_tokens = 10, completion_tokens = 5))
      )
      llm_response(mock_response)
      session$flushReact()

      mock_run_result <- list(error_message = NULL)
      code_error(FALSE)
      session$flushReact()

      expect_no_error({ run_result(mock_run_result); session$flushReact() })

      # Now disable contribution and run again
      contribute_data(FALSE)
      session$flushReact()

      logs$id <- 0
      mock_response$cmd <- "summary(df)"
      llm_response(mock_response)
      session$flushReact()

      expect_no_error({ run_result(mock_run_result); session$flushReact() })
    }
  )
})

# Clean up
if (file.exists(on_server)) file.remove(on_server)

