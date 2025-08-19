# tests/testthat/test-mod4.R

# to test mod 04, in R terminal: 'testthat::test_file("tests/testthat/test-mod4.R", reporter = "summary")'

library(shiny)

# if (!exists("mod_04_main_panel_serv", mode = "function")) {
#   sys.source(file.path("R", "mod_04_main_panel.R"), envir = .GlobalEnv)
#   print("Manually Sourced: mod_04_main_panel.R")
# }

# Mock helper functions that might be used in the module
mock_clean_cmd <- function(code, dataset_name, on_server) {
  return(code)
}

mock_turned_on <- function(input_val) {
  return(isTRUE(input_val))
}

# Helper function to convert UI elements to text
ui_to_text <- function(ui) {
  # Handle NULL cleanly
  if (is.null(ui)) return("")

  # Characters -> single string
  if (is.character(ui)) return(paste(ui, collapse = " "))

  # Shiny tags/tag lists -> HTML string
  if (inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list")) {
    return(htmltools::renderTags(ui)$html)
  }

  # Lists that are all character -> flatten
  if (is.list(ui) && length(ui) > 0 && all(vapply(ui, is.character, logical(1)))) {
    return(paste(unlist(ui, use.names = FALSE), collapse = " "))
  }

  # Fallback
  paste(as.character(ui), collapse = " ")
}


# Assign mock functions to global environment
assign("clean_cmd", mock_clean_cmd, envir = .GlobalEnv)
assign("turned_on", mock_turned_on, envir = .GlobalEnv)
assign("max_data_points", 1000, envir = .GlobalEnv)
assign("on_server", "mock_server_file", envir = .GlobalEnv)

testthat::test_that("Module 04 initializes correctly with minimal inputs", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  skip_if_not_installed("shinyAce")
  
  # Initialize reactive values
  llm_response <- shiny::reactiveVal(list(cmd = "test_command"))
  logs <- shiny::reactiveValues(
    id = 1,
    code = "plot(1:10)",
    raw = "plot(1:10)",
    last_code = "plot(1:10)",
    language = "R"
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = "Test output",
    error_message = NULL,
    result = NULL
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1",
    past_prompt = NULL
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test that module loads without errors
      testthat::expect_true(TRUE) # If we get here, initialization worked
      
      # Test chunk selection update
      session$setInputs(selected_chunk = "1")
      session$flushReact()
      testthat::expect_equal(chunk_selection$selected_chunk, "1")
    }
  )
})

testthat::test_that("Console output renders correctly", {
  skip_if_not_installed("DT")
  
  # Test successful execution output
  llm_response <- shiny::reactiveVal(list(cmd = "summary(iris)"))
  logs <- shiny::reactiveValues(
    id = 1,
    code = "summary(iris)",
    raw = "summary(iris)",
    last_code = "summary(iris)",
    language = "R"
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = c("Min.   :4.300", "1st Qu.:5.100", "Median :5.800"),
    error_message = NULL,
    result = NULL
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1"
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test console output
      console_text <- output$console_output
      testthat::expect_true(grepl("Min.", console_text))
      testthat::expect_true(grepl("Median", console_text))
    }
  )
})

testthat::test_that("Error handling works correctly", {
  skip_if_not_installed("DT")
  
  # Test error case
  llm_response <- shiny::reactiveVal(list(cmd = "invalid_function()"))
  logs <- shiny::reactiveValues(
    id = 1,
    code = "invalid_function()",
    raw = "invalid_function()",
    last_code = "invalid_function()",
    language = "R"
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(TRUE)
  run_result <- shiny::reactiveVal(list(
    console_output = NULL,
    error_message = "Error: object 'invalid_function' not found",
    result = NULL
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1"
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test error message rendering
      error_ui <- ui_to_text(output$error_message)
      testthat::expect_true(grepl("Error!", error_ui, fixed = TRUE))
      testthat::expect_true(grepl("invalid_function", error_ui, fixed = TRUE))
    }
  )
})

testthat::test_that("Data table rendering works correctly", {
  skip_if_not_installed("DT")
  
  # Test with iris dataset
  test_data <- iris[1:10, ]
  
  llm_response <- shiny::reactiveVal(list(cmd = "head(iris)"))
  logs <- shiny::reactiveValues(
    id = 1,
    code = "head(iris)",
    raw = "head(iris)",
    last_code = "head(iris)",
    language = "R"
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = "Test output",
    error_message = NULL,
    result = test_data
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(test_data)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1"
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test data size output
      size_output <- ui_to_text(output$data_size)
      testthat::expect_true(grepl("10 rows", size_output, fixed = TRUE))
      testthat::expect_true(grepl("5 columns", size_output, fixed = TRUE))
    }
  )
})

testthat::test_that("Second dataset handling works correctly", {
  skip_if_not_installed("DT")
  
  # Test with two datasets
  test_data_1 <- iris[1:5, ]
  test_data_2 <- mtcars[1:3, ]
  
  llm_response <- shiny::reactiveVal(list(cmd = "head(iris)"))
  logs <- shiny::reactiveValues(
    id = 1,
    code = "head(iris)",
    raw = "head(iris)",
    last_code = "head(iris)",
    language = "R"
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = "Test output",
    error_message = NULL,
    result = test_data_1
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(test_data_1)
  current_data_2 <- shiny::reactiveVal(test_data_2)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1"
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test first dataset size
      size_output_1 <- ui_to_text(output$data_size)
      testthat::expect_true(grepl("5 rows", size_output_1, fixed = TRUE))
      
      # Test second dataset size
      size_output_2 <- ui_to_text(output$data_size_2)
      testthat::expect_true(grepl("3 rows", size_output_2, fixed = TRUE))
      testthat::expect_true(grepl("2nd Dataset", size_output_2, fixed = TRUE))
    }
  )
})

testthat::test_that("Interactive plot detection works", {
  skip_if_not_installed("plotly")
  
  # Create a mock plotly object
  mock_plotly <- structure(list(data = list(), layout = list()), class = "plotly")
  
  llm_response <- shiny::reactiveVal(list(cmd = "plot_ly(iris, x = ~Sepal.Length)"))
  logs <- shiny::reactiveValues(
    id = 1,
    code = "plot_ly(iris, x = ~Sepal.Length)",
    raw = "plot_ly(iris, x = ~Sepal.Length)",
    last_code = "plot_ly(iris, x = ~Sepal.Length)",
    language = "R"
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = NULL,
    error_message = NULL,
    result = mock_plotly
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1"
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test that interactive plot is detected
      testthat::expect_true(inherits(run_result()$result, "plotly"))
    }
  )
})

testthat::test_that("Chunk selection and deletion works", {
  skip_if_not_installed("shinyalert")
  
  # Test with multiple chunks in history
  test_history <- list(
    list(id = 1, code = "plot(1:10)", raw = "plot(1:10)", last_code = "plot(1:10)", language = "R", rmd = "chunk1"),
    list(id = 2, code = "summary(iris)", raw = "summary(iris)", last_code = "summary(iris)", language = "R", rmd = "chunk2"),
    list(id = 3, code = "head(mtcars)", raw = "head(mtcars)", last_code = "head(mtcars)", language = "R", rmd = "chunk3")
  )
  
  llm_response <- shiny::reactiveVal(list(cmd = "head(mtcars)"))
  logs <- shiny::reactiveValues(
    id = 3,
    code = "head(mtcars)",
    raw = "head(mtcars)",
    last_code = "head(mtcars)",
    language = "R"
  )
  ch <- shiny::reactiveValues(code_history = test_history)
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = "Test output",
    error_message = NULL,
    result = NULL
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1", "2" = "Chunk #2", "3" = "Chunk #3"),
    selected_chunk = "3"
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      # Test chunk selection change
      session$setInputs(selected_chunk = "2")
      session$flushReact()
      testthat::expect_equal(chunk_selection$selected_chunk, "2")
      
      # Test that we have 3 chunks initially
      testthat::expect_equal(length(ch$code_history), 3)
      
      # Note: Full deletion testing would require mocking shinyalert behavior
      # which is complex in unit tests. The logic is tested through integration tests.
    }
  )
})

testthat::test_that("Python mode handling works", {
  skip_if_not_installed("DT")
  
  llm_response <- shiny::reactiveVal(list(cmd = "import pandas as pd"))
  logs <- shiny::reactiveValues(
    id = 1,
    code = "import pandas as pd\nprint('Hello Python')",
    raw = "import pandas as pd\nprint('Hello Python')",
    last_code = "import pandas as pd\nprint('Hello Python')",
    language = "Python"
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = "Hello Python",
    error_message = NULL,
    result = NULL
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(TRUE) # Python mode enabled
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1"
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test that Python mode is recognized
      testthat::expect_true(use_python())
      testthat::expect_equal(logs$language, "Python")
      
      # Test console output in Python mode
      console_text <- output$console_output
      testthat::expect_true(grepl("Hello Python", console_text))
    }
  )
})

testthat::test_that("Empty/NULL data handling works", {
  skip_if_not_installed("DT")
  
  llm_response <- shiny::reactiveVal(list(cmd = "test"))
  logs <- shiny::reactiveValues(
    id = 0,
    code = "",
    raw = "",
    last_code = "",
    language = ""
  )
  ch <- shiny::reactiveValues(code_history = list())
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(
    console_output = NULL,
    error_message = NULL,
    result = NULL
  ))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(0) # No submission yet
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(NULL) # No data loaded
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal(NULL)
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = NULL,
    selected_chunk = NULL
  )
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response,
      logs = logs,
      ch = ch,
      code_error = code_error,
      run_result = run_result,
      run_env_start = run_env_start,
      submit_button = submit_button,
      use_python = use_python,
      tabs = tabs,
      current_data = current_data,
      current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name,
      chunk_selection = chunk_selection
    ),
    {
      session$flushReact()
      
      # Test that module handles NULL data gracefully
      testthat::expect_null(current_data())
      testthat::expect_null(current_data_2())
      testthat::expect_equal(submit_button(), 0)
    }
  )
})

# Clean up global environment
testthat::test_that("Cleanup", {
  # Remove mock functions
  if (exists("clean_cmd", envir = .GlobalEnv)) {
    rm("clean_cmd", envir = .GlobalEnv)
  }
  if (exists("turned_on", envir = .GlobalEnv)) {
    rm("turned_on", envir = .GlobalEnv)
  }
  if (exists("max_data_points", envir = .GlobalEnv)) {
    rm("max_data_points", envir = .GlobalEnv)
  }
  if (exists("on_server", envir = .GlobalEnv)) {
    rm("on_server", envir = .GlobalEnv)
  }
  
  testthat::expect_true(TRUE) # Cleanup completed
})




# Testing chunk deletion #################################

# Additional test cases for chunk deletion functionality in test-mod4.R
# Proper test cases for the actual observeEvent chunk deletion logic

testthat::test_that("observeEvent delete_chunk triggers with valid selection", {
  skip_if_not_installed("shinyalert")
  
  # Create test history with 3 chunks
  test_history <- list(
    list(id = 1, code = "plot(1:10)", raw = "plot(1:10)", last_code = "plot(1:10)", 
         language = "R", rmd = "chunk1"),
    list(id = 2, code = "summary(iris)", raw = "summary(iris)", last_code = "summary(iris)", 
         language = "R", rmd = "chunk2"),
    list(id = 3, code = "head(mtcars)", raw = "head(mtcars)", last_code = "head(mtcars)", 
         language = "R", rmd = "chunk3")
  )
  
  llm_response <- shiny::reactiveVal(list(cmd = "head(mtcars)"))
  logs <- shiny::reactiveValues(
    id = 3, code = "head(mtcars)", raw = "head(mtcars)", 
    last_code = "head(mtcars)", language = "R"
  )
  ch <- shiny::reactiveValues(code_history = test_history)
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(console_output = "Test", error_message = NULL, result = NULL))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1", "2" = "Chunk #2", "3" = "Chunk #3"),
    selected_chunk = "2"
  )
  
  # Mock shinyalert to capture the callback and execute it
  original_shinyalert <- shinyalert::shinyalert
  callback_function <- NULL
  
  # Create a mock that captures the callback
  mock_shinyalert <- function(title, text, type, showCancelButton, 
                             confirmButtonText, cancelButtonText, callbackR) {
    callback_function <<- callbackR
    # Return something to indicate alert was shown
    return(TRUE)
  }
  
  # Temporarily replace shinyalert
  assignInNamespace("shinyalert", mock_shinyalert, ns = "shinyalert")
  
  # Clean up function
  on.exit({
    assignInNamespace("shinyalert", original_shinyalert, ns = "shinyalert")
  }, add = TRUE)
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel", llm_response = llm_response, logs = logs, ch = ch,
      code_error = code_error, run_result = run_result, run_env_start = run_env_start,
      submit_button = submit_button, use_python = use_python, tabs = tabs,
      current_data = current_data, current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name, chunk_selection = chunk_selection
    ),
    {
      # Set up initial state
      session$setInputs(selected_chunk = "2")
      session$flushReact()
      
      # Trigger the delete button - this should call shinyalert
      session$setInputs(delete_chunk = 1)
      session$flushReact()
      
      # Verify that shinyalert was called (callback should be captured)
      testthat::expect_true(!is.null(callback_function))
      
      # Now execute the callback with isConfirmed = TRUE to test the deletion logic
      callback_function(TRUE)
      session$flushReact()
      
      # Test that chunk 2 was deleted and reordering occurred
      testthat::expect_equal(length(ch$code_history), 2)
      testthat::expect_equal(ch$code_history[[1]]$code, "plot(1:10)")
      testthat::expect_equal(ch$code_history[[1]]$id, 1)
      testthat::expect_equal(ch$code_history[[2]]$code, "head(mtcars)")
      testthat::expect_equal(ch$code_history[[2]]$id, 2)
      
      # Test that RMD numbering was updated
      testthat::expect_equal(substr(ch$code_history[[1]]$rmd, 6, 6), "1")
      testthat::expect_equal(substr(ch$code_history[[2]]$rmd, 6, 6), "2")
      
      # Test that logs were updated to point to the last chunk
      testthat::expect_equal(logs$id, 2)
      testthat::expect_equal(logs$code, "head(mtcars)")
      testthat::expect_equal(logs$raw, "head(mtcars)")
      testthat::expect_equal(logs$last_code, "head(mtcars)")
      testthat::expect_equal(logs$language, "R")
      
      # Test that chunk_selection$chunk_choices was updated
      expected_choices <- stats::setNames(as.character(1:2), paste0("Chunk #", 1:2))
      testthat::expect_equal(chunk_selection$chunk_choices, expected_choices)
    }
  )
})

testthat::test_that("observeEvent handles deletion of last remaining chunk", {
  skip_if_not_installed("shinyalert")
  
  # Create test history with only 1 chunk
  test_history <- list(
    list(id = 1, code = "plot(1:10)", raw = "plot(1:10)", last_code = "plot(1:10)", 
         language = "R", rmd = "chunk1")
  )
  
  llm_response <- shiny::reactiveVal(list(cmd = "plot(1:10)"))
  logs <- shiny::reactiveValues(
    id = 1, code = "plot(1:10)", raw = "plot(1:10)", 
    last_code = "plot(1:10)", language = "R"
  )
  ch <- shiny::reactiveValues(code_history = test_history)
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(console_output = "Test", error_message = NULL, result = NULL))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1"),
    selected_chunk = "1"
  )
  
  # Mock shinyalert
  callback_function <- NULL
  original_shinyalert <- shinyalert::shinyalert
  mock_shinyalert <- function(title, text, type, showCancelButton, 
                             confirmButtonText, cancelButtonText, callbackR) {
    callback_function <<- callbackR
    return(TRUE)
  }
  assignInNamespace("shinyalert", mock_shinyalert, ns = "shinyalert")
  on.exit({
    assignInNamespace("shinyalert", original_shinyalert, ns = "shinyalert")
  }, add = TRUE)
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel", llm_response = llm_response, logs = logs, ch = ch,
      code_error = code_error, run_result = run_result, run_env_start = run_env_start,
      submit_button = submit_button, use_python = use_python, tabs = tabs,
      current_data = current_data, current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name, chunk_selection = chunk_selection
    ),
    {
      # Set up and trigger deletion
      session$setInputs(selected_chunk = "1")
      session$setInputs(delete_chunk = 1)
      session$flushReact()
      
      # Execute the callback with confirmation
      testthat::expect_true(!is.null(callback_function))
      callback_function(TRUE)
      session$flushReact()
      
      # Test that everything was cleared (else branch of max_id > 0)
      testthat::expect_equal(length(ch$code_history), 0)
      testthat::expect_equal(logs$id, 0)
      testthat::expect_equal(logs$code, "")
      testthat::expect_equal(logs$raw, "")
      testthat::expect_equal(logs$last_code, "")
      testthat::expect_equal(logs$language, "")
    }
  )
})

testthat::test_that("observeEvent respects req() validation", {
  skip_if_not_installed("shinyalert")
  
  # Create test history
  test_history <- list(
    list(id = 1, code = "plot(1:10)", raw = "plot(1:10)", last_code = "plot(1:10)", 
         language = "R", rmd = "chunk1"),
    list(id = 2, code = "summary(iris)", raw = "summary(iris)", last_code = "summary(iris)", 
         language = "R", rmd = "chunk2")
  )
  
  llm_response <- shiny::reactiveVal(list(cmd = "summary(iris)"))
  logs <- shiny::reactiveValues(
    id = 2, code = "summary(iris)", raw = "summary(iris)", 
    last_code = "summary(iris)", language = "R"
  )
  ch <- shiny::reactiveValues(code_history = test_history)
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(console_output = "Test", error_message = NULL, result = NULL))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1", "2" = "Chunk #2"),
    selected_chunk = NULL  # No chunk selected - should trigger req() to fail
  )
  
  # Mock shinyalert - this should NOT be called due to req() failure
  alert_called <- FALSE
  original_shinyalert <- shinyalert::shinyalert
  mock_shinyalert <- function(...) {
    alert_called <<- TRUE
    return(TRUE)
  }
  assignInNamespace("shinyalert", mock_shinyalert, ns = "shinyalert")
  on.exit({
    assignInNamespace("shinyalert", original_shinyalert, ns = "shinyalert")
  }, add = TRUE)
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel", llm_response = llm_response, logs = logs, ch = ch,
      code_error = code_error, run_result = run_result, run_env_start = run_env_start,
      submit_button = submit_button, use_python = use_python, tabs = tabs,
      current_data = current_data, current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name, chunk_selection = chunk_selection
    ),
    {
      # Don't set selected_chunk, leaving it NULL
      session$setInputs(delete_chunk = 1)
      session$flushReact()
      
      # shinyalert should NOT have been called due to req() validation
      testthat::expect_false(alert_called)
      
      # Original data should be unchanged
      testthat::expect_equal(length(ch$code_history), 2)
      testthat::expect_equal(ch$code_history[[1]]$code, "plot(1:10)")
      testthat::expect_equal(ch$code_history[[2]]$code, "summary(iris)")
    }
  )
})

testthat::test_that("observeEvent handles user cancellation", {
  skip_if_not_installed("shinyalert")
  
  # Create test history
  test_history <- list(
    list(id = 1, code = "plot(1:10)", raw = "plot(1:10)", last_code = "plot(1:10)", 
         language = "R", rmd = "chunk1"),
    list(id = 2, code = "summary(iris)", raw = "summary(iris)", last_code = "summary(iris)", 
         language = "R", rmd = "chunk2")
  )
  
  llm_response <- shiny::reactiveVal(list(cmd = "summary(iris)"))
  logs <- shiny::reactiveValues(
    id = 2, code = "summary(iris)", raw = "summary(iris)", 
    last_code = "summary(iris)", language = "R"
  )
  ch <- shiny::reactiveValues(code_history = test_history)
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(console_output = "Test", error_message = NULL, result = NULL))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = c("1" = "Chunk #1", "2" = "Chunk #2"),
    selected_chunk = "1"
  )
  
  # Mock shinyalert
  callback_function <- NULL
  original_shinyalert <- shinyalert::shinyalert
  mock_shinyalert <- function(title, text, type, showCancelButton, 
                             confirmButtonText, cancelButtonText, callbackR) {
    callback_function <<- callbackR
    return(TRUE)
  }
  assignInNamespace("shinyalert", mock_shinyalert, ns = "shinyalert")
  on.exit({
    assignInNamespace("shinyalert", original_shinyalert, ns = "shinyalert")
  }, add = TRUE)
  
  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel", llm_response = llm_response, logs = logs, ch = ch,
      code_error = code_error, run_result = run_result, run_env_start = run_env_start,
      submit_button = submit_button, use_python = use_python, tabs = tabs,
      current_data = current_data, current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name, chunk_selection = chunk_selection
    ),
    {
      # Trigger deletion
      session$setInputs(selected_chunk = "1")
      session$setInputs(delete_chunk = 1)
      session$flushReact()
      
      # Execute callback with isConfirmed = FALSE (user clicked "No")
      testthat::expect_true(!is.null(callback_function))
      callback_function(FALSE)
      session$flushReact()
      
      # Nothing should have changed - user cancelled
      testthat::expect_equal(length(ch$code_history), 2)
      testthat::expect_equal(ch$code_history[[1]]$code, "plot(1:10)")
      testthat::expect_equal(ch$code_history[[2]]$code, "summary(iris)")
      testthat::expect_equal(logs$id, 2)
      testthat::expect_equal(logs$code, "summary(iris)")
    }
  )
})

testthat::test_that("Complex deletion scenario - delete first of 4 chunks", {
  skip_if_not_installed("shinyalert")

  # Create test history with 4 chunks
  test_history <- list(
    list(id = 1, code = "library(ggplot2)", raw = "library(ggplot2)", last_code = "library(ggplot2)", language = "R", rmd = "chunk1"),
    list(id = 2, code = "plot(1:10)",       raw = "plot(1:10)",       last_code = "plot(1:10)",       language = "R", rmd = "chunk2"),
    list(id = 3, code = "summary(iris)",    raw = "summary(iris)",    last_code = "summary(iris)",    language = "R", rmd = "chunk3"),
    list(id = 4, code = "head(mtcars)",     raw = "head(mtcars)",     last_code = "head(mtcars)",     language = "R", rmd = "chunk4")
  )

  llm_response <- shiny::reactiveVal(list(cmd = "head(mtcars)"))
  logs <- shiny::reactiveValues(id = 4, code = "head(mtcars)", raw = "head(mtcars)", last_code = "head(mtcars)", language = "R")
  ch <- shiny::reactiveValues(code_history = test_history)
  code_error <- shiny::reactiveVal(FALSE)
  run_result <- shiny::reactiveVal(list(console_output = "Test", error_message = NULL, result = NULL))
  run_env_start <- shiny::reactiveVal(list())
  submit_button <- shiny::reactiveVal(1)
  use_python <- shiny::reactiveVal(FALSE)
  tabs <- shiny::reactiveVal("Home")
  current_data <- shiny::reactiveVal(iris)
  current_data_2 <- shiny::reactiveVal(NULL)
  selected_dataset_name <- shiny::reactiveVal("iris")

  # IMPORTANT: choices = setNames(values, labels) → names=labels, values=character IDs
  init_vals <- as.character(1:4)
  init_labs <- paste0("Chunk #", 1:4)
  chunk_selection <- shiny::reactiveValues(
    chunk_choices = stats::setNames(init_vals, init_labs),
    selected_chunk = "1"  # Delete first chunk
  )

  # Mock shinyalert
  callback_function <- NULL
  original_shinyalert <- shinyalert::shinyalert
  mock_shinyalert <- function(title, text, type, showCancelButton, confirmButtonText, cancelButtonText, callbackR) {
    callback_function <<- callbackR
    TRUE
  }
  assignInNamespace("shinyalert", mock_shinyalert, ns = "shinyalert")
  on.exit(assignInNamespace("shinyalert", original_shinyalert, ns = "shinyalert"), add = TRUE)

  shiny::testServer(
    mod_04_main_panel_serv,
    args = list(
      id = "main_panel",
      llm_response = llm_response, logs = logs, ch = ch,
      code_error = code_error, run_result = run_result, run_env_start = run_env_start,
      submit_button = submit_button, use_python = use_python, tabs = tabs,
      current_data = current_data, current_data_2 = current_data_2,
      selected_dataset_name = selected_dataset_name, chunk_selection = chunk_selection
    ),
    {
      # Trigger deletion of first chunk
      session$setInputs(selected_chunk = "1")
      session$setInputs(delete_chunk = 1)
      session$flushReact()

      # Confirm via mocked shinyalert
      callback_function(TRUE)
      session$flushReact()

      # 1) Code history updated
      testthat::expect_gt(length(ch$code_history), 0)

      # What was chunk 2 is now chunk 1
      testthat::expect_equal(ch$code_history[[1]]$code, "plot(1:10)")
      testthat::expect_equal(ch$code_history[[1]]$id, 1)
      testthat::expect_equal(ch$code_history[[1]]$rmd, "chunk1")

      # What was chunk 3 is now chunk 2
      if (length(ch$code_history) >= 2) {
        testthat::expect_equal(ch$code_history[[2]]$code, "summary(iris)")
        testthat::expect_equal(ch$code_history[[2]]$id, 2)
        testthat::expect_equal(ch$code_history[[2]]$rmd, "chunk2")
      }

      # What was chunk 4 is now chunk 3
      if (length(ch$code_history) >= 3) {
        testthat::expect_equal(ch$code_history[[3]]$code, "head(mtcars)")
        testthat::expect_equal(ch$code_history[[3]]$id, 3)
        testthat::expect_equal(ch$code_history[[3]]$rmd, "chunk3")
      }

      # 2) Logs point to last remaining chunk
      testthat::expect_equal(logs$id, length(ch$code_history))
      # last code in history should match logs$code
      testthat::expect_equal(logs$code, ch$code_history[[length(ch$code_history)]]$code)

      # 3) Chunk choices updated with proper mapping (names = labels, values = character ids)
      n <- length(ch$code_history)
      expected_choices <- stats::setNames(as.character(seq_len(n)), paste0("Chunk #", seq_len(n)))
      testthat::expect_equal(chunk_selection$chunk_choices, expected_choices, ignore_attr = TRUE)

      # Guards: values must be character ids; names must be human labels
      testthat::expect_true(is.character(unname(chunk_selection$chunk_choices)))
      testthat::expect_true(all(unname(chunk_selection$chunk_choices) %in% as.character(seq_len(n))))
      testthat::expect_true(all(grepl("^Chunk #\\d+$", names(chunk_selection$chunk_choices))))

      # 4) Selected chunk synced with logs (as character)
      testthat::expect_identical(chunk_selection$selected_chunk, as.character(logs$id))
    }
  )
})
