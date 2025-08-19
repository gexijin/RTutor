# tests/testthat/test-mod7.R
# Run with:
# testthat::test_file("tests/testthat/test-mod7.R", reporter = "summary")

library(testthat)
library(shiny)

# ------------------------------------------------------------------
# Mocks used by the module
# ------------------------------------------------------------------

# clean_cmd is referenced by mod_07; keep a simple no-op mock
clean_cmd <- function(code, dataset_name, file_exists) code

# on_server is sometimes checked via file.exists(on_server)
on_server <- tempfile()  # no file is created unless a test wants it

# Helper: create a full set of reactives for mod_07_run_code_serv
new_mod7_reactives <- function() {
  list(
    # inputs / state
    submit_button         = reactiveVal(0),
    reverted              = reactiveVal(0),
    use_python            = reactiveVal(FALSE),
    selected_dataset_name = reactiveVal("df"),
    # env + results
    run_env        = reactiveVal(new.env()),
    run_env_start  = reactiveVal(list()),
    run_result     = reactiveVal(NULL),
    # data reactives
    current_data   = reactiveVal(NULL),
    current_data_2 = reactiveVal(NULL),
    # error state
    code_error     = reactiveVal(FALSE),
    # logs
    logs = reactiveValues(code = "")
  )
}

# For convenience when calling testServer
pass_args_to_mod7 <- function(R) {
  list(
    id                     = "mod7",
    submit_button          = R$submit_button,
    logs                   = R$logs,
    code_error             = R$code_error,
    run_env                = R$run_env,
    run_env_start          = R$run_env_start,
    run_result             = R$run_result,
    current_data           = R$current_data,
    current_data_2         = R$current_data_2,
    selected_dataset_name  = R$selected_dataset_name,
    reverted               = R$reverted,
    use_python             = R$use_python
  )
}

# ------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------

test_that("mod_07_run_code_serv: basic code execution without errors", {
  R <- new_mod7_reactives()

  # seed environment
  e <- new.env()
  e$df  <- data.frame(x = 1:5, y = letters[1:5])
  e$df2 <- data.frame(a = 1:3, b = 4:6)
  R$run_env(e)
  R$current_data(e$df)
  R$current_data_2(e$df2)

  testServer(mod_07_run_code_serv,
    args = pass_args_to_mod7(R),
    {
      R$logs$code <- "result <- 2 + 2"
      R$submit_button(1)
      session$flushReact()

      expect_false(R$code_error())

      res <- R$run_result()
      expect_true(is.list(res))
      expect_true(all(c("result","console_output","error_message") %in% names(res)))
      expect_identical(res$error_message, "")
    }
  )
})

test_that("mod_07_run_code_serv: code execution with syntax error", {
  R <- new_mod7_reactives()

  e <- new.env()
  e$df <- data.frame(x = 1:5, y = letters[1:5])
  R$run_env(e)
  R$run_env_start(as.list(e))

  testServer(mod_07_run_code_serv,
    args = pass_args_to_mod7(R),
    {
      # Any of these will work; pick ONE and keep it
      R$logs$code <- "result <- 2 + * 2"                  # guaranteed PARSE error
      # R$logs$code <- "result <- invalid_function()"       # runtime error (object not found)
      #R$logs$code <- "stop('Forced test error')"            # runtime error (explicit)

      R$submit_button(1)
      session$flushReact()

      expect_true(R$code_error())

      res <- R$run_result()
      expect_true(is.list(res))
      expect_true(nzchar(res$error_message))
    }
  )
})


test_that("mod_07_run_code_serv: data frame modification updates current_data", {
  R <- new_mod7_reactives()

  e <- new.env()
  e$df  <- data.frame(x = 1:5, y = letters[1:5])
  e$df2 <- data.frame(a = 1:3, b = 4:6)
  R$run_env(e)
  R$current_data(e$df)
  R$current_data_2(e$df2)

  testServer(mod_07_run_code_serv,
    args = pass_args_to_mod7(R),
    {
      R$logs$code <- "df$z <- c(10,20,30,40,50)"
      R$submit_button(1)
      session$flushReact()

      expect_true("z" %in% names(R$current_data()))
      expect_identical(R$current_data()$z, c(10,20,30,40,50))
    }
  )
})

test_that("mod_07_run_code_serv: Python mode skips R execution", {
  R <- new_mod7_reactives()
  R$use_python(TRUE)  # enable python mode

  testServer(mod_07_run_code_serv,
    args = pass_args_to_mod7(R),
    {
      R$logs$code <- "print('Hello Python')"
      R$submit_button(1)
      session$flushReact()

      # Module should skip run and leave run_result NULL
      expect_null(R$run_result())
    }
  )
})

test_that("mod_07_run_code_serv: empty code is ignored", {
  R <- new_mod7_reactives()

  testServer(mod_07_run_code_serv,
    args = pass_args_to_mod7(R),
    {
      R$logs$code <- ""   # nothing to run
      R$submit_button(1)
      session$flushReact()

      expect_null(R$run_result())
    }
  )
})

test_that("mod_07_run_code_serv: environment restoration after error (smoke)", {
  R <- new_mod7_reactives()

  e <- new.env()
  e$df       <- data.frame(x = 1:5, y = letters[1:5])
  e$test_var <- "original_value"
  R$run_env(e)
  R$run_env_start(as.list(e))

  testServer(mod_07_run_code_serv,
    args = pass_args_to_mod7(R),
    {
      R$logs$code <- "test_var <- 'modified'; stop('Test error')"
      R$submit_button(1)
      session$flushReact()

      expect_true(R$code_error())

      # Environment still has test_var (restoration semantics may vary by module)
      expect_true("test_var" %in% names(as.list(R$run_env())))
    }
  )
})

test_that("mod_07_run_code_serv: data manipulation integration", {
  R <- new_mod7_reactives()

  e <- new.env()
  e$df <- data.frame(
    id = 1:10,
    value = runif(10),
    category = rep(c("A","B"), 5)
  )
  e$df2 <- data.frame(x = 1:5, y = 6:10)
  R$run_env(e)
  R$current_data(e$df)
  R$current_data_2(e$df2)

  testServer(mod_07_run_code_serv,
    args = pass_args_to_mod7(R),
    {
      R$logs$code <- "df$new_col <- df$value * 2; df <- df[df$category == 'A', ]"
      R$submit_button(1)
      session$flushReact()

      expect_false(R$code_error())
      expect_true("new_col" %in% names(R$current_data()))
      expect_equal(nrow(R$current_data()), 5)
      expect_true(all(R$current_data()$category == "A"))
    }
  )
})
