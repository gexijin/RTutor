

#____________________________________________________________________________
#  Run the code, data prep, show code
#____________________________________________________________________________


mod_07_run_code_serv <- function(id, run_env, run_env_start, run_result, submit_button,
                                 reverted, logs, use_python, selected_dataset_name,
                                 current_data, current_data_2, code_error) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Run the code ###

    # stores the results after running the generated code
    # returns error indicator and message
    # Sometimes returns NULL, even when code runs fine. Especially when
    # a base R plot is generated

    observeEvent(
      eventExpr = list(
        submit_button(),  # when submit is clicked
        reverted(),       # or when a previous code chunk is selected
        logs$code
      ),{
      req(logs$code != "")
      req(!use_python())
      # print("Run Code")

      result <- NULL
      console_output <- NULL
      error_message <- ""

      withProgress(message = "Running the code ...", {
        incProgress(0.4)

        run_env_start(as.list(run_env())) # keep a copy of the crime scene

        result <- tryCatch({
          eval_result <- eval(
            parse(text = clean_cmd(logs$code, selected_dataset_name(), file.exists(on_server))),
            envir = run_env()
          )
          console_output <- capture.output(print(eval_result))
          eval_result                # without this, interactive plots don't work
        }, error = function(e) {
          list(error_message = e$message)  # won't work if not inside a list!
        })

        # update the error message, if any
        if (length(names(result)) != 0) {
          if (names(result)[1] == "error_message") {
            error_message <- result$error_message
          }
        }

        # Code was Run with error
        if (error_message != "") {
          run_env(list2env(run_env_start()))  # revert the environment
          showNotification(
            "Resubmit the same request to see if ChatGPT can resolve the error.
            If that fails, change the request.",
            duration = 10
          )
          code_error(TRUE)
        } else{
          code_error(FALSE)
        }

        # Check to see if df changed from running the code
        if (!is.null(current_data())) {
          row_check <- nrow(current_data()) == nrow(run_env()$df) # Check if # of rows are same
          col_check <- ncol(current_data()) == ncol(run_env()$df) # Check if # of columns are same
          if (row_check && col_check) {
            val_check <- length(which(current_data() != run_env()$df)) # Check if values are the same
            if (val_check > 0) {
              current_data(run_env()$df)
            }
          } else {
            current_data(run_env()$df)
          }
        }

        # Check to see if df2 changed from running the code
        if (!is.null(current_data_2())) {
          row_check <- nrow(current_data_2()) == nrow(run_env()$df2) # Check if # of rows are same
          col_check <- ncol(current_data_2()) == ncol(run_env()$df2) # Check if # of columns are same
          if (row_check && col_check) {
            val_check <- length(which(current_data_2() != run_env()$df2)) # Check if values are the same
            if (val_check > 0) {
              current_data_2(run_env()$df2)
            }
          } else {
            current_data_2(run_env()$df2)
          }
        }

        run_result(
          list(
            result = result,
            console_output = console_output,
            error_message = error_message
          )
        )
      })
    })

  })
}