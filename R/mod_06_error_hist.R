

  #                             Module 06
  #____________________________________________________________________________
  #  Error handling, record keeping/chunk history
  #____________________________________________________________________________


  mod_06_error_hist_serv <- function(id, submit_button, llm_response, logs, counter,
                                     reverted, use_python, run_result, python_to_html,
                                     code_error, input_text, llm_prompt, run_env,
                                     run_env_start, chunk_selection, Rmd_chunk,
                                     current_data, current_data_2, contribute_data,
                                     selected_dataset_name, user_file
                                     ) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # API Connection Error Modal
    api_error_modal <- shiny::modalDialog(
      title = "API connection error!",
      tags$h4("Is the API key is correct?", style = "color:red"),
      tags$h4("How about the WiFi?", style = "color:red"),
      tags$h4("Maybe the openAI.com website is taking forever to respond.", style = "color:red"),
      tags$h5("If you keep having trouble, send us an email.", style = "color:red"),
      tags$h4(
        "Auto-reset ...",
        style = "color:blue; text-align:right"
      ),
      easyClose = TRUE,
      size = "s"
    )

    # Warning message when reached 10 cents, 20c, 30c ...
    observeEvent(submit_button(), {
      req(file.exists(on_server))
      req(!llm_response()$error)

      cost_session <-  counter$costs * 10
      if (cost_session %% 5  == 0 & cost_session != 0) {
        shiny::showModal(
          shiny::modalDialog(
            size = "s",
            easyClose	= TRUE,
            h4(
              paste0(
                "Cumulative API Cost reached ",
                cost_session,
                "¢"
              )
            ),
            h4("Slow down. Please try to use your own API key.")
          )
        )
      }
    })

    # Capture error when running the generated code
    code_error <- reactive({
      error_status <- FALSE
      req(submit_button()) # Require the submit button to be pushed
      if (use_python()) { # Python
        return(python_to_html() == -1)
      } else { # R
        return(!is.null(run_result()$error_message) && run_result()$error_message != "")
      }
    })

    # Show Error Notification
    observeEvent(code_error(), {
      if (code_error()) { # if there's an error
        showNotification(
          "Resubmit the same request to see if ChatGPT can resolve the error.
          If that fails, change the request.",
          duration = 10
        )
      }
    })

    # Update Logs when Submitted
    observeEvent(submit_button(), {
      req(llm_response())   # necessary for sequence of app's events

      logs$id <- logs$id + 1

      logs$code <-  llm_response()$cmd

      logs$raw <- llm_response()$cmd
      # remove one or more blank lines in the beginning.
      logs$raw <- gsub("^\n+", "", logs$raw)
      logs$last_code <- ""
      logs$language <- ifelse(use_python(), "Python", "R")

      # A list holds current request
      current_code <- list(
        id = logs$id,
        code = logs$code,
        raw = logs$raw, # for print
        prompt = input_text(),
        prompt_all = llm_prompt(), # entire prompt, as sent to openAI
        error = code_error(),
        error_message = run_result()$error_message,
        rmd = Rmd_chunk(),
        language = logs$language,
        # saves the rendered file in the logs object.
        html_file = ifelse(use_python(), python_to_html(), -1),
        prompt_tokens = llm_response()$response$usage$prompt_tokens,
        output_tokens = llm_response()$response$usage$completion_tokens,
        # save a copy of the data in the environment as a list.
        # if save environment, only reference is saved.
        # This needs more memory, but works.
        env = run_env_start() # it is a list;
      )

      logs$code_history <- append(logs$code_history, list(current_code))

      choices <- seq_along(logs$code_history)
      names(choices) <- paste0("Chunk #", choices)

      # Directly update chunk selection
      chunk_selection$chunk_choices <- choices
      chunk_selection$selected_chunk <- logs$id

    })

    # Change code when past code is selected
    observeEvent(chunk_selection$selected_chunk, {
      req(chunk_selection$selected_chunk)

      id <- chunk_selection$selected_chunk
      id <- as.integer(id)

      logs$code <- logs$code_history[[id]]$code
      logs$raw <- logs$code_history[[id]]$raw


      # Switch to previous chunks
      if (id < length(logs$code_history)) {
        # convert list to environment;
        # update the run_env reactive value.
        # restore the environment to the before running the ith chunk
        run_env(list2env(logs$code_history[[id]]$env))
        current_data(run_env()$df)
        current_data_2(run_env()$df2)

        # enable re-calculation of the code
        reverted(reverted() + 1)

        showNotification(
          ui = paste("Switched back to chunk #", id, "."),
          id = "revert_chunk",
          duration = 5,
          type = "warning"
        )
      }

      # Directly update prompt display based on chunk selection
      chunk_selection$past_prompt <- logs$code_history[[id]]$prompt

    })

    observeEvent(submit_button(), {
      req(llm_prompt())
      req(logs$code)

      if(contribute_data()) {
        # remove user data, only keep column names and data type
        txt <- capture.output(str(current_data(), vec.len = 0))
        txt <- gsub(" levels .*$", " levels", txt)
        try(
          save_data(
            date = Sys.Date(),
            time = format(Sys.time(), "%H:%M:%S"),
            request = llm_prompt(),
            code = logs$code,
            error_status = code_error(),  # 1 --> error!  0 --> no error, success!!
            data_str = paste(txt, collapse = "\n"),
            dataset = selected_dataset_name(),
            session = session$token,
            filename = ifelse(is.null(user_file()[1, 1]), " ", user_file()[1, 1]),
            filesize = ifelse(is.null(user_file()[1, 2]), " ", user_file()[1, 2]),
            chunk = counter$requests,
            api_time = counter$time,
            tokens = counter$tokens_current,
            language = logs$language
          )
        )
      }
    })


    # Return reactive values so they can be used outside the module
    return(
      list(
        api_error_modal = api_error_modal,
        code_error = code_error
      )
    )
  })
}
