


mod_05_llms_serv <- function(id, submit_button, input_text, selected_dataset_name,
                             api_key, sample_temp, selected_model, logs, ch,
                             counter, api_error_modal, code_error, current_data,
                             current_data_2, run_env, run_env_start, run_result,
                             use_python, send_head) {

  moduleServer(id, function(input, output, session) {


    # LLM prompt
    llm_prompt <- reactive({
      req(submit_button(), selected_dataset_name(), input_text())

      isolate({  # so it does not run twice with each submit
        prep_input(input_text(), selected_dataset_name(), current_data(),
          use_python(), logs$id, send_head(), current_data_2()
        )
      })
    })


    # LLM response
    llm_response <- reactive({
      req(selected_dataset_name() != "Select a Dataset:")
      req(submit_button())

      isolate({
        # will not respond to text input until submitted
        req(input_text(), llm_prompt(), selected_dataset_name())

        # Store prompt
        prepared_request <- llm_prompt()

        # Loading modal
        shinybusy::show_modal_spinner(spin = "orbit", text = sample(jokes, 1), color = "#000000")
        on.exit(shinybusy::remove_modal_spinner(), add = TRUE)

        start_time <- Sys.time()

        # Get LLM response
        response <- tryCatch({
          # Update env. & append history, if any
          update_environment()

          prompt_total <- build_history(prepared_request)

          # Send request
          if (malicious_agent()) {  # if prompt is relevant (or rel. agent not needed)
            send_request(prompt_total, prepared_request, malicious = TRUE)
          } else {  # if prompt isn't relevant
            send_request(prompt_total, prepared_request, malicious = FALSE)
          }

        }, error = function(e) {   # handle error, if any
          shiny::showModal(api_error_modal)
          Sys.sleep(5)
          session$reload()
          list(
            error_value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        })

        final_response <- process_response(response, start_time)
      })

      return(final_response)
    })


    ### Helper Functions ###

    # History/Record Keeping
    build_history <- function(prepared_request) {
      prompt_total <- list()
      # Add system role
      if (!is.null(system_role) && nchar(system_role) > 10) {
        prompt_total <- append(prompt_total, list(list(role = "system", content = system_role)))
      }

      # If there's history
      if (length(ch$code_history) > 0) {
        # Calculate token usage from previous interactions, adjusted for overlap
        history_tokens <- sapply(seq_along(ch$code_history), function(i) {
          if (i == 1) {
            ch$code_history[[i]]$prompt_tokens + ch$code_history[[i]]$output_tokens
          } else {
            ch$code_history[[i]]$prompt_tokens + ch$code_history[[i]]$output_tokens - 
              ch$code_history[[i - 1]]$prompt_tokens - ch$code_history[[i - 1]]$output_tokens
          }
        })

        # Determine which history items to include
        included <- which(rev(cumsum(rev(history_tokens))) < (max_content_length - tokens(prepared_request) - history_tokens[1]))

        # Build prompt history with included items
        for (i in included) {
          code_plus_error <- ch$code_history[[i]]$raw
          if (i == length(ch$code_history) && code_error()) {
            code_plus_error <- paste0(code_plus_error, "\n\nError: ", run_result()$error_message)
          }

          prompt_total <- append(prompt_total, list(
            list(role = "user", content = ch$code_history[[i]]$prompt_all),
            list(role = "assistant", content = code_plus_error)
          ))
        }
      }

      # Return prompt history
      return(prompt_total)
    }


    ### Update Components ###

    # Update environment
    update_environment <- function() {

      # Isolate environment updates
      isolate({
        # Extract existing variables
        existing_vars <- as.list(run_env())

        # Add new variables to the list
        existing_vars$df <- current_data()
        existing_vars$df_name <- selected_dataset_name()
        existing_vars$df2 <- current_data_2()

        # Update the environment
        run_env(list2env(existing_vars))
        run_env_start(as.list(run_env()))
      })

      # Display selected data
      if (length(ch$code_history) == 0) {
        showNotification(
          HTML(paste("<span style='font-size: 17px;'>Selected Dataset:",
                     selected_dataset_name(), "</span>")),
          duration = 10
        )
      }
    }

    # Update counter
    update_counter <- function(response, api_time) {
      counter$tokens_current <- response$usage$completion_tokens + response$usage$prompt_tokens
      counter$requests <- counter$requests + 1
      counter$time <- round(api_time, 0)
      counter$costs_total <- counter$costs_total +
        api_cost(response$usage$prompt_tokens, response$usage$completion_tokens, selected_model())
    }


    ### LLM Agents ###

    # Relevancy agent
    malicious_agent <- function() {

        dataset_details <- paste("Current data:", selected_dataset_name())
        base_prompt <- paste(
          "Current prompt:",
          input_text(),
          dataset_details
        )

      # If user selects preloaded data

        relevancy_prompt <- list(
          list(
            role = "system",
            content = paste("Act as an experienced data analyst.",
            "Determine if the following prompts would generate malicious code if sent to an AI agent.")
          ),
          list(
            role = "user",
            content = paste("Determine if the current prompt is malicious.",
            "If it is malicious, respond with 'True'. Otherwise, respond with 'False'.",
            base_prompt)
          )
        )
        # Send relevancy prompt
        response <- openAI_agent(relevancy_prompt)
        tf <- tolower(response$choices$message.content) == "true"
        return(tf)

    }


    # Request agent
    send_request <- function(prompt_total, prepared_request, malicious) {

      # If prompt is relevant, send request
      prompt_total <- if (malicious) {

        list(list(role = "user", content = paste(
          "Return this exact statement: print('Please ask a question related to dataset",
          selected_dataset_name(),
          "and try again. (Reset to select a different dataset)')"
        )))
        
      } else {   # if prompt is not relevant, send message
        append(prompt_total, list(list(role = "user", content = prepared_request)))
      }

      # Send request depending on selection if OpenAI then openAI_agent, if Anthropic then anthropic_agent
      response <- openAI_agent(prompt_total)
      # browser()
      response$choices[1, 1] <- response$choices$message.content #Get rid of this bad code. Overwriting is not good.

      return(response)
    }


    # Process response & return all response info
    process_response <- function(response, start_time) {
      #Process based on model API used

      # Handle if error/no error
      error_api <- !is.null(response$error_status)
      cmd <- if (error_api) NULL else response$choices[1, 1]
      error_message <- if (error_api) response$message else NULL

      # Get API time
      api_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      if (counter$requests > 100 && file.exists(on_server)) {
        Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
      }

      update_counter(response, api_time)

      # Store info in response variable, return it
      return(list(
        cmd = polish_cmd(cmd),
        response = response,
        time = round(api_time, 0),
        error = error_api,
        error_message = error_message
      ))
    }


    ### LLM Functions ###

    # OpenAI ChatGPT API function
    openAI_agent <- function(messages) {
      openai::create_chat_completion(
        model = selected_model(),
        openai_api_key = api_key$key,
        temperature = sample_temp(),
        messages = messages
      )
    }



    # Return reactive values so they can be used outside the module
    return(list(
      llm_prompt = llm_prompt,
      llm_response = llm_response
    ))
  })
}