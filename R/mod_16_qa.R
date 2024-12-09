#____________________________________________________________________________
#  Q & A
#____________________________________________________________________________


mod_16_qa_ui <- function(id) {

  ns <- NS(id)

  tagList(
    conditionalPanel(
      condition = paste0("output['", ns("show_qa"), "'] === 'show'"),
      fluidRow(
        column(12,
          textInput(
            inputId = ns("ask_question"),
            label = HTML("<span style='font-size: 18px;'>Ask About Results</span>"),
            placeholder = "Q&A on code, results, error, or statistics in general",
            value = ""
          ),
          tippy::tippy_this(
            ns("ask_question"),
            "'Walk me through this code', 'What does this result mean?',
            'What is this error about?', 'Explain logistic regression',
            'List R packages for time series analysis'.
            Hit Enter to send your request.",
            theme = "light-border"
          ),
          shinyjs::hidden(actionButton(ns("ask_button"), strong("Ask RTutor"))),
          hr(class = "custom-hr")
        )
      )
    )
  )
}



mod_16_qa_serv <- function(id, submit_button, ch, code_error, run_result, api_error_modal, counter,
  selected_model, api_key, sample_temp, selected_dataset_name) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$show_qa <- renderText({
      if (submit_button() >= 1) {
        return("show")
      } else {
        return("hide")
      }
    })
    outputOptions(output, "show_qa", suspendWhenHidden = FALSE)


    answer_one <- reactive({
      req(input$ask_button)

      isolate({
      req(input$ask_question)
        #----------------------------Prep question
        txt <- input$ask_question

        # force to within 280 characters
        if (nchar(txt) > max_char_question) {
          txt <- substr(txt, 1, max_char_question)
          showNotification(
            paste("Only the first", max_char_question, " characters will be used."),
            duration = 10
          )
        }

        # Check if the last character is not a period, questionmark, or exlamation
        if (!(substr(txt, nchar(txt), nchar(txt)) %in% c(".","?","!"))) {
        # If the last character is not a period, add it to the end
          txt <- paste(txt, ".", sep = "")
        }

        prepared_request <- txt

        #----------------------------Send request
        shinybusy::show_modal_spinner(spin = "orbit", text = paste(sample(jokes, 1)),color = "#000000")
        on.exit(shinybusy::remove_modal_spinner(), add = TRUE)

        start_time <- Sys.time()

                # Get LLM response
        response <- tryCatch({
          # Update env. & append history, if any
          update_environment()
          
          prompt_total <- build_history(prepared_request)

          # Send request
          send_request_qa(prompt_total, prepared_request)

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



    chat_content <- reactiveVal(c())

    observeEvent(input$ask_button, {
      new_message <- answer_one()
      if (new_message != "") {
        chat_content(c(new_message, chat_content()))
        updateTextInput(
          session,
          inputId = "ask_question",
          label = NULL,
          placeholder = "Ask a question on the code or statistics",
          value = ""
        )
      }
    })

    output$answer <- renderUI({
      req(input$ask_button)
      req(answer_one())
      HTML(paste(chat_content(), collapse = "\n <hr> \n"))
    })

    # JavaScript to trigger the send button when Enter key is pressed
    shinyjs::runjs("
        $('#qa-ask_question').on('keyup', function (e) {
            if (e.keyCode === 13) {
                setTimeout(function(){
                    $('#qa-ask_button').click();
                }, 500);  // Delay of 100 milliseconds
            }
        });
    ")


    observeEvent(answer_one(), {
      showModal(
        modalDialog(
          title = strong("Chat With Your Tutor"),
          # Custom CSS to make the chat area scrollable
          tags$head(
            tags$style(HTML("
                .modal-dialog {width: 30%;max-width: 30%;margin: 20px;}
                #chat_window {height: 500px;width: 100%;overflow-y: auto;padding:
                  10px;border-radius: 5px;}
            "))
          ),
          div( id = "chat_window", htmlOutput(ns("answer"))),
          tags$head(
            tags$style(
              "#answer{
                color: purple;
                font-size: 16px
              }"
            )
          ),
          footer = tagList(
            modalButton("Close")
          ),
          size = "s",
          easyClose = TRUE
        )
      )
    })



    ### Helper Functions ###

    # History/Record Keeping
    build_history <- function(prepared_request) {
      prompt_total <- list()

      # Add system role
      if (!is.null(system_role_tutor) && nchar(system_role_tutor) > 10) {
        prompt_total <- append(prompt_total, list(list(role = "system", content = system_role_tutor)))
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
          code_plus_console <- ch$code_history[[i]]$raw

          if (i == length(ch$code_history)) {
            if(code_error()){
              code_plus_console <- paste0(code_plus_console, "\n\nError: ", run_result()$error_message)
            } else{
                result <- paste(run_result()$console_output, collapse = "\n")
                code_plus_console <- paste0(code_plus_console, "\n\nResult: ", result,"\n")
            }
          }

          prompt_total <- append(prompt_total, list(
            list(role = "user", content = ch$code_history[[i]]$prompt_all),
            list(role = "assistant", content = code_plus_console)
          ))
        }
      }

      # Return prompt history
      return(prompt_total)
    }


    ### Update Components ###

      # Update environment
      update_environment <- function() {

        # Update environment
        # run_env(rlang::env(run_env(), df = current_data(), df_name = selected_dataset_name()))
        # run_env_start(as.list(run_env()))

        # Display selected data
        if (length(ch$code_history) == 0) {
          showNotification(paste("Selected dataset:", selected_dataset_name()), duration = 10)
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
      relevancy_agent <- function() {

        # If user selects preloaded data,    '&& != user_upload'
        # if (dataset_name() != no_data) {
        #   dataset_details <- paste("the built-in R dataset", selected_dataset_name())
        # } #else if (dataset_name() == user_upload) {
        # #  dataset_details <- metadata you create
        # #}

        # If user selects preloaded data
        if (selected_dataset_name() == no_data){ # Skip relevancy agent if user selects 'no data'
          return(TRUE)
        } else if (selected_dataset_name() == user_upload){
          return(TRUE)
        } else {

          dataset_details <- paste("the built-in R dataset", selected_dataset_name())

          base_prompt <- paste(
            "Current prompt:",
            input$ask_question, #input_text()
            "Current dataset:",
            dataset_details
          )

          if (length(ch$code_history) == 0) {
            relevancy_prompt <- list(
              list(role = "system", content = paste("Act as an experienced data analyst.",
                "Determine if the following prompts are relevant to the current dataset:",
                dataset_details)
              ),
              list(role = "user", content = paste("Determine if the current prompt is relevant to the selected dataset.",
                "If it is relevant, respond with 'True'. Otherwise, respond with 'False'.",
                base_prompt)
              )
            )
          } else {
            relevancy_prompt <- list(list(
              role = "user",
              content = paste(
                "Determine if the current prompt is relevant to any of the previous prompts.",
                "The prompt is relevant if it is a followup question for the analysis on the current dataset.",
                "If the prompt is a question about a different dataset it is not relevant.",
                "The prompt is also relevant if it is a modification for the visualizations.",
                "If it is relevant, respond with 'True'. Otherwise, respond with 'False'.",
                base_prompt
              )
            ))
          }

          # Send relevancy prompt
          response <- openAI_agent(relevancy_prompt)

          # TRUE if response is true, else FALSE
          is_relevant <- tolower(response$choices$message.content) == "true"
          return(is_relevant)
        }
      }


      # Request agent
      send_request_qa <- function(prompt_total, prepared_request) {


        # If prompt is relevant, send request
        prompt_total <- append(prompt_total, list(list(role = "user", content = prepared_request))) #paste0(prepared_request, dataset_details)

        # Send request
        response <- openAI_agent(prompt_total)
        response$choices[1, 1] <- response$choices$message.content

        return(response)
      }


      # Process response & return all response info
      process_response <- function(response, start_time) {

        # Handle if error/no error
        error_api <- !is.null(response$error_status)
        cmd <- if (error_api) NULL else response$choices[1, 1]
        # error_message <- if (error_api) response$message else NULL

        # Get API time
        api_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        if (counter$requests > 100 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
        }

        update_counter(response, api_time)


        # final_response$response
        humor <- c(
          "Seriously? Statistics only!",
          "Come on. Statistics only!",
          "You know better. Statistics only!",
          "Bruh... I am a statistics tutor! ",
          "Are you kidding? Statistics only!",
          "Gee..., Statistics only!!"
        )
        if(is.null(cmd)){
          cmd <- "Error in LLM Response"
        }else{
          if (grepl("No comment", cmd)) {
            cmd <- paste(
              sample(humor, 1),
              "     Ask again with more context. It might
              be helpful to add \"in statistics\" to the question."
            )
          }
        }

        # Replace double newlines with HTML paragraph tags
        cmd2 <- polish_cmd(cmd)
        cmd2 <- gsub("\n\n", "</p><p>", cmd2)
        cmd2 <- paste0("<p><strong>", input$ask_question, "</strong></p>", "<p>", cmd2, "</p>")

        return(cmd2)



        # Store info in response variable, return it
        # return(list(
        #   cmd = polish_cmd(cmd),
        #   response = response,
        #   time = round(api_time, 0),
        #   error = error_api,
        #   error_message = error_message
        # ))
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




    # Return all reactive values so they can be used outside the module
    return(
      list(

      )
    )

  })
}