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
            label = HTML("<span style='font-size: 18px;'>4. Ask About Results</span>"),
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
      return("show")
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
      req(input$ask_button, answer_one())
      # chat_content() is a vector of HTML strings (newest first); display oldest first
      blocks <- rev(chat_content())
      html_blocks <- lapply(seq_along(blocks), function(i) {
        tagList(
          div(class = "qa-answer-block", HTML(blocks[[i]])),
          if (i < length(blocks)) hr(class = "qa-divider") else NULL
        )
      })
      tagList(html_blocks)
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
      showModal(modalDialog(
        title = strong("Chat With Your Tutor"),
        tags$style(HTML("
          .modal-dialog { width: 55% !important; max-width: 55% !important; }
          #chat_window {
            height: 520px; width: 100%; overflow-y: auto;
            padding: 14px 18px; border-radius: 6px;
            background: #fafafa;
          }
          .qa-answer-block { margin-bottom: 20px; }
          .qa-question {
            font-size: 15px; font-weight: 600; color: #333;
            border-left: 3px solid #90BD8C; padding-left: 10px;
            margin-bottom: 8px;
          }
          .qa-answer-block h1, .qa-answer-block h2, .qa-answer-block h3 {
            font-size: 16px; font-weight: 700; margin-top: 10px; color: #222;
          }
          .qa-answer-block p  { font-size: 14px; line-height: 1.6; color: #333; margin: 6px 0; }
          .qa-answer-block ul, .qa-answer-block ol { padding-left: 20px; margin: 6px 0; }
          .qa-answer-block li { font-size: 14px; line-height: 1.6; color: #333; }
          .qa-answer-block code {
            background: #f0f0f0; padding: 1px 5px;
            border-radius: 3px; font-size: 13px; font-family: monospace;
          }
          .qa-answer-block pre {
            background: #f5f5f5; border: 1px solid #ddd;
            border-radius: 4px; padding: 10px; overflow-x: auto;
          }
          .qa-answer-block pre code { background: none; padding: 0; }
          .qa-answer-block strong { color: #111; }
          .qa-divider { border: none; border-top: 1px solid #e0e0e0; margin: 16px 0; }
        ")),
        div(id = "chat_window", htmlOutput(ns("answer"))),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    })



    ### Helper Functions ###

    # History/Record Keeping
    build_history <- function(prepared_request) {
      prompt_total <- list()

      # Add system role
      if (!is.null(system_role_tutor) && nchar(system_role_tutor) > 10) {
        system_content <- format_content(system_role_tutor)
        prompt_total <- append(prompt_total,
          list(list(role = "system", content = system_content))
        )
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
            if (code_error()) {
              code_plus_console <- paste0(code_plus_console, "\n\nError: ", run_result()$error_message)
            } else {
              result <- paste(run_result()$console_output, collapse = "\n")
              code_plus_console <- paste0(code_plus_console, "\n\nResult: ", result,"\n")
            }
          }

          prompt_total <- append(prompt_total, list(
            list(role = "user", content = format_content(ch$code_history[[i]]$prompt_all)),
            list(role = "assistant", content = format_content(code_plus_console))
          ))
        }
      }

      # Return prompt history
      return(prompt_total)
    }

    # Format prompt content based on API key status & toggle status
    format_content <- function(text) {
      if (!is.null(api_key$key) && nchar(api_key$key) > 0 && api_key$switch_on) {
        # 1. Format for OpenAI
        return(paste(text))
      } else {
        # 2. Format for Azure
        return(list(list(
          type = "text",
          text = text
        )))
      }
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

      # Request agent
      send_request_qa <- function(prompt_total, prepared_request) {
        # Format content based on API key status
        formatted_request <- format_content(prepared_request)
        
        # Append user request
        prompt_total <- append(prompt_total, list(list(
          role = "user",
          content = formatted_request
        )))

        # Send request to appropriate agent
        response <- if (!is.null(api_key$key) && nchar(api_key$key) > 0 && api_key$switch_on) {
          openAI_agent(prompt_total)
        } else {
          azure_openAI_agent(prompt_total)
        }

        return(response)
      }


      # Process response & return all response info
      process_response <- function(response, start_time) {

        # Handle if error/no error
        error_api <- !is.null(response$error_status)
        cmd <- if (error_api) NULL else response$choices$message.content

        # error_message <- if (error_api) response$message else NULL

        # Get API time
        api_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        if (counter$requests > 100 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
        }

        update_counter(response, api_time)

        humor <- c(
          "Seriously? Statistics only!",
          "Come on. Statistics only!",
          "You know better. Statistics only!",
          "Bruh... I am a statistics tutor! ",
          "Are you kidding? Statistics only!",
          "Gee..., Statistics only!!"
        )
        if (is.null(cmd)) { # If response is null
          cmd <- "Error in LLM Response"
          return(cmd)
        } else {
          if (grepl("No comment", cmd)) { # If response is irrelevant
            cmd <- paste(
              sample(humor, 1),
              "     Ask again with more context. It might
              be helpful to add \"in statistics\" to the question."
            )
            return(cmd)
          }
        }

        # Render markdown response to HTML
        rendered <- commonmark::markdown_html(cmd, extensions = TRUE)
        cmd2 <- paste0(
          "<div class='qa-question'>", htmltools::htmlEscape(input$ask_question), "</div>",
          rendered
        )

        return(cmd2)
      }


      ### LLM Functions ###

      # OpenAI ChatGPT API function
      openAI_agent <- function(messages) {
        print("OpenAI")

        # Check if the selected model is "o4-mini"
        model_name <- selected_model()

        response <- tryCatch(
          {
            if (model_name == "o4-mini") {
              # Call API without temperature
              res <- openai::create_chat_completion(
                model = model_name,
                openai_api_key = api_key$key,
                messages = messages
              )
            } else {
              # Call API with temperature
              res <- openai::create_chat_completion(
                model = model_name,
                openai_api_key = api_key$key,
                temperature = sample_temp(),
                messages = messages
              )
            }
            # print(res)  # Uncomment for debugging
            res  # Return response
          },
          error = function(e) {
            print(e)  # Print error details
            return(NULL)
          }
        )
        return(response)
      }

      # Azure OpenAI ChatGPT API function
      azure_openAI_agent <- function(messages) {
        print("Azure")

        create_chat_completion_azure(
          model = selected_model(),
          api_version = api_versions[[selected_model()]],
          temperature = sample_temp(),
          messages = messages
        )
      }

  })
}