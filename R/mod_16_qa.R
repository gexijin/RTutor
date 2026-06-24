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
          div(
            style = "margin-bottom: 5px; position: relative;",
            textInput(
              inputId = ns("ask_question"),
              label = HTML("<span style='font-size: 18px;'>4. Explore Further</span>"),
              placeholder = "Q&A on code, results, error, or statistics in general",
              value = ""
            ),
            tags$div(
              id = ns("qa_dropdown"),
              class = "qa-dropdown",
              style = "display: none;",
              tags$ul(
                class = "qa-dropdown-list",
                tags$li(
                  class = "qa-dropdown-item",
                  `data-value` = "Explain code line by line",
                  "Explain code line by line"
                )
              )
            ),
            tippy::tippy_this(
              ns("ask_question"),
              "'What does this result mean?',
              'What is this error about?', 'Explain logistic regression',
              'List R packages for time series analysis'.<br>
              Hit 'Enter' key to send.",
              theme = "light-border"
            ),
            shinyjs::hidden(actionButton(ns("ask_button"), "Ask")),
            #hr(class = "custom-hr")
          )
        )
      )
    )
  )
}



mod_16_qa_serv <- function(id, submit_button, ch, code_error, run_result, api_error_modal, counter,
  selected_model, api_key, sample_temp, selected_dataset_name, qa_by_chunk, chunk_selection) {

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

        # Enforce minimum question length
        if (nchar(trimws(txt)) < 7) return(NULL)

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

        start_time <- Sys.time()
        api_error_occurred <- FALSE

        # Get LLM response
        response <- tryCatch({
          # Update env. & append history, if any
          update_environment()

          prompt_total <- build_history(prepared_request)

          # Send request
          send_request_qa(prompt_total, prepared_request)

        }, error = function(e) {   # handle error, if any
          api_error_occurred <<- TRUE
          shinybusy::remove_modal_spinner()   # close spinner first, then show error modal
          shiny::showModal(api_error_modal(e$message))
          list(
            error_value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        })

        if (!api_error_occurred) shinybusy::remove_modal_spinner()

        final_response <- process_response(response, start_time)
      })
      return(final_response)
    })



    observeEvent(input$ask_button, {
      new_entry <- answer_one()
      if (!is.null(new_entry)) {
        # Store answer under current chunk ID (append = oldest first)
        chunk_id   <- as.character(chunk_selection$selected_chunk)
        current    <- qa_by_chunk()
        current[[chunk_id]] <- c(current[[chunk_id]], list(new_entry))
        qa_by_chunk(current)

        updateTextInput(
          session,
          inputId = "ask_question",
          label = NULL,
          placeholder = "Q&A on code, results, error, or statistics in general",
          value = ""
        )

        # Auto-scroll to the Q&A section after the UI has had time to render
        shinyjs::runjs("
          setTimeout(function() {
            var qa = document.getElementById('qa-section');
            if (qa) qa.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }, 400);
        ")
      }
    })


    # JavaScript: suggestion dropdown + validation + Enter key submit
    shinyjs::runjs("
      // Show dropdown on focus when input is empty
      $(document).on('focus', '#qa-ask_question', function() {
        if ($(this).val().trim() === '') {
          $('#qa-qa_dropdown').show();
        }
      });

      // Hide dropdown as soon as user starts typing
      $(document).on('input', '#qa-ask_question', function() {
        $('#qa-qa_dropdown').hide();
      });

      // Populate input when a suggestion is clicked (mousedown fires before blur)
      $(document).on('mousedown', '.qa-dropdown-item', function() {
        var val = $(this).data('value');
        $('#qa-ask_question').val(val);
        Shiny.setInputValue('qa-ask_question', val);
        $('#qa-qa_dropdown').hide();
      });

      // Hide dropdown when clicking anywhere outside the input or dropdown
      $(document).on('mousedown', function(e) {
        if (!$(e.target).closest('#qa-ask_question, #qa-qa_dropdown').length) {
          $('#qa-qa_dropdown').hide();
        }
      });

      // Block button click when invalid; let native Shiny handler fire when valid
      $(document).on('click', '#qa-ask_button', function(e) {
        if ($('#qa-ask_question').val().trim().length < 7) {
          e.preventDefault();
          e.stopImmediatePropagation();
          return false;
        }
      });

      // Enter key: hide dropdown then trigger button click
      $(document).on('keyup', '#qa-ask_question', function(e) {
        if (e.keyCode === 13) {
          $('#qa-qa_dropdown').hide();
          setTimeout(function() { $('#qa-ask_button').click(); }, 100);
        }
      });
    ")





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
    format_content <- function(text) paste(text)


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
        call_cost <- api_cost(response$usage$prompt_tokens, response$usage$completion_tokens, selected_model())
        counter$costs_total <- counter$costs_total + call_cost
        message(sprintf("[COST] %-25s $%.6f  (total: $%.6f)", "Q&A response", call_cost, counter$costs_total))
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

        response <- llm_agent(prompt_total)

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
        question_text <- htmltools::htmlEscape(input$ask_question)

        if (is.null(cmd)) { # If response is null
          return(list(question = question_text, answer = "<p>Error in LLM Response</p>"))
        }

        if (grepl("No comment", cmd)) { # If response is irrelevant
          answer_text <- paste(
            sample(humor, 1),
            "Ask again with more context. It might be helpful to add \"in statistics\" to the question."
          )
          return(list(question = question_text, answer = paste0("<p>", htmltools::htmlEscape(answer_text), "</p>")))
        }

        # Render markdown response to HTML
        rendered <- commonmark::markdown_html(cmd, extensions = TRUE)
        return(list(question = question_text, answer = rendered))
      }


      ### LLM Functions ###

      llm_agent <- function(messages) {
        p <- resolve_provider(api_key)
        create_response(language_models[[default_model]], messages, p$key, p$endpoint)
      }

  })
}