#____________________________________________________________________________
#  Send Request
#____________________________________________________________________________


mod_03_send_request_ui <- function(id) {

  ns <- NS(id)

  tagList(

    tags$label("3. Prompt",
      style = "font-size: 18px;font-weight: bold;color: #000;display: block;margin-bottom: 5px;"),

    textAreaInput(
      inputId = ns("input_text"),
      label = NULL,
      placeholder = "Ask questions or request analyses in English or other languages. For general questions, briefly explain the data first. See examples below.",
      rows = 6
    ),

    # Example Prompts
    uiOutput(ns("prompt_ui")),
    hr(class = "custom-hr"),

    # Quality gate feedback panel (hidden until a vague prompt is detected)
    uiOutput(ns("quality_feedback_ui")),

    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between;",
          div(
            # Reset Button
            actionButton(ns("reset_button"), strong("Reset")),

            tippy::tippy_this(
              ns("reset_button"),
              "Resets only chat history & code. To start with new data, refresh the page.",
              theme = "light-border"
            )
          ),
          div(
            # Submit Button
            actionButton(ns("submit_button"), strong("Submit")),

            tippy::tippy_this(
              ns("submit_button"),
              "ChatGPT can return different results for the same request.",
              theme = "light-border"
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        width = 12,
        hr(class = "custom-hr")
      )
    )

  )
}


mod_03_send_request_serv <- function(id, chunk_selection, user_file,
                                     selected_dataset_name, use_python,
                                     quality_cleared, api_key, current_data,
                                     do_soft_reset, counter) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tracks whether the user has already seen a quality warning for the current prompt
    quality_warned <- reactiveVal(FALSE)

    # Load previous prompts based on selected chunk
    observeEvent(chunk_selection$selected_chunk, {
      req(chunk_selection$past_prompt)

      updateTextAreaInput(
        session,
        inputId = "input_text",
        value = chunk_selection$past_prompt
      )
    })

    # Load demo prompts based on selected data
    observeEvent(input$demo_prompt, {
      req(input$demo_prompt != demo$requests[1]) # "Example requests"

      updateTextAreaInput(
        session,
        inputId = "input_text",
        value = input$demo_prompt
      )
    })

    # Display demo prompts (example requests)
    output$prompt_ui <- renderUI({
      req(is.null(user_file()))

      # Filter examples based on selected dataset
      choices <- switch(selected_dataset_name(),
        "no_data" = demo$requests[demo$data == "No Data"],
        "iris" = demo$requests[demo$data == "Iris (examples)"],
        "mpg" = demo$requests[demo$data == "MPG (examples)"],
        "airquality" = demo$requests[demo$data == "Air Quality (examples)"],
        "diamonds" = demo$requests[demo$data == "Diamonds (examples)"],
        "CO2" = demo$requests[demo$data == "CO2 (examples)"],
        "ToothGrowth" = demo$requests[demo$data == "Tooth Growth (examples)"],
        "pressure" = demo$requests[demo$data == "Pressure (examples)"],
        "ChickWeight" = demo$requests[demo$data == "Chick Weights (examples)"],
        "rna_seq" = demo$requests[demo$data == "RNA Seq (examples)"],
        demo$requests[demo$data == "Select a Dataset:"]
      )

      # Additional filtering based on use_python()
      if (!use_python()) {
        choices <- choices[demo$R[match(choices, demo$requests)] == 1]
      } else {
        choices <- choices[demo$Python[match(choices, demo$requests)] == 1]
      }

      names(choices) <- demo$name[match(choices, demo$requests)]

      tagList(
        fluidRow(
          column(
            width = 5,
            div("", class = "padding")
          ),
          column(
            width = 7,
            align = "left",
            selectInput(
              inputId = ns("demo_prompt"),
              choices = choices,
              selected = NULL,
              label = NULL
            )
          )
        )
      )
    })

    # Default quality feedback panel to empty
    output$quality_feedback_ui <- renderUI(NULL)

    # User Request Handling
    observeEvent(input$submit_button, {
      # if user's request too short, do not send
      if (nchar(input$input_text) < min_query_length) {
        showNotification(
          paste("Request too short! Should be more than", min_query_length, "characters."),
          duration = 10
        )
        return()
      }
      # if user's request too long, do not send
      if (nchar(input$input_text) > max_query_length) {
        showNotification(
          paste("Request too long! Should be less than", max_query_length, "characters."),
          duration = 10
        )
        return()
      }
      # if no file is selected, do not send
      if (selected_dataset_name() == data_placeholder) {
        showNotification(
          "Please select a dataset in Step 1 before submitting.",
          duration = 10
        )
        return()
      }

      # If user already saw the quality warning and is re-clicking Submit to bypass
      if (quality_warned()) {
        quality_warned(FALSE)
        output$quality_feedback_ui <- renderUI(NULL)
        quality_cleared(quality_cleared() + 1)
        return()
      }

      # Run quality check before calling the LLM wrapped in tryCatch
      notif_id <- showNotification("Checking prompt...", duration = NULL)
      result <- tryCatch(
        check_prompt_quality(
          prompt       = input$input_text,
          api_key      = api_key,
          dataset_name = selected_dataset_name(),
          col_names    = colnames(current_data())
        ),
        error = function(e) {
          message("[QUALITY] check_prompt_quality failed: ", e$message)
          list(verdict = "ok", suggestions = character(0))
        }
      )
      removeNotification(notif_id)

      # Track cost of this mini call
      if (!is.null(result$usage)) {
        mini_cost <- api_cost(result$usage$prompt_tokens, result$usage$completion_tokens, "gpt-4o-mini")
        counter$costs_total <- counter$costs_total + mini_cost
        message(sprintf("[COST] %-25s $%.6f  (total: $%.6f)", "Prompt quality check", mini_cost, counter$costs_total))
      }

      if (result$verdict == "ok") {
        output$quality_feedback_ui <- renderUI(NULL)
        quality_cleared(quality_cleared() + 1)
      } else {
        quality_warned(TRUE)
        suggestions <- result$suggestions
        output$quality_feedback_ui <- renderUI({
          div(
            style = "background-color: #fff8e1; border-left: 3px solid #ffc107; padding: 10px; margin-bottom: 10px;",
            tags$p(strong("\u26a0\ufe0f Your prompt may need more detail.")),
            tags$p("Here are some suggestions (or click Submit again to proceed with your original prompt):"),
            radioButtons(
              inputId  = ns("suggestion_choice"),
              label    = NULL,
              choices  = c(suggestions, "Keep my original prompt"),
              selected = if (length(suggestions) > 0) suggestions[1] else "Keep my original prompt"
            ),
            actionButton(ns("submit_anyway"), strong("Submit"), class = "btn-warning")
          )
        })
      }
    })

    # Feedback panel Submit button: apply selected suggestion (if any) then release the gate
    observeEvent(input$submit_anyway, {
      choice <- input$suggestion_choice
      if (!is.null(choice) && choice != "Keep my original prompt") {
        updateTextAreaInput(session, "input_text", value = choice)
      }
      output$quality_feedback_ui <- renderUI(NULL)
      quality_warned(FALSE)
      quality_cleared(quality_cleared() + 1)
    })

    observeEvent(input$reset_button, {
      quality_warned(FALSE)
      output$quality_feedback_ui <- renderUI(NULL)
      do_soft_reset()
    })


    # Return all reactive values so they can be used outside the module
    return(
      list(
        input_text = reactive(input$input_text),
        submit_button = reactive(input$submit_button),
        reset_button = reactive(input$reset_button)
      )
    )

  })
}