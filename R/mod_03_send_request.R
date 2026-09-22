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
      placeholder = "Ask questions or request analyses in English or other languages. For general questions, briefly explain the data first. Select data to see examples below.",
      rows = 6
    ),

    # Example Prompts
    uiOutput(ns("prompt_ui")),
    #hr(class = "custom-hr"),

    # Quality gate feedback panel (hidden until a vague prompt is detected)
    uiOutput(ns("quality_feedback_ui")),

    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between; margin-top: 7px;",
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
                                     do_soft_reset, counter, is_follow_up) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tracks whether the user has already seen a quality warning for the current prompt
    quality_warned <- reactiveVal(FALSE)
    # The exact prompt text that was warned about; Submit overrides only if it is unchanged
    warned_prompt  <- reactiveVal("")

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
      req(selected_dataset_name() == "no_data")

      # Filter examples based on selected dataset
      choices <- switch(selected_dataset_name(),
        "no_data"          = demo$requests[demo$data == "No Data"],
        "fertilizer"       = demo$requests[demo$data == "Fertilizer (examples)"],
        "salary_main"      = demo$requests[demo$data == "Salary Main (examples)"],
        "diet_interaction" = demo$requests[demo$data == "Diet Interaction (examples)"],
        "grade_interaction"= demo$requests[demo$data == "Grade Interaction (examples)"],
        "exam_grades"      = demo$requests[demo$data == "Exam Grades (examples)"],
        "movie_release"    = demo$requests[demo$data == "Movie Release (examples)"],
        "mutual_funds"     = demo$requests[demo$data == "Mutual Funds (examples)"],
        "acupuncture"      = demo$requests[demo$data == "Acupuncture (examples)"],
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

      # Re-clicking Submit on the same warned prompt overrides the (one-time) detail block
      if (quality_warned() && identical(input$input_text, warned_prompt())) {
        quality_warned(FALSE)
        output$quality_feedback_ui <- renderUI(NULL)
        quality_cleared(quality_cleared() + 1)
        return()
      }

      # Follow-ups tweak existing output, so they are only screened for off-topic.
      # An edited prompt after a warning is treated the same way: never block for detail twice.
      off_topic_only <- is_follow_up() || quality_warned()
      quality_warned(FALSE)

      notif_id <- showNotification("Checking prompt...", duration = NULL)
      result <- tryCatch(
        check_prompt_quality(
          prompt         = input$input_text,
          api_key        = api_key,
          dataset_name   = selected_dataset_name(),
          col_names      = colnames(current_data()),
          col_types      = vapply(current_data(), function(x) class(x)[1], character(1)),
          off_topic_only = off_topic_only
        ),
        error = function(e) {
          message("[QUALITY] check_prompt_quality failed: ", e$message)
          list(verdict = "ok", missing = character(0), suggestions = character(0))
        }
      )
      removeNotification(notif_id)

      # Track cost of this mini call
      if (!is.null(result$usage)) {
        mini_cost <- api_cost(result$usage$prompt_tokens, result$usage$completion_tokens, language_models[[default_model]])
        counter$costs_total <- counter$costs_total + mini_cost
        message(sprintf("[COST] %-25s $%.6f  (total: $%.6f)", "Prompt quality check", mini_cost, counter$costs_total))
      }

      box_style <- "background-color: #fff8e1; border-left: 3px solid #ffc107; padding: 10px; margin-top: 8px; margin-bottom: 10px;"

      if (result$verdict == "off_topic") {
        # No override: off-topic prompts never reach the code generator
        output$quality_feedback_ui <- renderUI(div(
          style = box_style,
          tags$p(strong("⚠️ Your prompt appears to be off-topic.")),
          tags$p(
            "RTutor only runs prompts about your dataset, statistics, or data science. ",
            "Please rewrite your prompt so it is on topic, then click Submit."
          )
        ))
      } else if (result$verdict == "vague") {
        quality_warned(TRUE)
        warned_prompt(input$input_text)
        output$quality_feedback_ui <- renderUI(div(
          style = box_style,
          tags$p(strong("⚠️ Your prompt is missing something the code needs:")),
          tags$ul(lapply(result$missing, tags$li)),
          tags$p(
            style = "margin-top: 6px; color: #666;",
            "(or click Submit again to run your original prompt anyway)"
          )
        ))
      } else {
        output$quality_feedback_ui <- renderUI(NULL)
        if (length(result$suggestions) > 0) {
          showNotification(
            tagList(strong("Tip for next time:"), tags$ul(lapply(result$suggestions, tags$li))),
            duration = 12, type = "message", id = "quality_tip"  # styled in mod_01_styles.R
          )
        }
        quality_cleared(quality_cleared() + 1)
      }
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