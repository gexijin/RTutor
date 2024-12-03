#____________________________________________________________________________
#  Send Request
#____________________________________________________________________________


mod_03_send_request_ui <- function(id) {

  ns <- NS(id)

  tagList(

    hr(class = "custom-hr"),
    tags$label("3. Send Request",
      style = "font-size: 18px;font-weight: bold;color: #333;display: block;margin-bottom: 5px;"),

    textAreaInput(
      inputId = ns("input_text"),
      label = NULL,
      placeholder = "Ask questions or request analyses in English or other languages. For general questions, briefly explain the data first. See examples below.",
      rows = 6
    ),

    # Example Prompts
    uiOutput(ns("prompt_ui")),
    hr(class = "custom-hr"),

    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between;",
          div(
            # Submit Button
            actionButton(ns("submit_button"), strong("Submit")),

            tippy::tippy_this(
              ns("submit_button"),
              "ChatGPT can return different results for the same request.",
              theme = "light-border"
            )
          ),
          div(
            # Reset Button
            actionButton(ns("reset_button"), strong("Reset")),

            tippy::tippy_this(
              ns("reset_button"),
              "Reset before asking a new question. Clears data objects, chat history, & code chunks.",
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
                                     selected_dataset_name) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

      names(choices) <- demo$name[match(choices, demo$requests)]

      tagList(
        # CSS Styles
        tags$head(tags$style(HTML(paste0("
          .padding {padding-top: 10px;padding-left: 10px;padding-bottom: 10px;}

          #", ns("demo_prompt"), "+div .selectize-input {background-color: #F6FFF5 !important;border-color: #90BD8C !important;color: #000 !important;}
          #", ns("demo_prompt"), "+div .selectize-dropdown {background-color: #F6FFF5 !important;border-color: #90BD8C !important;color: #000 !important;}            
        ")))),

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

    # User Request Handling
    observeEvent(input$submit_button, {
      # if user's request too short, do not send
      if (nchar(input$input_text) < min_query_length) {
        showNotification(
          paste(
            "Request too short! Should be more than ",
            min_query_length,
            " characters."
          ),
          duration = 10
        )
      }
      # if user's request too long, do not send
      if (nchar(input$input_text) > max_query_length) {
        showNotification(
          paste(
            "Request too long! Should be less than ",
            max_query_length,
            " characters."
          ),
          duration = 10
        )
      }

      # if no file is selected, do not send
      if (selected_dataset_name() == "Select a Dataset:") {
        showNotification(
          paste("No file found. Please select a dataset and try again."),
          duration = 10
        )
      }
    })

    observeEvent(input$reset_button, {
      # reset session
      session$reload()
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