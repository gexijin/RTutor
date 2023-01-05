###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

#                            1.
#____________________________________________________________________________
#  General UI, observers, etc.
#____________________________________________________________________________

  # increase max input file size
  options(shiny.maxRequestSize = 10 * 1024^2) # 10MB

  pdf(NULL) #otherwise, base R plots sometimes do not show.

  # load demo data when clicked
  observeEvent(input$demo_prompt, {
    req(input$select_data)
    if (input$demo_prompt != demos_mpg[1]) {
      updateTextInput(
        session,
        "input_text",
        value = input$demo_prompt
      )
    } else { # if not mpg data, reset
      updateTextInput(
        session,
        "input_text",
        value = "",
        placeholder =
"Upload a file or use demo data. Then just ask questions or request analyses in plain English. For general questions, briefly explain the data first, especially the relevant columns. See examples above. If unsuccessful, try again with the same request or ask differently. Code works correctly some of the times. To use voice input, click Settings."
      )
    }
  })

  observeEvent(input$user_file, {
    updateSelectInput(
      session,
      "select_data",
      selected = uploaded_data
    )
  }, ignoreInit = TRUE, once = TRUE)

  observeEvent(input$submit_button, {
    updateTextInput(session, "submit_button", label = "Re-submit")
    shinyjs::hideElement(id = "Intro")
  })

  observe({
    shinyjs::hideElement(id = "load_message")
  })

#                                    2.
#____________________________________________________________________________
#  Voice naration
#____________________________________________________________________________

  # had to use this. Otherwise, the checkbox returns to false
  # when the popup is closed and openned again.
  use_voice <- reactive({
      use_voice <- FALSE #default
      tem <- is.null(input$use_voice_button)
      if(!is.null(input$use_voice)) {
        use_voice <- input$use_voice
      }
      return(use_voice)
  })

  # Use voice input?
  output$use_heyshiny <- renderUI({
    req(use_voice())
      tagList(
        heyshiny::useHeyshiny(language = "en-US"), # configure the heyshiny
        heyshiny::speechInput(
          inputId = "hey_cmd",
          command = "hey cox *msg"  # hey cox is more sensitive than 'hi tutor'
        ), # set the input
      )
  })

   # read the speech input
  observeEvent(input$hey_cmd, {
    speech <- input$hey_cmd
    message(speech)

    if (input$tabs == "Home")    {
      if (grepl("^continue", speech)) {

        speech <- paste0(
          input$input_text, # current prompt
          ". ",  # add . and space.
          gsub("^continue", "", speech) # remove the continue
        )
      }

      updateTextInput(
        session,
        "input_text",
        value = speech
      )
    } else if (input$tabs == "Ask") {

      speech <- paste0(
        input$input_text, # current prompt
        ". ",  # add . and space.
        gsub("^continue", "", speech) # remove the continue
      )
      updateTextInput(
        session,
        "ask_question",
        value = speech
      )

    }

  })

  #                             3.
  #____________________________________________________________________________
  #  Loading data
  #____________________________________________________________________________

  # uploaded data
  user_data <- reactive({
    req(input$user_file)
    in_file <- input$user_file
    in_file <- in_file$datapath
    req(!is.null(in_file))

    isolate({
      file_type <- "read_excel"
      # Excel file ---------------
      if(grepl("xls$|xlsx$", in_file, ignore.case = TRUE)) {
        df <- readxl::read_excel(in_file)
        df <- as.data.frame(df)
      } else {
        #CSV --------------------
        df <- read.csv(in_file)
        file_type <- "read.csv"
        # Tab-delimented file ----------
        if (ncol(df) == 2) {
          df <- read.table(
            in_file,
            sep = "\t",
            header = TRUE
          )
          file_type <- "read.table"
        }
      }
      # clean column names
      df <- df %>% janitor::clean_names()
      return(
        list(
          df = df,
          file_type = file_type
        )
      )
    })
  })


  # showing the current dataset. Warning if no is uploaded.
  output$selected_dataset <- renderText({
      req(input$submit_button)
      # when submit is clicked, but no data is uploaded.

      if(input$select_data == uploaded_data) {
        if(is.null(input$user_file)) {
          txt <- "No file uploaded! Please Reset and upload your data first."
        } else {
          txt <- "Dataset: uploaded."
        }
      } else {
        txt <- paste0("Dataset: ", input$select_data)
      }

      return(txt)
  })

  output$data_upload_ui <- renderUI({

    # Hide this input box after the first run.
    req(input$submit_button == 0)

    fileInput(
      inputId = "user_file",
      label = "File Upload",
      accept = c(
        "text/csv",
        "text/comma-separated-values",
        "text/tab-separated-values",
        "text/plain",
        ".csv",
        ".tsv",
        ".txt",
        ".xls",
        ".xlsx"
      )
    )
  })

  output$demo_data_ui <- renderUI({

    # Hide this input box after the first run.
    req(input$submit_button == 0)

    selectInput(
      inputId = "select_data",
      label = "Data",
      choices = datasets,
      selected = "mpg",
      multiple = FALSE,
      selectize = FALSE
    )

  })

  output$prompt_ui <- renderUI({
    req(input$select_data)
    # hide after data is uploaded
    req(is.null(input$user_file))

    if (input$select_data == "mpg") {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_mpg,
        label = "Example requests:"
      )
    } else if (input$select_data == no_data) {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_no_data,
        label = "Example requests:"
      )
    } else if (input$select_data == "diamonds") {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_diamond,
        label = "Example requests:"
      )
    } else if (input$select_data == rna_seq) {
      selectInput(
        inputId = "demo_prompt",
        choices = demos_rna_seq,
        label = "Example requests:"
      )
    } else {
      return(NULL)
    }
  })



  #                             4.
  #____________________________________________________________________________
  # API key management
  #____________________________________________________________________________
  # pop up modal for Settings
  observeEvent(input$api_button, {
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        footer = modalButton("Confirm"),
        tagList(
          fluidRow(
            column(
              width = 4,
              sliderInput(
                inputId = "temperature",
                label = "Sampling temperature",
                min = 0,
                max = 1,
                value = sample_temp(),
                step = .1,
                round = FALSE,
                width = "100%"
              )
            ),
            column(
              width = 8,
              p("This important parameter controls the AI's behavior in choosing 
              among possible answers. A higher sampling temperature tells the AI 
              to take more risks, producing more diverse and creative 
              solutions when the same request is repeated. A lower  temperature
              (such as 0) results in more
              conservative and well-defined solutions, 
              but less variety when repeated.
              "),
            )
          ),
          hr(),
          h4("Use your own API key"),
          h5("We pay a small fee to use the AI for every request.
            If you use this regularily, 
            please take a few minutes to create your own API key: "),

          tags$ul(
              tags$li(
                "Create a personal account at",
                a(
                  "OpenAI.",
                  href = "https://openai.com/api/",
                  target = "_blank"
                )
              ),
              tags$li("After logging in, click \"Personal\" from top right."),
              tags$li(
                "Click \"Manage Account\" and then \"Billing\",
                where you can add \"Payment methods\" and set \"Usage 
                limits\". $5 per month is more than enough."
              ),
              tags$li(
                "Click \"API keys\" to create a new key, 
                which can be copied and pasted it below."
              ),
          ),
          textInput(
            inputId = "api_key",
            label = h5("Paste your API key from OpenAI:"),
            value = NULL,
            placeholder = "sk-..... (51 characters)"
          ),
          uiOutput("valid_key"),
          uiOutput("save_api_ui"),
          verbatimTextOutput("session_api_source"),
          hr(),

          fluidRow(
            column(
              width = 6,
              checkboxInput(
                inputId = "use_voice",
                label = strong("Enable voice narration"),
                value = use_voice()
              )
            ),
            column(
              width = 6,
              # this causes the use_voice() to refresh twice,
              # triggering the permission seeking in Chrome.
              # Don't know why, but this works. I'm a stable genius.
              actionButton("use_voice_button", strong("Seek mic permission"))
            )
          ),
          h5("First select the checkbox and then seek 
          permission to use the microphone. Your browser should have a popup 
          window. Otherwise, check the both ends of the URL bar for a 
          blocked icon, which
          could be clicked to grant permission. If successful, you will see 
          a red dot on top of the tab in Chrome.
          Voice naration can be used in both the Main and the 
          Ask Me Anything tabs by just saying \"Hey Cox ...\" 
          in honor of the statistician David Cox.     
          If not satisfied, try again to overwrite. 
          To continue, say \"Hey Cox Continue ...\""),
        ),
        hr(),
        fluidRow(
          column(
            width = 4,
            checkboxInput(
              inputId = "numeric_as_factor",
              label = strong("Treat as factors"),
              value = convert_to_factor()
            ),
            tippy::tippy_this(
              elementId = "numeric_as_factor",
              tooltip = "Treat the columns that looks like a category 
              as a category. This applies to columns that contain numbers
              but have very few unique values. ",
              theme = "light-border"
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = "max_levels_factor",
              label = "Max levels",
              value = max_levels_factor(),
              min = 5,
              max = 50,
              step = 1
            ),
            tippy::tippy_this(
              elementId = "max_levels_factor",
              tooltip = "To convert a numeric column as category, 
              the column must have no more than this number of unique values.",
              theme = "light-border"
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = "max_proptortion_factor",
              label = "Max proportion",
              value = max_proptortion_factor(),
              min = 0.05,
              max = 0.5,
              step = 0.1
            ),
            tippy::tippy_this(
              elementId = "max_proptortion_factor",
              tooltip = "To convert a numeric column as category, 
              the number of unique values in a column must not exceed 
              more this proportion of the total number of rows.",
              theme = "light-border"
            )
          )
        ),
        h5("Some columns contains numbers but should be treated 
        as categorical values or factors. For example, we sometimes 
        use 1 to label success and 0 for failure.
        If this is selected, using the default setting, a column 
        is treated as categories when the number of unique values 
        is less than or equal to 12, and less than 10% of the total rows."
        ),
        hr(),
        fluidRow(
          column(
            width = 4,
            checkboxInput(
              inputId = "contribute_data",
              label = "Help us make RTutor better",
              value = contribute_data()
            )
          ),
          column(
            width = 8,
            h5("Save your requests and the structure of your data 
            such as column names and data types, not the data itself. 
            We can learn from users about creative ways to use AI. 
            And we can try to improve unsuccessful attempts. ")
          )
        ),

      )
    )
  })
  # api key for the session
  api_key_session <- reactive({

    api_key <- api_key_global
    session_key_source <- key_source

    if(!is.null(input$api_key)) {
      key1 <- input$api_key
      key1 <- clean_api_key(key1)

      if (validate_api_key(key1)) {
        api_key <- key1
        session_key_source <- "pasted!"
      }
    }
    return(
      list(
        api_key = api_key,
        key_source = session_key_source
      )
    )
  })

  output$session_api_source <- renderText({
    txt <- api_key_session()$api_key

    # The following is essential for correctly getting the 
    # environment variable on Linux!!! Don't ask.
    tem <- Sys.getenv("OPEN_API_KEY")
    paste0(
      "Current API key: ",
      substr(txt, 1, 4),
      ".....",
      substr(txt, nchar(txt) - 4, nchar(txt)),
      " (",
      api_key_session()$key_source,
      ")"
    )
  })

  output$save_api_ui <- renderUI({
    req(input$api_key)

    # only show this when running locally.
    req(!file.exists(on_server))
    req(validate_api_key(input$api_key))

    tagList(
      actionButton(
        inputId = "save_api_button",
        label = "Save key file for next time."
      ),
      tippy::tippy_this(
        elementId = "save_api_button",
        tooltip = "Save to a local file, 
        so that you do not have to copy and paste next time.",
        theme = "light-border"
      )
    )
  })

  output$valid_key <- renderUI({
    req(input$api_key)

    if(validate_api_key(input$api_key)) {
      h4(
        "Key looks good. Just close this window.",
        style = "color:blue"
      )
    } else {
      h4(
        "That does not look like a valid key!",
        style = "color:red"
      )
    }
  })

  # only save key, if app is running locally.
  observeEvent(input$save_api_button, {
    req(input$save_api_button)
    req(input$api_key)
    writeLines(input$api_key, "api_key.txt")
  })

  # only save key, if app is running locally.
  observeEvent(input$submit_button, {
    # if too short, do not send.
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
    # if too short, do not send. 
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
  })


  #                        5.
  #____________________________________________________________________________
  # Send API Request, handle API errors
  #____________________________________________________________________________

  sample_temp <- reactive({
      temperature <- default_temperature #default
      if (!is.null(input$temperature)) {
         temperature <- input$temperature
      }
      return(temperature)
  })

  openAI_prompt <- reactive({
    req(input$submit_button)
    req(input$select_data)
    prep_input(input$input_text, input$select_data, current_data())
  })

  openAI_response <- reactive({
    req(input$submit_button)

    isolate({  # so that it will not responde to text, until submitted
      req(input$input_text)
      prepared_request <- openAI_prompt()
      req(prepared_request)

      # when submit is clicked, but no data is uploaded.
      if(input$select_data == uploaded_data) {
        req(user_data())
      }

      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = paste(
          sample(jokes, 1)
        ),
        color = "#000000"
      )

      start_time <- Sys.time()

      # Send to openAI
      tryCatch(
        response <- openai::create_completion(
          engine_id = language_model,
          prompt = prepared_request,
          openai_api_key = api_key_session()$api_key,
          max_tokens = 500,
          temperature = sample_temp()
        ),
        error = function(e) {
          # remove spinner, show message for 5s, & reload
          shinybusy::remove_modal_spinner()
          shiny::showModal(api_error_modal)
          Sys.sleep(5)
          session$reload()

          list(
            error_value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        }
      )

      error_api <- FALSE
      # if error returns true, otherwise 
      #  that slot does not exist, returning false.
      # or be NULL
      error_api <- tryCatch(
        !is.null(response$error_status),
        error = function(e) {
          return(TRUE)
        }
      )

      error_message <- NULL
      if(error_api) {
        cmd <- NULL
        response <- NULL
        error_message <- response$message
      } else {
        cmd <- response$choices[1, 1]
      }

      api_time <- difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )[[1]]

      # if more than 10 requests, slow down. Only on server.
      if(counter$requests > 20 && file.exists(on_server)) {
        Sys.sleep(counter$requests / 5 + runif(1, 0, 5))
      }
      if(counter$requests > 50 && file.exists(on_server)) {
        Sys.sleep(counter$requests / 10 + runif(1, 0, 10))
      }
      if(counter$requests > 100 && file.exists(on_server)) {
        Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
      }

      shinybusy::remove_modal_spinner()

    # update usage via global reactive value
    counter$tokens <- counter$tokens + response$usage$completion_tokens
    counter$requests <- counter$requests + 1
    counter$time <- round(api_time, 0)
    counter$tokens_current <- response$usage$completion_tokens

      return(
        list(
          cmd = cmd,
          response = response,
          time = round(api_time, 0),
          error = error_api,
          error_message = error_message
        )
      )
    })
  })

  # a modal shows api connection error
  api_error_modal <- shiny::modalDialog(
    title = "API connection error!",
    tags$h4("Is the API key is correct?", style = "color:red"),
    tags$h4("How about the WiFi?", style = "color:red"),
    tags$h5("If you are on RTutor.ai, maybe Dr G's API usage 
    is more than he can affort this month.", style = "color:red"),
    tags$h4(
      "Auto-reset ...", 
      style = "color:blue; text-align:right"
    ),
    easyClose = TRUE,
    size = "s"
  )


  # show a warning message when reached 10c, 20c, 30c ...
  observeEvent(input$submit_button, {
    req(file.exists(on_server))
    req(!openAI_response()$error)

    cost_session <-  round(counter$tokens * 2e-3, 0)
    if (cost_session %% 20  == 0 & cost_session != 0) {
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

  output$openAI <- renderText({
    req(openAI_response()$cmd)
    res <- logs$raw
    # Replace multiple newlines with just one.
    #res <- gsub("\n+", "\n", res)
    # Replace emplty lines,  [ ]{0, }--> zero or more space
    #res <- gsub("^[ ]{0, }\n", "", res)
    res <- gsub("```", "", res)

  })

 # Defining & initializing the reactiveValues object
  logs <- reactiveValues(
    id = 0, # 1, 2, 3, id for code chunk
    code = "", # cumulative code
    raw = "",  # cumulative orginal code for print out
    last_code = "", # last code for Rmarkdown
    code_history = list() # keep all code chunks
  )

  observeEvent(input$submit_button, {
    logs$id <- logs$id + 1
    # if not continue
    if(!input$continue) {
      logs$code <- openAI_response()$cmd

      logs$raw <- openAI_response()$response$choices[1, 1]
      # remove one or more blank lines in the beginning.
      logs$raw <- gsub("^\n+", "", logs$raw)

      logs$last_code <- ""

    } else { # if continue
      logs$last_code <- logs$code  # last code
      logs$code <- paste(
        logs$code,
        "\n",
        "#-------------------------------",
        openAI_response()$cmd
      )
      logs$raw <- paste(
        logs$raw,
        "\n\n#-------------------------\n",
        gsub("^\n+", "", openAI_response()$response$choices[1, 1])
      )
    }

    # A list holds current request
    current_code <- list(
      id = logs$id,
      code = logs$code,
      raw = logs$raw, # for print
      prompt = input$input_text,
      error = code_error(),
      rmd = Rmd_chunk()
    )

    logs$code_history <- append(logs$code_history, list(current_code))

    choices <- 1:length(logs$code_history)
    names(choices) <- paste0("Chunk #", choices)
    # update chunk choices
    updateSelectInput(
      inputId = "selected_chunk",
      label = "AI generated code:",
      choices = choices,
      selected = logs$id
    )

    # turn off continue button
    updateCheckboxInput(
      session = session,
      inputId = "continue",
      label = "Continue from this chunk",
      value = FALSE
    )
  })

  # change code when past code is selected.
  observeEvent(input$selected_chunk, {
    #req(run_result())

    id <- as.integer(input$selected_chunk)
    logs$code <- logs$code_history[[id]]$code
    logs$raw <- logs$code_history[[id]]$raw
    updateTextInput(
      session,
      "input_text",
      value = logs$code_history[[id]]$prompt
    )


  })

  output$usage <- renderText({
    req(input$submit_button != 0 || input$ask_button != 0)

    paste0(
      "R",
      counter$requests, ":  ",
      counter$tokens_current,
      " tokens, ",
      counter$time,
      " second(s)"
    )
  })

  output$total_cost <- renderText({
    if(input$submit_button == 0 & input$ask_button == 0) {
      return("OpenAI charges 2¢ per 1000 tokens/words 
      from our account. Heavy users 
      please use your own account. See Settings."
      )
    } else {
    #req(openAI_response()$cmd)
      paste0(
        "Cumulative API Cost: ",
        sprintf("%5.1f", counter$tokens * 2e-3),
        "¢"
      )
    }
  })

  output$temperature <- renderText({
    req(openAI_response()$cmd)

    paste0(
        "Temperature: ",
        sample_temp()
    )
  })

   output$retry_on_error <- renderText({
     req(code_error())
     if(code_error()) {
      "Error! Try again or change request."
     }

   })
 # Defining & initializing the reactiveValues object
  counter <- reactiveValues(
    tokens = 0, # cummulative tokens
    requests = 0, # cummulative requests    
    tokens_current = 0,  # tokens for current query
    time = 0 # response time for current
  )


  #                            6.
  #____________________________________________________________________________
  # Run the code, shows plots, code, and errors
  #____________________________________________________________________________

  # stores the results after running the generated code.
  # return error indicator and message

  # Note that the code is run three times!!!!!

  # Sometimes returns NULL, even when code run fine. Especially when
  # a base R plot is generated.
  run_result <- reactive({
    req(logs$code)
    req(input$submit_button != 0)
    withProgress(message = "Running the code ...", {
      incProgress(0.4)
      tryCatch(
        eval(
          parse(
            text = clean_cmd(
              logs$code,
              input$select_data
            )
          )
        ),
        error = function(e) {
          list(
            error_value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        }
      )
    })
  })

  # Error when run the generated code?
  code_error <- reactive({
    error_status <- FALSE
    req(input$submit_button != 0)

    # if error returns true, otherwise 
    #  that slot does not exist, returning false.
    # or be NULL
    try(  # if you do not 'try', the entire app quits! :-)
      if (is.list(run_result())) {
      req(!is.null(names(run_result())[1]))
        if (names(run_result())[1] == "error_value") {
          error_status <- TRUE
        }
      }
    )
    return(error_status)
  })


  output$error_message <- renderUI({
    req(!is.null(code_error()))
    if(code_error()) {
      h4(paste("Error!", run_result()$message), style = "color:red")
    } else {
      return(NULL)
    }

  })

  # just capture the screen output
  output$console_output <- renderText({
    req(!code_error())
    req(logs$code)
    withProgress(message = "Running the code for console...", {
      incProgress(0.4)
      try(
        out <- capture.output(eval(
          parse(
            text = clean_cmd(logs$code, input$select_data)
          )
          )
       )
      )

      # this works most of the times, but not when cat is used.
      #out <- capture.output(
      #    run_result()
      #)
      paste(out, collapse = "\n")
    })
  })

  # base R plots can not be auto generated from the run_results() object
  # must run the code again.
  output$result_plot <- renderPlot({
    req(!code_error())
    req(logs$code)
    withProgress(message = "Generating a plot ...", {
      incProgress(0.4)
      try(
        eval(
          parse(
            text =  clean_cmd(logs$code, input$select_data)
          )
        )
      )
    })
  })

  output$result_plotly <- plotly::renderPlotly({
    req(!code_error())
    req(
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive)
    )

    g <- run_result()
    # still errors some times, when the returned list is not a plot
    if(is.character(g) || is.data.frame(g) || is.numeric(g)) {
      return(NULL)
    } else {
      return(g)
    }

  })

  output$plot_ui <- renderUI({
    req(input$submit_button)
    
    if (code_error() || input$submit_button == 0) {
      return()
    } else if (
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive) # converted
    ){
      plotly::plotlyOutput("result_plotly")
    } else {
      plotOutput("result_plot")
    }
  })

  # reset to FALSE after each submission
  observeEvent(input$submit_button, {
    updateCheckboxInput(
      session = session,
      inputId = "make_ggplot_interactive",
      label = "Make it interactive!",
      value = FALSE
    )
  })

  observeEvent(input$submit_button, {
    # hide it by default
    shinyjs::hideElement(id = "make_ggplot_interactive")
    req(!code_error())
    req(logs$code)
    txt <- paste(openAI_response()$cmd, collapse = " ")

    if (grepl("ggplot", txt) && # if  ggplot2, and it is 
      !is_interactive_plot() && #not already an interactive plot, show
       # if there are too many data points, don't do the interactive
      !(dim(current_data())[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
    ) {
    shinyjs::showElement(id = "make_ggplot_interactive")
    }
  })

  is_interactive_plot <- reactive({
    # only true if the plot is interactive, natively.
    req(input$submit_button)
    req(logs$code)
    req(!code_error())
    if (
      grepl(
        "plotly|plot_ly|ggplotly",
        paste(logs$code, collapse = " ")
      )
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  output$tips_interactive <- renderUI({
    req(input$submit_button)
    req(openAI_response()$cmd)
    if(is_interactive_plot() ||   # natively interactive
      turned_on (input$make_ggplot_interactive)
     ) {
      tagList(
        p("Interactive plot. Mouse over to see values. Select a region to zoom. 
        Click on the legends to deselect a group. 
        Double click a category to hide all others. 
        Use the menu on the top right for other functions."
        )
      )
    }
  })

  rna_seq_data <- reactive({
    req(input$select_data == rna_seq)

    df <- read.csv(app_sys("app", "www", "GSE37704.csv"))
    return(df)
  })


  # had to use this. Otherwise, the checkbox returns to false
  # when the popup is closed and openned again.
  convert_to_factor <- reactive({
      convert <- TRUE #default
      if (!is.null(input$numeric_as_factor)) {
        convert <- input$numeric_as_factor
      }
      return(convert)
  })

  max_proptortion_factor <- reactive({
      max_proptortion <- TRUE #default
      if(!is.null(input$max_proptortion_factor)) {
        max_proptortion <- input$max_proptortion_factor
      }
      if(max_proptortion < 0.05) {
        max_proptortion <- 0.05
      }
      if(max_proptortion > 0.5) {
        max_proptortion <- 0.5
      }
      return(max_proptortion)
  })


   max_levels_factor <- reactive({
      max_levels_1 <- max_levels #default
      if (!is.null(input$max_levels_factor)) {
        max_levels_1 <- input$max_levels_factor
      }
      if (max_levels_1 < 2) {
        max_levels_1 <- 2
      }
      if (max_levels_1 > 100) {
        max_levels_1 <- 100
      }
      return(max_levels_1)
  })

  # The current data, just for showing.
  current_data <- reactive({
    req(input$select_data)

    if(input$select_data == uploaded_data) {
      eval(parse(text = paste0("df <- user_data()$df")))
    } else if(input$select_data == no_data){
      df <- NULL #as.data.frame("No data selected or uploaded.")
    } else if(input$select_data == rna_seq){
      df <- rna_seq_data()
    } else {
      # otherwise built-in data is unavailable when running from R package.
      library(tidyverse)
      eval(parse(text = paste0("df <- ", input$select_data)))
    }

    if (convert_to_factor()) {
      df <- numeric_to_factor(
        df,
        max_levels_factor(),
        max_proptortion_factor()
      )
    }

    # if the first column looks like id?
    if(
      length(unique(df[, 1])) == nrow(df) &&  # all unique
      is.character(df[, 1])  # first column is character
    ) {
       row.names(df) <- df[, 1]
       df <- df[, -1]
    }

    # sometimes no row is left after processing.
    if(is.null(df)) { # no_data
      return(NULL)
    } else if(nrow(df) == 0) {
      return(NULL)
    } else { # there are data in the dataframe

      return(df)
    }
  })


  # The data, after running the chunk
  data_afterwards <- reactive({
    req(input$select_data)
    req(current_data())

    if (input$submit_button == 0) {
      return(current_data())
    }

    df <- current_data()
    # This updates the data by running hte entire code one more time.
    if(input$submit_button != 0) {
      if (code_error() == FALSE && !is.null(logs$code)) {
        withProgress(message = "Updating values ...", {
          incProgress(0.4)
          try(
            eval(
              parse(
                text = clean_cmd(logs$code, input$select_data)
              )
            ),
          )
        })
      }
    }

    # sometimes no row is left after processing.
    if(is.null(df)) { # no_data
      return(NULL)
    } else if(nrow(df) == 0) {
      return(NULL)
    } else { # there are data in the dataframe
      return(df)
    }
  })


  output$data_table_DT <- DT::renderDataTable({
    req(data_afterwards())
    DT::datatable(
      current_data(),
      options = list(
        lengthMenu = c(5, 20, 50, 100),
        pageLength = 10,
        dom = 'ftp',
        scrollX = "400px"
      ),
      rownames = TRUE
    )
  })

  output$data_size <- renderText({
    req(!is.null(data_afterwards()))
    paste(
      dim(data_afterwards())[1], "rows X ",
      dim(data_afterwards())[2], "columns"
    )
  })
  output$data_structure <- renderPrint({
    req(!is.null(data_afterwards()))
    str(data_afterwards())
  })

  output$data_summary <- renderText({
    req(!is.null(data_afterwards()))
    paste(
      capture.output(
        summary(data_afterwards())
      ),
      collapse = "\n"
    )
  })



  #                                 7.
  #____________________________________________________________________________
  # Logs and Reports
  #____________________________________________________________________________


  observeEvent(input$submit_button, {
    choices <- 1:length(logs$code_history)
    names(choices) <- paste0("Chunk #", choices)
    updateSelectInput(
      inputId = "selected_chunk_report",
      label = "Chunks to include (Use backspace to delete):",
      selected = "All chunks without errors",
      choices = c(
        "All chunks",
        "All chunks without errors",
        choices
      )
    )

  })

  # collect all RMarkdown chunks
  Rmd_total <- reactive({

  Rmd_script <- ""

  # if first chunk
  Rmd_script <- paste0(
    Rmd_script,
    # Get the data from the params list-----------
    "\nDeveloped by [Steven Ge](https://twitter.com/StevenXGe) using API access via the 
[openai](https://cran.rstudio.com/web/packages/openai/index.html)
    package  to 
    [OpenAI's](https://cran.rstudio.com/web/packages/openai/index.html) \"",
    language_model,
    "\" model.",
    "\n\nRTutor Website: [https://RTutor.ai](https://RTutor.ai)",
    "\nSource code: [GitHub.](https://github.com/gexijin/RTutor)\n"
  )

  # if the first chunk & data is uploaded,
  # insert script for reading data
  if (input$select_data == uploaded_data) {

    # Read file
    file_name <- input$user_file$name
    if(user_data()$file_type == "read_excel") {
      txt <- paste0(
        "# install.packages(readxl)\nlibrary(readxl)\ndf <- read_excel(\"",
        file_name,
        "\")"
      )

    }
    if (user_data()$file_type == "read.csv") {
      txt <- paste0(
        "df <- read.csv(\"",
        file_name,
        "\")"
      )
    }
    if (user_data()$file_type == "read.table") {
      txt <- paste0(
        "df <- read.table(\"",
        file_name,
        "\", sep = \"\t\", header = TRUE)"
      )
    }

    Rmd_script <- paste0(
      "\n### 0. Read File\n",
      "```{R, eval = FALSE}\n",
      txt,
      "\n```\n"
    )
  }

  #------------------Add selected chunks
  if("All chunks" %in% input$selected_chunk_report) {
      ix <- 1:length(logs$code_history)
  } else if("All chunks without errors" %in% input$selected_chunk_report) {
    ix <- c()
    for (i in 1:length(logs$code_history)) {
      if(!logs$code_history[[i]]$error) {
        ix <- c(ix, i)
      }
    }
  } else {  # selected
    ix <- as.integer(input$selected_chunk_report)
  }

  for (i in ix) {
    Rmd_script <- paste0(Rmd_script, "\n", logs$code_history[[i]]$rmd)
  }
  return(Rmd_script)
  })



  # Markdown chunk for the current request
  Rmd_chunk <- reactive({
    req(openAI_response()$cmd)
    req(openAI_prompt())

    Rmd_script <- ""
    Rmd_script <- paste0(
      Rmd_script,
      # Get the data from the params list for every chunk-----------
      # Do not change this without changing the output$Rmd_source function
      # this chunk is removed for local knitting.
      "```{R, echo = FALSE}\n",
      "df <- params$df\n",
      "```\n"
    )

    # User request----------------------
    Rmd_script  <- paste0(
      Rmd_script,
      "\n### ",
      counter$requests,
      ". ",
      paste(
        #remove pre-inserted commands
        gsub(
          paste0(
            "\n|",
            pre_text,
            "|",
            after_text,
            ".*"
          ),
          "",
          openAI_prompt()
        ),
        collapse = " "
      ),
      paste(
        "\n Sampling temperature:",
        sample_temp()
      ),
      "\n"
    )

    # R Markdown code chunk----------------------
    #if error when running the code, do not run
    if (code_error() == TRUE) {
      Rmd_script <- paste0(
        Rmd_script,
        "```{R, eval = FALSE}"
      )
    } else {
      Rmd_script <- paste0(
        Rmd_script,
        "```{R}"
      )
    }

    cmd <- openAI_response()$cmd
    # remove empty line
    if(nchar(cmd[1]) == 0) {
      cmd <- cmd[-1]
    }

    if(input$continue) {
      cmd <- c(logs$last_code, "\n#-----------------", cmd)
    }

    # Add R code
    Rmd_script <- paste0(
      Rmd_script,
      paste(
        cmd,
        collapse = "\n"
      ),
      "\n```\n"
    )

    # indicate error
    if (code_error()) {
      Rmd_script <- paste0(
        Rmd_script,
        "** Error **  \n"
      )
    }

    return(Rmd_script)
  })

  output$html_report <- renderUI({
    req(openAI_response()$cmd)
    tagList(
      downloadButton(
        outputId = "report",
        label = "Report"
      ),
      tippy::tippy_this(
        "report",
        "Download a HTML report for this session.",
        theme = "light-border"
      )
   )
  })

  output$rmd_chunk_output <- renderText({
    req(Rmd_chunk())
    Rmd_total()
  })

  # Markdown report from DataExplorer package; does not work
#  output$eda_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
#    filename = "DataExplorer_report.html",
#    content = function(file) {
#      DataExplorer::create_report(
#        iris,
#        output_file = file,
#        output_dir = file.path(file),
#        knit_root_dir = file.path(file)
#      )
#    }
#  )

  # Markdown report
  output$Rmd_source <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "RTutor.Rmd",
    content = function(file) {
      Rmd_script <- paste0(
        "---\n",
        "title: \"RTutor report\"\n",
        "author: \"RTutor, Powered by ChatGPT\"\n",
        "date: \"",
        date(), "\"\n",
        "output: html_document\n",
        "---\n",
        # this chunk is not needed when they download the Rmd and knit locally
        gsub(
          "```\\{R, echo = FALSE\\}\ndf <- params\\$df\n```\n",
          "", 
          Rmd_total()
        )
      )
      writeLines(Rmd_script, file)
    }
  )

  # Markdown report
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "RTutor_report.html",
    content = function(file) {
      withProgress(message = "Generating Report ...", {
        incProgress(0.2)

        tempReport <- file.path(tempdir(), "report.Rmd")
        # tempReport
        tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)

        req(openAI_response()$cmd)
        req(openAI_prompt())

        #RMarkdown file's Header
        Rmd_script <- paste0(
          "---\n",
          "title: \"RTutor.ai report\"\n",
          "author: \"RTutor v.",
          release,
          ", Powered by ChatGPT\"\n",
          "date: \"",
          date(), "\"\n",
          "output: html_document\n",
          "params:\n",
          "  df:\n",
          "printcode:\n",
          "  label: \"Display Code\"\n",
          "  value: TRUE\n",
          "  input: checkbox\n",
          "---\n"
        )

        Rmd_script <- paste0(
          Rmd_script,
          "\n\n### "
        )

        # R Markdown code chunk----------------------

        # Add R code
        Rmd_script <- paste(
          Rmd_script,
          Rmd_total()
        )

        write(
          Rmd_script,
          file = tempReport,
          append = FALSE
        )

        # Set up parameters to pass to Rmd document
        params <- list(df = iris) # dummy

        # if uploaded, use that data
        req(input$select_data)
        if (input$select_data != no_data) {
          params <- list(
            df = current_data()
          )
        }

        req(params)
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          input = tempReport, # markdown_location,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      })
    }
  )



#                                  8.
#______________________________________________________________________________
#
#  Server rebooting every 24 hours; this gives a warning
#______________________________________________________________________________

  # returns hour and minutes
  time_var <- reactive({
    input$submit_button
    min <- format(Sys.time(), "%M")
    hr <- format(Sys.time(), "%H")
    return(list(
      min = as.integer(min),
      hr = as.integer(hr)
    ))
  })


  output$timer_ui <- renderUI({
    #rebot at 7:56, 15:56, 23:56 ...
    if (
      time_var()$min >= 56 &&
      time_var()$hr %% 24 == 23 &&  # time_var()$hr %% 8 == 7 &&
      file.exists(on_server)
      ) {
      h4(
        paste(
          "Server rebooting in a few minutes. ",
          " Download your files. Reload this site after being 
          disconnected at the top of the hour."
        ),
        style = "color:red"
      )

    }
  })


#                                  9.
#______________________________________________________________________________
#
#  Q and A
#______________________________________________________________________________
  # load demo data when clicked
  observeEvent(input$demo_question,{
     if(input$demo_question != demo_questions[1]) {
      updateTextInput(
        session,
        "ask_question",
        value = input$demo_question
      )
    } else { # if not mpg data, reset
      updateTextInput(
        session,
        "ask_question",
        value = "",
        placeholder = "Ask me anything statistics. See examples. Enable voice naration in Settings."
      )
    }
  })

output$answer <- renderText({
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

    # If the last character is not a stop, add it.
    # Otherwise, GPT3 will add a sentence.

    # The following 5 lines were generated by ChatGPT!!!!!
    # Check if the last character is not a period
    if (substr(txt, nchar(txt), nchar(txt)) != ".") {
    # If the last character is not a period, add it to the end
      txt <- paste(txt, ".", sep = "")
    }

    prepared_request <- paste(
      "If the next question is not related to statistics or data science 
       say 'Statistics only!' ",
      txt
    )

    #----------------------------Send request
    shinybusy::show_modal_spinner(
      spin = "orbit",
      text = paste(
        sample(jokes, 1)
      ),
      color = "#000000"
    )

    start_time <- Sys.time()

    # Send to openAI
    tryCatch(
      response <- openai::create_completion(
        engine_id = language_model,
        prompt = prepared_request,
        openai_api_key = api_key_session()$api_key,
        max_tokens = 200,
        temperature = sample_temp()
      ),
      error = function(e) {
        # remove spinner, show message for 5s, & reload
        shinybusy::remove_modal_spinner()
        shiny::showModal(api_error_modal)
        Sys.sleep(5)
        session$reload()

        list(
          error_value = -1,
          message = capture.output(print(e$message)),
          error_status = TRUE
        )
      }
    )


    error_api <- FALSE
    # if error returns true, otherwise 
    #  that slot does not exist, returning false.
    # or be NULL
    error_api <- tryCatch(
      !is.null(response$error_status),
      error = function(e) {
        return(TRUE)
      }
    )

    error_message <- NULL
    if (error_api) {
      cmd <- NULL
      response <- NULL
      error_message <- response$message
    } else {
      cmd <- response$choices[1, 1]
    }

    api_time <- difftime(
      Sys.time(),
      start_time,
      units = "secs"
    )[[1]]

    # if more than 10 requests, slow down. Only on server.
    if (counter$requests > 20 && file.exists(on_server)) {
      Sys.sleep(counter$requests / 5 + runif(1, 0, 5))
    }
    if (counter$requests > 50 && file.exists(on_server)) {
      Sys.sleep(counter$requests / 10 + runif(1, 0, 10))
    }
    if (counter$requests > 100 && file.exists(on_server)) {
      Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
    }

    shinybusy::remove_modal_spinner()

    # update usage via global reactive value
    counter$tokens <- counter$tokens + response$usage$completion_tokens
    counter$requests <- counter$requests + 1
    counter$time <- round(api_time, 0)
    counter$tokens_current <- response$usage$completion_tokens

    humor <- c(
      "Seriously? Statistics only!",
      "Come on. Statistics only!",
      "You know better. Statistics only!",
      "Bruh... I am a statistics tutor! ",
      "Are you kidding? Statistics only!",
      "Gee..., Statistics only!!"
    )

    ans <- response$choices[1, 1]
    if (grepl("Statistics only!", ans)) {
      ans <- paste(
        sample(humor, 1),
        "     If you are not
       trying to be funny, ask again with more context. It might
        be helpful to add \"in statistics\" to the question."
      )
    }
    return(ans)
  })

})

#                                      10.
#______________________________________________________________________________
#
#  Exploratory Data Analysis
#______________________________________________________________________________

  output$dfSummary <- renderText({
    req(current_data())
    res <- capture.output(summarytools::dfSummary(current_data()))
    res <- paste(res, collapse = "\n")
    return(res)
  })

  output$table1_inputs <- renderUI({
    req(ggpairs_data())
    df <- ggpairs_data()
    selectInput(
      inputId = "table1_strata",
      label = "Select a category for strata",
      choices = colnames(df)[!sapply(df, is.numeric)],
      multiple = FALSE
    )
  })

  output$table1 <- renderText({
    req(ggpairs_data())
    df <- ggpairs_data()
    req(input$table1_strata)
    options(width = 3000)
    withProgress(message = "Calculating table1 ...", {
      incProgress(0.3)
      ix <- match(input$table1_strata, colnames(df))
      res <- capture.output(
        tableone::CreateTableOne(
          vars = colnames(df)[-ix],
          data = df,
          strata = input$table1_strata
        )
      )
      res <- paste(res, collapse = "\n")
    })
    return(res)
  })

  output$distribution_category <- renderPlot({
    withProgress(message = "Barplots of categorical variables ...", {
      incProgress(0.3)
      DataExplorer::plot_bar(current_data())
    })
  },
  width = 800,
  height = 800
  )

  output$distribution_numeric <- renderPlot({
    withProgress(message = "Creating histograms ...", {
      incProgress(0.3)
      DataExplorer::plot_histogram(current_data())
    })
  })

  output$qq_numeric <- renderPlot({
    withProgress(message = "Generating QQ plots ...", {
      incProgress(0.3)
      DataExplorer::plot_qq(current_data())
    })
  })

  output$corr_map <- renderPlot({
    withProgress(message = "Generating correlation map ...", {
      incProgress(0.3)
      #GGally::ggpairs(current_data())
      df <- current_data()
      df <- df[, sapply(df, is.numeric)]
      M <- cor(df)
      testRes <- corrplot::cor.mtest(df, conf.level = 0.95)
      corrplot::corrplot(
        M,
        p.mat = testRes$p,
        method = 'circle',
        type = 'lower',
        insig = 'blank',
        addCoef.col = 'black',
        number.cex = 0.8,
        order = 'AOE',
        diag = FALSE
      )
    })
  })

  ggpairs_data <- reactive({
    df <- current_data()
    cat_variables <- colnames(df)[!sapply(df, is.numeric)]
    # ggpairs does not tolerate variables with too many levels
    for (v in cat_variables) {
      counts <- sort(table(df[, v]), decreasing = TRUE)
      # more than 12 levels?
      if (length(counts) > max_levels) {
        # if the top 12 levels represent more than 30% of the observations
        if (sum(counts[1:max_levels]) / dim(df)[1] > 0.30) {

          df[, v] <- unlist(
            sapply(
              1:dim(df)[1],
              function(x) {
                if (df[x, v] %in% names(counts)[1:max_levels]) {
                  return(df[x, v])
                } else {
                  return("Other")
                }
              }
            )
          )
        } else {
          # too many levels, remove this column. Likely names
          df <- df[, !(colnames(df) %in% v)]
        }
      }
    }
    return(df)
  })

  output$ggpairs_inputs <- renderUI({
    req(ggpairs_data())
    df <- ggpairs_data()
    selected <- colnames(df)
    if(length(selected) > 3) {
      selected <- sample(selected, 3)
    }
    tagList(
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = "ggpairs_variables",
            label = "Select variables",
            choices = colnames(df),
            multiple = TRUE,
            selected = selected
          )

        ),
        column(
          width = 6,
          selectInput(
            inputId = "ggpairs_variables_color",
            label = "Select a category for coloring",
            choices = colnames(df)[!sapply(df, is.numeric)],
            multiple = FALSE
          )
        )
      )
    )

  })

  output$ggpairs <- renderPlot({
    req(input$ggpairs_variables)
    #req(input$ggpairs_variables_color)
    req(length(input$ggpairs_variables) > 0)
    req(ggpairs_data())

    withProgress(message = "Running ggpairs ...", {
      incProgress(0.3)
      df <- as.data.frame(ggpairs_data())
      if(input$ggpairs_variables_color != "") {
        GGally::ggpairs(
          df[, input$ggpairs_variables],
          mapping = aes(
            color = df[, input$ggpairs_variables_color],
            alpha = 0.5
          )
        )

      } else {  # no color
        GGally::ggpairs(
          df[, input$ggpairs_variables]
        )
      }
    })
  },
  width = 1200,
  height = 1200)



#                                      11.
#______________________________________________________________________________
#
#  Miscellaneous
#______________________________________________________________________________

  output$RTutor_version <- renderUI({
    h4(paste("RTutor Version", release))
  })

 output$package_list <- renderUI({
    all <- .packages(all.available = TRUE)
    all <- sapply(
      all,
      function(x) paste(x, paste0(packageVersion(x), collapse = "."))
    )
    all <- unname(all)
    #all <- c("", all)

    selectInput(
      inputId = "installed_packages",
      label = paste0(
        "Search for installed packages ( ",
        length(all),
        " total)"
      ),
      choices = all,
      selected = NULL
    )
  })


  output$slava_ukraini <- renderUI({
    if (input$submit_button == 0 && input$ask_button == 0) {
      tagList(
        br(),
        h4("Slava Ukraini!")
      )

    }
  })

  output$session_info <- renderUI({
    i <- c("<br><h4>R session info: </h4>")
    i <- c(i, capture.output(sessionInfo()))
    HTML(paste(i, collapse = "<br/>"))
  })


  contribute_data <- reactive({
      save_info <- TRUE #default
      if(!is.null(input$contribute_data)) {
        save_info <- input$contribute_data
      }
      return(save_info)
  })

  # save user data when allowed
  observeEvent(input$submit_button, {
    req(openAI_prompt())
    req(logs$code)
    req(code_error())
    if(contribute_data()) {
      try(
        save_data(
          date = Sys.Date(),
          time = format(Sys.time(), "%H:%M:%S"),
          request = openAI_prompt(),
          code = logs$code,
          error_status = code_error(),
          data_str = paste(capture.output(str(current_data())), collapse = "\n")
        )
      )
    }
  })
}
