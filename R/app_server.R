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
  observe({
    req(input$demo_prompt)
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
"Upload a file or use demo data. Then just ask questions or request analyses in plain English. See examples above. For different solutions, try again with the same request. Code works correctly some of the times. To use voice input, click Settings."
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

  observe({
    if (input$submit_button > 0) {
      updateTextInput(session, "submit_button", label = "Re-submit")
    }
  })

  # Switch to Main tab when Submit button is clicked
  observeEvent(input$submit_button, {
    updateTabsetPanel(
      session,
      "tabs",
      selected = "Main"
    )
  })

    welcome_modal <- shiny::modalDialog(
    title = "Terms & Conditions",
    tags$p(
      "No guarantee for the correctness of the generated code."
    ),
    # hides the loading message
    shinyjs::hideElement(id = "load_message"),
    tags$p(" 
      The RTutor.ai website and the 
      source code (CC BY-NC 3.0 license) are freely 
      availble for academic and 
      non-profit organizations only. 
      Commercial use beyond testing please contact ",
    a(
      "gexijin@gmail.com.",
      href = "mailto:gexijin@gmail.com?Subject=RTutor"
      )
    ),
    footer = modalButton("Accept"),
    easyClose = FALSE,
    size = "s"
  )

  shiny::showModal(welcome_modal)


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

    if (input$tabs == "Main")    {
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
    } else if (input$tabs == "AMA") {

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
        txt <- paste0("Dataset: ", input$select_data, ". To switch, click Reset.")
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
        paste("Request too short! Should be more than ", min_query_length, " characters."),
        duration = 10
      )
    }
    # if too short, do not send. 
    if (nchar(input$input_text) > max_query_length) {
        showNotification(
          paste("Request too long! Should be less than ", max_query_length, " characters."),
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
    prep_input(input$input_text, input$select_data)
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
        cmd <- clean_cmd(response$choices[1, 1], input$select_data)
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
  observe({
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
    res <- r_code$raw
    # Replace multiple newlines with just one.
    res <- gsub("\n+", "\n", res)
    # Replace emplty lines,  [ ]{0, }--> zero or more space
    res <- gsub("^[ ]{0, }\n", "", res)
    res <- gsub("```", "", res)

  })

 # Defining & initializing the reactiveValues object
  r_code <- reactiveValues(
    code = "", # cumulative code
    raw = "",  # cumulative orginal code for print out
    cmd = "" # last code for Rmarkdown
  )

  observeEvent(input$submit_button, {
    # if not continue
    if(!input$continue) {
      r_code$code <- openAI_response()$cmd
      r_code$raw <- openAI_response()$response$choices[1, 1]
      r_code$cmd <- ""

    } else { # if continue
      r_code$cmd <- r_code$code  # last code
      r_code$code <- c(r_code$code, "\n", openAI_response()$cmd)
      r_code$raw <- paste(r_code$raw, openAI_response()$response$choices[1, 1])
    }

    updateCheckboxInput(
      session = session,
      inputId = "continue",
      label = "Continue",
      value = FALSE
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
    req(openAI_response()$cmd)
    withProgress(message = "Running the code ...", {
      incProgress(0.4)
      tryCatch(
        eval(parse(text = r_code$code)),
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
    req(!is.null(names(run_result())[1]))
    error_status <- FALSE

    # if error returns true, otherwise 
    #  that slot does not exist, returning false.
    # or be NULL
    try(  # if you do not 'try', the entire app quits! :-)
      if (is.list(run_result())) {
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
    req(openAI_response()$cmd)
    withProgress(message = "Running the code for console...", {
      incProgress(0.4)    
      out <- capture.output(
          eval(parse(text = r_code$code))
      )
      paste(out, collapse = "\n")
    })
  })

  output$result_plot <- renderPlot({
    req(openAI_response()$cmd)
      withProgress(message = "Plotting ...", {
        incProgress(0.4)
        tryCatch(
          eval(parse(text = r_code$code)),
          error = function(e) {
            list(
              value = -1,
              message = capture.output(print(e$message)),
              error_status = TRUE
            )
          }
        )
      })
  })

  output$result_plotly <- plotly::renderPlotly({
    req(openAI_response()$cmd)
    req(
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive)
    )
    withProgress(message = "Plotting ...", {
      incProgress(0.4)
      tryCatch(
        g <- eval(parse(text = r_code$code)),
        error = function(e) {
          list(
            value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        }
      )

      # still errors some times, when the returned list is not a plot
      if(is.character(g) || is.data.frame(g) || is.numeric(g)) {
        return(NULL)
      } else {
        return(g)
      }
    })

  })

  output$plot_ui <- renderUI({
    req(input$submit_button)
    req(openAI_response()$cmd)
    if(code_error() || input$submit_button == 0) {
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

  # remaining issue. Hide when app starts up.
  observe({
    # hide it by default
    shinyjs::hideElement(id = "make_ggplot_interactive")
    req(openAI_prompt())

    # if  ggplot2, and it is not already an interactive plot, show
    if(grepl("ggplot", paste(openAI_response()$cmd, collapse = " ")) &&
      !is_interactive_plot()
    ) {
    shinyjs::showElement(id = "make_ggplot_interactive")
    }
  })

  is_interactive_plot <- reactive({
    # only true if the plot is interactive, natively.
    req(input$submit_button)
    if(
      grepl(
        "plotly|plot_ly|ggplotly",
        paste(openAI_response()$cmd, collapse = " ")
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
    req(openAI_prompt())
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




  output$data_table <- renderTable({
    req(input$select_data)
    # otherwise built-in data is unavailable when running from R package.
    library(tidyverse)
    if(input$select_data == uploaded_data) {
      eval(parse(text = paste0("user_data()$df[1:20, ]")))
    } else {
      eval(parse(text = paste0(input$select_data, "[1:20, ]")))
    }
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )

  output$data_table_DT <-DT::renderDataTable({
    req(input$select_data)
    # otherwise built-in data is unavailable when running from R package.
    library(tidyverse)

    if(input$select_data == uploaded_data) {
      eval(parse(text = paste0("df <- user_data()$df")))
    } else if(input$select_data == no_data){
      df = NULL #as.data.frame("No data selected or uploaded.")
    } else {
      eval(parse(text = paste0("df <- ", input$select_data)))
    }
    DT::datatable(
      df, 
      options = list(
        lengthMenu = c(5, 20, 50, 100),
        pageLength = 20,
        dom = 'ftp',
        scrollX = "400px"
      ),
      rownames = FALSE
    )
  }
  )

  #                                 7.
  #____________________________________________________________________________
  # Logs and Reports
  #____________________________________________________________________________

  output$session_info <- renderUI({
    i <- c("<br><h4>R session info: </h4>")
    i <- c(i, capture.output(sessionInfo()))
    HTML(paste(i, collapse = "<br/>"))
  })

 # Defining & initializing the reactiveValues object
  Rmd_total <- reactiveValues(code = "")

  observeEvent(input$submit_button, {
    Rmd_total$code <- paste0(Rmd_total$code, Rmd_chuck())
  })

  # Markdown chuck for the current request
  Rmd_chuck <- reactive({

    req(openAI_response()$cmd)
    req(openAI_prompt())

    Rmd_script <- ""

    # if first chuck
    if(input$submit_button == 1) {
      Rmd_script <- paste0(
        Rmd_script,
        # Get the data from the params list-----------
        "\n\nDeveloped by [Steven Ge](https://twitter.com/StevenXGe) using 
        API access (via the 
        [openai](https://cran.rstudio.com/web/packages/openai/index.html)
        package ) to 
        [OpenAI's](https://cran.rstudio.com/web/packages/openai/index.html) \"",
        language_model,
        "\" model.",
        "\n\nRTutor Website: [http://RTutor.ai](http://RTutor.ai)",
        "\nSource code: [GitHub.](https://github.com/gexijin/RTutor)"
      )
    }

    # if the first chunk & data is uploaded,
    # insert script for reading data
    if (input$submit_button == 1 && input$select_data == uploaded_data) {

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

    # User request----------------------
    Rmd_script  <- paste0(
      Rmd_script,
      "\n\n### ",
      counter$requests,
      ". ",
      paste(
        #remove pre-inserted commands
        gsub(
          paste0(
            "\n|",
            pre_text,
            "|",
            "Use the ", 
            input$select_data, 
            " data frame. "
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

    # R Markdown code chuck----------------------
    #if error when running the code, do not run
    if (code_error() == TRUE) {
      Rmd_script <- paste0(
        Rmd_script,
        "```{R, eval = FALSE}\n"
      )
    } else {
      Rmd_script <- paste0(
        Rmd_script,
        "```{R}\n"
      )
    }

    # if uploaded, remove the line: df <- user_data()$df
    cmd <- openAI_response()$cmd
    if(input$select_data == uploaded_data) {
      cmd <- cmd[-1]
    }

    if(input$continue) {
      cmd <- c(r_code$cmd, "\n", cmd)
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

output$rmd_chuck_output <- renderText({
  req(Rmd_chuck())
  Rmd_total$code
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
        Rmd_total$code
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
          # Get the data from the params list-----------
          "\n\n```{R, echo = FALSE}\n",
          "df <- params$df\n",
          "\n```\n",
          "\n\n### "
        )

        # R Markdown code chuck----------------------

        # Add R code
        Rmd_script <- paste(
          Rmd_script,
          Rmd_total$code
        )

        write(
          Rmd_script,
          file = tempReport,
          append = FALSE
        )

        # Set up parameters to pass to Rmd document
        params <- list(
          df = iris #dummy
        )

        # if uploaded, use that data
        req(input$select_data)
        if(input$select_data == uploaded_data) {
          params <- list(
            df = user_data()$df
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
#  Server rebooting every 2 hours; this gives a warning
#______________________________________________________________________________

  # Initialize the timer, 180 seconds
  timer <- reactiveVal(180)

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

  # observer that invalidates every second.
  observe({
    invalidateLater(1000, session)
    isolate({
      timer(timer() - 1)
    })
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
          lubridate::seconds_to_period(timer()),
          ": server reboots at midnight UTC. ",
          " Download your files. Refresh at the top of the hr."
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
  observe({
    req(input$demo_question)

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
        placeholder = "Ask RTutor anything statistics. See examples. Enable voice naration in Settings."
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
      cmd <- clean_cmd(response$choices[1, 1], input$select_data)
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
#  Miscellaneous
#______________________________________________________________________________

  output$RTutor_version <- renderUI({
    h4(paste("RTutor Version", release))
  })

  output$list_of_packages <- renderUI({
    all <- .packages(all.available = TRUE)
    all <- sapply(all, function(x) paste(x, paste0(packageVersion(x), collapse = ".")))
    all <- unname(all)
    all <- c("", all)
    selectInput(
      inputId = "installed_packages",
      label = paste0("Search for installed packages ( ", length(all), " total)"),
      choices = all,
      selectize = TRUE,
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

}
