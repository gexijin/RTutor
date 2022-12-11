#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


###################################################################
# Server
###################################################################


  pdf(NULL) #otherwise, base R plots sometimes do not show.

  # load demo data when clicked
  observe({
    req(input$demo_prompt)
    req(input$select_data)
    if(input$select_data == "mpg" && input$demo_prompt != demos[1]) {
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
"Clearly state the desired statistical analysis in plain English. See examples above.

To see alternative solutions, try again with the same request.

The generated code only works correctly some of the times."
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

  #____________________________________________________________________________
  # UI for loading data
  #____________________________________________________________________________
  # pop up modal for selections ----
  observeEvent(input$api_button, {
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        h3("Advanced AI is not free!"),
        h4("If you use this regularily, 
        please create your own OpenAI account. 
        Otherwise, the small fee for many users adds up quickly.
        Do not bankrupt a math professor!
        It only take a a few minutes: "),

        tags$ul(
            tags$li(
              "Create a personal account at",
              a(
                "OpenAI.",
                href = "https://openai.com/api/",
                target = "_blank"
              )
            ),
            tags$li("After logging in, click on \"Personal\" from top left."),
            tags$li(
              "Click \"Manage Account\" and then \"Billing\",
              where you can add \"Payment methods\" and set \"Usage 
              limits\". $5 per month is more than enough."
            ),
            tags$li(
              "Click on \"API keys\" to create a new key, 
              which can be copied and pasted it below."
            ),
        ),
        textInput(
          inputId = "api_key",
          label = "Paste your API key from OpenAI, then close this window.",
          value = NULL,
          placeholder = "sk-..... (51 characters)"
        ),
        h5(
          "This key will used just for this session. 
          It will not be saved on our server."
        ),
        uiOutput("valid_key"),
        br(),
        uiOutput("save_api_ui"),
        br(),
        textOutput("session_api_source")
      )
    )
  })

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

  output$data_upload_ui <- renderUI({

    # Hide this input box after the first run.
    req(input$submit_button == 0)

    fileInput(
      inputId = "user_file",
      label = "Upload a file",
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
      label = "Demo data",
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
        choices = demos,
        label = NULL
      )
    } else {
      return(NULL)
    }
  })

  #____________________________________________________________________________
  # API key management
  #____________________________________________________________________________

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


  #____________________________________________________________________________
  # Send API Request, handle API errors
  #____________________________________________________________________________

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
          max_tokens = 500
        ),
        error = function(e) {
          # remove spinner, show message for 5s, & reload
          shinybusy::remove_modal_spinner()
          shiny::showModal(api_error_modal)
          Sys.sleep(5)
          session$reload()

          list(
            value = -1,
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

      #issue: check status

      shinybusy::remove_modal_spinner()
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
      if( cost_session %% 10  == 0 & cost_session != 0) {
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
            h4("Slow down. Please do not bankrupt Dr. Ge. 
            Use your own API key. 
            Or PayPal him some funds (gexijin@gmail.com)!")
          )
        )
      }

    })


  output$openAI <- renderText({
    req(openAI_response()$cmd)
    res <- openAI_response()$response$choices[1, 1]
    # Replace multiple newlines with just one.
    res <- gsub("\n+", "\n", res)
    # Replace emplty lines,  [ ]{0, }--> zero or more space
    res <- gsub("^[ ]{0, }\n", "", res)
    res <- gsub("```", "", res)

  })

  output$usage <- renderText({
    req(openAI_response()$cmd)

    paste0(
      "R",
      counter$requests, ":  ",
      openAI_response()$response$usage$completion_tokens,
      " tokens, ",
      openAI_response()$time,
      " second(s)"
    )
  })

  output$total_cost <- renderText({
    if(input$submit_button == 0) {
      return("OpenAI charges 0.2¢ per 1k tokens/words 
      from Dr. Ge's account. Heavy users 
      please use your own account (below)."
      )
    } else {
    req(openAI_response()$cmd)
      paste0(
        "Cumulative API Cost: ",
        sprintf("%6.2f", counter$tokens * 2e-3),
        "¢"
      )
    }
  })

 # Defining & initializing the reactiveValues object
  counter <- reactiveValues(tokens = 0, requests = 0)
  observeEvent(input$submit_button, {
    counter$tokens <- counter$tokens + 
      openAI_response()$response$usage$completion_tokens
    counter$requests <- counter$requests + 1
  })


  #____________________________________________________________________________
  # Shows plots, code, and errors
  #____________________________________________________________________________

  # stores the results after running the generated code.
  # return error indicator and message

  # Note that the code is run three times!!!!!
  run_result <- reactive({
    req(openAI_response()$cmd)

    tryCatch(
      eval(parse(text = openAI_response()$cmd)),
      error = function(e) {
        list(
          value = -1,
          message = capture.output(print(e$message)),
          error_status = TRUE
        )
      }
    )

  })

  # just capture the screen output
  output$console_output <- renderText({
    req(openAI_response()$cmd)
    out <- capture.output(
        eval(parse(text = openAI_response()$cmd))
    )
    paste(out, collapse = "\n")
  })

  output$result_plot <- renderPlot({
    req(openAI_response()$cmd)

    tryCatch(
      eval(parse(text = openAI_response()$cmd)),
      error = function(e) {
        list(
          value = -1,
          message = capture.output(print(e$message)),
          error_status = TRUE
        )
      }
    )

  })

  output$plot_ui <- renderUI({
    req(input$submit_button)
    req(openAI_response()$cmd)
    if(code_error() || input$submit_button == 0) {
      return()
    } else {
      plotOutput("result_plot")
    }

  })

  # Error when run the generated code?
  code_error <- reactive({
    req(!is.null(run_result()))
    req(input$submit_button)
    req(openAI_response()$cmd)

    error_status <- FALSE

    # if error returns true, otherwise 
    #  that slot does not exist, returning false.
    # or be NULL
    error_status <- tryCatch(
      !is.null(run_result()$error_status),
      error = function(e) {
        return(TRUE)
      }
    )
    return(error_status)
  })


  output$error_message <- renderUI({
    req(run_result())
    if(code_error()) {

      h4(paste("Error!", run_result()$message), style = "color:red")
    } else {
      return(NULL)
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
        "\n\nDeveloped by [Steven Ge](https://twitter.com/StevenXGe) based on the 
        [openai](https://cran.rstudio.com/web/packages/openai/index.html) R package.",
        "\n\nWebsite: [http://RTutor.ai](http://RTutor.ai)",
        "\nSource code: [GitHub.](https://github.com/gexijin/RTutor)"
      )
    }

    # if the first chunk & data is uploaded,
    # insert script for reading data
    if(input$submit_button == 1 && input$select_data == uploaded_data) {

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
        gsub("\n", " ", openAI_prompt()),
        collapse = "\n"
      ),
      "\n\n"
    )

    # R Markdown code chuck----------------------
    #if error when running the code, do not run
    if (code_error()) {
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
          "\n\nDeveloped by [Steven Ge](https://twitter.com/StevenXGe) based on the 
          [openai](https://cran.rstudio.com/web/packages/openai/index.html) R package.",
          "\n\nWebsite: [http://RTutor.ai](http://RTutor.ai)",
          "\nSource code: [GitHub.](https://github.com/gexijin/RTutor)"
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


# Run the application
# shiny::runApp("app.R")

}
