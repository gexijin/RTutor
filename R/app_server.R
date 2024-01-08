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

  # limit max file size to 10MB, if it is running on server
  if(file.exists(on_server)){ #server
    options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB
  } else { # local
    options(shiny.maxRequestSize = 10000 * 1024^2) # 10 GB
  }

  pdf(NULL) #otherwise, base R plots sometimes do not show.

  # load demo data when clicked
  observeEvent(input$demo_prompt, {
    req(input$select_data)
    if (input$demo_prompt != demo$requests[1]) {
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
"Upload a file or use demo data. Then just ask questions or request analyses in English or other languages. For general questions, briefly explain the data first. See examples above."
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
    shinyjs::hideElement(id = "load_message")
  })

  # after file is uploaded, hide some UI elements. 
  # https://stackoverflow.com/questions/19686581/make-conditionalpanel-depend-on-files-uploaded-with-fileinput
  # On the UI, changed to output.file_uploaded == 0
  output$file_uploaded <- reactive({
    return(!is.null(input$user_file))
  })
  outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)

  observeEvent(input$reset_button, {
    # reset session
    session$reload()
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
          command = paste(wake_word, "*msg")  # hey cox is more sensitive than 'hi tutor'
        ), # set the input
      )
  })

   # read the speech input
  observeEvent(input$hey_cmd, {
    speech <- input$hey_cmd
    # message(speech)
    #showNotification(speech)

    if (input$tabs == "Home")    {
      if (grepl("^continue", speech)) {
        speech <- paste0(
          input$input_text, # current prompt
          ". ",  # add . and space.
          gsub("^continue", "", speech) # remove the continue
        )
      }
      # submit the request when user said  action verb
      if (tolower(speech) %in% action_verbs) {
        shinyjs::click("submit_button")
      } else {
        updateTextInput(
          session,
          "input_text",
          value = speech
        )
      }

    } else if (input$tabs == "Ask") {
      # submit the request when user said  action verb
      if (tolower(speech) %in% action_verbs) {
        shinyjs::click("ask_button")
      } else {
        updateTextInput(
          session,
          "ask_question",
          value = speech
        )
      }
    }

  })

  # copy error message
  observeEvent(code_error(), {
    # not Davinci
    req(selected_model() != "text-davinci-003")
    req(code_error())
    output$send_error_message <- renderUI({
      tagList(
        actionButton(
          inputId = "send_error",
          label = strong("Copy error message")
        ),
        tags$head(tags$style(
          "#send_error{font-size: 16px;color: purple}"
        ))
      )
    })
  })

  observeEvent(input$send_error, {

    updateTextInput(
      session,
      "input_text",
      value = paste0("Fix this error from running the last chunk. Error: ", run_result()$error_message)
    )
  })

# Show notification when error
  observeEvent(code_error(), {
  # show notification message
    if(code_error()) {
      showNotification(
        "Resubmit the same request to see if ChatGPT can resolve the error.
        If that fails, change the request.",
        duration = 10
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
      df <- data.frame()
      file_type <- "read_excel"
      # Excel file ---------------
      if (grepl("xls$|xlsx$", in_file, ignore.case = TRUE)) {
        try(
          df <- readxl::read_excel(in_file)
        )
        df <- as.data.frame(df)
      } else {
        #CSV --------------------
        try(
          df <- read.csv(in_file)
        )
        file_type <- "read.csv"
        # Tab-delimented file ----------
        if (ncol(df) <= 1) { # unable to parse with comma
          try(
            df <- read.table(
              in_file,
              sep = "\t",
              header = TRUE
            )
          )
          file_type <- "read.table"
        }
      }

      if (ncol(df) == 0) { # no data read in. Empty
        return(NULL)
      } else {
        # clean column names
        df <- df %>% janitor::clean_names()
        return(
          list(
            df = df,
            file_type = file_type
          )
        )
      }
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
    req(is.null(input$user_file))
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

    # subset based on dataset
    demo_related <- subset(
      demo,
      data == input$select_data
    )

    #subset based on R or Python
    if(input$use_python) {
      demo_related <- subset(
        demo_related,
        Python == 1
      )
    } else {
      demo_related <- subset(
        demo_related,
        R == 1
      )
    }

    choices <- demo_related$requests
    names(choices) <- demo_related$name

    if (input$select_data %in% c("mpg", no_data, "diamonds", rna_seq)) {
      return(
        selectInput(
          inputId = "demo_prompt",
          choices = choices,
          label = NULL
        )
      )
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
                # Custom CSS to make the chat area scrollable
        tags$head(
            tags$style(HTML("
                #settings_window {
                    height: 400px;  /* Adjust the height as needed */
                    overflow-y: auto;  /* Enables vertical scrolling */
                    padding: 10px;
                    border-radius: 5px;
                }
            "))
        ),
        div( id = "settings_window", 

          tagList(
            fluidRow(
              column(
                width = 2,
                "Model:",
                align = "center"
              ),
              column(
                width = 10,
                align = "left",
                selectInput(
                  inputId = "language_model",
                  choices = language_models,
                  label = NULL,
                  selected = selected_model()
                )
              ),
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
                min = 3,
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
          )
        ), #div
        easyClose = TRUE
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

  selected_model <- reactive({
      model <- language_models[default_model] #gpt-4
      if (!is.null(input$language_model)) {
         model <- input$language_model
      }
      # get the name of the model for display
      names(model) <- names(language_models)[language_models == model]
      return(model)
  })

  openAI_prompt <- reactive({
    req(input$submit_button)
    req(input$select_data)
    req(input$input_text)
    isolate({ # so that it does not do it twice with each submit
      prep_input(input$input_text, input$select_data, current_data(), input$use_python, logs$id, selected_model(), df2 = current_data_2())      
    })

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
        text = sample(jokes, 1),
        color = "#000000"
      )

      start_time <- Sys.time()
      #cat("\n", prepared_request, "\n")
      # Send to openAI
      tryCatch(
        if(selected_model() == "text-davinci-003") { # completion model: davinci-text-003
          response <- openai::create_completion(
            engine_id = selected_model(),
            prompt = prepared_request,
            openai_api_key = api_key_session()$api_key,
            max_tokens = 1000,
            temperature = sample_temp()
          )
        } else {

          prompt_total <- list()

          # System role: You are an experience programmar, etc
          if (!is.null(system_role)) {
            if (nchar(system_role) > 10) {
              prompt_total <- append(
                prompt_total,
                list(list(
                  role = "system",
                  content = system_role
                ))
              )
            }
          }

          # add history, first, if any
          if (length(logs$code_history) > 0) {
        
            # manage context length. If it is too long, remove the oldest ones, except the first one
            history_tokens <- sapply(
              1:length(logs$code_history), 
              function(i) {
                if(i == 1) {
                  logs$code_history[[i]]$prompt_tokens + logs$code_history[[i]]$output_tokens
                } else {
                  # since the chat history includes previous prompt and output
                  logs$code_history[[i]]$prompt_tokens + logs$code_history[[i]]$output_tokens  - logs$code_history[[i - 1]]$prompt_tokens - logs$code_history[[i - 1]]$output_tokens
                }
            })
 
            #cumulative from backwards
            cum_sum <- rev(cumsum(rev(history_tokens))) 
                                                                  # new request               # first one
            cutoff <-  max_content_length - tokens(prepared_request) - history_tokens[1]

            cum_sum[1] <- 0 # do not remove the first one
            included <- which(cum_sum < cutoff)  # 1, 4, 5, 6, 7

            # add each chunk, only keep chunk
            history <- list()
            for(i in included) {
              history <- append(
                history,
                list(list(role = "user", content = logs$code_history[[i]]$prompt_all))
              )

              #append error message. Only the last one
              # prevent error status are not logged correctly
              code_plus_error <- logs$code_history[[i]]$raw
              if(i == length(logs$code_history) && code_error()) {
                code_plus_error <- paste0(
                  code_plus_error,
                  "\n\nError: ",
                  run_result()$error_message
                )
              }

              history <- append(
                history,
                list(list(role = "assistant", content = code_plus_error))
              )
            }
            prompt_total <- append(prompt_total, history)
          }

          # add new user prompt
          prompt_total <- append(
            prompt_total,
            list(list(role = "user", content = prepared_request))
          )

          response <- openai::create_chat_completion(  # chat model: gpt-3.5-turbo, gpt-4
            model = selected_model(),
            openai_api_key = api_key_session()$api_key,
            #max_tokens = 500,
            temperature = sample_temp(),
              messages = prompt_total
          )

          # to make the returned code at the same spot, as davinci model.
          response$choices[1, 1] <- response$choices$message.content
        },
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

      if(0) {
        # if more than 10 requests, slow down. Only on server.
        if(counter$requests > 20 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 5 + runif(1, 0, 5))
        }
        if(counter$requests > 50 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 10 + runif(1, 0, 10))
        }
      }

      if(counter$requests > 100 && file.exists(on_server)) {
        Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
      }

      shinybusy::remove_modal_spinner()

    # update usage via global reactive value/ ouput token is twice as expensive
    counter$tokens_current <- response$usage$completion_tokens + response$usage$prompt_tokens    
    counter$requests <- counter$requests + 1
    counter$time <- round(api_time, 0)
    counter$costs_total <- counter$costs_total + 
      api_cost(response$usage$prompt_tokens, response$usage$completion_tokens, selected_model())

      return(
        list(
          cmd = polish_cmd(cmd),
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
      tags$h4("Maybe the openAI.com website is taking forever to respond.", style = "color:red"),    
    tags$h5("If you keep having trouble, send us an email.", style = "color:red"),
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

    cost_session <-  counter$costs * 10
    if (cost_session %% 5  == 0 & cost_session != 0) {
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
    language = "", # Python or R
    code_history = list(), # keep all code chunks

  )

  observeEvent(input$submit_button, {
    logs$id <- logs$id + 1

    logs$code <-  openAI_response()$cmd

    logs$raw <- openAI_response()$cmd #openAI_response()$response$choices[1, 1]
    # remove one or more blank lines in the beginning.
    logs$raw <- gsub("^\n+", "", logs$raw)
    logs$last_code <- ""
    logs$language <- ifelse(input$use_python, "Python", "R")

    # A list holds current request
    current_code <- list(
      id = logs$id,
      code = logs$code,
      raw = logs$raw, # for print
      prompt = input$input_text,
      prompt_all = openAI_prompt(), # entire prompt, as sent to openAI
      error = code_error(),
      error_message = run_result()$error_message,
      rmd = Rmd_chunk(),
      language = ifelse(input$use_python, "Python", "R"),
      # saves the rendered file in the logs object.
      html_file = ifelse(input$use_python, python_to_html(), -1),
      prompt_tokens = openAI_response()$response$usage$prompt_tokens,
      output_tokens = openAI_response()$response$usage$completion_tokens,
      # save a copy of the data in the environment as a list.
      # if save environment, only reference is saved. 
      # This needs more memory, but works.
      env = run_env_start() # it is a list; 
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
  })

  # change code when past code is selected.
  observeEvent(input$selected_chunk, {
    #req(run_result())
    req(input$selected_chunk)
    id <- as.integer(input$selected_chunk)
    logs$code <- logs$code_history[[id]]$code
    logs$raw <- logs$code_history[[id]]$raw

    #Switch to previous chunks
    if(id < length(logs$code_history)) {
      # convert list to environment; 
      # update the run_env reactive value.
      # restore the environment to the before  running the ith chunk
      run_env(list2env(logs$code_history[[id]]$env))

      # enable re-calculation of the code
      reverted(reverted() + 1)

      showNotification(
        ui = paste("Switched back to chunk #", id,
        ". Any change in the data is also reverted." ),  
        id = "revert_chunk",
        duration = 5,
        type = "warning"
      )
    }

    updateTextInput(
      session,
      "input_text",
      value = logs$code_history[[id]]$prompt
    )

    # change language 
    if(input$submit_button != 0) {
      updateCheckboxInput(
        session = session,
        inputId = "use_python",
        value = (logs$code_history[[id]]$language == "Python")
      )
    }

  })

  output$usage <- renderText({
    req(input$submit_button != 0)

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
    if(input$submit_button == 0) {
      return("OpenAI charges us $1 for about 60 requests via GPT-4 Turbo. Heavy users please
      use your own API key (Settings), or help cover the fee via PayPal(gexijin@gmail.com)."
      )
    } else {
    #req(openAI_response()$cmd)
      paste0(
        "Total API Cost: $",
        sprintf("%5.3f", counter$costs_total) 
      )
    }
  })

  output$temperature <- renderText({
    req(openAI_response()$cmd)

    paste0(
        names(selected_model()),
        ", Temperature=",
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
    costs_total = 0, # cummulative cost
    requests = 0, # cummulative requests    
    tokens_current = 0,  # tokens for current query
    time = 0 # response time for current
  )


  #                            6.
  #____________________________________________________________________________
  # Run the code, shows plots, code, and errors
  #____________________________________________________________________________

  # define a reactive variable that holds an R environment
  # This is needed for the Rmd chunk.
  run_env <- reactiveVal(new.env())

  # a list stores all data objects before running the code.
  run_env_start <- reactiveVal(list()) 

  # stores the results after running the generated code.
  # return error indicator and message
  # Sometimes returns NULL, even when code run fine. Especially when
  # a base R plot is generated.

  # define a reactive variable. Reactive function not returning error
  run_result <- reactiveVal(list())

  # change value when a previous code chunk is selected.
  reverted <- reactiveVal(0)

  observeEvent(
    eventExpr = {
      input$submit_button  # when submit is clicked 
      reverted()           # or when a previous code chunk is selected
      logs$code
    }, {
    req(logs$code != "")
    req(!input$use_python)
    result <- NULL
    console_output <- NULL
    error_message <- NULL

    withProgress(message = "Running the code ...", {
      incProgress(0.4)

      run_env_start(as.list(run_env())) # keep a copy of the crime scene

      result <- tryCatch({
        eval_result <- eval(
          #parse(text = "log('error')"),
          parse(text = clean_cmd(logs$code, input$select_data, file.exists(on_server))), 
          envir = run_env()
        )
        console_output <- capture.output(print(eval_result))
        eval_result  # without this the interactive plots does not work
      }, error = function(e) {
        list(error_message = e$message) # won't work if not inside a list!!!!
      })

      # update the error message, if any
      if(length(names(result)) != 0) {
        if(names(result)[1] == "error_message") {
          error_message <- result$error_message
        }
      }

      # Run with error
      if(!is.null(error_message)) {
        run_env(list2env(run_env_start())) # revert the environment
      }

      run_result(list(
        result = result,
        console_output = console_output,
        error_message = error_message
      ))
    })
  })


  # Error when run the generated code?
  code_error <- reactive({
    error_status <- FALSE
    req(input$submit_button != 0)
    if(!input$use_python) { # R
      return(!is.null(run_result()$error_message) && run_result()$error_message != "")
    } else { # Python
      return(python_to_html() == -1)
    }
  })

  output$error_message <- renderUI({
    req(code_error())
    req(logs$code)  
    if(code_error()) {
      h4(paste("Error!", run_result()$error_message), style = "color:red")
    } else {
      return(NULL)
    }
  })

  output$console_output <- renderText({
    req(!code_error())
    paste(run_result()$console_output, collapse = "\n")
  })

  output$result_plot <- renderPlot({
    req(!code_error())
    req(logs$code)
    # Check if the result is not a ggplot or a known plot type
    if (inherits(run_result()$result, "ggplot") || is.null(run_result()$console_output)) {
      return(run_result()$result)
    } else {
      # If the result is not a ggplot (e.g., corrplot), re-evaluate the command_string, 
      #under the parent environment of the run_env()
      tmp_env <- list2env(run_env_start())
      tryCatch({
        eval_result <- eval(
          parse(text = clean_cmd(logs$code, input$select_data, file.exists(on_server))), 
          envir = tmp_env
        )
      })
    }
  })

  output$result_plotly <- plotly::renderPlotly({
    req(!code_error())
    req(!input$use_python)
    req(
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive)
    )

    g <- run_result()$result
    # still errors some times, when the returned list is not a plot
    if(is.character(g) || is.data.frame(g) || is.numeric(g)) {
      return(NULL)
    } else {
      return(g)
    }

  })

  output$result_CanvasXpress <- canvasXpress::renderCanvasXpress({
    req(!code_error())
    req(!input$use_python)

    g <- run_result()$result
    if (
      turned_on(input$make_cx_interactive) &&
      !is.character(g) &&
      !is.data.frame(g) &&
      !is.numeric(g)
    ) {
      g <- canvasXpress::canvasXpress(g)
    } else {
      g <- canvasXpress::canvasXpress(destroy = TRUE)
    }
    return(g)
  })

  # Remind user to uncheck.
  observe({

    req(input$make_cx_interactive && input$tabs == "Home")
    showNotification(
      ui = paste("Please uncheck the CanvasXpress
      box before proceeding to the next request."),
      id = "uncheck_canvasXpress",
      duration = NULL,
      type = "error"
    )
  })

  # Remove messages if the tab changes --------
  observe({
    req(!input$make_cx_interactive || input$tabs != "Home")
    removeNotification("uncheck_canvasXpress")
  })


  output$plot_ui <- renderUI({
    req(input$submit_button)
    req(!input$use_python)
    req(!code_error())
    req(logs$code)
    if (
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive) # converted
    ){
      plotly::plotlyOutput("result_plotly")
    } else if (
      turned_on(input$make_cx_interactive) # converted
    ) {
      canvasXpress::canvasXpressOutput("result_CanvasXpress")
    } else {
      plotOutput("result_plot")
    }
  })

  observe({
    # hide it by default
    shinyjs::hideElement(id = "make_ggplot_interactive")
    updateCheckboxInput(
      session = session,
      inputId = "make_ggplot_interactive",
      label = "Interactive via plotly",
      value = FALSE
    )

    req(!code_error())
    req(logs$code)
    txt <- paste(openAI_response()$cmd, collapse = " ")

    if (inherits(run_result()$result, "ggplot") && # if  ggplot2, and it is 
      !is_interactive_plot() && #not already an interactive plot, show
       # if there are too many data points, don't do the interactive
      !(dim(current_data())[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
    ) {
      shinyjs::showElement(id = "make_ggplot_interactive")
    }
  })

  observe({
    # hide it by default
    shinyjs::hideElement(id = "make_cx_interactive")
    updateCheckboxInput(
      session = session,
      inputId = "make_cx_interactive",
      label = "Interactive via CanvasXpress",
      value = FALSE
    )

    req(!code_error())
    req(logs$code)
    txt <- paste(openAI_response()$cmd, collapse = " ")

    if (inherits(run_result()$result, "ggplot") && # if  canvasXpress, and it is 
      !is_interactive_plot() && #not already an interactive plot, show
       # if there are too many data points, don't do the interactive
      !(dim(current_data())[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
    ) {
      shinyjs::showElement(id = "make_cx_interactive")
    }
  })


  is_interactive_plot <- reactive({
    # only true if the plot is interactive, natively.
    req(input$submit_button)
    req(logs$code)
    req(!code_error())
    if (inherits(run_result()$result, "plotly")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  output$tips_interactive <- renderUI({
    req(input$submit_button)
    req(openAI_response()$cmd)
    if(is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive)
     ) {
      tagList(
        p("Mouse over to see values. Select a region to zoom. 
        Click on the legends to deselect a group. 
        Double click a category to hide all others. 
        Use the menu on the top right for other functions."
        )
      )
    } else if (turned_on(input$make_cx_interactive)) {
      tagList(
        p("To reset, press ESC. Or mouse over the top, 
        then click the reset button on the top left. 
        Mouse over to see values. Select a region to zoom. 
        Click on the legends to deselect a group. 
        Double click a category to hide all others. 
        Use the menu on the top right for other functions.
        Right click for more options."
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
      max_proptortion <- unique_ratio #default
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
      max_levels_1 <- max_levels_factor_conversion #default
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

  # The current data
  current_data <- reactiveVal(NULL)

  observeEvent(input$select_data, {
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
      current_data(NULL)
    } else if(nrow(df) == 0) {
      current_data(NULL)
    } else { # there are data in the dataframe

      current_data(df)
    }
    # add the data to the current environment
    run_env(rlang::env(run_env(), df = current_data()))
    run_env_start(as.list(run_env()))
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
        if(!input$use_python && logs$language == "R") { # not python
          df <- run_env()$df
        }
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
      data_afterwards(),
      options = list(
        lengthMenu = c(5, 20, 50, 100),
        pageLength = 10,
        dom = 'ftp',
        scrollX = "400px"
      ),
      rownames = FALSE
    )
  })

  output$data_table <- renderTable({
    req(data_afterwards())

    data_afterwards()[
      1:min(20, nrow(data_afterwards())),
      ]
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

  #ploting missing values
  output$missing_values <- plotly::renderPlotly({
    req(!is.null(data_afterwards()))
    p <- missing_values_plot(data_afterwards())
    if(!is.null(p)) { 
      plotly::ggplotly(p)
    } else {
      return(NULL)
    }
  })


  output$data_table_DT_2 <- DT::renderDataTable({
    req(data_afterwards_2())
    DT::datatable(
      data_afterwards_2(),
      options = list(
        lengthMenu = c(5, 20, 50, 100),
        pageLength = 10,
        dom = 'ftp',
        scrollX = "400px"
      ),
      rownames = FALSE
    )
  })

  output$data_size_2 <- renderText({
    req(!is.null(data_afterwards_2()))
    paste(
      dim(data_afterwards_2())[1], "rows X ",
      dim(data_afterwards_2())[2], "columns"
    )
  })

  output$data_structure_2 <- renderPrint({
    req(!is.null(data_afterwards_2()))
    str(data_afterwards_2())
  })

  output$data_summary_2 <- renderText({
    req(!is.null(data_afterwards_2()))
    paste(
      capture.output(
        summary(data_afterwards_2())
      ),
      collapse = "\n"
    )
  })

  #ploting missing values
  output$missing_values_2 <- plotly::renderPlotly({
    req(!is.null(data_afterwards_2()))
    p <- missing_values_plot(data_afterwards_2())
    if(!is.null(p)) { 
      plotly::ggplotly(p)
    } else {
      return(NULL)
    }
  })

  observe({
    if(input$select_data != no_data && !is.null(data_afterwards())) {
    shinyjs::show(id = "first_file")      
    } else {
      shinyjs::hide(id = "first_file")
    }
  })


  # The data, after running the chunk
  data_afterwards_2 <- reactive({
    req(!is.null(input$user_file_2))
    req(input$select_data)
    req(current_data())

    if (input$submit_button == 0) {
      return(current_data_2())
    }

    df <- current_data()
    # This updates the data by running hte entire code one more time.
    if(input$submit_button != 0) {
      if (code_error() == FALSE && !is.null(logs$code)) {
        if(!input$use_python && logs$language == "R") { # not python
          df <- run_env()$df2
        }
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

  output$data_structure_2 <- renderPrint({
    req(!is.null(data_afterwards_2()))
    str(data_afterwards_2())
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
    selected_model(),
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

  Rmd_script <- paste0(
    Rmd_script,
    # Get the data from the params list for every chunk-----------
    # Do not change this without changing the output$Rmd_source function
    # this chunk is removed for local knitting.
    "```{R, echo = FALSE}\n",
    "df <- params$df\ndf2 <- params$df2\n",
    "```\n"
  )
  
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

    if(input$use_python) {
      Rmd_script <- paste0(
        Rmd_script,
        "```{R}\n",
        "library(reticulate)\n",
        "```\n",
        "```{python, message=FALSE}\n",
        "df = r.df\n",
        "```\n"
      )
    }

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
      paste0(
        "\n ",
        names(selected_model()),
        " (Temperature=",
        sample_temp(),
        ")"
      ),
      "\n"
    )

    # R Markdown code chunk----------------------
    if(!input$use_python) {  # R code chunk
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
    } else {  # Python code chunk
      #if error when running the code, do not run
      if (python_to_html() == -1) {
        Rmd_script <- paste0(
          Rmd_script,
          "```{python, eval = FALSE}"
        )
      } else {
        Rmd_script <- paste0(
          Rmd_script,
          "```{python}"
        )
      }
    }
    cmd <- openAI_response()$cmd
    # remove empty line
    if(nchar(cmd[1]) == 0) {
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
      actionButton(
        inputId = "report",
        label = "Session report"
      ),
      tippy::tippy_this(
        "report",
        "Render a HTML report for this session.",
        theme = "light-border"
      )
   )
  })

  output$rmd_chunk_output <- renderText({
    req(Rmd_chunk())
    Rmd_total()
  })

  output$eda_report_ui <- renderUI({
    req(input$select_data != no_data)
    req(!input$use_python)
    req(!is.null(current_data()))
    df <- ggpairs_data()
    tagList(
      br(),
      fluidRow(
        column(
          width = 3,
          actionButton(
            inputId = "render_eda_report_rtutor",
            label = "Render Report"
          )
        )
      ),
      br(),
      selectInput(
        inputId = "eda_target_variable",
        label = "Select a target variable (optional):",
        choices = c("<None>", colnames(df)),
        multiple = FALSE
      ),
      br(),
      checkboxGroupInput(
        inputId = "eda_variables",
        label = "Select up to 20 variables:",
        choices = colnames(df),
        selected = colnames(df)
      )
    )

  })
   # when user uploads a file and has more than 20 columns, only the first 20 is selected by eda_variables.
  observeEvent(input$user_file, {
    req(!is.null(input$user_file))
    req(input$select_data == uploaded_data)
    req(!input$use_python)
    req(!is.null(ggpairs_data()))
    df <- ggpairs_data()
    if(ncol(df) > max_eda_var) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "eda_variables",
        label = "Deselect variables to ignore(optional):",
        choices = colnames(df),
        selected = colnames(df)[1:max_eda_var]
      )
    }
  })

  # if user selects more than 20 columns for the eda_variables, only the first 20 is selected by eda_variables. Show a warning.
  observeEvent(c(input$eda_variables, input$eda_target_variable), {
    req(!input$use_python)
    req(!is.null(ggpairs_data()))

    selected_var <- input$eda_variables
    update_selection <- FALSE
    # if the selected target variable is not included in the eda_variables, add it to the top of the list.
    if (input$eda_target_variable != "<None>" && !(input$eda_target_variable %in% selected_var)) {
      selected_var <- c(input$eda_target_variable, selected_var)
      update_selection <- TRUE
    }

    if(length(selected_var) > max_eda_var) {
      selected_var <- selected_var[1:max_eda_var]

      showNotification(
        ui = paste("Only the first 20 variables are selected for EDA. 
        Please deselect some variables to continue."),
        id = "eda_variables_warning",
        duration = 5,
        type = "error"
      )
      update_selection <- TRUE
    }

    # if target variable is selected, add to it; if too many, only keep the first 20
    if(update_selection){
      updateCheckboxGroupInput(
        session = session,
        inputId = "eda_variables",
        label = "Deselect variables to ignore(optional):",
        choices = colnames(ggpairs_data()),
        selected = selected_var
      )
    }
  })

  eda_file <- reactiveVal(NULL)

  observeEvent(input$render_eda_report_rtutor, {
    req(input$select_data != no_data)
    req(!input$use_python)
    req(!is.null(current_data()))


    withProgress(message = "Generating Report (5 minutes)", {
      incProgress(0.2)
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "RTutor_EDA.Rmd")
      # tempReport
      tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)
      output_file <- gsub("Rmd$", "html", tempReport)
      # This should retrieve the project location on your device:
      # "C:/Users/bdere/Documents/GitHub/idepGolem"
      #wd <- getwd()

      markdown_location <- app_sys("app/www/eda.Rmd")
      file.copy(from = markdown_location, to = tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        df = ggpairs_data()[, input$eda_variables],
        target = input$eda_target_variable
      )
      req(params)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      tryCatch({
        rmarkdown::render(
          input = tempReport, # markdown_location,
          output_file = output_file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }, 
        error = function(e) {
          showNotification(
            ui = paste("Error when generating the report. Please try again."),
            id = "eda_report_error",
            duration = 5,
            type = "error"
          )
      },
        finally = {
          eda_file(output_file)
          # show modal with download button
          showModal(modalDialog(
            title = "Successfully rendered the report!",
            downloadButton(
              outputId = "eda_report_rtutor",
              label = "Download"
            ),
            easyClose = TRUE
          ))
        }
      )
    })
  })


  # Markdown report
  output$eda_report_rtutor <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "RTutor_EDA.html",
    content = function(file) {
      validate(
        need(!is.null(eda_file()), "File not found.")
      )
      file.copy(from = eda_file(), to = file, overwrite = TRUE)
    }
  )


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


  report_file <- reactiveVal(NULL)

  observeEvent(input$report, {
    req(input$select_data != no_data)
    req(!input$use_python)
    req(!is.null(current_data()))


    withProgress(message = "Generating Report (5 minutes)", {
      incProgress(0.2)
      tempReport <- file.path(tempdir(), "report.Rmd")
      # tempReport
      tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)

      req(openAI_response()$cmd)
      req(openAI_prompt())
      output_file <- gsub("Rmd$", "html", tempReport)

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
        "  df2:\n",
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
      df2 <- NULL
      if(!is.null(current_data_2())) {
        df2 <- current_data_2()          
      }
      # if uploaded, use that data
      req(input$select_data)
      if (input$select_data != no_data) {
        params <- list(
          df = current_data(),
          df2 = df2
        )
      }


      req(params)

      tryCatch({
        rmarkdown::render(
          input = tempReport, # markdown_location,
          output_file = output_file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }, 
        error = function(e) {
          showNotification(
            ui = paste("Error when generating the report. Please try again."),
            id = "report_error",
            duration = 5,
            type = "error"
          )
      },
        finally = {
          report_file(output_file)
          # show modal with download button
          showModal(modalDialog(
            title = "Successfully rendered the report!",
            downloadButton(
              outputId = "download_report",
              label = "Download"
            ),
            easyClose = TRUE
          ))
        }
      )
    })
  })


  # Markdown report
  output$download_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "RTutor_report.html",
    content = function(file) {
      validate(
        need(!is.null(report_file()), "File not found.")
      )
      file.copy(from = report_file(), to = file, overwrite = TRUE)
    }
  )


  # Markdown report
  output$report_dsdsdf <- downloadHandler(
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
          "  df2:\n",
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
        df2 <- NULL
        if(!is.null(current_data_2())) {
          df2 <- current_data_2()          
        }
        # if uploaded, use that data
        req(input$select_data)
        if (input$select_data != no_data) {
          params <- list(
            df = current_data(),
            df2 = df2
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

      # If the last character is not a stop, add it.
      # Otherwise, GPT3 will add a sentence.

      # The following 5 lines were generated by ChatGPT!!!!!
      # Check if the last character is not a period
      if (substr(txt, nchar(txt), nchar(txt)) != ".") {
      # If the last character is not a period, add it to the end
        txt <- paste(txt, ".", sep = "")
      }

      prepared_request <- txt 
      


      #----------------------------Send request
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = paste(
          sample(jokes, 1)
        ),
        color = "#000000"
      )
      prompt_total <- list()

      # System role: You are an experience programmar, etc
      if (!is.null(system_role)) {
        if (nchar(system_role) > 10) {
          prompt_total <- append(
            prompt_total,
            list(list(
              role = "system",
              content = system_role_tutor
            ))
          )
        }
      }

      # add history, first, if any
      if (length(logs$code_history) > 0) {
    
        # manage context length. If it is too long, remove the oldest ones, except the first one
        history_tokens <- sapply(
          1:length(logs$code_history), 
          function(i) {
            if(i == 1) {
              logs$code_history[[i]]$prompt_tokens + logs$code_history[[i]]$output_tokens
            } else {
              # since the chat history includes previous prompt and output
              logs$code_history[[i]]$prompt_tokens + logs$code_history[[i]]$output_tokens  - logs$code_history[[i - 1]]$prompt_tokens - logs$code_history[[i - 1]]$output_tokens
            }
        })

        #cumulative from backwards
        cum_sum <- rev(cumsum(rev(history_tokens))) 
                                                              # new request               # first one
        cutoff <-  max_content_length_ask - tokens(prepared_request) - history_tokens[1]

        cum_sum[1] <- 0 # do not remove the first one
        included <- which(cum_sum < cutoff)  # 1, 4, 5, 6, 7

        # add each chunk, only keep chunk
        history <- list()
        for(i in included) {
          history <- append(
            history,
            list(list(role = "user", content = logs$code_history[[i]]$prompt_all))
          )

          #Note error message is not properly stored in the logs variable. 
          # only add error for the current one
          # append error message, if any
          code <- logs$code_history[[i]]$raw
          if(i == length(logs$code_history) && code_error()) {
            code <- paste0(
              code,
              "\n\nError: ",
              run_result()$error_message,
              "\n"
            )
          }

          # append result, only the printed out. Figures in the future with gpt-4V
          if(i == length(logs$code_history) && !is.null(run_result()$console_output)) {
            result <- paste(run_result()$console_output, collapse = "\n")
            if(nchar(result) > 10) {
              code <- paste0(
                code,
                "\n\nResult: ",
                result,
                "\n"
              )              
            }
          }

          history <- append(
            history,
            list(list(role = "assistant", content = code))
          )
        }
        prompt_total <- append(prompt_total, history)
      }

      # add new user prompt
      prompt_total <- append(
        prompt_total,
        list(list(role = "user", content = prepared_request))
      )

      # Send to openAI
      tryCatch(
        response <- openai::create_chat_completion(
          model = selected_model(),
          openai_api_key = api_key_session()$api_key,
          temperature = sample_temp(),
          messages = prompt_total
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
        ans <- response$choices$message.content
      }

      shinybusy::remove_modal_spinner()

      # update usage via global reactive value
      # update usage via global reactive value/ ouput token is twice as expensive
      counter$tokens_current <- response$usage$completion_tokens + response$usage$prompt_tokens    
      counter$requests <- counter$requests + 1
      counter$costs_total <- counter$costs_total + 
        api_cost(response$usage$prompt_tokens, response$usage$completion_tokens, "gpt-3.5-turbo")


      humor <- c(
        "Seriously? Statistics only!",
        "Come on. Statistics only!",
        "You know better. Statistics only!",
        "Bruh... I am a statistics tutor! ",
        "Are you kidding? Statistics only!",
        "Gee..., Statistics only!!"
      )

      if (grepl("No comment", ans)) {
        ans <- paste(
          sample(humor, 1),
          "     Ask again with more context. It might
          be helpful to add \"in statistics\" to the question."
        )
      }
      # Replace double newlines with HTML paragraph tags
      ans <- gsub("\n\n", "</p><p>", ans)
      ans <- paste0("<p><strong>", input$ask_question, "</strong></p>", "<p>", ans, "</p>")

      return(ans)
    })

  })

  chat_content <- reactiveVal(c())

  observeEvent(input$ask_button, {
    new_message <- answer_one()
    if (new_message != "") {
      chat_content(c(new_message, chat_content()))
      #updateTextInput(session, "ask_question", value = "")
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
      $('#ask_question').on('keyup', function (e) {
          if (e.keyCode === 13) {
              setTimeout(function(){
                  $('#ask_button').click();
              }, 500);  // Delay of 100 milliseconds
          }
      });
  ")

  observeEvent(answer_one(), {
    showModal(
      modalDialog(
        title = "Chat with your tutor",
        # Custom CSS to make the chat area scrollable
        tags$head(
            tags$style(HTML("
                #chat_window {
                    height: 400px;  /* Adjust the height as needed */
                    overflow-y: auto;  /* Enables vertical scrolling */
                    padding: 10px;
                    border-radius: 5px;
                }
            "))
        ),
        div( id = "chat_window", htmlOutput("answer")),
        tags$head(
          tags$style(
            "#answer{
              color: purple;
              font-size: 14px
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

    # if more than 5000 rows, sample
    if (nrow(df) > 5000) {
      df <- df[sample(1:nrow(df), 5000), ]
    }
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
      df <- na.omit(df) # remove missing values
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

  # data used for EDA
  ggpairs_data <- reactive({
    df <- current_data()
    # if analyses are run, use the original data
    if(length(logs$code_history) > 0) {
      df <- logs$code_history[[1]]$env$df
    }
    #df <- na.omit(df) # remove missing values
    cat_variables <- colnames(df)[!sapply(df, is.numeric)]
    # ggpairs does not tolerate variables with too many levels
    for (v in cat_variables) {
      counts <- sort(table(df[, v]), decreasing = TRUE)
      # more than 12 levels?
      if (length(counts) > max_eda_levels) {
        # if the top 12 levels represent more than 30% of the observations
        if (sum(counts[1:max_eda_levels]) / dim(df)[1] > 0.30) {

          df[, v] <- unlist(
            sapply(
              1:dim(df)[1],
              function(x) {
                if (df[x, v] %in% names(counts)[1:max_eda_levels]) {
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
          width = 4,
          selectInput(
            inputId = "ggpairs_variables",
            label = "Select variables",
            choices = colnames(df),
            multiple = TRUE,
            selected = selected
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "ggpairs_variables_color",
            label = "Select a category for coloring",
            choices = colnames(df)[!sapply(df, is.numeric)],
            multiple = FALSE
          )
        ),
        #add a submit button to refresh the plot
        column(
          width = 3,
          actionButton(
            inputId = "ggpairs_submit",
            label = strong("Submit"),
            style = "margin-top: 15px;"
          )
        )
      )
    )

  })

  output$ggpairs <- renderPlot({
    req(ggpairs_data())    
    req(input$ggpairs_submit)
    isolate({
      req(input$ggpairs_variables)
      #req(input$ggpairs_variables_color)
      req(length(input$ggpairs_variables) > 0)

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

  output$RTutor_version_main <- renderUI({
    tagList(
      h3(paste("RTutor.ai ", release))
    )
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
    if (input$submit_button == 0) {
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

    if(contribute_data()) {
      # remove user data, only keep column names and data type
      txt <- capture.output(str(current_data(), vec.len = 0))
      txt <- gsub(" levels .*$", " levels", txt)
      try(
        save_data(
          date = Sys.Date(),
          time = format(Sys.time(), "%H:%M:%S"),
          request = openAI_prompt(),
          code = logs$code,
          error_status = code_error(),  # 1 --> error!  0 --> no error, success!!
          data_str = paste(txt, collapse = "\n"),
          dataset = input$select_data,
          session = session$token,
          filename = ifelse(is.null(input$user_file[1, 1]), " ", input$user_file[1, 1]),
          filesize = ifelse(is.null(input$user_file[1, 2]), " ", input$user_file[1, 2]),
          chunk = counter$requests,
          api_time = counter$time,
          tokens = counter$tokens_current,
          language = logs$language
        )
      )
    }
  })

  observeEvent(input$save_feedbck, {
    req(input$save_feedbck)
    feedback_len <- nchar(input$user_feedback)
    if (feedback_len < 5) {
      showNotification("Feedback is too short.")
    } else  if (feedback_len > 2000) {
      showNotification("Feedback is too long.")
    } else {
      showNotification("Thank you for your feedback!")

    try(
      save_comments(
        date = Sys.Date(),
        time = format(Sys.time(), "%H:%M:%S"),
        comments = input$user_feedback,
        helpfulness = input$helpfulness,
        experience = input$experience
      )
    )
    }

    # clear the comments after submitted. 
    # This prevents users submit the same thing twice.
    updateTextInput(
      session,
      "user_feedback",
      value = "",
      placeholder = "Any questions? Suggestions? Things you like, don't like?" 
    )


  })

  observe({
    shinyjs::toggle(id = "user_feedback", condition = input$Comments)
    shinyjs::toggle(id = "save_feedbck", condition = input$Comments)
    shinyjs::toggle(id = "helpfulness", condition = input$Comments)
    shinyjs::toggle(id = "experience", condition = input$Comments)

  })


#
#  Python
  output$python_markdown <- renderUI({
    req(openAI_response()$cmd)
    req(input$use_python)

    id <- as.integer(input$selected_chunk)
    rendered <- logs$code_history[[id]]$html_file
    req(rendered)

    if (rendered == -1) {
      p("Error!")
    } else {
      includeHTML(rendered)
    }
  })

  # file is renderred and stored in the html_file variable in logs$code_history
  python_to_html <- reactive({
    req(input$submit_button)
    req(logs$language == "Python")
    req(input$use_python)

    isolate({
      python_html(
        python_code = logs$code,
        select_data = input$select_data,
        current_data = current_data()
      )      
    })

  })

#                                      12.
#______________________________________________________________________________
#
#  Data Editing
#______________________________________________________________________________

  show_pop_up <- function() {
    showModal(
      modalDialog(
        title = "Verify data types (important!)",
        # Custom CSS to make the chat area scrollable
        tags$head(
            tags$style(HTML("
                #data_type_window {
                    height: 400px;  /* Adjust the height as needed */
                    overflow-y: auto;  /* Enables vertical scrolling */
                    padding: 10px;
                    border-radius: 5px;
                }
            "))
        ),
        div( id = "data_type_window", uiOutput("column_type_ui")),
        h4("If a column represents categories, choose 'Factor', even if 
        it contains numbers. For columns that are numbers, but with few unique values, RTutor 
        automatically convert them to factors. See Settings.", 
        style = "color: blue"),
        br(),
        footer = actionButton("dismiss_modal",label = "Dismiss"),
        size = "l",
        easyClose = TRUE
      )
    )
  }

  modal_closed <- reactiveVal(FALSE)

  observeEvent(input$dismiss_modal,{
    modal_closed(TRUE)
    shiny::removeModal()
  })  

  observeEvent(input$user_file, {
    show_pop_up()
  })

  # The notification is shown when the pop-up is closed
  observeEvent(modal_closed(), {
    req(modal_closed())
    shiny::showNotification(
      "Know thy enemy. Exploratory your data at the EDA tab first.",
      duration = 10
    )
  })

  # Trigger the pop-up when a file is uploaded
  observeEvent(input$data_edit_modal, {
    show_pop_up()
  }) 
   
  output$column_type_ui <- renderUI({
    req(current_data())
    req(input$select_data)
    column_names <- names(current_data())
    examples <- capture.output(str(current_data()))
    examples <- examples[-1]
    examples <- gsub(" \\$ ", "", examples)
    withProgress(message = "Verifying data types ...", {
      incProgress(0.3)
      lapply(seq_along(column_names), function(i) {
        column_name <- column_names[i]
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = paste0("column_type_", i),
              label = NULL,
              choices = c("Character" = "character",
                          "Numeric" = "numeric",
                          "Integer" = "integer",
                          "Date" = "Date",
                          "Factor" = "factor"),
              selected = class(current_data()[[i]])
            )
          ),
          column(
            width = 9,          
            align = "left",
            style = "margin-top: -5px;",
            h5(examples[i])
          )
        )

      })
    })
  })
  
  observe({
    req(current_data())
    for (i in seq_along(current_data())) {
      col_type <- input[[paste0("column_type_", i)]]
      if (!is.null(col_type)) {
        updated_data <- isolate(current_data())

        # when converting to factor the as function gives an error
        if(col_type == "factor") {
          updated_data[[i]] <- as.factor(updated_data[[i]])
        } else if (col_type == "Date") {
          updated_data[[i]] <- lubridate::parse_date_time(
            updated_data[[i]],
            orders = c("mdy", "dmy", "ymd")            
          )
          updated_data[[i]] <- as.Date(updated_data[[i]])
        } else {
          updated_data[[i]] <- as(updated_data[[i]], col_type)
        }
        current_data(updated_data)
      }
    }
  })


  output$data_description <- renderText({
    req(current_data())
    try(describe_data(current_data()))
  })

  observeEvent(input$data_desc_modal, {
    showModal(
      modalDialog(
        title = "Data description",
        # Custom CSS to make the chat area scrollable
        tags$head(
            tags$style(HTML("
                #description_window {
                    height: 400px;  /* Adjust the height as needed */
                    overflow-y: auto;  /* Enables vertical scrolling */
                    padding: 10px;
                    border-radius: 5px;
                }
            "))
        ),
        div( id = "description_window", textOutput("data_description")),
        tags$style(type="text/css", "#data_description {white-space: pre-wrap;}"),
        footer = tagList(
          modalButton("Close")
        ),
        size = "m",
        easyClose = TRUE
      )
    )
  })


  # Upload the second file------------------------------------------------------
  observe({
      if (!is.null(input$user_file_2)) {
          shinyjs::show("second_file_summary")  # Show the panel when a file is uploaded
          shinyjs::show("second_file")  # Show the panel when a file is uploaded
      } else {
          shinyjs::hide("second_file")  # Hide the panel if no file is uploaded
          shinyjs::hide("second_file_summary")  # Hide the panel if no file is uploaded
      }
  })
  output$data_upload_ui_2 <- renderUI({
    req(!is.null(input$user_file))

    req(is.null(input$user_file_2))

    fileInput(
      inputId = "user_file_2",
      label = "Upload 2nd file",
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

 # uploaded data
  user_data_2 <- reactive({

    req(input$user_file_2)
    in_file <- input$user_file_2
    in_file <- in_file$datapath
    req(!is.null(in_file))

    isolate({
      df <- data.frame()
      file_type <- "read_excel"
      # Excel file ---------------
      if (grepl("xls$|xlsx$", in_file, ignore.case = TRUE)) {
        try(
          df <- readxl::read_excel(in_file)
        )
        df <- as.data.frame(df)
      } else {
        #CSV --------------------
        try(
          df <- read.csv(in_file)
        )
        file_type <- "read.csv"

        # Tab-delimented file ----------
        if (ncol(df) <= 1) { # unable to parse with comma
          try(
            df <- read.table(
              in_file,
              sep = "\t",
              header = TRUE
            )
          )
          file_type <- "read.table"
        }
      }

      if (ncol(df) == 0) { # no data read in. Empty
        return(NULL)
      } else {

        # clean column names
        df <- df %>% janitor::clean_names()
        return(
          list(
            df = df,
            file_type = file_type
          )
        )
      }
    })
  })

show_pop_up_2 <- function() {
    showModal(
      modalDialog(
        title = "Verify data types (important!)  2",
        # Custom CSS to make the chat area scrollable
        tags$head(
            tags$style(HTML("
                #data_type_window {
                    height: 400px;  /* Adjust the height as needed */
                    overflow-y: auto;  /* Enables vertical scrolling */
                    padding: 10px;
                    border-radius: 5px;
                }
            "))
        ),
        div( id = "data_type_window", uiOutput("column_type_ui_2")),
        h4("If a column represents categories, choose 'Factor', even if 
        it is coded as numbers. Some columns are 
        automatically converted. For columns that are numbers, but with few unique values, RTutor 
        automatically convert them to factors. See Settings.", 
        style = "color: blue"),
        br(),
        footer = tagList(
          modalButton("Close")
        ),
        size = "l",
        easyClose = TRUE
      )
    )
  }

  observeEvent(input$user_file_2, {
     showNotification("2nd file uploaded! To use it, specify with its name df2.")
     show_pop_up_2()
  })

   
  # The current data
  current_data_2 <- reactiveVal(NULL)

  observeEvent(input$user_file_2, {
    req(input$select_data)

    if(input$select_data == uploaded_data) {
      eval(parse(text = paste0("df <- user_data_2()$df")))
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
      current_data_2(NULL)
    } else if(nrow(df) == 0) {
      current_data_2(NULL)
    } else { # there are data in the dataframe

      current_data_2(df)
    }

    run_env(list2env(append(as.list(run_env()), list(df2 = current_data_2()))))
    run_env_start(as.list(run_env()))
  })


  output$column_type_ui_2 <- renderUI({
    req(current_data_2())
    req(input$select_data)
    column_names <- names(current_data_2())
    examples <- capture.output(str(current_data_2()))
    examples <- examples[-1]
    examples <- gsub(" \\$ ", "", examples)

    lapply(seq_along(column_names), function(i) {
      column_name <- column_names[i]
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = paste0("column_type_2_", i),
            label = NULL,
            choices = c("Character" = "character",
                        "Numeric" = "numeric",
                        "Integer" = "integer",
                        "Date" = "Date",
                        "Factor" = "factor"),
            selected = class(current_data_2()[[i]])
          )
        ),
        column(
          width = 9,          
          align = "left",
          style = "margin-top: -5px;",
          h5(examples[i])
        )
      )

    })
  })

  # convert data types
  observe({
    req(current_data_2())
    for (i in seq_along(current_data_2())) {
      col_type <- input[[paste0("column_type_2_", i)]]
      if (!is.null(col_type)) {
        updated_data <- isolate(current_data_2())

        # when converting to factor the as function gives an error
        if(col_type == "factor") {
          updated_data[[i]] <- as.factor(updated_data[[i]])
        } else if (col_type == "Date") {
          updated_data[[i]] <- lubridate::parse_date_time(
            updated_data[[i]],
            orders = c("mdy", "dmy", "ymd")            
          )
          updated_data[[i]] <- as.Date(updated_data[[i]])
        } else {
          updated_data[[i]] <- as(updated_data[[i]], col_type)
        }
        current_data_2(updated_data)
      }
    }
  })

}
