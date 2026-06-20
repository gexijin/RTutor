##########################################################
# RTutor.AI | A Shiny app for chatting with your data.
# Author: Xijin Ge | ge@orditus.com
# © 2026 Orditus LLC
# No warranty & not for commercial use without a license.
##########################################################

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #                   House Keeping
  #_____________________________________________________________


  if(file.exists(on_server)){ #server
    options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB
    output$on_server <- renderText({"File Exists on Server"})
  } else { # local
    options(shiny.maxRequestSize = 10000 * 1024^2) # 10 GB
  }

  if(dev.cur() == 1){
    pdf(NULL) #otherwise, base R plots sometimes do not show.
  }

  # Ensure all devices are closed when the session ends
  session$onSessionEnded(function() {
    while (dev.cur() > 1) {
      dev.off()
    }
    pdf(NULL)
  })


  #                    Initialize Reactives
  #________________________________________________________________

  ## Module 04
  tabs <- reactive({ input$tabs })

  chunk_selection <- reactiveValues(
    chunk_choices = NULL,
    selected_chunk = NULL,
    past_prompt = NULL
  )


  ## Module 06
  logs <- reactiveValues(
    id = 0, # 1, 2, 3, id for code chunk
    code = "", # cumulative code
    raw = "",  # cumulative orginal code for print out
    last_code = "", # last code for Rmarkdown
    language = ""#, # Python or R
    # code_history = list(), # keep all code chunks
  )

  ch <- reactiveValues(code_history = list())

  counter <- reactiveValues(
    costs_total = 0, # cummulative cost
    requests = 0, # cummulative requests
    tokens_current = 0,  # tokens for current query
    time = 0 # response time for current
  )

  # change value when a previous code chunk is selected
  reverted <- reactiveVal(0)

  # Per-chunk Q&A history: named list keyed by chunk ID (as character)
  qa_by_chunk <- reactiveVal(list())


  ## Module 07
  # the current data
  current_data <- reactiveVal(NULL)
  current_data_2 <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  original_data_2 <- reactiveVal(NULL)
  code_error <- reactiveVal(FALSE)

  # define a reactive variable that holds an R environment
  # This is needed for the Rmd chunk
  run_env <- reactiveVal(new.env())

  # a list stores all data objects before running the code
  run_env_start <- reactiveVal(list())
  # define a reactive variable. Reactive function not returning error
  run_result <- reactiveVal(list())


  ## Module 15
  modal_closed <- reactiveVal(FALSE)

  # Incremented after prompt quality clears; gates mod_05/06/07/09 instead of submit_button
  quality_cleared <- reactiveVal(0)
  # Stores the latest plain-English error explanation from explain_error()
  error_explanation <- reactiveVal(list())



  #                    Modules and Outputs
  #________________________________________________________________

  #     Module 02 - 'Load Data'
  # __________________________________
  mod_02 <- mod_02_load_data_serv(
    id = "load_data",
    chunk_selection = chunk_selection,
    current_data = current_data,
    current_data_2 = current_data_2,
    original_data = original_data,
    original_data_2 = original_data_2,
    run_env = run_env,
    run_env_start = run_env_start,
    submit_button = submit_button,
    convert_to_factor = convert_to_factor,
    max_proportion_factor = max_proportion_factor,
    max_levels_factor = max_levels_factor
  )

  # Module 02 - Outputs
  selected_dataset_name <- mod_02$selected_dataset_name
  user_file <- mod_02$user_file
  user_file_2 <- mod_02$user_file_2
  user_data <- mod_02$user_data
  user_data_2 <- mod_02$user_data_2
  use_python <- FALSE

  # soft reset: clears execution state while preserving uploaded data
  do_soft_reset <- function() {
    logs$id        <- 0
    logs$code      <- ""
    logs$raw       <- ""
    logs$last_code <- ""
    logs$language  <- ""
    ch$code_history <- list()
    counter$costs_total    <- 0
    counter$requests       <- 0
    counter$tokens_current <- 0
    counter$time           <- 0
    code_error(FALSE)
    run_result(list())
    reverted(0)
    error_explanation(list())
    qa_by_chunk(list())
    chunk_selection$chunk_choices  <- NULL
    chunk_selection$selected_chunk <- NULL
    chunk_selection$past_prompt    <- NULL
    # Fresh env seeded with preserved data
    new_e <- new.env()
    new_e$df      <- current_data()
    new_e$df_name <- selected_dataset_name()
    new_e$df2     <- current_data_2()
    run_env(new_e)
    run_env_start(as.list(new_e))
    # Clear the prompt text area (namespaced input on parent session)
    updateTextAreaInput(session, "send_request-input_text", value = "")
  }



  #    Module 03 - 'Send Request'
  # __________________________________
  mod_03 <- mod_03_send_request_serv(
    id = "send_request",
    chunk_selection = chunk_selection,
    user_file = user_file,
    selected_dataset_name = selected_dataset_name,
    use_python = use_python,
    quality_cleared = quality_cleared,
    api_key = api_key,
    current_data = current_data,
    do_soft_reset = do_soft_reset,
    counter = counter
  )

  # Module 03 - Outputs
  input_text <- mod_03$input_text
  submit_button <- mod_03$submit_button
  reset_button <- mod_03$reset_button



  #     Module 04 - 'Main Panel'
  # __________________________________
  mod_04 <- mod_04_main_panel_serv(
    id = "main_panel",
    llm_response = llm_response,
    logs = logs,
    ch = ch,
    code_error = code_error,
    run_result = run_result,
    run_env_start = run_env_start,
    submit_button = submit_button,
    use_python = use_python,
    tabs = tabs,
    current_data = current_data,
    current_data_2 = current_data_2,
    selected_dataset_name = selected_dataset_name,
    chunk_selection = chunk_selection,
    run_env = run_env,
    reverted = reverted,
    api_key = api_key,
    error_explanation = error_explanation,
    input_text = input_text,
    counter = counter,
    qa_by_chunk = qa_by_chunk
  )



  #         Module 05 - 'LLMs'
  # __________________________________
  mod_05 <- mod_05_llms_serv(
    id = "llms",
    submit_button = quality_cleared,
    input_text = input_text,
    selected_dataset_name = selected_dataset_name,
    api_key = api_key,
    sample_temp = sample_temp,
    selected_model = selected_model,
    logs = logs,
    ch = ch,
    counter = counter,
    api_error_modal = api_error_modal,
    code_error = code_error,
    current_data = current_data,
    current_data_2 = current_data_2,
    run_env = run_env,
    run_env_start = run_env_start,
    run_result = run_result,
    use_python = use_python,
    send_head = send_head
  )

  # Module 05 - Outputs
  llm_prompt <- mod_05$llm_prompt
  llm_response <- mod_05$llm_response
  agent_name <- mod_05$agent_name



  #   Module 06 - 'Errors & History'
  # __________________________________
  mod_06 <- mod_06_error_hist_serv(
    id = "errors_and_history",
    submit_button = quality_cleared,
    llm_response = llm_response,
    logs = logs,
    ch = ch,
    counter = counter,
    reverted = reverted,
    use_python = use_python,
    run_result = run_result,
    python_to_html = python_to_html,
    input_text = input_text,
    llm_prompt = llm_prompt,
    run_env = run_env,
    run_env_start = run_env_start,
    chunk_selection = chunk_selection,
    Rmd_chunk = Rmd_chunk,
    current_data = current_data,
    current_data_2 = current_data_2,
    contribute_data = contribute_data,
    selected_dataset_name = selected_dataset_name,
    user_file = user_file,
    code_error = code_error
  )

  # Module 06 - Outputs
  api_error_modal <- mod_06$api_error_modal
  # code_error <- mod_06$code_error



  #       Module 07 - 'Run Code'
  # __________________________________
  mod_07 <- mod_07_run_code_serv(
    id = "run_code",
    run_env = run_env,
    run_env_start = run_env_start,
    run_result = run_result,
    submit_button = quality_cleared,
    reverted = reverted,
    logs = logs,
    use_python = use_python,
    selected_dataset_name = selected_dataset_name,
    current_data = current_data,
    current_data_2 = current_data_2,
    code_error = code_error
  )



  #        Module 09 - 'Report'
  # __________________________________
  mod_09 <- mod_09_report_serv(
    id = "report",
    submit_button = quality_cleared,
    ch = ch,
    selected_model = selected_model,
    agent_name = agent_name,
    llm_response = llm_response,
    input_text = input_text,
    use_python = use_python,
    logs = logs,
    sample_temp = sample_temp,
    code_error = code_error,
    python_to_html = python_to_html,
    current_data = current_data,
    current_data_2 = current_data_2,
    selected_dataset_name = selected_dataset_name,
    user_data = user_data,
    user_data_2 = user_data_2,
    user_file = user_file,
    user_file_2 = user_file_2
  )

  # Module 09 - Outputs
  Rmd_chunk <- mod_09$Rmd_chunk



  #         Module 10 - 'EDA'
  # __________________________________
  mod_10 <- mod_10_eda_serv(
    id = "eda",
    selected_dataset_name = selected_dataset_name,
    use_python = use_python,
    current_data = current_data,
    current_data_2 = current_data_2,
    ch = ch
  )



  #       Module 11 - 'Settings'
  # __________________________________
  mod_11 <- mod_11_settings_serv(
    id = "sett",
    submit_button = submit_button,
    llm_prompt = llm_prompt,
    code_error = code_error
  )

  # Module 11 - Outputs
  api_key <- mod_11$api_key
  selected_model <- mod_11$selected_model

  sample_temp <- mod_11$sample_temp
  use_python <- mod_11$use_python
  convert_to_factor <- mod_11$convert_to_factor
  max_proportion_factor <- mod_11$max_proportion_factor
  max_levels_factor <- mod_11$max_levels_factor
  send_head <- mod_11$send_head
  contribute_data <- mod_11$contribute_data

  # Log all settings to browser console once on session start
  session$onFlushed(function() {
    isolate({
      key_val  <- api_key$key
      key_disp <- if (nchar(key_val) > 4) paste0("****", substr(key_val, nchar(key_val) - 3, nchar(key_val))) else if (nchar(key_val) > 0) "****" else "(none)"
      shinyjs::runjs(paste0(
        'console.group("RTutor Settings");',
        'console.log("Model:             ', selected_model(),       '");',
        'console.log("Temperature:       ', sample_temp(),          '");',
        'console.log("API key:           ', key_disp,               '");',
        'console.log("API key source:    ', api_key$source,         '");',
        'console.log("Use Python:        ', use_python(),           '");',
        'console.log("Treat as factors:  ', convert_to_factor(),    '");',
        'console.log("Max factor levels: ', max_levels_factor(),    '");',
        'console.log("Max factor prop:   ', max_proportion_factor(),'");',
        'console.log("Send data sample:  ', send_head(),            '");',
        'console.log("Contribute data:   ', contribute_data(),      '");',
        'console.groupEnd();'
      ))
    })
  }, once = TRUE)

  #      Module 12 - 'About Tab'
  # __________________________________
  mod_12 <- mod_12_about_serv(
    id = "about"
  )



  #       Module 13 - 'FAQ Tab'
  # __________________________________
  mod_13 <- mod_13_faq_serv(
    id = "faq"
  )



  #   Module 15 - 'Data Types Modal'
  # __________________________________
  mod_15 <- mod_15_data_types_serv(
    id = "data_edit_modal",
    modal_closed = modal_closed,
    run_env = run_env,
    run_env_start = run_env_start,
    current_data = current_data,
    current_data_2 = current_data_2,
    original_data = original_data,
    original_data_2 = original_data_2,
    ch = ch,
    user_file = user_file,
    user_file_2 = user_file_2
  )

  # Module 15 - Outputs
  modal_closed <- mod_15$modal_closed


  mod_16 <- mod_16_qa_serv(
    id = "qa",
    submit_button = submit_button,
    ch = ch,
    code_error = code_error,
    run_result = run_result,
    api_error_modal = api_error_modal,
    counter = counter,
    selected_model = selected_model,
    api_key = api_key,
    sample_temp = sample_temp,
    selected_dataset_name = selected_dataset_name,
    qa_by_chunk = qa_by_chunk,
    chunk_selection = chunk_selection
  )


  #       Module 17 - 'Policies Tabs'
  # __________________________________
  #mod_17 <- mod_17_policies_serv(
  #  id = "policies"
  #)



  #                    Miscellaneous Functions
  #________________________________________________________________

  # File is rendered and stored in the html_file variable in code_history
  python_to_html <- reactive({
    req(submit_button())
    req(logs$language == "Python")
    req(use_python())

    isolate({
      python_html(
        python_code = logs$code,
        select_data = selected_dataset_name(),
        current_data = current_data()
      )
    })
  })

}
