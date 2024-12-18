##########################################################
# RTutor.AI | A Shiny app for chatting with your data.
# Author: Xijin Ge | ge@orditus.com
# © 2024 Orditus LLC
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



  #    Module 03 - 'Send Request'
  # __________________________________
  mod_03 <- mod_03_send_request_serv(
    id = "send_request",
    chunk_selection = chunk_selection,
    user_file = user_file,
    selected_dataset_name = selected_dataset_name,
    use_python = use_python
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
    chunk_selection = chunk_selection
  )



  #         Module 05 - 'LLMs'
  # __________________________________
  mod_05 <- mod_05_llms_serv(
    id = "llms",
    submit_button = submit_button,
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



  #   Module 06 - 'Errors & History'
  # __________________________________
  mod_06 <- mod_06_error_hist_serv(
    id = "errors_and_history",
    submit_button = submit_button,
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
    submit_button = submit_button,
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
    submit_button = submit_button,
    ch = ch,
    selected_model = selected_model,
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
  sample_temp <- mod_11$sample_temp
  selected_model <- mod_11$selected_model
  use_python <- mod_11$use_python
  convert_to_factor <- mod_11$convert_to_factor
  max_proportion_factor <- mod_11$max_proportion_factor
  max_levels_factor <- mod_11$max_levels_factor
  send_head <- mod_11$send_head
  contribute_data <- mod_11$contribute_data



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
    selected_dataset_name = selected_dataset_name
  )


  #       Module 17 - 'Policies Tabs'
  # __________________________________
  mod_17 <- mod_17_policies_serv(
    id = "policies"
  )



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
