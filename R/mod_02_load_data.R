#____________________________________________________________________________
#  Loading Data
#____________________________________________________________________________


mod_02_load_data_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        # Display dataset dropdown
        conditionalPanel(
          condition = paste0("output['", ns("show_option1"), "'] === 'show'"),
          selectInput(
            inputId = ns("user_selected_dataset"),
            label = HTML("<span style='font-size: 18px; font-weight: bold;'>1. Data</span>"),
            choices = available_datasets,
            selected = data_placeholder,
            multiple = FALSE
          )
        ),
        # Azure env var debug display
        uiOutput(ns("azure_debug")),
        # Display selected dataset
        conditionalPanel(
          condition = paste0("output['", ns("show_option1"), "'] === 'hide'"),
          uiOutput(ns("selected_dataset"))
        )
      ),
      column(
        width = 6,
        # File uploads 1 & 2
        uiOutput(ns("data_upload_ui")),
        uiOutput(ns("data_upload_ui_2"))
      )
    ),
    hr(class = "custom-hr")
  )
}



mod_02_load_data_serv <- function(id, chunk_selection, current_data,
                                  current_data_2, original_data, original_data_2,
                                  run_env, run_env_start, submit_button,
                                  convert_to_factor, max_proportion_factor,
                                  max_levels_factor) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(tidyverse) # otherwise built-in data is unavailable when running from R package.

    # First Dataset Upload ----------------------
    output$data_upload_ui <- renderUI({

      # LHS: Hide after first run; RHS: For when submitted accidentally
      # Third condition: keep visible when upload is selected but no file provided yet
      req(submit_button() == 0 ||
          input$user_selected_dataset == data_placeholder ||
          (input$user_selected_dataset == user_upload && is.null(input$user_file)))
      req(is.null(input$user_file)) # Hide after user inputs data

      fileInput(
        inputId = ns("user_file"),
        label = "Upload",
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

    # Uploaded data
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
          # CSV --------------------
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

    observeEvent(input$user_file, {
      updateSelectInput(
        session,
        inputId = "user_selected_dataset", # DO NOT NOT NOT put ns() around this ID!
        choices = available_datasets,      # It screws up everything!
        selected = user_upload
      )
    }, ignoreInit = TRUE, once = TRUE)


    # Read in rna_seq data
    rna_seq_data <- reactive({
      req(input$user_selected_dataset == rna_seq)

      df <- read.csv(app_sys("app", "www", "GSE37704.csv"))
      return(df)
    })

    # Read in built-in data, load data
    observeEvent(input$user_selected_dataset, {

      if (input$user_selected_dataset == user_upload) {
        eval(parse(text = paste0("df <- user_data()$df")))
      } else if (input$user_selected_dataset %in% c(no_data, data_placeholder)) {
        df <- NULL
      } else if (input$user_selected_dataset %in% uiuc_datasets) { # load data from www/demo_data
        df <- read.csv(app_sys("app", "www", "demo_data",
                paste0(input$user_selected_dataset, ".csv")))
      } else if (input$user_selected_dataset == rna_seq) {
        df <- rna_seq_data()
      } else {
        df <- get(input$user_selected_dataset)
      }

      if (convert_to_factor()) {
        df <- numeric_to_factor(
          df,
          max_levels_factor(),
          max_proportion_factor()
        )
      }

      # if the first column looks like ID
      # if (!is.null(df)) {
      #   if (
      #     length(unique(df[, 1])) == nrow(df) &&  # all unique
      #       is.character(df[, 1])  # first column is character
      #   ) {
      #     row.names(df) <- df[, 1]
      #     col_name <- colnames(df)[1]
      #     df <- df[, -1]

      #     shinyalert::shinyalert(
      #       title = paste(
      #         "Column", col_name,
      #         "has been recognized as an unique identifier and has been removed."
      #       ),
      #       text = NULL,
      #       type = "warning",
      #       showCancelButton = FALSE
      #     )
      #   }
      # }

      # sometimes no row is left after processing.
      if (is.null(df) || nrow(df) == 0) { # no_data
        current_data(NULL)
      } else { # there are data in the dataframe
        current_data(df)
        original_data(df)
      }

      isolate({
        existing_vars <- as.list(run_env())
        run_env(list2env(existing_vars))
        run_env_start(as.list(run_env()))
      })
    })


    # Second Dataset Upload ---------------------
    output$data_upload_ui_2 <- renderUI({
      req(!is.null(input$user_file))

      req(is.null(input$user_file_2)) # Hide after user inputs data

      fileInput(
        inputId = ns("user_file_2"),
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

    # Uploaded data
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


    observeEvent(input$user_file_2, {

      if (input$user_selected_dataset == user_upload) {
        eval(parse(text = paste0("df <- user_data_2()$df")))
      }
      if (convert_to_factor()) {
        df <- numeric_to_factor(
          df,
          max_levels_factor(),
          max_proportion_factor()
        )
      }

      # if the first column looks like ID
      if (
        length(unique(df[, 1])) == nrow(df) &&  # all unique
          is.character(df[, 1])  # first column is character
      ) {
        row.names(df) <- df[, 1]
        col_name <- colnames(df)[1]
        df <- df[, -1]

        shinyalert::shinyalert(
          title = paste(
            "Column", col_name,
            "has been recognized as an unique identifier and has been removed."
          ),
          text = NULL,
          type = "warning",
          showCancelButton = FALSE
        )
      }

      # sometimes no row is left after processing.
      if (is.null(df)) { # no_data
        current_data_2(NULL)
      } else if(nrow(df) == 0) {
        current_data_2(NULL)
      } else { # there are data in the dataframe
        current_data_2(df)
        original_data_2(df)
      }

      isolate({
        existing_vars <- as.list(run_env())
        existing_vars$df2 <- current_data_2()
        run_env(list2env(existing_vars))
        run_env_start(as.list(run_env()))
      })
    })


    # Azure env var debug display (last 4 chars only)
    output$azure_debug <- renderUI({
      key      <- Sys.getenv("AZURE_OPENAI_API_KEY")
      endpoint <- Sys.getenv("AZURE_OPENAI_API_ENDPOINT")
      mask_key      <- function(s) if (nchar(s) >= 4) paste0("...", substr(s, nchar(s) - 3, nchar(s))) else "(not set)"
      mask_endpoint <- function(s) if (nchar(s) >= 20) paste0(substr(s, 1, 20), "...") else if (nchar(s) > 0) s else "(not set)"
      HTML(paste0(
        "<small style='color:gray;'>",
        "Key: ", mask_key(key), "<br>",
        "Endpoint: ", mask_endpoint(endpoint),
        "</small>"
      ))
    })

    # Display selected dataset
    output$selected_dataset <- renderUI({
      req(submit_button())
      # when submit is clicked, but no data is uploaded.

      if (input$user_selected_dataset == user_upload) {
        if (is.null(input$user_file)) {
          return(NULL)  # show_option1 keeps the data section visible; no label needed
        } else {
          txt <- "Dataset: Upload"
        }
      } else if (input$user_selected_dataset == data_placeholder) {
        txt <- NULL
      } else {
        txt <- paste0("Data:\n", input$user_selected_dataset)
      }

      # Create a line break if dataset name is too long
      if (!is.null(input$user_selected_dataset) &&
        nchar(input$user_selected_dataset) > 25) {
        txt <- paste0(substr(txt, 1, 25), "<br>", substr(txt, 26, nchar(txt)))
      }

      return(HTML(paste0("<span style='font-size: 18px; font-weight: bold; white-space: normal; word-wrap: break-word;'>"
      , txt, "</span>")))
    })


    # Popup notification when submit is clicked with upload selected but no file present
    observeEvent(submit_button(), {
      req(input$user_selected_dataset == user_upload)
      req(is.null(input$user_file))
      showNotification(
        "No file uploaded! Please upload your data first.",
        type = "warning",
        duration = 8
      )
    })

    # Condition based on input from mod_03 for UI conditional panel
    output$show_option1 <- renderText({
      # Keep the data section visible if user chose Upload but hasn't provided a file
      if (submit_button() == 0 ||
          input$user_selected_dataset == data_placeholder ||
          (input$user_selected_dataset == user_upload && is.null(input$user_file))) {
        return("show")  # Show dataset dropdown
      } else {
        return("hide")  # Show selected dataset
      }
    })
    # Ensures this runs in background even when not called in UI
    outputOptions(output, "show_option1", suspendWhenHidden = FALSE)


    # Return all reactive values so they can be used outside the module
    return(
      list(
        selected_dataset_name = reactive(input$user_selected_dataset),
        user_file = reactive(input$user_file),
        user_file_2 = reactive(input$user_file_2),
        user_data = user_data,
        user_data_2 = user_data_2
      )
    )

  })
}