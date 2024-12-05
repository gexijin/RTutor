#____________________________________________________________________________
#  Data Types
#____________________________________________________________________________


mod_15_data_types_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        tags$label(
          "2. Modify Data Fields",
          style = "font-size: 18px;font-weight: bold;color: #333;
            display: block;margin-bottom: 5px;"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          tippy::tippy_this(
            ns("data_edit_modal"),
            "Numbers, Categories, etc.",
            theme = "light-border"
          ),
          actionButton(ns("data_edit_modal"), "Data Types"),
          align = "left"
        ),
        tags$head(tags$style(
          sprintf(
            "#%s {
              font-size: 16px;
              color: #000;
              background-color: #F6FFF5;
              border-color: #90BD8C;
              margin-top: 5px;
              }",
            ns("data_edit_modal")
          )
        )),
      )
    ),
    hr(class = "custom-hr")
  )
}


mod_15_data_types_serv <- function(id, modal_closed, run_env, run_env_start,
                                   current_data, current_data_2, original_data,
                                   original_data_2, logs, user_file,
                                   user_file_2) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Modals ###
    # Data Types Modal
    show_pop_up <- function() {
      showModal(
        modalDialog(
          title = div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div("Verify Data Types (Important!)"),
            div(
              actionButton(
                ns("learn_more"),
                label = HTML('
                  <span style="font-weight:bold;font-size:16px;">What are Data Types?</span>
                '),
                style = "margin-right:10px;background-color:#90BD8C;"
              )
            )
          ),
          tags$head(
            tags$style(HTML("
                #data_type_window {
                    height: 400px;  /* Adjust the height as needed */
                    overflow-y: auto;  /* Enables vertical scrolling */
                    padding: 10px;
                    border-radius: 5px;
                }
                .modal-footer {
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                }
                .button-group {
                  display: flex;
                  gap: 10px;  /* Adjusts the space between the buttons */
                  justify-content: flex-start;
                  flex-grow: 1;
                }
            "))
          ),
          # Custom CSS to make the chat area scrollable
          tabsetPanel(
            tabPanel("Dataset 1",
              div(id = "data_type_window", uiOutput(ns("column_type_ui"))),
              h4("For columns that are numbers, but with few unique values,
                 RTutor automatically converts them to factors (categories),
                 see Settings for more info.",
                style = "color: blue"
              )
            ),
            tabPanel("Dataset 2",
              condition = "1",
              div(id = "data_type_window", uiOutput(ns("column_type_ui_2"))),
              h4("For columns that are numbers, but with few unique values,
                 RTutor automatically converts them to factors (categories),
                 see Settings for more info.",
                style = "color: blue"
              )
            )
          ),
          br(),
          size = "l",
          footer = tagList(
            div(class = "button-group",
              actionButton(
                ns("revert_data"),
                label = "Revert to Original Data"
              ),
              uiOutput(ns("revert_data2_button"))
            ),
            actionButton(ns("dismiss_modal"), label = "Dismiss")
          ),
          easyClose = TRUE
        )
      )
    }

    # Trigger the pop-up when a file is uploaded or when button is clicked
    observeEvent(input$data_edit_modal, {
      show_pop_up()
    })

    observeEvent(user_file(), {
      show_pop_up()
    })
    observeEvent(user_file_2(), {
      showNotification(
        "2nd file uploaded! To use it, specify with its name 'df2'."
      )
      show_pop_up()
    })

    observeEvent(input$dismiss_modal, {
      modal_closed(TRUE)
      removeModal()
    })

    observeEvent(modal_closed(), {
      req(modal_closed())
      shiny::showNotification(
        "Know thy enemy. Explore your data at the EDA tab first.",
        duration = 10
      )
      modal_closed(FALSE)
    })


    # Learn More Modal (within Data Types Modal)
    learn_more_modal <- function() {
      showModal(
        modalDialog(
          title = "Understanding Data Types",
          easyClose = FALSE,
          size = "m",
          p("Data types determine how information is stored, processed, and displayed. Here's a breakdown of common types:"),
          tags$ul(
            tags$li(strong("Character (Text):"), " Words or letters with many unique values. Like names, labels, or phrases."),
            tags$li(strong("Numbers:"), " Decimal numbers."),
            tags$li(strong("Integers:"), " Whole numbers."),
            tags$li(strong("Categories (Factors):"), " Data with specific groups and few unique values. Like 'A/B/C/D', 'Small/Medium/Large', '1999/2000/2001'."),
            tags$li(strong("Dates:"), " Calendar dates.")
          ),
          p("Choosing the right data type ensures accurate analysis and avoids errors."),
          footer = actionButton(ns("close_learn_more_modal"), label = "Close")
        )
      )
    }

    # Handle Learn More buttons
    observeEvent(input$learn_more, {
      learn_more_modal()
    })

    # Close the Learn More modal
    observeEvent(input$close_learn_more_modal, {
      removeModal()
      show_pop_up()  # Reopen the Data Types Modal
    })


    ### Data Type Modifications ###

    # Column Type Dropdown - 1st Dataset
    output$column_type_ui <- renderUI({
      req(current_data())

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
                inputId = ns(paste0("column_type_", i)),
                label = NULL,
                choices = c("Character" = "character",
                            "Numeric" = "numeric",
                            "Integer" = "integer",
                            "Category" = "factor",
                            "Date" = "Date"),
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

    # Column Type Dropdown - 2nd Dataset
    output$column_type_ui_2 <- renderUI({
      req(current_data_2())
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
              inputId = ns(paste0("column_type_2_", i)),
              label = NULL,
              choices = c("Character" = "character",
                          "Numeric" = "numeric",
                          "Integer" = "integer",
                          "Category" = "factor",
                          "Date" = "Date"),
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


    # Update data based on column_type - 1st Dataset
    observe({
      req(current_data())
      req(input$revert_data == 0) #Negative: If we hit revert data button, we don't want to run all this junk
                                  #Positive: Run as long as we don't hit revert_data button
      req(is.null(input$submit_button) || input$submit_button == 0 || as.integer(input$selected_chunk) == length(logs$code_history))
      #Negative: If we revert a chunk we don't want to run all this junk
      #Positive: Run as long as our selected chunk is the current maximum code chunk.
        #(i.e. If submit_button == 0 then selected chunk would be NULL and maximum code chunk is 0\NULL)
      # req(run_data_process())
      for (i in seq_along(current_data())) {
        col_type <- input[[paste0("column_type_", i)]]
        if (!is.null(col_type)) {
          updated_data <- isolate(current_data())

          # when converting to factor the as function gives an error
          if (col_type == "factor") {
            updated_data[[i]] <- as.factor(updated_data[[i]])
          } else if (col_type == "Date") {
            updated_data[[i]] <- lubridate::parse_date_time(
              updated_data[[i]],
              orders = c("mdy", "dmy", "ymd")
            )
            updated_data[[i]] <- as.Date(updated_data[[i]])
          } else if(col_type == "numeric" & class(updated_data[[i]]) == "factor") {
            updated_data[[i]] <- as(as.character(updated_data[[i]]), col_type)
          } else {
            updated_data[[i]] <- as(updated_data[[i]], col_type)
          }
          current_data(updated_data)
          isolate({
            # run_env(rlang::env(run_env(), df = current_data()))
            # run_env_start(as.list(run_env()))

            # Code to update environment and not overwrite
            existing_vars <- as.list(run_env())
            existing_vars$df <- current_data()
            run_env(list2env(existing_vars))
            run_env_start(as.list(run_env()))
          })
        }
      }
    })

    # Update data based on column_type_2 - 2nd Dataset
    observe({
      req(current_data_2())
      req(input$revert_data2 == 0) #Negative: If we hit revert data button, we don't want to run all this junk
      req(is.null(input$submit_button) || input$submit_button == 0 || as.integer(input$selected_chunk) == length(logs$code_history))
      for (i in seq_along(current_data_2())) {
        col_type <- input[[paste0("column_type_2_", i)]]
        if (!is.null(col_type)) {
          updated_data <- isolate(current_data_2())

          # when converting to factor the as function gives an error
          if (col_type == "factor") {
            updated_data[[i]] <- as.factor(updated_data[[i]])
          } else if (col_type == "Date") {
            updated_data[[i]] <- lubridate::parse_date_time(
              updated_data[[i]],
              orders = c("mdy", "dmy", "ymd")
            )
            updated_data[[i]] <- as.Date(updated_data[[i]])
          } else if(col_type == "numeric" & class(updated_data[[i]]) == "factor") {
            updated_data[[i]] <- as(as.character(updated_data[[i]]), col_type)
          } else {
            updated_data[[i]] <- as(updated_data[[i]], col_type)
          }
          current_data_2(updated_data)
          isolate({
            # run_env(rlang::env(run_env(), df2 = current_data_2()))
            # run_env_start(as.list(run_env()))

            #Code to update environment and not overwrite
            existing_vars <- as.list(run_env())
            existing_vars$df2 <- current_data_2()
            run_env(list2env(existing_vars))
            run_env_start(as.list(run_env()))
          })
        }
      }
    })


    # Revert to Original Col Types - 1st Dataset
    observeEvent(input$revert_data, {
      # Add some requirements??
      # Update current_data() to be the original_data()
      current_data(original_data())
      column_names <- names(current_data())
      lapply(seq_along(column_names), function(i) {
        column_name <- column_names[i]
        updateSelectInput(
          session = session,
          inputId = ns(paste0("column_type_", i)),
          label = NULL,
          choices = c("Character" = "character",
                      "Numeric" = "numeric",
                      "Integer" = "integer",
                      "Category" = "factor",
                      "Date" = "Date"),
          selected = class(current_data()[[i]])
        )
      })

      existing_vars <- as.list(run_env())
      run_env(list2env(existing_vars))
      run_env_start(as.list(run_env()))

      # Close modal
      modal_closed(TRUE)
      shiny::removeModal()

      # Show message modal
      shiny::showModal(
        shiny::modalDialog(
          size = "s",
          easyClose	= TRUE,
          h5("Successfully Reverted to Original Data")
        )
      )
    })

    # 2nd Dataset Revert Button
    output$revert_data2_button <- renderUI({
      req(current_data_2())  # Only display if second dataset exists
      actionButton(ns("revert_data2"), label = "Revert to Original Data2")
    })

    # Revert to Original Col Types - 2nd Dataset
    observeEvent(input$revert_data2, {
      # Add some requirements??
      # Update current_data() to be the original_data()
      current_data_2(original_data_2())
      column_names <- names(current_data_2())
      lapply(seq_along(column_names), function(i) {
        column_name <- column_names[i]
        updateSelectInput(
          session = session,
          inputId = ns(paste0("column_type_2_", i)),
          label = NULL,
          choices = c("Character" = "character",
                      "Numeric" = "numeric",
                      "Integer" = "integer",
                      "Category" = "factor",
                      "Date" = "Date"),
          selected = class(current_data_2()[[i]])
        )
      })

      # run_env(rlang::env(run_env(), df2 = current_data_2()))
      # run_env_start(as.list(run_env()))

      existing_vars <- as.list(run_env())
      run_env(list2env(existing_vars))
      run_env_start(as.list(run_env()))


      # Close modal
      modal_closed(TRUE)
      shiny::removeModal()

      # Show message modal
      shiny::showModal(
        shiny::modalDialog(
          size = "s",
          easyClose	= TRUE,
          h5("Successfully Reverted df2 to Original Data")
        )
      )
    })


    # Return all reactive values so they can be used outside the module
    return(
      list(
        data_edit_modal = reactive(input$data_edit_modal),
        modal_closed = modal_closed
      )
    )
  })
}