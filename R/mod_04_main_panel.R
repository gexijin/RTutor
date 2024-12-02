
#____________________________________________________________________________
#  Main Panel
#____________________________________________________________________________


mod_04_main_panel_ui <- function(id) {

  ns <- NS(id)

  tagList(

    # 'First Time User' tab redirect
    tags$script(HTML("
    /* Update the active tab to 'First Time User' within the 'More' navbarMenu */
      $(document).on('click', '#first_user', function() {
        $('#tabs a[data-value=\"first-time-user\"]').tab('show');
      });
    ")),

    # Initial UI display
    conditionalPanel(
      condition = "input['send_request-submit_button'] == 0",
      fluidRow(
        column(
          width = 5,
          actionButton("first_user", strong("Quick start"), class = "first-user"),
          align = "left"
        ),
        column(
          width = 7,
          img(src = "www/logo.png", width = "155", height = "77"),
          align = "left"
        )
      ),
    ),

    # After submit is clicked
    conditionalPanel(
      condition = "input['send_request-submit_button'] != 0",
      fluidRow(
        column(
          width = 5,
          # Chunk select dropdown
          div(
            style = "display: inline-block; vertical-align: top; margin-right: 10px;",
            selectInput(
              inputId = ns("selected_chunk"),
              label = div("AI Generated Code:", style = "font-size: 18px;"),
              selected = NULL,
              choices = NULL
            )
          ),
          div(      # Align button next to dropdown
            style = "display: inline-block; vertical-align: top;
              padding-top: 30px; padding-bottom: 5px;",
            actionButton(
              ns("delete_chunk"),
              "Delete Chunk"
            )
          ),
          tags$head(tags$style(  # Button styling
            sprintf(
              "#%s {font-size: 14px; color: #000; background-color: #F6FFF5; border-color: #90BD8C;}",
              ns("delete_chunk")
            )
          )),
          tippy::tippy_this(
            ns("selected_chunk"),
            "You can go back to any previous code chunk and continue from there. The data will also be reverted to that point.",
            theme = "light-border"
          ),
          tippy::tippy_this(
            ns("delete_chunk"),
            "Don't like this code chunk? Click to remove.",
            theme = "light-border"
          )
        ),
        column(
          width = 7,
          # Checkbox to show code behind output
          checkboxInput(
            inputId = ns("show_code"),
            label = div(
              "Show Code",
              style = "font-size: 16px;padding-right: 25px;"
            ),
            value = TRUE
          ),
          align = "right"
        )
      ),

      # If checked, show the code
      conditionalPanel(
        condition = "input.show_code == true",
        ns = ns,
        verbatimTextOutput(ns("openAI"))
      ),

      conditionalPanel(
        condition = "true",

        # shows error message in local machine, but not on the server
        uiOutput(ns("error_message")),
        verbatimTextOutput(ns("console_output")),

        # Display plot result
        uiOutput(ns("plot_ui")),
        fluidRow(
          column(
            width = 5,
            # Checkbox to make output interactive
            checkboxInput(
              inputId = ns("make_ggplot_interactive"),
              label = NULL,
              value = FALSE
            ),
            align = "right"
          ),
          column(
            width = 5,
            checkboxInput(
              # Checkbox to make output interactive
              inputId = ns("make_cx_interactive"),
              label = NULL,
              value = FALSE
            ),
            align = "left"
          )
        ),
        br(),
        # Display helpful tips on interactive plots
        uiOutput(ns("tips_interactive"))
      )
    ),
    conditionalPanel(
      condition = "1",
      # First dataset
      hr(class = "custom-hr-thick"),
      uiOutput(ns("data_size")),
      DT::dataTableOutput(ns("data_table_DT")),
      # Second dataset
      hr(class = "custom-hr-thick"),
      uiOutput(ns("data_size_2")),
      DT::dataTableOutput(ns("data_table_DT_2")),
      # Data tables styling
      tags$head(
        tags$style(HTML("
          .dataTables_wrapper {background-color: #f8fcf8;border-color: #90BD8C;padding: 10px;border-radius: 5px;}
          .dataTables_wrapper table.dataTable tbody tr:nth-child(odd) {background-color: #f3faf3;}
          .dataTables_wrapper table.dataTable tbody tr:nth-child(even) {background-color: #fff;}
        "))
      )
    )
  )
}

mod_04_main_panel_serv <- function(id, llm_response, logs, code_error,
                                   run_result, run_env_start, submit_button,
                                   use_python, tabs, current_data, current_data_2,
                                   selected_dataset_name, chunk_selection) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # pop-up to warn users under age 13 & those with a for-profit org. NOT to use
    observe({
      commercial_use_modal <- shiny::modalDialog(
        title = "RTutor Usage Policy",

        tags$br(),
        tags$h4("RTutor is available to the public strictly for education and
          non-profit organizations. If you are affiliated with a company or intend
          to use RTutor for commercial activities, you must obtain a license from us.
          Please contact us at ",
          a("ge@orditus.com.", href = "mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com")
        ),

        tags$br(),
        tags$h4("We've updated our ",
          a("Privacy Policy", href = "www/privacypolicyRTutor.pdf", target = "_blank"),
          "and ",
          a("Terms of Use.", href = "www/termsofuseRTutor.pdf", target = "_blank"),
          " By continuing to RTutor.ai, you acknowledge and agree to these changes."
        ),

        footer = tagList(modalButton("Agree")),

        easyClose = TRUE,
        size = "l"
      )

      shiny::showModal(commercial_use_modal)
    })


    ###  Selecting Chunk  ###

    # Update the selectInput choices when number of chunks changes
    observe({
      req(chunk_selection$chunk_choices)
      req(chunk_selection$selected_chunk)

      updateSelectInput(
        session = session,
        inputId = "selected_chunk",
        choices = chunk_selection$chunk_choices,
        selected = chunk_selection$selected_chunk
      )
    })

    # React to user selection in the dropdown
    observeEvent(input$selected_chunk, {
      chunk_selection$selected_chunk <- input$selected_chunk
    })


    ###  Print Results or Error  ###

    # Print code chunk
    output$openAI <- renderPrint({
      req(llm_response()$cmd)
      res <- logs$raw
      res <- gsub("```", "", res)
      cat(res)
    })

    # Print results
    output$console_output <- renderText({
      req(!code_error())
      paste(run_result()$console_output, collapse = "\n")
    })

    # Display error messages
    output$error_message <- renderUI({
      req(code_error())
      req(logs$code)
      if(code_error()) {
        h4(paste("Error!", run_result()$error_message), style = "color:red")
      } else {
        return(NULL)
      }
    })


    ###  Plotting  ###

    # Plot results
    output$result_plot <- renderPlot({
      req(!code_error())
      req(logs$code)
      # Check if the result is not a ggplot or a known plot type
      if (inherits(run_result()$result, "ggplot") || is.null(run_result()$console_output)) {
        return(run_result()$result)
      } else {
        # If the result is not a ggplot (e.g., corrplot), re-evaluate the command_string,
        # under the parent environment of the run_env()
        tmp_env <- list2env(run_env_start())
        tryCatch({
          eval_result <- eval(
            parse(text = clean_cmd(logs$code, selected_dataset_name(), file.exists(on_server))),
            envir = tmp_env
          )
        })
      }
    })

    # Plot results - plotly
    output$result_plotly <- plotly::renderPlotly({
      req(!code_error())
      req(!use_python())
      req(
        is_interactive_plot() ||   # natively interactive
          turned_on(input$make_ggplot_interactive)
      )

      g <- run_result()$result
      # still errors some times, when the returned list is not a plot
      if (is.character(g) || is.data.frame(g) || is.numeric(g)) {
        return(NULL)
      } else {
        return(g)
      }
    })

    # Plot results - canvasXpress
    output$result_CanvasXpress <- canvasXpress::renderCanvasXpress({
      req(!code_error())
      req(!use_python())

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

    # Checks to render plotly or canvasXpress if applicable
    output$plot_ui <- renderUI({
      req(submit_button())
      req(!use_python())
      req(!code_error())
      req(logs$code)

      if (
        is_interactive_plot() ||   # natively interactive
          turned_on(input$make_ggplot_interactive) # converted
      ) {
        plotly::plotlyOutput(ns("result_plotly"))
      } else if (
        turned_on(input$make_cx_interactive) # converted
      ) {
        canvasXpress::canvasXpressOutput(ns("result_CanvasXpress"))
      } else {
        plotOutput(ns("result_plot"))
      }
    })

    # Display tips for interactive plots
    output$tips_interactive <- renderUI({
      req(submit_button())

      if (is_interactive_plot() ||   # natively interactive
          turned_on(input$make_ggplot_interactive) # converted
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

    # Check if plot is interactive
    is_interactive_plot <- reactive({
      # only true if the plot is interactive, natively.
      req(submit_button())
      req(logs$code)
      req(!code_error())
      if (inherits(run_result()$result, "plotly")) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })

    # Reminder for user to uncheck interactive plot
    observe({
      req(input$make_cx_interactive)
      req(tabs() == "Home")
      showNotification(
        ui = paste("Please uncheck the CanvasXpress
        box before proceeding to the next request."),
        id = "uncheck_canvasXpress",
        duration = 10,
        type = "error"
      )
    })

    # Remove reminder messages if the tab changes
    observe({
      if (is.null(input$make_cx_interactive) || tabs() != "Home") {
        removeNotification("uncheck_canvasXpress")
      }
    })

    # Hide interactive checkbox initially
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
      txt <- paste(llm_response()$cmd, collapse = " ")

      # if not a dataframe, create dummy data
      if ("data.frame" %in% class(current_data())) {
        df <- current_data()
      } else {
        df <- data.frame(value = rep(1, 3))
      }

      if (inherits(run_result()$result, "ggplot") && # if ggplot2, and it is
          !is_interactive_plot() && # not already an interactive plot, show
          # if there are too many data points, don't do the interactive
          !(dim(df)[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
      ) {
        shinyjs::showElement(id = "make_ggplot_interactive")
      }
    })

    # Hide interactive checkbox initially
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
      txt <- paste(llm_response()$cmd, collapse = " ")

      # if not a dataframe, create dummy data
      if ("data.frame" %in% class(current_data())) {
        df <- current_data()
      } else {
        df <- data.frame(value = rep(1, 3))
      }

      if (inherits(run_result()$result, "ggplot") && # if canvasXpress, and it is
         !is_interactive_plot() && # not already an interactive plot, show
         # if there are too many data points, don't do the interactive
         !(dim(df)[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
      ) {
        shinyjs::showElement(id = "make_cx_interactive")
      }
    })

    # First Dataset Table
    output$data_table_DT <- DT::renderDataTable({
      req(current_data())
      DT::datatable(
        current_data(),
        options = list(
          lengthMenu = c(5, 20, 50, 100),
          pageLength = 10,
          dom = "ftp",
          scrollX = "400px"
        ),
        rownames = FALSE
      )
    })

    output$data_size <- renderUI({
      req(!is.null(current_data()))
      tagList(
        h4("Selected Dataset"),
        paste(
          dim(current_data())[1], "rows X",
          dim(current_data())[2], "columns"
        )
      )
    })

    # Second Dataset Table
    output$data_table_DT_2 <- DT::renderDataTable({
      req(current_data_2())
      DT::datatable(
        current_data_2(),
        options = list(
          lengthMenu = c(5, 20, 50, 100),
          pageLength = 10,
          dom = "ftp",
          scrollX = "400px"
        ),
        rownames = FALSE
      )
    })

    output$data_size_2 <- renderUI({
      req(!is.null(current_data_2()))
      tagList(
        h4("2nd Dataset (Must specify, e.g. 'create a piechart of X in df2.')"),
        paste(
          dim(current_data_2())[1], "rows X",
          dim(current_data_2())[2], "columns"
        )
      )
    })


    observeEvent(input$delete_chunk, {

      req(input$selected_chunk)
      shinyalert::shinyalert(
        title = paste0("Delete Code Chunk ", input$selected_chunk, "?"),
        text = NULL,
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes",
        cancelButtonText = "No",
        callbackR = function(isConfirmed) {
          if (isConfirmed) {
            # What current chunk is selected??
            id_pre <- as.integer(input$selected_chunk)
            logs$code_history[[id_pre]] <- NULL # R Automatically shifts list down

            max_id <- length(logs$code_history)

            if (max_id > 0){ # Order Operation MATTERS!!!!
              # Order Operation 1 (Reorder Code History ID's & rmd chunk numbering)
              logs$code_history <- lapply(1:max_id, function(i) {
                logs$code_history[[i]]$id = i
                substr(logs$code_history[[i]]$rmd,6,6) = as.character(i)
                logs$code_history[[i]]
              })

              # Order Operation 2 (Update current code info)
              logs$id <- logs$code_history[[max_id]]$id
              logs$code <- logs$code_history[[max_id]]$code
              logs$raw <- logs$code_history[[max_id]]$raw
              logs$last_code <- logs$code_history[[max_id]]$last_code
              logs$language <- logs$code_history[[max_id]]$language


              choices <- 1:length(logs$code_history)
              names(choices) <- paste0("Chunk #", choices)
              chunk_selection$chunk_choices <- choices

              # Update chunk choices
              updateSelectInput(
                session = session,
                inputId = "selected_chunk",
                label = div("AI Generated Code:", style = "font-size: 18px;"),
                choices = choices,
                selected = logs$id
              )

            } else {
              logs$id <- 0
              logs$code = ""
              logs$raw = ""
              logs$last_code = ""
              logs$language = ""
              logs$code_history <- list()

              # update chunk choices
              updateSelectInput(
                session = session,
                inputId = "selected_chunk",
                label = div("AI Generated Code:", style = "font-size: 18px;"),
                choices = "",
                selected = NULL
              )
            }
          }
        }
      )
    })

  })
}
