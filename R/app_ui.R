###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    navbarPage(
      "RTutor",
    #  windowTitle = "RTutor",
      id = "tabs",
      tabPanel(
        title = "Home",
        div(
          id = "load_message",
          h2("Chat with your data via AI"),
        ),
        uiOutput("use_heyshiny"),
        # move notifications and progress bar to the center of screen
        tags$head(
          tags$style(
            HTML(".shiny-notification {
                  width: 200px;
                  position:fixed;
                  top: calc(80%);
                  left: calc(10%);
                  }
                  "
                )
            )
        ),

        # Sidebar with a slider input for number of bins
        sidebarLayout(
          sidebarPanel(
#            uiOutput("timer_ui"),
            fluidRow(
              column(
                width = 6,
                textOutput("selected_dataset")
              ),
              column(
                width = 6,
p(HTML("<div align=\"right\"> <A HREF=\"javascript:history.go(0)\">Reset</A></div>"))
              )
            ),
            fluidRow(
              column(
                width = 6,
                uiOutput("demo_data_ui")
              ),
              column(
                width = 6,
                uiOutput("data_upload_ui")
              )
            ),

            uiOutput("prompt_ui"),

            tags$style(type = "text/css", "textarea {width:100%}"),
            tags$textarea(
              id = "input_text",
              placeholder = NULL,
              rows = 8, ""
            ),

            fluidRow(
              column(
                width = 4,
                actionButton("submit_button", strong("Submit")),
                tags$head(tags$style(
                  "#submit_button{font-size: 16px;color: red}"
                )),
                tippy::tippy_this(
                  "submit_button",
                  "ChatGPT can return different results for the same request.",
                  theme = "light-border"
                )
              ),
              column(
                width = 4,
                actionButton("api_button", "Settings")
              ),
              column(
                width = 4,
                checkboxInput("use_python", "Python", value = FALSE)
              )
            ),
            br(),
            #textOutput("usage"),
            #textOutput("total_cost"),
            textOutput("temperature"),
            #uiOutput("slava_ukraini"),
            br(),
            textOutput("retry_on_error"),
            fluidRow(
              column(
                width = 4,
                actionButton("save_feedbck", "Save code"),
              ),
              column(
                width = 8,
                tags$style(type = "text/css", "textarea {width:100%}"),
                tags$textarea(
                  id = "user_feedback",
                  placeholder = "Short description",
                  rows = 1,
                  ""
                )
              )
            ),
            br(),
            selectInput(
              inputId = "selected_chunk_saved",
              label = "Retrieve Saved Code:",
              selected = NULL,
              choices = NULL
            ),
          ),

      ###############################################################################
      # Main
      ###############################################################################

          mainPanel(
            shinyjs::useShinyjs(),
            conditionalPanel(
              condition = "input.submit_button == 0",
              uiOutput("RTutor_version_main"),
              h3(
                "Ask questions regarding the demography, events and medications data."
              )
            ),
            conditionalPanel(
              condition = "input.submit_button != 0",
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "selected_chunk",
                label = "AI generated code:",
                selected = NULL,
                choices = NULL
              )
            ),
            column(
              width = 4,
              style = "margin-top: 10px;",
              checkboxInput(
                inputId = "continue",
                label = "Continue from this chunk",
                value = FALSE
              ),
              tippy::tippy_this(
                "continue",
                "If selected, the current R scripts will be kept in the next questions. We build upon the code chunk.",
                theme = "light-border"
              )
            )
          ),
              conditionalPanel(
                condition = "input.use_python == 0",

                uiOutput("error_message"),
                h4("Results:"),

                # shows error message in local machine, but not on the server
                verbatimTextOutput("console_output"),
                uiOutput("plot_ui"),
                checkboxInput(
                  inputId = "make_ggplot_interactive",
                  label = NULL,
                  value = FALSE
                ),
                br(),
                uiOutput("tips_interactive"), 
              ),
            ),
            #verbatimTextOutput("data_structure"),
            tableOutput("data_table")


          ) #mainPanel
        ) #sideBarpanel
      ), #tabPanel
      tabPanel(
        title = "Code",
        value = "Code",
        conditionalPanel(
          condition = "input.submit_button != 0",
          verbatimTextOutput("openAI")
        )

      ),
      tabPanel(
        title = "Data",
        value = "Data",
        textOutput("data_size"),
        DT::dataTableOutput("data_table_DT")
      ),

    ),

    tags$head(includeHTML(app_sys("app", "www", "ga.html")))
    ,tags$head(includeScript(app_sys("app", "www", "ga.js")))
    # old covid tracker called "virus"

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(
      ico = "icon",
      rel = "shortcut icon",
      resources_path = "www",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RTutor"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
