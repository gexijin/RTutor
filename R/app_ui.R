###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
# Updated: Daniel Burkhalter    dburkhalter1500@gmail.com
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

    # Add color to UI
    tags$head(tags$style(HTML("
      /* navbar */
      .navbar {background-color: #C1E2BE;border-color: #90BD8C; color: #181818;font-weight: bold;}

      /* tabs */
      .navbar-default .navbar-nav > li > a {background-color: #C1E2BE;border-color: #9AC596;color: #181818;}

      /* active tab */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {background-color: #A0BB9E;color: #181818;font-weight: bold;}

      /* sidebar panel */
      .well {background-color: #C1E2BE;border-color: #90BD8C;}

      /* select input & action button */
      .custom-select-input, .custom-action-button
      {background-color: #F6FFF5;border-color: #90BD8C;color: #000;}

      /* horizontal line */
      .custom-hr{border-top: 1px solid #90BD8C;}
      .custom-hr-thick{border-top: 3px solid #90BD8C;}
    "))),

    navbarPage(
      HTML('<span style="color: black;">RTutor</span>'),
      id = "tabs",
      tabPanel(
        title = "Home",
        div(
          id = "load_message",
          h2("Chat with your data via AI ..."),
        ),
        # move notifications and progress bar to the center of screen
        tags$head(
          tags$style(
            HTML(".shiny-notification {
                  width: 300px;
                  position:fixed;
                  top: calc(90%);
                  left: calc(10%);
                  }
                  "
            )
          )
        ),
        # Embed the CSS directly in the UI
        tags$style("
          .modal-dialog {
            position: absolute;
            bottom: 0;
          }
        "),
        # Sidebar with a slider input for number of bins
        sidebarLayout(
          sidebarPanel(

            fluidRow(
              column(
                width = 6,
                textOutput("selected_dataset")
              )
            ),
            fluidRow(
              column(
                width = 6,
                uiOutput("demo_data_ui")
              ),
              column(
                width = 6,
                uiOutput("data_upload_ui"),
                uiOutput("data_upload_ui_2")
              )
            ),
            hr(class = "custom-hr"),

            fluidRow(
              column(
                width = 12,
                tags$label("2. Modify Data Fields",
                style = "font-size: 14px;font-weight: bold;color: #333;display: block;margin-bottom: 5px;")
              ),
              br(),
              column(
                width = 12,
                actionButton("data_edit_modal", "Data Types"),
                align = "left",
                tags$head(tags$style(
                  "#data_edit_modal{font-size: 14px;color: #000;background-color: #F6FFF5;border-color: #90BD8C;}"
                ))
              )
            ),

            hr(class = "custom-hr"),

            tags$label("3. Send Request(s)",
                style = "font-size: 14px; font-weight: bold; color: #333; display: block; margin-bottom: 5px;"),

            tags$style(HTML("
              textarea {
                width: 100%;
                background-color: #F6FFF5;
                border-color: #90BD8C;
              }
            ")),
            tags$textarea(
              id = "input_text",
              placeholder = NULL,
              rows = 8, ""
            ),

            # Example Prompts
            uiOutput("prompt_ui"),

            hr(class = "custom-hr"),

            fluidRow(
              column(
                width = 12,
                div(
                  style = "display: flex; justify-content: space-between;",
                  div(
                    actionButton("submit_button", strong("Submit")),
                    tags$head(tags$style(
                      "#submit_button{font-size: 16px;color: blue;background-color: #F6FFF5;border-color: #90BD8C;}"
                    )),
                    tippy::tippy_this(
                      "submit_button",
                      "ChatGPT can return different results for the same request.",
                      theme = "light-border"
                    )
                  ),
                  div(
                    # Reset Button
                    actionButton(inputId = "reset_button", label = strong("Reset")),
                    tags$head(tags$style(
                      "#reset_button{font-size: 16px;color: red;background-color: #F6FFF5;border-color: #90BD8C;}"
                    )),
                    tippy::tippy_this(
                      "reset_button",
                      "Reset before asking a new question. Clears data objects, chat history, & code chunks.",
                      theme = "light-border"
                    )
                  )
                )

                )
            #   column(
            #     width = 4,
            #     actionButton("api_button", "Settings"),
            #     tags$head(tags$style(
            #           "#api_button{color: black;background-color: #F6FFF5;border-color: #90BD8C;}"
            #     )),
            #   ),
            #   column(
            #     width = 4,
            #     checkboxInput("use_python", "Python", value = FALSE)
            # )


            ),
            fluidRow(
              column(12,
                # Horizontal Line
                hr(class = "custom-hr")
              )
            ),
            conditionalPanel(
              condition = "input.submit_button >= 1",
              fluidRow(
                column(12,
                  tags$head(
                    tags$style(HTML("
                      #ask_question {
                        width: 100%;
                        background-color: #F6FFF5;
                        border-color: #90BD8C;
                      }
                    "))
                  ),
                  textInput(
                    inputId = "ask_question",
                    label = "Ask about Results",
                    placeholder = "Q&A: Ask about the code, result, error, or statistics in general.",
                    value = ""
                  ),

                  tippy::tippy_this(
                    "ask_question",
                    "Walk me through this code. What does this result mean?
                    What is this error about? Explain logistic regression.
                    List R packages for time series analysis.
                    Hit Enter to send the request.",
                    theme = "light-border"
                  ),
                  shinyjs::hidden(actionButton("ask_button", strong("Ask RTutor"))),
                  hr(class = "custom-hr")
                  )
              )
            ),

            # hr(class = "custom-hr"),
            # textOutput("usage"),
            # textOutput("total_cost"),
            # textOutput("temperature"),
            textOutput("retry_on_error")
          ),

      ###############################################################################
      # Main
      ###############################################################################

          mainPanel(
            shinyjs::useShinyjs(),

            conditionalPanel(
              condition = "output.file_uploaded == 0 && input.submit_button == 0",

              # uiOutput("RTutor_version_main"),
              fluidRow(
                column(
                  width = 4,
                  p(
                    HTML("First Time Users:"), #<span style='font-size: 54px;'>&rarr;</span>
                    style = "font-size: 34px; margin: 0; padding-top: 10px;"
                  ),
                  align = "right"
                ),
                column(
                  width = 5,
                  actionButton("first_user", strong("Start Here!")),
                  tags$head(tags$style(
                    "#first_user{font-size: 36px; color: Black; background-color: #90BD8C}"
                  )),
                  align = "left"
                ),
                column(
                  width = 3,
                  img(
                    src = "www/logo.png",
                    width = "155",
                    height = "77"
                  ),
                  align = "center"
                )
              )
            ),
            hr(class = "custom-hr-thick"),
            conditionalPanel(
              condition = "input.submit_button != 0",
              fluidRow(
                column(
                  width = 4,
                  tagList(
                    selectInput(
                      inputId = "selected_chunk",
                      label = "AI generated code:",
                      selected = NULL,
                      choices = NULL
                    ),
                    tags$style(
                      HTML("#selected_chunk+div .selectize-input {
                            background-color: #F6FFF5 !important;
                            border-color: #90BD8C !important;
                            color: #000 !important;
                            }
                            #selected_chunk+div .selectize-dropdown {
                            background-color: #F6FFF5 !important;
                            border-color: #90BD8C !important;
                            color: #000 !important;
                            }"
                      )
                    )
                  ),
                  tippy::tippy_this(
                    "selected_chunk",
                    "You can go back to any previous code chunk and continue from there. The data will also be reverted to that point.",
                    theme = "light-border"
                  )
                )
              ),
              verbatimTextOutput("openAI"),
              conditionalPanel(
                condition = "input.use_python == 0",

                uiOutput("error_message"),
                strong("Results:"),

                # shows error message in local machine, but not on the server
                verbatimTextOutput("console_output"),
                uiOutput("plot_ui"),
                fluidRow(
                  column(
                    width = 5,
                    checkboxInput(
                      inputId = "make_ggplot_interactive",
                      label = NULL,
                      value = FALSE
                    ),
                    align = "right"
                  ),
                  column(
                    width = 5,
                    checkboxInput(
                      inputId = "make_cx_interactive",
                      label = NULL,
                      value = FALSE
                    ),
                    align = "left"
                  )
                ),
                br(),
                uiOutput("tips_interactive"),
              ),
              conditionalPanel(
                condition = "input.use_python == 1",
                uiOutput("python_markdown")
              )
            ),

            # br(),

            shinyjs::hidden(
              div(
                id = "first_file",
                # hr(class = "custom-hr"),
                h4("Selected Dataset"),
                textOutput("data_size"),
                tags$head(
                  tags$style(HTML("
                    .dataTables_wrapper {
                      background-color: #f8fcf8;
                      border-color: #90BD8C;
                      padding: 10px;
                      border-radius: 5px;
                    }
                    .dataTables_wrapper table.dataTable tbody tr:nth-child(odd) {
                      background-color: #f3faf3;
                    }
                    .dataTables_wrapper table.dataTable tbody tr:nth-child(even) {
                      background-color: #ffffff;
                    }
                  "))
                ),
                DT::dataTableOutput("data_table_DT")
              )
            ),
            shinyjs::hidden(
              div(
                id = "second_file",
                hr(class = "custom-hr"),
                h4("2nd dataset: df2     (Must specify, e.g. 'create a piechart of X in df2.')"),
                textOutput("data_size_2"),
                DT::dataTableOutput("data_table_DT_2")

              )
            )

          ) #mainPanel
        ) #sideBarpanel
      ), #tabPanel

      tabPanel(
        title = div(id = "eda_tab", "EDA"),
        value = "EDA",
        tippy::tippy_this(
          "eda_tab",
          "Exploratory Data Analysis",
          theme = "light-border"
        ),
        tabsetPanel(
          tags$head(
            tags$style(HTML("
              .nav-tabs {background-color: #D9EDD8;                  /* Background color - tab headers */}
              .nav-tabs > li > a {color: #5b5b5b;                           /* Text color - tab headers */
                border: 1px solid #D9EDD8;border-radius: 4px;}
              .nav-tabs > li > a:hover {background-color: #90BD8C;         /* Hover color - tab headers */
                color: #000;}
              .nav-tabs > li.active > a {background-color: #f3faf3;    /* Background color - active tab */
                color: #000;border: 1px solid #D9EDD8;border-bottom-color: transparent;}
            "))
          ),

          tabPanel(
            title = "Basic",
            hr(class = "custom-hr-thick"),
            h4(strong("Data Structure: df")),
            verbatimTextOutput("data_structure"),
            hr(class = "custom-hr-thick"),
            h4(strong("Data Summary: df")),
            verbatimTextOutput("data_summary"),
            plotly::plotlyOutput("missing_values", width = "60%"),
            shinyjs::hidden(
              div(
                id = "second_file_summary",
                br(), hr(class = "custom-hr"),
                h4("Data structure: df2"),
                verbatimTextOutput("data_structure_2"),
                br(), hr(class = "custom-hr"),
                h4("Data summary: df2"),
                verbatimTextOutput("data_summary_2"),
                plotly::plotlyOutput("missing_values_2", width = "60%")
              )
            )
          ),
          tabPanel(
            title = "Summary",
            hr(class = "custom-hr-thick"),
            verbatimTextOutput("dfSummary"),
            h4(
              "Generated by the ",
              a(
                "summarytools",
                href="https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html",
                target = "_blank"
              ),
              "package using the command:summarytools::dfSummary(df)."
            )
          ),
          tabPanel(
            title = "Table1",
            hr(class = "custom-hr-thick"),
            uiOutput("table1_inputs"),
            verbatimTextOutput("table1"),
            h4(
              "Generated by the CreateTableOne() function in the",
              a("tableone",
                href="https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html",
                target = "_blank"),
              "package."
            )
          ),

          tabPanel(
            title = "Categorical",
            hr(class = "custom-hr-thick"),
            div(
              style = "position: relative; padding-bottom: 10px;",
              plotOutput("distribution_category", width = "1300px", height = "1000px")
            ),
            h4(
              "Generated by the plot_bar() function in the",
              a("DataExplorer",
                href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                target = "_blank"),
              "package."
            )
          ),
          tabPanel(
            title = "Numerical",
            hr(class = "custom-hr-thick"),
            plotOutput("qq_numeric"),
            plotOutput("distribution_numeric"),
            h4(
              "Generated by the plot_qq() and plot_histogram() functions in the",
              a(
                "DataExplorer",
                href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                target = "_blank"
              ),
              "package."
            )
          ),
          tabPanel(
            title = "Correlation",
            hr(class = "custom-hr-thick"),
            plotOutput("corr_map"),
            h4(
              "Generated by the corr_plot() functions in the",
              a(
                "corrplot",
                href="https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html",
                target = "_blank"
              ),
              "package. Blanks indicate no significant correlations."
            )
          ),
          tabPanel(
            title = "GGpairs",
            hr(class = "custom-hr-thick"),
            uiOutput("ggpairs_inputs"),
            div(
              style = "position: relative; padding-bottom: 10px;",
              plotOutput("ggpairs", width = "1300px", height = "1300px")
            ),
            h4(
              "Generated by the ggpairs() functions in the",
              a(
                "GGally",
                href="https://cran.r-project.org/web/packages/GGally/index.html",
                target = "_blank"
              ),
              "package."
            )
          ),
          tabPanel(
            title = "EDA Reports",
            hr(class = "custom-hr-thick"),
            h4(strong("Comprehensive EDA (Exploratory Data Analysis)")),
            uiOutput("eda_report_ui")
          )
        )
      ),

      tabPanel(
        title = div(id = "report_tab", "Report"),
        value = "Report",
        tippy::tippy_this(
          "report_tab",
          "Download a Results Report",
          theme = "light-border"
        ),
        br(),
        fluidRow(
          column(
            width = 5,
            tagList(
              selectInput(
                inputId = "selected_chunk_report",
                label = "Code chunks to include:",
                selected = NULL,
                choices = NULL,
                multiple = TRUE
              ),
              tags$style(
                HTML("#selected_chunk_report+div .selectize-input {
                      background-color: #F6FFF5 !important;
                      border-color: #90BD8C !important;
                      color: #000 !important;
                      }
                      #selected_chunk_report+div .selectize-dropdown {
                      background-color: #F6FFF5 !important;
                      border-color: #90BD8C !important;
                      color: #000 !important;
                      }"
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            uiOutput("html_report"),
          ),
          column(
            width = 8,
            downloadButton(
              outputId = "Rmd_source",
              label = "RMarkdown"
            ),
            tags$head(tags$style(
              "#Rmd_source{font-size: 16px;color: #000;background-color: #C1E2BE;border-color: #90BD8C;}"
            )),
            tippy::tippy_this(
              "Rmd_source",
              "Download a R Markdown source file.",
              theme = "light-border"
            )
          )
        ),
        br(),
        verbatimTextOutput("rmd_chunk_output")
      ),

      navbarMenu(
        title = "More",
        tabPanel(
          title = "First Time User",
          value = "first-time-user",
          # uiOutput("RTutor_version_main"),
          fluidRow(
            column(
              width = 9,
              h4(
                "Start by watching an 8-min ",
                a(
                  "YouTube video!",
                  href="https://youtu.be/a-bZW26nK9k",
                  target = "_blank"
                ),
                style="color:red"
              ),
              # h5("5/14/2024: GPT-4o becomes default.  Nov. 1, 2023: (v0.98.2): Generate ",
              #   a(
              #     "a comprehensive EDA report.",
              #     href="https://htmlpreview.github.io/?https://github.com/gexijin/gEDA/blob/main/example_report.html",
              #     target = "_blank"
              #   ),
              #  " Oct 28, 2023 (v0.98):  Ask questions about the code, result, error, or statistics! Upload a second file.
              # Oct 23, 2023 (v0.97): GPT-4 becomes the default.
              # Using ggplot2 is now preferred. Consectitive data manipulation is enabled."),
              h5("See",
                a(
                  "GitHub",
                  href = "https://github.com/gexijin/RTutor"
                ),
                " for source code, bug reports, and instructions to install RTutor as an R package.
                As a small startup, we are open to partnerships with both academia and industry.
              We can do demos and seminars via Zoom if time permits."
              ),
              h5("Also try ",
                a(
                  "Chatlize.ai,",
                  href="https://chatlize.ai",
                  target = "_blank"
                ),
                " a more general platform for analyzing data through chats. Multiple files with different formats. Python support."
              ),

              align = "left"
            ),
            column(
              width = 3,
              img(
                src = "www/logo.png",
                width = "155",
                height = "77"
              ),
              align = 'left'
            )
          ),

          hr(class = "custom-hr"),

          fluidRow(
            column(
              width = 12,
              h3("Quick start:"),
              tags$ul(
                tags$li(
                  "Explore the data at the EDA tab first.  Then start with simple requests
                  such as distributions, basic plots. Gradually add complexity.
                  ", style = "color:red"
                ),
                tags$li(
                  "The default model is now GPT-4 Turbo, which is slower and expensive, but more accurate.
                  In the same session, previous questions and code chunks become the context for your new request.
                  For example, you can simply say \"Change background color to white\" to refine the
                  plot generated by the previous chunk. You can also clean your data step by step. "
                ),
                tags$li(
                  "To analyze a new dataset, or to start over, click the Reset button first. "
                ),
                tags$li(
                  "Prepare and clean your data in Excel first. Name columns properly.
                  ChatGPT tries to guess the meaning of column names, even if they are abbrievated."
                ),
                tags$li(
                  "RTutor can only analyze traditional statistics data, where rows are
                  observations and columns are variables. For complex data, try https://chatlize.ai."
                ),
                tags$li(
                  "Once uploaded, your data is automatically loaded into
                  R as a data frame called df. You do NOT need to ask RTutor to load data.
                  Check if the data types of the columns are correct.
                  Change if needed, especially when numbers are used to code for categories."
                ),
                tags$li(
                  "An additional file can be uploaded as df2 to be analyze togehter.
                  To use it, you must specify 'df2' in your prompts. "
                ),
                tags$li(
                  "Use the Q&A box to ask questions about the code, result, or error messages.
                  You can ask for methods to use or develop a plan. "
                ),
                tags$li(
                  "Before sending your request to OpenAI, we do prompt engineering based on the uploaded data.
                  We add \"Generate R code\" to the beginning, and
                  append something like \"Use the df data frame.
                  Note that highway is numeric, ...\" afterward.
                  If you are not using any data (plot a function or simulations),
                  choose \"No data\" from the Data dropdown."
                ),
                tags$li(
                  "Your data is not sent to OpenAI. Nor is it stored in our webserver after the session.
                  If you explain the background of the data and the meaning of
                  the columns, you can ask general questions like asking a clueless statistician."
                ),
                tags$li(
                  "Be skeptical. The generated code can be logically wrong even if it produces results without error."
                )
              )
            )
          )
        ),
        tabPanel(
          title = "About",
          value = "About",
          uiOutput("RTutor_version"),
          p("RTutor uses ",
            a(
              "OpenAI's",
              href = "https://openai.com/",
              target = "_blank"
            ),
            " powerful large language models",
            " to translate natural language into R (or Python) code, which is then excuted.",
            "You can request your analysis,
            just like asking a real person.",
            "Upload a data file (CSV, TSV/tab-delimited text files, and Excel)
            and just analyze it in plain English.
            Your results can be downloaded as an HTML report in minutes!"
          ),
          p("NO WARRANTY! Some of the scripts run but may yield incorrect result.
          Please use the auto-generated code as a starting
          point for further refinement and validation."
          ),

          hr(class = "custom-hr"),
          p(" Written by Dr. Steven Ge",
            a(
              "(Twitter, ",
              href = "https://twitter.com/StevenXGe",
              target = "_blank"
            ),
            a(
              "LinkedIn),",
              href = "https://www.linkedin.com/in/steven-ge-ab016947/",
              target = "_blank"
            ),
            " as part of RTutor LLC. For feedback, please email",
            a(
              "gexijin@gmail.com.",
              href = "mailto:gexijin@gmail.com?Subject=RTutor"
            ),
            " Source code at ",
            a(
              "GitHub,",
              href = "https://github.com/gexijin/RTutor"
            ),
            " from where you can also find
            instruction to install RTutor as an R package.
            The RTutor website and the source code is free for non-profit organizations ONLY. Licensing is required for commercial use."
          ),
          h4("For businesses, RTutor can be customized and locally installed to
          easily gain insights from your data (files, SQL databases, or APIs) at a low cost. We will be happy to discuss."),

          hr(class = "custom-hr"),
          p("RTutor went viral on ",
              a(
                "LinkedIn, ",
                href = "https://www.linkedin.com/feed/update/urn:li:activity:7008179918844956672/"
              ),
              a(
                "Twitter, ",
                href = "https://twitter.com/StevenXGe/status/1604861481526386690"
              ),
              a(
                "Twitter(Physacourses),",
                href = "https://twitter.com/Physacourses/status/1602730176688832513?s=20&t=z4fA3IPNuXylm3Vj8NJM1A"
              ),
              " and ",
              a(
                "Facebook (Carlo Pecoraro).",
                href = "https://www.facebook.com/physalia.courses.7/posts/1510757046071330"
              )
          ),

          # hr(class = "custom-hr"),

          # uiOutput("package_list"),

          hr(class = "custom-hr"),

          fluidRow(
            column(
              width = 4,
              uiOutput("package_list")
            ),
            # Site Update Log component
            column(
              width = 8,
              checkboxInput("site_update_log", strong("See Site Updates Log & R Session Info"), FALSE),
                tags$head(tags$style(
                  "#site_update_log{font-size: 16px;color: black}"
              ))
            )
          ),

          hr(class = "custom-hr"),

          # Session Info Section
          fluidRow(
            conditionalPanel(
              condition = "input.site_update_log == 1",
              column(
                width = 6,
                div(
                  tagList(
                    tags$head(
                      tags$style(HTML(".site-updates-wrapper table {background-color: #f3faf3;border-top: 2px solid #90BD8C;}
                        .site-updates-wrapper table thead th, .site-updates-wrapper table td {border: 2px solid #90BD8C;}
                        .site-updates-wrapper table tbody tr:nth-child(odd) {background-color: #f3faf3;}
                        .site-updates-wrapper table tbody tr:nth-child(even) {background-color: #ffffff;}
                      "))
                    ),
                    div(
                      class = "site-updates-wrapper",
                      tableOutput("site_updates_table")
                    )
                  )
                )
              ),
              column(
                width = 6,
                uiOutput("session_info")
              )
            )
          ),
          hr(class = "custom-hr")
        ),
        tabPanel(
          title = "FAQ",
          fluidRow(
            column(
              width = 6,
              h4(style = "font-weight: bold", "Frequently asked Questions"),
              uiOutput("faq_list"),
              tags$style(HTML("
                .faq-answer {
                  display: none;
                  padding-left: 10px;
                }
                .faq-question {
                  cursor: pointer;
                  padding: 5px;
                  border: 1px solid #90BD8C;
                  background-color: #F6FFF5;
                }
              ")),
              tags$script(HTML('
                $(document).on("click", ".faq-question", function() {
                  var answer = $(this).next(".faq-answer");
                  if (answer.is(":visible")) {
                    answer.hide();
                  } else {
                    answer.show();
                  }
                });
              '))
            ),
            column(
              width = 6,
              h4(style = "font-weight: bold", "Comments & Questions"),
              tags$style(type = "text/css", "textarea {width:100%}"),
              tags$textarea(
                id = "user_feedback",
                placeholder = "Any questions? Suggestions? Things you like, don't like? Leave your email if you want to hear back from us.",
                rows = 4,
                ""
              ),
              radioButtons("helpfulness", "How useful is RTutor?",
                c(
                  "Not at all",
                  "Slightly",
                  "Helpful",
                  "Extremely"
                ),
                selected = "Slightly"
              ),
              radioButtons("experience", "Your experience with R:",
                c(
                  "None",
                  "Beginner",
                  "Intermediate",
                  "Advanced"
                ),
                selected = "Beginner"
              ),
              actionButton("save_feedbck", "Save Feedback"),
              tags$head(tags$style(
                "#save_feedbck{font-size: 16px;color: #000;background-color: #C1E2BE;border-color: #90BD8C;}"
              )),
            )
          )
        ),
        tabPanel(
          title = "Settings",
          div(id = "settings_window",
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
                  uiOutput("language_model")
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  uiOutput("change_temperature")
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
              hr(class = "custom-hr"),
              fluidRow(
                column(
                  width = 12,
                  h4("Use your own API key"),
                  h5("We pay a small fee to use the AI for every request.
                    If you use this regularly,
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
                        which can be copied and pasted below."
                      ),
                  ),
                  uiOutput("valid_key"),
                  uiOutput("save_api_ui")
                )
              ),
            ),
            fluidRow(
              column(
                width = 4,
                textInput(
                  inputId = "api_key",
                  label = h5("Paste your API key from OpenAI:"),
                  value = NULL,
                  placeholder = "sk-..... (51 characters)"
                )
              ),
              column(
                  width = 8,
                  h5("Current API Key:"),
                  verbatimTextOutput("session_api_source")
              )
            ),

            hr(class = "custom-hr"),
            fluidRow(
              column(
                width = 4,
                checkboxInput(
                  inputId = "use_python",
                  label = "Python",
                  value = FALSE)
              ),
              column(
                width = 8,
                h5("Use Python instead of R for generating code and results.")
              )
            ),

            hr(class = "custom-hr"),
            fluidRow(
              column(
                width = 4,
                uiOutput("numeric_as_factor"),
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
                uiOutput("max_levels_factor"),
                tippy::tippy_this(
                  elementId = "max_levels_factor",
                  tooltip = "To convert a numeric column as category,
                  the column must have no more than this number of unique values.",
                  theme = "light-border"
                )
              ),
              column(
                width = 4,
                uiOutput("max_proptortion_factor"),
                tippy::tippy_this(
                  elementId = "max_proptortion_factor",
                  tooltip = "To convert a numeric column as category,
                  the number of unique values in a column must not exceed
                  more this proportion of the total number of rows.",
                  theme = "light-border"
                )
              )
            ),
            h5("Some columns contain numbers, but should be treated
            as categorical values or factors. For example, we sometimes
            use 1 to label success and 0 for failure.
            If this is selected, using the default setting, a column
            is treated as categories when the number of unique values
            is less than or equal to 12, and less than 10% of the total rows."
            ),
            hr(class = "custom-hr"),
            fluidRow(
              column(
                width = 4,
                uiOutput("contribute_data")
              ),
              column(
                width = 8,
                h5("Save your requests and the structure of your data
                such as column names and data types, not the data itself.
                We can learn from users about creative ways to use AI.
                And we can try to improve unsuccessful attempts. ")
              )
            )
          )
        )
      ),

    tags$script(HTML("
      $(document).on('click', '#first_user', function() {
        // Update the active tab to 'First Time User' within the 'More' navbarMenu
        $('#tabs a[data-value=\"first-time-user\"]').tab('show');
      });
    "))
    ),

    tags$head(includeHTML(app_sys("app", "www", "ga.html")))
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
      app_title = "RTutor 0.98"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
