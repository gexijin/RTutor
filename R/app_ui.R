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
    # theme = bslib::bs_theme(bootswatch = "darkly"),
      id = "tabs",
      tabPanel(
        title = "Home",
        div(
          id = "load_message",
          h2("Chat with your data via AI ..."),
        ),
        #uiOutput("use_heyshiny"), # remove it
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
            #uiOutput("timer_ui"),

            fluidRow(
              column(
                width = 6,
                textOutput("selected_dataset")
              ),
              column(
                width = 6,
                actionButton("reset_button", strong("Reset")),
                tags$head(tags$style(
                  "#reset_button{font-size: 16px;color: blue}"
                )),
                align = "right",
                tippy::tippy_this(
                  "reset_button",
                  "Reset before uploading a new file. Clears data objects, chat history, and code chunks.",
                  theme = "light-border"
                )
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
                ,uiOutput("data_upload_ui_2")
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
            textInput(
              inputId = "ask_question",
              label = NULL,
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
            br(),
            fluidRow(
              column(
                width = 6,
                actionButton("data_edit_modal", "Data Types")
              ),
              column(
                width = 6,
                actionButton("data_desc_modal", "Description")
              )
            ),
            textOutput("usage"),
            textOutput("total_cost"),
            textOutput("temperature"),
            #uiOutput("slava_ukraini"),
            #br(),
            textOutput("retry_on_error"),
            checkboxInput("Comments", "Comments & questions"),
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
            actionButton("save_feedbck", "Save Feedback")
          ),

      ###############################################################################
      # Main
      ###############################################################################

          mainPanel(
            shinyjs::useShinyjs(),

            conditionalPanel(
              condition = "output.file_uploaded == 0 && input.submit_button == 0",

              uiOutput("RTutor_version_main"),
              fluidRow(
                column(
                  width = 3,
                  img(
                    src = "www/tutor.jpg",
                    width = "120",
                    height = "108"
                  ),
                  align = 'left'
                ),
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
                  h4("Nov. 9: Switching to the new GPT-4 Turbo model.  Nov. 1: (v0.98.2): RTutor can generate ",
                    a(
                      "a comprehensive EDA report.",
                      href="https://htmlpreview.github.io/?https://github.com/gexijin/gEDA/blob/main/example_report.html",
                      target = "_blank"
                    ),  
                   " Oct 28 (v0.98):  Ask questions about the code, result, error, or statistics! Upload a second file.
                  Oct 23 (v0.97): GPT-4 becomes the default.
                  Using ggplot2 is now preferred. Consectitive data manipulation is enabled."),
                  h4("See",
                    a(
                      "GitHub",
                      href = "https://github.com/gexijin/RTutor"
                    ),
                    " for source code, bug reports, and instructions to install RTutor as an R package.
                    As a small startup, we are open to partnerships with both academia and industry. 
                  We can do demos and seminars via Zoom if time permits."
                  ),
                  h4("Also try ",
                    a(
                      "Chatlize.ai,",
                      href="https://chatlize.ai",
                      target = "_blank"
                    ),
                    " a more general platform for analyzing data through chats. Multiple files with different formats. Python support."
                  ),

                  align = "left"
                )
              ),
              hr(),

              h3("Quick start:"),
              tags$ul(
                tags$li(
                  "Explore the data at the EDA tab first.  Then start analyzing the data using simple requests 
                  such as distributions, basic plots & simple models. Gradually add complexity.
                  ", style = "color:red"
                ),
                tags$li(
                  "The default model is now GPT-4 Turbo, which is slower and expensive, but more accurate.
                  In the same session, previous questions and code chunks become the context for your new request.
                  For example, you can simply say \"Change background color to white\" to refine the
                  plot generated by the previous chunk. You can also clean your data step by step. "
                ),
                tags$li(
                  "To analyze a new, unrelated dataset, or to start over, click the Reset button first. 
                  Always delete the photos of your ex-girlfriends before chasing new ones."
                ), 
                tags$li(
                  "Prepare and clean your data in Excel first! Name columns properly. 
                  ChatGPT tries to guess the meaning of column names, even abbrievated.
                  RTutor can only analyze traditional 
                  statistics data, where rows are 
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
                  You can ask for methods to use or develop a step by step plan. "
                ),
                tags$li(
                  "Before sending your request to OpenAI, we add \"Generate R code\" before it, and 
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
                #uiOutput("send_error_message"),
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

            br(),

            shinyjs::hidden(
              div(
                id = "first_file",
                hr(),
                h4("Default dataset:  df"),
                textOutput("data_size"),
                DT::dataTableOutput("data_table_DT")
              )
            ),
            shinyjs::hidden(
              div(
                id = "second_file",
                hr(),
                h4("2nd dataset: df2     (Must specify, e.g. 'create a piechart of X in df2.')"),
                textOutput("data_size_2"),
                DT::dataTableOutput("data_table_DT_2")

              )
            )
            #,tableOutput("data_table"),


          ) #mainPanel
        ) #sideBarpanel
      ), #tabPanel

      tabPanel(
        title = "EDA",
        value = "EDA",
        tabsetPanel(
          tabPanel(
            title = "Basic",
            h4("Data structure: df"),
            verbatimTextOutput("data_structure"),
            hr(),
            h4("Data summary: df"),
            verbatimTextOutput("data_summary"),
            plotly::plotlyOutput("missing_values", width = "60%"),
            shinyjs::hidden(
              div(
                id = "second_file_summary",
                br(),hr(),
                h4("Data structure: df2"),
                verbatimTextOutput("data_structure_2"),
                br(),hr(),
                h4("Data summary: df2"),
                verbatimTextOutput("data_summary_2"),
                plotly::plotlyOutput("missing_values_2", width = "60%")
              )
            )
          ),
          tabPanel(
            title = "Summary",
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
            uiOutput("table1_inputs"),
            verbatimTextOutput("table1"),
            h4(
              "Generated by the CreateTableOne() function in the",
              a(
                "tableone",
                href="https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html",
                target = "_blank"
              ),
              "package."
            )
          ),

          tabPanel(
            title = "Categorical",
            h4(
              "Generated by the plot_bar() function in the",
              a(
                "DataExplorer",
                href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                target = "_blank"
              ),
              "package."
            ),
            plotOutput("distribution_category")
          ),
          tabPanel(
            title = "Numerical",
            h4(
              "Generated by the plot_qq() and plot_histogram() functions in the",
              a(
                "DataExplorer",
                href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                target = "_blank"
              ),
              "package."
            ),
            plotOutput("qq_numeric"),
            plotOutput("distribution_numeric"),

          ),
          tabPanel(
            title = "Correlation",
            h4(
              "Generated by the corr_plot() functions in the",
              a(
                "corrplot",
                href="https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html",
                target = "_blank"
              ),
              "package. Blanks indicate no significant correlations."
            ),
            plotOutput("corr_map")
          ),
          tabPanel(
            title = "GGpairs",
            uiOutput("ggpairs_inputs"),
            h4(""),
            h4(
              "Please wait 1 minutes for this plot to be generated by the ggpairs() functions in the",
              a(
                "GGally",
                href="https://cran.r-project.org/web/packages/GGally/index.html",
                target = "_blank"
              ),
              "package."
            ),
            plotOutput("ggpairs")
          ),
          tabPanel(
            title = "EDA Reports",
            h4("Comprehensive EDA (exploratory data analysis)"),
            uiOutput("eda_report_ui")
          )
        )
      ),

      tabPanel(
        title = "Report",
        value = "Report",
        br(),
        selectInput(
          inputId = "selected_chunk_report",
          label = "Code chunks to include:",
          selected = NULL,
          choices = NULL,
          multiple = TRUE
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput("html_report")
          ),
          column(
            width = 6,
            downloadButton(
              outputId = "Rmd_source",
              label = "RMarkdown"
            ),
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
          " to translate natural language into R code, which is then excuted.",
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

        hr(),
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

        hr(),
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

        hr(),

        uiOutput("package_list"),

        hr(),

        h3("Frequently asked questions:"),

        h5("1.	What is RTutor?"),
        p("It is an artificial intelligence (AI)-based app that enables 
        users to interact with your data via natural language.
        After uploading a 
        dataset, users ask questions about or request analyses in 
        English. The app generates and runs R code to answer that question 
        with plots and numeric results."),

        h5("2.	How does RTutor work?"),
        p("The requests are structured and sent to OpenAI’s AI
        system, which returns R code. The R code is cleaned up and 
        executed in a Shiny 
        environment, showing results or error messages. Multiple 
        requests are logged to produce an R Markdown file, which can be
          knitted into an HTML report. This enables record keeping 
          and reproducibility."),

        h5("3. Is my data uploaded to OpenAI?"),
        p("No. The column names of your data, not the data itself, is sent to OpenAI as a prompt to generate R code. Your data is not stored in our server after the session."),

        h5("4.	Who is it for?"),
        p("The primary goal is to help people with some R experience to learn
        R or be more productive. RTutor can be used to quickly speed up the 
        coding process using R. It gives you a draft code to test and 
        refine. Be wary of bugs and errors. "),

        h5("5.	How do you make sure the results are correct? "),
        p("Try to word your question differently. And try 
        the same request several time. A higher temperature parameter will give 
        diverse choices. Then users can double-check to see 
        if you get the same results from different runs."),

        h5("6.	Can you use RTutor to do R coding homework?"),
        p("No. That will defy the purpose. You need to learn
        R coding properly to be able to tell if the generated 
        R coding is correct.  "),

        h5("7.	Can private companies use RTutor? "),
        p("No. It can be tried as a demo. RTutor website 
        dnd source code are freely available for non-profit organizations
        only and distributed using the CC NC 3.0 license."),

        h5("8.	Can you run RTutor locally?"),
        p("Yes. Download the R package and install it locally. 
        Then you need to obtain an API key from OpenAI."),

        h5("9.	Why do I get different results with the same request? "),
        p("OpenAI’s language model has a certain degree of randomness 
        that could be adjusted by parameters called \"temperature\". 
        Set this in  Settings"),

        h5("10.	Can people without R coding experience use RTutor for statistical analysis? "),
        p("Not entirely. This is because the generated code can be wrong.
        However, it could be used to quickly conduct data 
        visualization, and exploratory data analysis (EDA). 
        Just be mindful of this experimental technology. "),

        h5("11.	Can this replace statisticians or data scientists?"),
        p("No. But RTutor can make them more efficient."),

        h5("12.	How do I  write my request effectively?"),
        p("Imagine you have a summer intern, 
        a collge student 
        who took one semester of statistics and R. You send the 
        intern emails with instructions and he/she sends 
        back code and results. The intern is not experienced, 
        thus error-prone, but is hard working. Thanks to AI, this
        intern is lightning fast and nearly free."),

        h5("13. Can I install R package in the AI generated code?"),
        p("No. But we are working to pre-install all the R
          packages on the server! Right now we finished the top 5000 most 
          frequently used R packages. Chances are that your favorite package
          is already installed."),

        h5("14. Can I upload big files to the site?"),
        p("Not if it is more than 10MB. Try to get a small portion of your data. 
        Upload it to the site to get the code, which can be run locally on your 
        laptop. Alternatively, download RTutor R package, and use it from your
        own computer."),


        h5("15. Voice input does not work!"),
        p("One of the main reason
        is that your browser block the website site from accessing the microphone. 
        Make sure you access the site using",
        a(
          "https://RTutor.ai.",
          href = "https://RTutor.ai"
        ),
        "With http, mic access is automatically blocked in Chrome.
        Speak closer to the mic. Make sure there 
        is only one browser tab using the mic. "),

        h5("16. Is that your photo? "),
        p("No. I am an old guy. The photo was synthesized by AI. 
        Using prompts \'statistics tutor\', the image was generated by ",
        a(
          "Stable Diffusion 2.0.",
          href = "https://stability.ai/"
        ),
        "If you look carefully, you can see that her fingers are messed up."),


        hr(),
        h4("Update log:"),
        tags$ul(
           tags$li(
            "v0.98.3  11/1/2023. Fix issue with EDA report when the target variable is categorical or not specified."
          ),  
           tags$li(
            "v0.98.2  11/1/2023. Comprehensive EDA report!"
          ),           
          tags$li(
            "v 0.98  10/28/2023. Ask questions about code, error. Second data file upload."
          ),          
          tags$li(
            "v 0.97  10/23/2023. GPT-4 becomes the default. Make ggplot2 a preferred method for plotting.
             Use R environment to enable successive data manipulation."
          ),
          tags$li(
            "v 0.96  9/26/2023. Include column names in all requests. GPT-4 is available."
          ),
          tags$li(
            "v 0.95  6/11/2023. ChatGPT(gpt-3.5-turbo) becomes default model."
          ),
          tags$li(
            "v 0.94  4/21/2023. Interactive plots using CanvasXpress."
          ),
          tags$li(
            "v 0.93  3/26/2023. Change data types. Add data description. Improve voice input."
          ),
          tags$li(
            "v 0.92  3/8/2023. Includes description of data structure in prompt."
          ),
          tags$li(
            "v 0.91  2/6/2023. Voice input is improved. 
            Just enable micphone and say Tutor..."
          ),
          tags$li(
            "v 0.90  1/15/2023. Generates and runs Pyton code in addition to R!"
          ),
          tags$li(
            "v 0.8.6  1/8/2023. Add description of the levels in factors."
          ),
          tags$li(
            "v 0.8.5  1/6/2023. Demo in many foreign languages."
          ),
          tags$li(
            "v 0.8.4  1/5/2023. Collect  user feedback."
          ),
          tags$li(
            "v 0.8.3  1/5/2023. Collect some user data for improvement."
          ),
          tags$li(
            "v 0.8.2  1/4/2023. Auto convert first column as row names."
          ),
          tags$li(
            "v 0.8.1  1/3/2023. Option to convert some numeric columns with few unique levels to factors."
          ),
          tags$li(
            "v 0.8.0  1/3/2023. Add description of columns (numeric vs. categorical)."
          ),
          tags$li(
            "v 0.7.6 12/31/2022. Add RNA-seq data and example requests."
          ),
          tags$li(
            "v 0.7.5 12/31/2022. Redesigned UI."
          ),
          tags$li(
            "v 0.7 12/27/2022. Add EDA tab."
          ),
          tags$li(
            "v 0.6 12/27/2022. Keeps record of all code chunks for resue and report."
          ),
          tags$li(
            "v 0.5 12/24/2022. Keep current code and continue."
          ),
          tags$li(
            "v 0.4 12/23/2022. Interactive plot. Voice input optional."
          ),
          tags$li(
            "v0.3 12/20/2022. Add voice recognition."
          ),
          tags$li(
            "V0.2 12/16/2022. Add temperature control. Server reboot reminder."
          ),
          tags$li(
            "V0.1 12/11/2022. Initial launch"
          )
        ),
        hr(),
        uiOutput("session_info")
      ),

#      tabPanel(
#        title = "Disqus",
#        value = "Disqus",
#        div(
#        tags$head(includeHTML(app_sys("app", "www", "disqus.html")))
#        )
#      )
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
