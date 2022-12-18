#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
fluidPage(
  titlePanel("RTutor - Chat with your data via AI"),
  windowTitle = "RTutor",
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        # Application title

      p(HTML("<div align=\"right\"> <A HREF=\"javascript:history.go(0)\">Reset</A></div>")),
      fluidRow(
        column(
          width = 4,
          uiOutput("demo_data_ui")
        ),
        column(
          width = 8,
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

      br(), br(),
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
          width = 8,
          textOutput("retry_on_error"),
        )
      ),


      br(), br(),
      textOutput("usage"),
      textOutput("total_cost"),
      textOutput("temperature"),
      br(),
      fluidRow(
        column(
          width = 6,
          uiOutput("html_report")
        ),
        column(
          width = 6,
          actionButton("api_button", "Settings")
        )
      ),
      uiOutput("slava_ukraini"),
      uiOutput("timer_ui")
#      downloadButton(
#        outputId = "eda_report",
#        label = "EDA"
#      ),
    ),

###############################################################################
# Main
###############################################################################

    mainPanel(

      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Main",
          value = "Main",
          h4("AI generated R code:"),
          #codeModules::codeOutput("code_out"),
          verbatimTextOutput("openAI"),
          uiOutput("error_message"),
          h4("Results:"),

          # shows error message in local machine, but not on the server
          verbatimTextOutput("console_output"),
          uiOutput("plot_ui"),
          hr(),
          DT::dataTableOutput("data_table_DT")
          #,tableOutput("data_table")
        ),

        tabPanel(
          title = "Log",
          value = "Log",
          br(),
          downloadButton(
            outputId = "Rmd_source",
            label = "RMarkdown"
          ),
          tippy::tippy_this(
            "Rmd_source",
            "Download a R Markdown source file.",
            theme = "light-border"
          ),
          br(), br(),
          verbatimTextOutput("rmd_chuck_output")
        ),

        tabPanel(
          title = "Ask me anything",
          value = "Ask me anything",
          br(),
          #img(
          #  src = "inst/app/www/tutor.png",
          #  width = "688",
          #  height = "618"
          #),
          fluidRow(
            column(
              width = 7,
              tags$style(type = "text/css", "textarea {width:100%}"),
              tags$textarea(
                id = "ask_question",
                placeholder = NULL,
                rows = 2,
                ""
              )
            ),
            column(
              width = 5,
              selectInput(
                inputId = "demo_question",
                choices = demo_questions,
                label = NULL
              )
            )
          ),

          actionButton("ask_button", strong("Ask RTutor")),
          br(),
          hr(),
          wellPanel(textOutput("answer")),
          tags$head(
            tags$style(
              "#answer{
                color: purple;
                font-size: 16px
              }"
            )
          ),

        ),

        tabPanel(
          title = "FAQs",
          value = "FAQs",
          h4("1.	What is RTutor?"),
          p("It is an artificial intelligence (AI)-based app that enables 
          users to interact with your data via natural language.
           After uploading a 
          dataset, users ask questions about or request analyses in 
          English. The app generates and runs R code to answer that question 
          with plots and numeric results."),

          h4("2.	How does RTutor work?"),
          p("The requests are structured and sent to OpenAI’s AI
           system, which returns R code. The R code is cleaned up and 
           executed in a Shiny 
           environment, showing results or error messages. Multiple 
           requests are logged to produce an R Markdown file, which can be
            knitted into an HTML report. This enables record keeping 
            and reproducibility."),

          h4("3.	Can people without R coding experience use RTutor for statistical analysis? "),
          p("Not entirely. This is because the generated code can be wrong.
           However, it could be used to quickly conduct data 
           visualization, and exploratory data analysis (EDA). 
           Just be mindful of this experimental technology. "),

          h4("4.	Who is it for?"),
          p("The primary goal is to help people with some R experience to learn
           R or be more productive. RTutor can be used to quickly speed up the 
           coding process using R. It gives you a draft code to test and 
           refine. Be wary of bugs and errors. "),

          h4("5.	How do you make sure the results are correct? "),
          p("Try to word your question differently. And try 
          the same request several time. A higher temperature parameter will give 
          diverse choices. Then users can double-check to see 
          if you get the same results from different runs."),

          h4("6.	Can you use RTutor to do R coding homework?"),
          p("No. That will defy the purpose. You need to learn
           R coding properly to be able to tell if the generated 
           R coding is correct.  "),

          h4("7.	Can private companies use RTutor? "),
          p("No. It can be tried as a demo. RTutor website 
          dnd source code are freely available for non-profit organizations
           only and distributed using the CC NC 3.0 license."),

          h4("8.	Can you run RTutor locally?"),
          p("Yes. Download the R package and install it locally. 
          Then you need to obtain an API key from OpenAI."),

          h4("9.	Why do I get different results with the same request? "),
          p("OpenAI’s language model has a certain degree of randomness 
          that could be adjusted by parameters called \"temperature\". 
          Set this in  Settings"),

          h4("10.	How much does the OpenAI’s cost per session?"),
          p("About $0.01 to $0.1, if you send 10 to 50 requests. We have a
           monthly usage limit. Once that is exceeded, the website will
            not work for the month. If you use it regularly, please use 
            your API key. Currently, RTutor receives no funding. 
            We might ask users to contribute later.
            "),

          h4("11.	Can this replace statisticians or data scientists?"),
          p("No. But RTutor can make them more efficient."),

          h4("12.	How do I effectively write my request?"),
          p("Imagine you have a summer intern, a collge student 
          who took one semester of statistics and R. You send the 
          intern emails with instructions and he/she sends 
          back code and results. The intern is not experienced, 
          thus error-prone, but is hard working. Thanks to AI, this
           intern is lightning fast and nearly free.")
        ),

        tabPanel(
          title = "About",
          value = "About",
          h4("RTutor Version 0.2"),
          h5("RTutor uses ",
            a(
              "OpenAI's",
              href = "https://openai.com/",
              target = "_blank"
            ),
            " powerful ",
            language_model,
            "language model",
            " to translate natural language into R code, which is then excuted.",
            "You can request your analysis,
            just like asking a real person.",
            "Upload a data file (CSV, TSV/tab-delimited text files, and Excel) 
            and just analyze it in plain English. 
            Your results can be downloaded as an HTML report in minutes!"
          ),
          h5("NO WARRANTY! Some of the scripts run but yield incorrect result. 
          Please use the auto-generated code as a starting 
          point for further refinement and validation."),

          h5("OpenAI's models are accessed via API, which is not free. Please do not abuse it."),

          p(" Personal hobby project by",
            a(
              "Steven Ge.",
               href = "https://twitter.com/StevenXGe",
               target = "_blank"
            ),
            " For feedback, please email",
            a(
              "gexijin@gmail.com.",
              href = "mailto:gexijin@gmail.com?Subject=RTutor"
            ),
            " Source code at ",
            a(
              "GitHub.",
              href = "https://github.com/gexijin/RTutor"
            )
          ),

          h4("Update log:"),
          p("V 0.2 12/16/2022. Add temperature control."),
          p("V0.1 12/11/2022. Initial launch"),

          h4("RTutor went viral!"),
          p(
            a(
              "Initial post on LinkedIn;  ",
              href = "https://www.linkedin.com/feed/update/urn:li:activity:7008179918844956672/"
            ),
            " ",
            a(
              "@Physacourses on Twitter;  ",
              href = "https://twitter.com/Physacourses/status/1602730176688832513?s=20&t=z4fA3IPNuXylm3Vj8NJM1A"
            ),
            " ",
            a(
              "Carlo Pecoraro on Facebook.",
              href = "https://www.facebook.com/physalia.courses.7/posts/1510757046071330"
            )
          ),

          uiOutput("session_info")
        )
      )
    )
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
      ico = "favicon",
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
