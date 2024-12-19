#____________________________________________________________________________
#  About Tab
#____________________________________________________________________________


mod_12_about_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        h2(strong("About"), style = "padding-left: 25px;color: black;"),
        hr(class = "custom-hr-thick")
      )
    ),

    div(
      uiOutput(ns("RTutor_version_main")),
      style = "padding-left: 20px;"
    ),
    br(),

    p("RTutor uses ",
      a(
        "OpenAI's",
        href = "https://openai.com/",
        target = "_blank"
      ),
      " powerful large language models to translate natural language into
      R (or Python) code, which is then excuted. You can request your
      analysis, just like asking a real person. Upload a data file (CSV,
      TSV/tab-delimited text files, and Excel) and just analyze it in
      plain English. Your results can be downloaded as an HTML report in
      minutes!",
      style = "font-size: 18px;padding-left: 20px;padding-right: 20px;"
    ),

    br(),
    p("NO WARRANTY! Some of the scripts run but may yield incorrect
      results. Please use the auto-generated code as a starting point for
      further refinement and validation.",
      style = "font-size: 18px;padding-left: 20px;padding-right: 20px;"
    ),
    p(
      " Source code at ",
      a(
        "GitHub,",
        href = "https://github.com/gexijin/RTutor",
        target = "_blank"
      ),
      " from where you can also find
      instruction to install RTutor as an R package.",
      style = "font-size: 18px;padding-left: 20px;padding-right: 20px;"
    ),
    p("RTutor went viral on ",
      a(
        "LinkedIn, ",
        href = "https://www.linkedin.com/feed/update/urn:li:activity:7008179918844956672/",
        target = "_blank"
      ),
      a(
        "Twitter, ",
        href = "https://twitter.com/StevenXGe/status/1604861481526386690",
        target = "_blank"
      ),
      a(
        "Twitter(Physacourses),",
        href = "https://twitter.com/Physacourses/status/1602730176688832513?s=20&t=z4fA3IPNuXylm3Vj8NJM1A",
        target = "_blank"
      ),
      " and ",
      a(
        "Facebook (Carlo Pecoraro).",
        href = "https://www.facebook.com/physalia.courses.7/posts/1510757046071330",
        target = "_blank"
      ),
      style = "font-size: 18px;padding-left: 20px;padding-right: 20px;"
    ),
    br(), br(),
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
      " as part of Orditus LLC. For feedback, please email",
      a(
        "ge@orditus.com.",
        href = "mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com"
      ),
      style = "font-size: 18px;padding-left: 20px;padding-right: 20px;"
    ),
    p(
      "Explore our other AI tools at ",
      a("Orditus.com!",
        href = "https://orditus.com/",
        target = "_blank"
      ),
      style = "font-size: 23px;padding-left: 20px;padding-right: 20px;"
    ),
    br(),

    hr(class = "custom-hr"),

    h3("The RTutor website and the source code is free for non-profit
      organizations ONLY. Licensing is required for commercial use.",
      style = "padding-left: 20px;padding-right: 20px;margin-top: 10px;"
    ),
    h4("For businesses, RTutor can be customized and locally installed to
      easily gain insights from your data (files, SQL databases, or APIs)
      at a low cost. We would be happy to discuss licensure.",
      style = "padding-left: 20px;padding-right: 35px;margin-bottom: 10px;"
    ),

    hr(class = "custom-hr"),
    fluidRow(
      column(
        width = 6,
        div(
          uiOutput(ns("package_list")),
          style = "padding-left: 40px;"
        )
      ),
      # Site Update & Session Info Checkbox
      column(
        width = 6,
        checkboxInput(
        ns("site_update_log"),
        div(
          style = "display: inline-flex; align-items: center; vertical-align: middle;",
          tags$h4(
            style = "margin: 0;",
            tags$strong(HTML("<span style='white-space: nowrap;'>See Site
              Updates Log & R Session Info</span>"
            ))
          ),
        ),
        FALSE)
      )
    ),

    hr(class = "custom-hr"),

    fluidRow(
      conditionalPanel(
        condition = paste0("input['", ns("site_update_log"), "'] == 1"),
        # Site Update Log Table
        column(
          width = 6,
          br(),
          h4(strong("Site Updates Log")),
          div(
            tagList(
              div(
                class = "site-updates-wrapper",
                tableOutput(ns("site_updates_table")),
                style = "padding-left: 45px;"
              )
            )
          ),
          align = "center",
        ),
        # Session Info Text
        column(
          width = 6,
          div(
            uiOutput(ns("session_info")),
            style = "padding-left: 45px;"
          ),
        )
      )
    ),
    br(), br()
  )
}















mod_12_about_serv <- function(id) {

    moduleServer(id, function(input, output, session) {
        ns <- session$ns

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
            inputId = ns("installed_packages"),
            label = h4(strong(HTML(
                paste0(
                "<span style='white-space: nowrap;'>",
                "Search for installed packages (",
                length(all),
                " total)</span>"
                )
            ))),
            choices = all,
            selected = NULL
            )
        })

          # 'About' tab Site Updates table
        output$site_updates_table <- renderTable({
            site_updates_df
        }, striped = FALSE)

        output$session_info <- renderUI({
            i <- c("<br><h4><strong>R Session Information: </strong></h4>")
            i <- c(i, capture.output(sessionInfo()))
            HTML(paste(i, collapse = "<br/>"))
        })

    })

}