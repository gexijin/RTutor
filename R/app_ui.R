##########################################################
# RTutor.AI | A Shiny app for chatting with your data.
# Author: Xijin Ge | ge@orditus.com
# © 2024 Orditus LLC
# No warranty & not for commercial use without a license.
##########################################################


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(

    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    ### Style Module ###
    mod_01_styles("styles"),#NS("")

    tags$footer(
      style = "position: fixed;bottom: 0;width: 100%;background-color: #F6FFF5;
        padding: 10px;text-align: center;z-index: 99;",
      span("© 2024 Orditus LLC | "),
      actionLink(inputId = "ppolicy", "Privacy Policy"),
      span(" | "),
      actionLink(inputId = "tofu", "Terms of Use"),
      span(" | "),
      a("Orditus.com", href = "https://orditus.com/", target = "_blank")
    ), # footer

    navbarPage(
      title = HTML('<span style="color: black;font-size: 20px;">RTutor</span>'),
      id = "tabs",

      ### 'Home' Tab Panel ###
      tabPanel(
        title = HTML('<span style="color: black;font-size: 18px;">Home</span>'),

        sidebarLayout(
          ### Sidebar ###
          sidebarPanel(
            mod_02_load_data_ui("load_data"),
            mod_15_data_types_ui("data_edit_modal"),
            mod_03_send_request_ui("send_request"),
            mod_16_qa_ui("qa"),
            mod_17_policies_ui("policies")
          ),

          ### Main Panel ###
          mainPanel(
            mod_04_main_panel_ui("main_panel")
          )
        )
      ), #tabPanel

      ### 'EDA' Tab Panel ###
      tabPanel(
        title = HTML('<span style="color: black;font-size: 18px;">EDA</span>'),
        value = "EDA",
        tippy::tippy_this(
          "eda_tab",
          "Exploratory Data Analysis",
          theme = "light-border"
        ),

        ### EDA Module ###
        mod_10_eda_ui("eda")
      ),

      ### 'Report' Tab Panel ###
      tabPanel(
        title = HTML('<span style="color: black;font-size: 18px;">Report</span>'),
        value = "Report",
        tippy::tippy_this(
          "report_tab",
          "Download a Results Report",
          theme = "light-border"
        ),

        ### Report Module ###
        mod_09_report_ui("report")
      ),

      navbarMenu(
        title = HTML('<span style="color: black;font-size: 18px;">More</span>'),

        tabPanel(
          title = HTML('<span style="color: black;font-size: 18px;">First Time User</span>'),
          value = "first-time-user",
          mod_14_first_time_user_ui("first_time_user")
        ),

        ### 'About' Tab Panel ###
        tabPanel(
          title = HTML('<span style="color: black;font-size: 18px;">About</span>'),
          value = "About",
          mod_12_about_ui("about")
        ),

        tabPanel(
          title = HTML('<span style="color: black;font-size: 18px;">FAQ</span>'),
          value = "FAQ",
          mod_13_faq_ui("faq")
        ),

        ### 'Settings' Tab Panel ###
        tabPanel(
          title = HTML('<span style="color: black;font-size: 18px;">Settings</span>'),
          value = "Settings",
          ### Settings Module ###
          mod_11_settings_ui("sett")
        )
      ),

      ### Hidden Policies Tabs ###
      tabPanel(title = "Privacy Policy", value = "privacy_policy", privacy_policy_content()),
      tabPanel(title = "Terms of Use", value = "terms_of_use", terms_of_use_content())
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
      app_title = "RTutor 2.00"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
