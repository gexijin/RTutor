#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  # Configure Polished with your App UID and API key
  polished::polished_config(
    app_name = "RTutor",
    api_key = Sys.getenv("polish_api_key"),
    firebase_config = list(
      apiKey = Sys.getenv("VITE_FIREBASE_API_KEY"),
      authDomain = Sys.getenv("VITE_FIREBASE_AUTH_DOMAIN"),
      projectId = Sys.getenv("VITE_FIREBASE_PROJECT_ID")
    ),
    sign_in_providers = c("email","google"),
    is_invite_required = FALSE
  )


  with_golem_options(
    app = shinyApp(
      ui = polished::secure_ui(
        app_ui, # Your existing UI function
        sign_in_page_ui = polished::sign_in_ui_default( # Optionally customize the sign-in page
          sign_in_module = polished::sign_in_module_2_ui("sign_in"),
          color = "#90BD8C",
          company_name = "Orditus LLC",
          logo_top = tags$div(
            "RTutor.ai", # Your custom text
            style = "font-size: 30px; font-weight: bold; color: #333; text-align: center; margin-top: 40px; margin-bottom: 20px;"
          ),
          logo_bottom = NULL
        )
      ),
      server = polished::secure_server(
        app_server,
        custom_sign_in_server = polished::sign_in_module_2()),
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}