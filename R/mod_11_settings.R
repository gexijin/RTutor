#____________________________________________________________________________
#  Settings
#____________________________________________________________________________


# Use for styling
common_styles <- list(
  h3_style = "padding-left: 20px;",
  div_style = "padding-left: 20px; font-size: 18px;",
  right_div_style = "padding-left: 85px; padding-right: 20px; font-size: 18px;"
)


mod_11_settings_ui <- function(id) {

  ns <- NS(id)

  # Helper function for creating tippy tooltips
  create_tippy <- function(id, tooltip) {
    tippy::tippy_this(id, tooltip, theme = "light-border")
  }

  tagList(
    # tabPanel(
    #   title = HTML('<span style="color: black;font-size: 18px;">Settings</span>'),
      div(
        id = "settings_window",
        # Header
        fluidRow(
          column(
            width = 12,
            h2(strong("Settings"), style = "padding-left: 25px; color: black;"),
            hr(class = "custom-hr-thick")
          ),

          # AI Model Settings
          column(
            width = 4,
            fluidRow(
              column(
                width = 12,
                h3(strong("AI Model"), style = common_styles$h3_style),
                div(
                  uiOutput(ns("language_model")),
                  style = common_styles$div_style
                )
              )
            )
          )
        ),

        # Python and Data Settings
        fluidRow(
          column(
            width = 6,
            hr(class = "custom-hr-thick"),
            fluidRow(
              column(
                width = 4,
                div(
                  uiOutput(ns("use_python")),
                  style = "padding-left: 75px; font-size: 18px;"
                ),
                create_tippy(ns("use_python"), "Beta Version")
              ),
              column(
                width = 8,
                h4("Use Python instead of R for generating code and results.")
              )
            ),
            hr(class = "custom-hr-thick"),

            # Contribute Data Settings
            fluidRow(
              column(
                width = 4,
                div(
                  uiOutput(ns("contribute_data")),
                  style = "padding-left: 45px; font-size: 18px;"
                )
              ),
              column(
                width = 8,
                h4("Allows us to save your requests and the structure of your data 
                  (like column names and data types, not the data itself). We can learn 
                  from users about creative ways to use AI and it helps in improving 
                  unsuccessful results.", style = "padding-right: 15px;")
              )
            ),

            # Send Head Settings
            fluidRow(
              column(
                width = 4,
                div(
                  uiOutput(ns("send_head")),
                  style = "padding-left: 45px; font-size: 18px;"
                )
              ),
              column(
                width = 8,
                h4("Allows us to optimize code output from OpenAI by sending with your prompt 
                  a random sample of 5 observations from your data", style = "padding-right: 15px;")
              )
            )
          ),

          # Factor Settings
          column(
            width = 6,
            hr(class = "custom-hr-thick"),
            fluidRow(
              column(
                width = 4,
                div(
                  uiOutput(ns("numeric_as_factor")),
                  style = "padding-left: 30px; padding-top: 15px; font-size: 18px;"
                ),
                create_tippy(
                  "numeric_as_factor",
                  "Treat the columns that look like a category as a category. 
                  This applies to columns that contain numbers but have very few unique values."
                )
              ),
              column(
                width = 4,
                div(
                  uiOutput(ns("max_levels_factor")),
                  style = "padding-right: 20px; font-size: 18px;"
                ),
                create_tippy(
                  "max_levels_factor",
                  "To convert a numeric column to a category, the column must have no more 
                  than this number of unique values."
                )
              ),
              column(
                width = 4,
                div(
                  uiOutput(ns("max_proportion_factor")),
                  style = "padding-right: 20px; font-size: 18px;"
                ),
                create_tippy(
                  "max_proportion_factor",
                  "To convert a numeric column as a category, the number of unique values in a column 
                  must not be more than this proportion of the total number of rows."
                )
              )
            ),
            h4("Some columns contain numbers, but should be treated as categorical values or factors. 
              For example, we sometimes use 1 to label success and 0 for failure. If this is selected, 
              using the default setting, a column is treated as categories when the number of unique 
              values is less than or equal to 5, and less than 5% of the total rows.",
               style = "padding-left: 15px; padding-right: 20px;")
          )
        )
      )
    # )
  )
}


mod_11_settings_serv <- function(id, llm_prompt,
                                 code_error) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    ### LLM Parameters ###


    ## Other Settings ##
    # Selected model
    selected_model <- reactive({
      model <- language_models[default_model]  # name: 5.4 mini, value: "gpt-5.4-mini"
      if (!is.null(input$language_model)) {
        model <- input$language_model
      }
      # get the name of the model for display
      names(model) <- names(language_models)[language_models == model]
      return(model)
    })

    output$language_model <- renderUI({
      selectInput(
        inputId = ns("language_model"),
        choices = language_models,
        label = NULL,
        selected = selected_model()
      )
    })



    # Use Python for results
    use_python <- reactive({
      use_py <- FALSE  # default
      # if (!is.null(input$use_python)) {  # Uncomment once python is fixed!
      #   use_py <- input$use_python
      # }
      return(use_py)
    })

    output$use_python <- renderUI({
      checkboxInput(
        inputId = ns("use_python"),
        label = strong("Python"),
        value = use_python()
      )
    })


    ### Data Prep ###

    # Convert data columns to factors
    # Treat the columns that look like a category as a category.
    # Applies to columns that contain numbers but have very few unique values.
    # The default is that these conversions are on.
    convert_to_factor <- reactive({
      convert <- TRUE   # default, to turn off: use 'convert <- FALSE'
      if (!is.null(input$numeric_as_factor)) {
        convert <- input$numeric_as_factor
      }
      return(convert)
    })

    output$numeric_as_factor <- renderUI({
      checkboxInput(
        inputId = ns("numeric_as_factor"),
        label = strong("Treat as factors"),
        value = convert_to_factor()
      )
    })

    max_levels_factor <- reactive({
      max_levels <- max_levels_factor_conversion  # default
      if (!is.null(input$max_levels_factor)) {
        max_levels <- input$max_levels_factor
      }
      return(max_levels)
    })

    output$max_levels_factor <- renderUI({
      numericInput(
        inputId = ns("max_levels_factor"),
        label = "Maximum Levels",
        value = max_levels_factor(),
        min = 3,
        max = 50,
        step = 1
      )
    })

    max_proportion_factor <- reactive({
      max_proportion <- unique_ratio   # default
      if (!is.null(input$max_proportion_factor)) {
        max_proportion <- input$max_proportion_factor
      }
      if (max_proportion < 0.05) {
        max_proportion <- 0.05
      }
      if (max_proportion > 0.5) {
        max_proportion <- 0.5
      }
      return(max_proportion)
    })

    output$max_proportion_factor <- renderUI({
      numericInput(
        inputId = ns("max_proportion_factor"),
        label = "Maximum Proportion",
        value = max_proportion_factor(),
        min = 0.05,
        max = 0.5,
        step = 0.1
      )
    })



    ### Misc. ###

    # Save basic user data
    contribute_data <- reactive({
      save_info <- TRUE  # default
      if (!is.null(input$contribute_data)) {
        save_info <- input$contribute_data
      }
      return(save_info)
    })

    output$contribute_data <- renderUI({
      checkboxInput(
        inputId = ns("contribute_data"),
        label = strong("Help us make RTutor better"),
        value = contribute_data()
      )
    })

    # Send first few rows of user's data to LLM
    send_head <- reactive({
      send_info <- TRUE  # default
      if (!is.null(input$send_head)) {
        send_info <- input$send_head
      }
      return(send_info)
    })

    output$send_head <- renderUI({
      checkboxInput(
        inputId = ns("send_head"),
        label = strong("Help make code generation better"),
        value = send_head()
      )
    })


    # Return reactive values so they can be used outside the module
    return(
      list(
        selected_model = selected_model,
        use_python = use_python,
        convert_to_factor = convert_to_factor,
        max_proportion_factor = max_proportion_factor,
        max_levels_factor = max_levels_factor,
        send_head = send_head,
        contribute_data = contribute_data
      )
    )
  })
}