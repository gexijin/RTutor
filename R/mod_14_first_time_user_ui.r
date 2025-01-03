#____________________________________________________________________________
#  First Time User Tab
#____________________________________________________________________________

mod_14_first_time_user_ui <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      column(
        width = 9,
        h2(
          strong("First Time User"),
          style = "padding-left: 25px; color: black;"
        ),
        hr(class = "custom-hr-thick")
      )
    ),
    fluidRow(
      column(
        width = 9,
        h3(
          "Start by watching an 8-min ",
          a(
            "YouTube video!",
            href = "https://youtu.be/a-bZW26nK9k",
            target = "_blank"
          ),
          style = "color: red;padding-left: 20px;"
        ),
        h4("See",
          a(
            "GitHub",
            href = "https://github.com/gexijin/RTutor",
            target = "_blank"
          ),
          " for source code, bug reports, and instructions to install
          RTutor as an R package. As a small startup, we are open to
          partnerships with both academia and industry. We can do demos
          and seminars via Zoom if time permits.",
          style = "padding-left: 20px;margin-bottom: 3px;"
        ),
        h4("Also try ",
          a(
            "Chatlize.ai,",
            href = "https://chatlize.ai",
            target = "_blank"
          ),
          " a more general platform for analyzing data through chats.
          Multiple files with different formats. Python support.",
          style = "padding-left: 20px;"
        ),

        align = "left"
      ),
      column(
        width = 3,
        img(
          src = "www/logo.png",
          width = "172",
          height = "90"
        ),
        align = "center"
      )
    ),

    hr(class = "custom-hr"),

    fluidRow(
      column(
        width = 12,
        h3(strong("Quick Start:"), style = "padding-left: 20px;"),
        tags$ul(
          tags$style(HTML("ul li {margin-bottom: 3px;}")),
          tags$li(
            "Explore the data at the EDA tab first. Then start with
            simple requests such as distributions, basic plots. Gradually
            add complexity.", style = "color: red;"
          ),
          tags$li(
            "The default model is now GPT-4o. In the same session, previous
            questions and code chunks become the context for your new
            request. For example, you can simply say \"Change background
            color to white\" to refine the plot generated by the previous
            chunk. You can also clean your data step by step."
          ),
          tags$li(
            "To analyze a new dataset, or to start over, click the Reset
            button first."
          ),
          tags$li(
            "Prepare and clean your data in Excel first. Name columns
            properly. ChatGPT tries to guess the meaning of column names,
            even if they are abbrievated."
          ),
          tags$li(
            "RTutor can only analyze traditional statistics data, where
            rows are observations and columns are variables. For complex
            data, try https://chatlize.ai."
          ),
          tags$li(
            "Once uploaded, your data is automatically loaded into R as a
            data frame called df. You do NOT need to ask RTutor to load
            data. Check if the data types of the columns are correct.
            Change if needed, especially when numbers are used to code
            for categories."
          ),
          tags$li(
            "An additional file can be uploaded as df2 to be analyzed
            together. To use it, you must specify 'df2' in your
            prompts."
          ),
          tags$li(
            "Use the Q&A box to ask questions about the code, result, or
            error messages. You can ask for methods to use or develop a
            plan. "
          ),
          tags$li(
            "Before sending your request to OpenAI, we do prompt
            engineering based on the uploaded data. We add \"Generate
            R code\" to the beginning, and append something like \"Use
            the df data frame. Note that highway is numeric, ...\"
            afterward. If you are not using any data (plot a function or
            simulations), choose \"No data\" from the Data dropdown."
          ),
          tags$li(
            "By default, we randomly select 5 rows of data and send to
            OpenAI for enhanced coding results, you can opt out in Settings.
            Your data is not stored on our web server after the session.
            If you explain the background of the data and the meaning of
            the columns, you can ask general questions as if you were
            asking a clueless statistician."
          ),
          tags$li(
            "Be skeptical. The generated code can be logically wrong even
            if it produces results without error."
          ),
          style = "font-size: 18px;padding-left: 75px;"
        )
      )
    )
  )

}