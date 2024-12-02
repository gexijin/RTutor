#____________________________________________________________________________
#  FAQ Dropdown
#____________________________________________________________________________

mod_13_faq_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        h2("Frequently asked Questions",
          style = "font-weight: bold;padding-left: 25px;color: black;"
        ),
        hr(class = "custom-hr-thick"),

        div(
          uiOutput(ns("faq_list")),
          style = "padding-left: 20px;padding-right: 20px;"
        ),
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
        h2("Comments & Questions",
          style = "font-weight: bold;padding-left: 20px;color: black;"
        ),
        hr(class = "custom-hr-thick"),

        div(
          tags$textarea(
            id = ns("user_feedback"),
            placeholder = "Any questions? Suggestions? Things you like, don't like? Leave your email if you want to hear back from us.",
            rows = 4,
            "",
            style = "font-size: 18px;padding: 7px;"
          ),
          style = "padding-left: 20px;padding-right: 20px;"
        ),

        div(
          radioButtons(ns("helpfulness"), "How useful is RTutor?",
            c("Not at all", "Slightly", "Helpful", "Extremely"),
            selected = "Slightly"
          ),
          radioButtons(ns("experience"), "Your experience with R:",
            c("None", "Beginner", "Intermediate", "Advanced"),
            selected = "Beginner"
          ),
          style = "font-size: 18px;padding-left: 20px;"
        ),

        div(
          actionButton(
            ns("save_feedbck"),
            label = strong("Save Feedback"),
            class = "custom-action-button",
            style = "font-size: 20px;"
          ),
          style = "padding-left: 20px;"
        )
      )
    )
  )
}


mod_13_faq_serv <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$save_feedbck, {
      req(input$save_feedbck)
      feedback_len <- nchar(input$user_feedback)
      if (feedback_len < 5) {
        showNotification("Feedback is too short.")
      } else  if (feedback_len > 2000) {
        showNotification("Feedback is too long.")
      } else {
        showNotification("Thank you for your feedback!")

        try(
          save_comments(
            date = Sys.Date(),
            time = format(Sys.time(), "%H:%M:%S"),
            comments = input$user_feedback,
            helpfulness = input$helpfulness,
            experience = input$experience
          )
        )
      }

      # clear the comments after submitted.
      # This prevents users submit the same thing twice.
      updateTextInput(
        session,
        "user_feedback",
        value = "",
        placeholder = "Any questions? Suggestions? Things you like, don't like?"
      )
    })

    observe({
      shinyjs::toggle(id = "user_feedback", condition = TRUE)
      shinyjs::toggle(id = "save_feedbck", condition = TRUE)
      shinyjs::toggle(id = "helpfulness", condition = TRUE)
      shinyjs::toggle(id = "experience", condition = TRUE)
    })

    output$faq_list <- renderUI({
      faq_items <- lapply(seq_len(nrow(faqs)), function(i) {
        tags$div(
          class = "faq-item",
          tags$h5(
            class = "faq-question",
            faqs$question[i]
          ),
          tags$p(
            class = "faq-answer",
            faqs$answer[i]
          )
        )
      })
      tagList(faq_items)
    })

  })
}