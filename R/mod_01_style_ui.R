# HTML Style Module File

# Define a UI module for the HTML styles
mod_01_style_ui <- function(id) {
  ns <- shiny::NS(id)

  # Add color to UI
  tags$head(tags$style(HTML("

    body {padding-bottom: 80px;}

    /* navbar */
    .navbar {
        background-color: #C1E2BE;border-color: #90BD8C;
        color: #181818;font-weight: bold;
    }

    /* tabs */
    .navbar-default .navbar-nav > li > a {
        background-color: #C1E2BE;border-color: #9AC596;color: #181818;
    }

    /* active tab */
    .navbar-default .navbar-nav > .active > a, 
    .navbar-default .navbar-nav > .active > a:focus, 
    .navbar-default .navbar-nav > .active > a:hover {
        background-color: #A0BB9E;color: #181818;font-weight: bold;
    }

    /* sidebar panel */
    .well {background-color: #C1E2BE;border-color: #90BD8C;}

    /* selectInput & actionButton */
    .custom-select-input, .custom-action-button, .custom-download-button
    { font-size: 24px;color: #000;background-color: #C1E2BE;
      border-color: #90BD8C;
    }

    /* selectInput extra customization */
    .selectize-input, .selectize-dropdown {background-color: #F6FFF5 !important;
      border-color: #90BD8C !important;color: #000 !important; font-size: 18px;}

    /* selectInput font size specification */
    .select-input-font {font-size: 18px;}

    /* textarea, textInput, numericInput */
    textarea, input[type = 'text'], input[type='number']
    { width: 100%;background-color: #F6FFF5;border-color: #90BD8C;
      font-size: 18px;
    }

    /* horizontal line (hr()) */
    .custom-hr{border-top: 1px solid #90BD8C;}
    .custom-hr-thick{border-top: 3px solid #90BD8C;}

    /* tippy this pop-ups */
    .tippy-content {font-size: 15px !important;}

    /* Responsive styles, for mobile browsing */
    @media (max-width: 1000px) {
      .productIntro h2{margin: 25px;font-size: 40px;}
      .productIntro p {margin: 25px;}
      .twocol .column.left {
        margin:25px !important;padding: 20px !important;align-items: flex-start;
      }
      .twocol .column.right {margin: 25px !important;padding: 0px;}
      .twocol .column.left h1 {font-size: 40px !important;}
      .twocol .column.left h2 {font-size: 25px !important;}
      .policy h1, .policy h2, .policy h3 {
        margin: 25px !important;padding: 10px !important;
      }
      .policy h1 {font-size: 35px;}
      .policy h2 {font-size: 28px;}
      .policy h3 {font-size: 21px;}
      .policy p{margin: 25px !important;}
    }

    .shiny-notification {
      width: 300px;position: fixed;top: calc(90%);left: calc(10%);
    }

    .modal-dialog {
      position: absolute;bottom: 0;
    }

    /* EDA Tab */
    /* Background color - tab headers */
    .nav-tabs {background-color: #D9EDD8;}
    /* Text color - tab headers */
    .nav-tabs > li > a {color: #5b5b5b;font-size: 16px;
      border: 1px solid #D9EDD8;border-radius: 4px;}
    /* Hover color - tab headers */
    .nav-tabs > li > a:hover {background-color: #90BD8C;
      color: #000;}
    /* Background color - active tab */
    .nav-tabs > li.active > a {background-color: #f3faf3;
      color: #000;border: 1px solid #D9EDD8;border-bottom-color: transparent;}

    .faq-answer {
      display: none;padding-left: 10px; font-size: 18px;
    }
    
    .faq-question {
      cursor: pointer;padding: 7px;border: 1px solid #90BD8C;
      background-color: #F6FFF5;font-size: 18px;
    }
  ")))
  
}