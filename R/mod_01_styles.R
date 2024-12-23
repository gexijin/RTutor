# HTML Style Module File

# Define a UI module function for the HTML styles
mod_01_styles_ui <- function(id) {

  ns <- NS(id)

  # Add color to UI
  tags$head(tags$style(HTML("

    body {padding-bottom: 80px;}

    /* navbar */
    /* .navbar is a built in bootstrap class */
    .navbar {
      background-color: #C1E2BE;
      border-color: #90BD8C;
      color: #181818;
      font-weight: bold;
    }

    /* tabs */
    .navbar-default .navbar-nav > li > a {
      background-color: #C1E2BE;
      border-color: #9AC596;
      color: #181818;
    }

    /* active tab */
    .navbar-default .navbar-nav > .active > a, 
    .navbar-default .navbar-nav > .active > a:focus, 
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #A0BB9E;
      color: #181818;
      font-weight: bold;
    }

    /* sidebar panel */
        /* .well is a built in bootstrap class, */
    .well {background-color: #C1E2BE; border-color: #90BD8C; color: #000;}


    /* Session Report & RMarkdown */
    .custom-action-button, .custom-download-button
    { font-size: 24px;
      color: #000;
      background-color: #C1E2BE;
      border-color: #90BD8C;
    }

    /* Customize Dropdown Menus */
    .selectize-input, .selectize-dropdown {
      background-color: #F6FFF5 !important;
      border-color: #90BD8C !important;
      color: #000 !important;
      font-size: 18px;
      }

    /* Report Tab */
        /* Code chunks to include: */
    .select-input-font {font-size: 18px;}

    .padding {padding-top: 10px; padding-left: 10px; padding-bottom: 10px;}

    /* textarea, textInput, numericInput */
    textarea, input[type = 'text'], input[type='number']
    {
      width: 100%;
      background-color: #F6FFF5;
      border-color: #90BD8C;
      font-size: 18px;
    }

    /* horizontal line (hr()) */
    .custom-hr{border-top: 1px solid #90BD8C;}
    .custom-hr-thick{border-top: 3px solid #90BD8C;}

    /* tippy this pop-ups. Built in class from Tippy.js library*/
    .tippy-content {font-size: 15px !important;}

    /* policy styles */
    .policy {
      background-color: #ededed;
      background-size: cover;
      background-position: center;
      min-height: 500px;
      margin: 0 !important;
      padding-top: 0px;
      display: flex;
      justify-content: center;
      border: 50px solid #bedbb7;
      color: #262626;
      text-align: left;
      flex-direction: column;
    }
    .policy h1 {
      font-size: 40px;
      padding-top: 90px;
      margin-left: 125px;
      font-weight: bold;
    }
    .policy h2 {
      font-size: 25px;
      padding-top: 40px;
      margin-left: 125px;
      font-weight: bold;
    }
    .policy h3 {
      font-size: 20px;
      padding-top: 20px;
      margin-left: 125px;
      font-weight: bold;
    }
    .policy p {
      font-size: 17px;
      margin-top: 20px;
      margin-right: 125px;
      margin-left: 125px;
    }

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

   /* Built in class for Shiny Library */ 
    .shiny-notification {
      width: 300px;
      position: fixed;
      top: calc(90%);
      left: calc(10%);
    }

  /* Built in bootstrap class */
    .modal-dialog {
      position: absolute;
      bottom: 0;
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
      color: #000; border: 1px solid #D9EDD8; border-bottom-color: transparent;}


    /* FAQ. Custom class */
    .faq-answer {
      display: none; padding-left: 10px; font-size: 18px;
    }
    .faq-question {
      cursor: pointer;padding: 7px;border: 1px solid #90BD8C;
      background-color: #F6FFF5;font-size: 18px;
    }


    /* Specific Styling  -- (put an '*' after each 'for'/'id'/'class') */
    /*                      (this ensures it is applied across all modules) */


    /* Module 2 */
        /* Styling the Label where InputId contains 'user_file' */
        /* Note: Most of our Label formatting is currently done with inline code */
    .control-label[for*='user_file'] { font-size: 18px; font-weight: bold; }

    /* Module 3 */
      /* The #'module tag'-'id' is used with modules */
    #send_request-submit_button {
      font-size: 18px;
      color: red !important;
      background-color: #F6FFF5;
      border-color: #90BD8C;
      }

    #send_request-reset_button {
      font-size: 18px;
      background-color: #F6FFF5;
      border-color: #90BD8C;
      }

    #send_request-input_text {
      width: 100%;
      background-color: #F6FFF5;
      border-color: #90BD8C;
      font-size: 16px;
      resize: vertical;
      }
    
    /* Module 4 */
        /* Quick start */
    [class*=first-user]{font-size: 16px;color: #000;background-color: #90BD8C;
    transition: background-color 0.3s, box-shadow 0.3s;}
    [class*=first-user]:hover {
      background-color: #66AFFF;box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
    }

    /* Module 12 */
    [class*=site-updates-wrapper] table
      {background-color: #f3faf3;border-top: 2px solid #90BD8C;}
    [class*=site-updates-wrapper] table thead th,
      [class*=site-updates-wrapper] table td
        {border: 2px solid #90BD8C;}
    [class*=site-updates-wrapper] table tbody tr:nth-child(odd)
      {background-color: #f3faf3;}
    [class*=site-updates-wrapper] table tbody tr:nth-child(even)
      {background-color: #ffffff;}


    /* Welcome Banner */
    #rtutor-banner {
      background: linear-gradient(to right, #afd0ad, #deecdd, #afd0ad);
      text-align: center;padding: 20px;border-radius: 10px;
      margin-right: 30px;margin-bottom: 20px;border: 1px solid #90BD8C;
    }
    #rtutor-banner img {height: 50px;margin-bottom: 15px;vertical-align:middle;}
    #rtutor-banner h2 {font-size: 24px;margin: 0;padding-bottom: 10px;}
    #rtutor-banner p {font-size: 18px;margin: 0;}


  ")))
}
