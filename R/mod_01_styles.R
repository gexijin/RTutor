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

    /* Hide tabs not shown in simplified version (EDA, More dropdown, policy tabs) */
    .navbar-nav li:has(a[data-value='EDA']),
    .navbar-nav li.dropdown,
    .navbar-nav li:has(a[data-value='privacy_policy']),
    .navbar-nav li:has(a[data-value='terms_of_use']) { 
      display: none;
    }

    /* Only hide EDA tab */
    /* .navbar-nav li:has(a[data-value='EDA']) { 
      display: none;
    } */

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

    /* iPad / tablet responsive styles (768px – 1199px) */
    @media (min-width: 768px) and (max-width: 1199px) {

      /* Keep Bootstrap 3 grid columns side-by-side (prevents sidebar stacking) */
      .col-sm-4 { width: 28% !important; float: left !important; }
      .col-sm-8 { width: 72% !important; float: left !important; }

      /* Tighten sidebar padding so content fits without overflow */
      .well { padding: 12px 10px; }

      /* Sidebar section labels: 1. Data, 2. Data Types, 3. Prompt, 4. Ask About Results */
      /* !important needed because these labels use inline style='font-size:18px' */
      .well label,
      .well .control-label,
      .well label span,
      .well .control-label span { font-size: 13px !important; }

      /* 'Data: mpg' selected dataset display — renderUI span with inline font-size */
      [id*='selected_dataset'] span { font-size: 13px !important; }

      /* Input fields and dropdowns */
      .well textarea,
      .well input[type='text'],
      .well input[type='number'] { font-size: 14px; }
      .well .selectize-input,
      .well .selectize-dropdown { font-size: 14px !important; }

      /* Prompt textarea placeholder text */
      #send_request-input_text::placeholder { font-size: 13px !important; }
      #qa-ask_question::placeholder { font-size: 13px !important; }

      /* Submit and Reset buttons */
      #send_request-submit_button,
      #send_request-reset_button { font-size: 14px !important; }

      /* Compact navbar tab labels */
      .navbar-nav > li > a span { font-size: 15px !important; }
      .navbar-brand { font-size: 18px; }

      /* Footer: smaller text and padding */
      footer { font-size: 12px !important; padding: 6px !important; }

      /* Body padding accounts for the slimmer footer */
      body { padding-bottom: 45px; }

      /* 3. Prompt: expand example-prompt dropdown to full sidebar width */
      /* The dropdown sits in col-sm-7 with a col-sm-5 empty spacer beside it */
      .well .col-sm-5 { display: none !important; }
      .well .col-sm-7 { width: 100% !important; }

      /* 1. Data: stack Browse button and filename vertically so text isn't cut off */
      .well .input-group { display: flex !important; flex-direction: column; }
      .well .input-group .input-group-btn { width: 100%; }
      .well .input-group .input-group-btn .btn {
        width: 100%;
        border-radius: 4px !important;
        font-size: 13px;
      }
      .well .input-group > .form-control {
        width: 100% !important;
        margin-top: 3px;
        border-radius: 4px !important;
        font-size: 12px;
      }

      /* Remove space below file upload area (three sources): */
      /* 1. Progress bar rendered by fileInput() — 20px height + 20px margin even before upload */
      .well .shiny-file-input-progress { display: none !important; }
      /* 2. shiny-input-container wrapper margin */
      [id*='data_upload_ui'] .shiny-input-container { margin-bottom: 0 !important; }
      /* 3. Empty second-upload uiOutput still renders as a block */
      /* FRAGILE: :empty only matches if the element has zero child nodes, including whitespace.
         Works for Shiny's uiOutput when renderUI returns NULL (renders a truly empty div),
         but will silently stop working if Shiny ever injects a whitespace text node inside
         the div. If the gap reappears, inspect the element in DevTools — if the div has a
         whitespace child, switch to: [id*='data_upload_ui_2'] { min-height: 0; height: 0; overflow: hidden; } */
      [id*='data_upload_ui_2']:empty { display: none !important; }
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

    /* Small device footer fix (< 600px) */
    /* All footer items sit on one line — on narrow screens they overflow to the right */
    @media (max-width: 600px) {
      footer {
        display: flex !important;
        flex-wrap: wrap !important;
        justify-content: center !important;
        column-gap: 8px;
        padding: 5px 8px !important;
        font-size: 11px !important;
        line-height: 1.8;
      }
      /* Extra padding so content isn't hidden behind the 2-line footer */
      body { padding-bottom: 60px; }
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


    /* Module 16 - Q&A */
    #qa-ask_question::placeholder {
      font-size: 16px;
    }

    .shiny-input-container:has(#qa-ask_question) {
      margin-bottom: 15px;
    }

    /* Q&A suggestion dropdown */
    .qa-dropdown {
      position: absolute;
      width: 100%;
      background: white;
      border: 1px solid #90BD8C;
      border-top: none;
      border-radius: 0 0 4px 4px;
      z-index: 1000;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .qa-dropdown-list {
      list-style: none;
      margin: 0;
      padding: 0;
    }
    .qa-dropdown-item {
      padding: 8px 12px;
      cursor: pointer;
      font-size: 14px;
      color: #3a6b38;
    }
    .qa-dropdown-item:hover {
      background-color: #e8f5e7;
    }

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
      color: green !important;
      background-color: #F6FFF5;
      border-color: #90BD8C;
      }

    #send_request-reset_button {
      font-size: 18px;
      color: red !important;
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
