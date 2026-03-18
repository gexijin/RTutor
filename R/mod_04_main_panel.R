
#____________________________________________________________________________
#  Main Panel
#____________________________________________________________________________


mod_04_main_panel_ui <- function(id) {

  ns <- NS(id)

  tagList(

    # 'First Time User' tab redirect
    tags$script(HTML("
    /* Update the active tab to 'First Time User' within the 'More' navbarMenu */
      $(document).on('click', '#first_user', function() {
        $('#tabs a[data-value=\"first-time-user\"]').tab('show');
      });
    ")),

    # Chunk rename — triggered by the pencil button, NOT by clicking the select.
    tags$script(HTML("
      // Toggle Ace editor read-only state when the user clicks into / out of
      // the code display.  focusout (not blur) is used because blur does not bubble.
      function setAceEditable(on) {
        try { ace.edit('main_panel-code_display').setReadOnly(!on); } catch(e) {}
      }
      $(document).on('mousedown', '#main_panel-code_display', function() {
        setAceEditable(true);
      });
      $(document).on('focusout', '#main_panel-code_display', function() {
        setAceEditable(false);
      });

      $(document).on('click', '#rtutor-rename-btn', function() {
        var $select  = $(this).siblings().find('select').first();
        if (!$select.length) {
          $select = $('#main_panel-selected_chunk');
        }
        var chunkId   = $select.val();
        var chunkName = $select.find('option:selected').text();
        if (!chunkId) return;

        var $wrapper = $('#rtutor-rename-btn').closest('div[style*=\"flex\"]');
        var $btn     = $('#rtutor-rename-btn');
        var $selWrap = $select.closest('[id$=\"select_wrapper\"]');

        // Remove any stale rename widget
        $('#rtutor-rename-container').remove();

        // Hide the dropdown and pencil button
        $selWrap.hide();
        $btn.hide();

        // Build vanilla rename widget — never managed by Shiny
        var $inp = $('<input>', {
          type: 'text', id: 'rtutor-rename-input', value: chunkName
        }).css({
          fontSize: '14px', padding: '4px 8px', height: '34px',
          border: '1px solid #ccc', borderRadius: '4px',
          width: '190px', boxSizing: 'border-box'
        });

        var bStyle = { fontSize:'14px', padding:'5px 10px', borderRadius:'4px',
                       cursor:'pointer', height:'34px', border:'1px solid' };
        var $ok = $('<button>&#x2713;</button>').css(
          $.extend({}, bStyle, {marginLeft:'4px', color:'#fff',
                                background:'#5a9e56', borderColor:'#4a8e46'})
        );
        var $x  = $('<button>&#x2715;</button>').css(
          $.extend({}, bStyle, {marginLeft:'4px', color:'#000',
                                background:'#F6FFF5', borderColor:'#90BD8C'})
        );

        var $box = $('<div>', {id:'rtutor-rename-container'})
          .css({display:'inline-flex', alignItems:'center'})
          .append($inp, $ok, $x);

        $selWrap.after($box);
        $inp.focus();
        $inp[0].select();

        function doConfirm() {
          var name = $inp.val().trim();
          if (name) {
            Shiny.setInputValue('main_panel-chunk_rename_result',
              {id: parseInt(chunkId), name: name}, {priority: 'event'});
          }
          doCleanup();
        }
        function doCleanup() {
          $box.remove();
          $selWrap.show();
          $btn.show();
        }

        $inp.on('keydown', function(e) {
          if (e.key === 'Enter')  { e.preventDefault(); doConfirm(); }
          if (e.key === 'Escape') { e.preventDefault(); doCleanup(); }
        });
        $ok.on('click', doConfirm);
        $x.on('click',  doCleanup);

        setTimeout(function() {
          $(document).one('click.rename', function(ev) {
            if (!$(ev.target).closest('#rtutor-rename-container').length &&
                ev.target.id !== 'rtutor-rename-btn') {
              doCleanup();
            }
          });
        }, 0);
      });
    ")),

    # Initial UI display
    conditionalPanel(
      condition = "input['send_request-submit_button'] == 0",
      div(
        id = "rtutor-banner",
        fluidRow(
          column(
            width = 3,
            div(
              img(
                src = "www/hex_sticker_rtutor_black.png",
                alt = "RTutor Logo",
                style = "width: 125px; height: auto; display: block; margin: auto; margin-top: 10px;"
              ),
              br(),
              actionButton("first_user", strong("Quick Start"), class = "first-user"),
              style = "text-align: center;"
            )
          ),
          column(
            width = 9,
            div(
              style = "display: flex; align-items: center; height: 100%; min-height: 150px;", 
              div(
                p("No code? No problem. Analyze data in your own languages."),
                p("Upload your data, ask questions, and get results in seconds!"),
                br(),
                p(
                  "Also try ",
                  a(
                    "Chatlize.ai,",
                    href = "https://chatlize.ai",
                    target = "_blank"
                  ),
                  " a more flexible AI platform."
                ),
                style = "text-align: left; font-size: 18px; margin-left: 20px;"
              )
            )
          )
        )
      ),

      ),

    # After submit is clicked
    conditionalPanel(
      condition = "input['send_request-submit_button'] != 0",
      # Toolbar: [dropdown | Delete Chunk] .............. [Save | Resubmit | Show Code]
      tags$style(HTML("
        .shiny-input-container { margin-bottom: 0 !important; }
        /* Style the native chunk-selector to match the app's green theme */
        #main_panel-selected_chunk {
          background-color: #F6FFF5;
          border-color: #90BD8C;
          color: #000;
          font-size: 16px;
          height: 34px;
          border-radius: 4px;
          padding: 4px 8px;
        }
      ")),
      div(
        style = "display: flex; align-items: center; justify-content: space-between;
          margin-top: 10px; margin-bottom: 7px;",
        # Left group: chunk selector + delete
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          tags$div(
            id = ns("select_wrapper"),
            selectInput(
              inputId = ns("selected_chunk"),
              label = NULL,
              selected = NULL,
              choices = NULL,
              selectize = FALSE
            )
          ),
          # Pencil button to rename chunk (not a Shiny input, just a trigger for JS)
          tags$button(
            id    = "rtutor-rename-btn",
            type  = "button",
            style = "font-size: 15px; padding: 4px 8px; line-height: 1;
              border: 1px solid #90BD8C; background: #F6FFF5;
              border-radius: 4px; cursor: pointer;",
            "\u270F"   # pencil icon
          ),
          actionButton(
            ns("delete_chunk"),
            "\U0001F5D1", # trash bin icon
            style = "font-size: 15px; color: #000; background-color: #F6FFF5;
              border-color: #90BD8C; padding: 4px 8px; line-height: 1;"
          )
        ),
        # Right group: save + resubmit (shown when dirty) + show code
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          shinyjs::hidden(
            actionButton(
              ns("save_code"),
              "Save",
              style = "font-size: 14px; color: #000; background-color: #F6FFF5;
                border-color: #90BD8C; padding: 6px 12px;"
            )
          ),
          shinyjs::hidden(
            actionButton(
              ns("resubmit_code"),
              "Resubmit",
              style = "font-size: 14px; color: #fff; background-color: #5a9e56;
                border-color: #4a8e46; padding: 6px 12px;"
            )
          ),
          checkboxInput(
            inputId = ns("show_code"),
            label = div("Show Code", style = "font-size: 16px; padding-right: 10px;"),
            value = TRUE
          )
        )
      ),

      # Tooltips
      tippy::tippy_this(
        ns("selected_chunk"),
        "Select a previous code chunk to view or continue from it.",
        theme = "light-border"
      ),
      tippy::tippy_this(
        "rtutor-rename-btn",
        "Rename this chunk.",
        theme = "light-border"
      ),
      tippy::tippy_this(
        ns("delete_chunk"),
        "Don't like this code chunk? Click to remove.",
        theme = "light-border"
      ),
      tippy::tippy_this(
        ns("save_code"),
        "Save edits to this chunk without re-running.",
        theme = "light-border"
      ),
      tippy::tippy_this(
        ns("resubmit_code"),
        "Re-run the edited code and save it to this chunk.",
        theme = "light-border"
      ),

      # If checked, show the code
      conditionalPanel(
        condition = "input.show_code == true",
        ns = ns,
        uiOutput(ns("code_results"))
      ),

      conditionalPanel(
        condition = "true",

        # shows error message in local machine, but not on the server
        uiOutput(ns("error_message")),
        verbatimTextOutput(ns("console_output")),

        # Display plot result
        uiOutput(ns("plot_ui")),
        fluidRow(
          column(
            width = 5,
            # Checkbox to make output interactive
            checkboxInput(
              inputId = ns("make_ggplot_interactive"),
              label = NULL,
              value = FALSE
            ),
            align = "right"
          ),
          column(
            width = 5,
            checkboxInput(
              # Checkbox to make output interactive
              inputId = ns("make_cx_interactive"),
              label = NULL,
              value = FALSE
            ),
            align = "left"
          )
        ),
        br(),
        # Display helpful tips on interactive plots
        uiOutput(ns("tips_interactive"))
      )
    ),
    conditionalPanel(
      condition = "1",
      # First dataset
      uiOutput(ns("data_size")),
      DT::dataTableOutput(ns("data_table_DT")),
      # Second dataset
      uiOutput(ns("data_size_2")),
      DT::dataTableOutput(ns("data_table_DT_2")),
      # Data tables styling
      tags$head(
        tags$style(HTML("
          .dataTables_wrapper {background-color: #f8fcf8;border-color: #90BD8C;padding: 10px;border-radius: 5px;}
          .dataTables_wrapper table.dataTable tbody tr:nth-child(odd) {background-color: #f3faf3;}
          .dataTables_wrapper table.dataTable tbody tr:nth-child(even) {background-color: #fff;}
        "))
      )
    )
  )
}

mod_04_main_panel_serv <- function(id, llm_response, logs, ch, code_error,
                                   run_result, run_env_start, submit_button,
                                   use_python, tabs, current_data, current_data_2,
                                   selected_dataset_name, chunk_selection,
                                   run_env, reverted, api_key) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ###  Code Edit State  ###

    # ID of chunk currently being resubmitted (NULL when not in resubmit)
    resubmit_chunk_id <- reactiveVal(NULL)

    # Per-session counter for LLM security reviews — caps API calls from rapid resubmits
    security_check_count <- reactiveVal(0)


    # True when the editor content differs from the stored code for the selected chunk
    is_dirty <- reactive({
      current_code <- input$code_display
      if (is.null(current_code)) return(FALSE)
      sel <- chunk_selection$selected_chunk
      if (is.null(sel)) return(FALSE)
      id <- as.integer(sel)
      if (id < 1 || id > length(ch$code_history)) return(FALSE)
      stored <- gsub("```", "", ch$code_history[[id]]$raw)
      trimws(current_code, "right") != trimws(stored, "right")
    })

    # Show / hide Save & Resubmit buttons based on dirty state
    observe({
      req(submit_button())
      if (isTRUE(is_dirty())) {
        shinyjs::show("save_code")
        shinyjs::show("resubmit_code")
      } else {
        shinyjs::hide("save_code")
        shinyjs::hide("resubmit_code")
      }
    })

    # Save: persist edited code to history without re-running
    observeEvent(input$save_code, {
      req(input$code_display)
      req(chunk_selection$selected_chunk)
      chunk_id <- as.integer(chunk_selection$selected_chunk)
      new_code <- input$code_display

      ch$code_history[[chunk_id]]$code <- new_code
      ch$code_history[[chunk_id]]$raw  <- new_code
      # Update the code block inside the stored Rmd chunk
      ch$code_history[[chunk_id]]$rmd <- sub(
        "(?s)```\\{R\\}\\n.*?\\n```",
        paste0("```{R}\n", trimws(new_code, "right"), "\n```"),
        ch$code_history[[chunk_id]]$rmd,
        perl = TRUE
      )
      showNotification("Code saved.", duration = 3, type = "message")
      # is_dirty() becomes FALSE automatically because ch$code_history$raw is updated
    })

    # Resubmit: auto-save + restore pre-chunk env + re-run
    observeEvent(input$resubmit_code, {
      req(input$code_display)
      req(chunk_selection$selected_chunk)
      chunk_id <- as.integer(chunk_selection$selected_chunk)
      req(chunk_id >= 1 && chunk_id <= length(ch$code_history))
      new_code <- input$code_display

      # --- Pre-save security check on editor content ---
      validation <- validate_r_code(new_code)
      if (!validation$safe) {
        issue_list <- paste0("<li>", validation$issues, "</li>", collapse = "")
        showNotification(
          ui = HTML(paste0(
            "<b>Code blocked — security violation:</b><ul>", issue_list, "</ul>"
          )),
          type = "error",
          duration = 20
        )
        return()
      }

      # --- Layer 2: LLM review if diff is significant (max 20 checks per session) ---
      original_code <- ch$code_history[[chunk_id]]$raw
      if (diff_is_significant(original_code, new_code) && security_check_count() < 20) {
        security_check_count(security_check_count() + 1)

        notify_id <- showNotification("Reviewing edits...", duration = NULL, type = "message")

        review_prompt <- paste0(
          "A student edited the following R code chunk in a statistics tutoring app.\n\n",
          "ORIGINAL:\n", original_code, "\n\n",
          "EDITED:\n", new_code, "\n\n",
          "Does the edited version attempt anything outside legitimate data analysis — ",
          "such as file system access, network calls, environment variable access, ",
          "or executing system commands?"
        )

        result <- call_llm_check(review_prompt, api_key)
        removeNotification(notify_id)

        if (is.null(result)) {
          showNotification(
            ui = HTML("<b>Code blocked — security review could not complete (API unavailable). Please try again.</b>"),
            type = "error",
            duration = 15
          )
          return()
        }

        if (grepl("^yes", result, ignore.case = TRUE)) {
          message("[SECURITY] LLM review flagged edited code in chunk ", chunk_id,
                  " at ", Sys.time(), "\nEdited code:\n", new_code)
          showNotification(
            ui = HTML("<b>Code blocked — edits flagged as potentially harmful.</b>"),
            type = "error",
            duration = 20
          )
          return()
        }
      }

      # Auto-save to history
      ch$code_history[[chunk_id]]$code <- new_code
      ch$code_history[[chunk_id]]$raw  <- new_code
      ch$code_history[[chunk_id]]$rmd <- sub(
        "(?s)```\\{R\\}\\n.*?\\n```",
        paste0("```{R}\n", trimws(new_code, "right"), "\n```"),
        ch$code_history[[chunk_id]]$rmd,
        perl = TRUE
      )

      # Update logs so mod_07 runs the right code
      logs$code <- new_code
      logs$raw  <- new_code  # triggers editor re-render (same content)

      # Restore the environment that existed before this chunk originally ran
      run_env(list2env(ch$code_history[[chunk_id]]$env))

      # Track that a resubmit is in progress (for post-run history update)
      resubmit_chunk_id(chunk_id)

      # Warn if later chunks now depend on this changed chunk
      if (chunk_id < length(ch$code_history)) {
        showNotification(
          paste0(
            "Chunk #", chunk_id, " re-run. Chunks ",
            chunk_id + 1, "\u2013", length(ch$code_history),
            " may need to be re-run."
          ),
          duration = 8,
          type = "warning"
        )
      }

      # Trigger mod_07 to execute the edited code
      reverted(reverted() + 1)
    })

    # After a resubmit run completes, update error status in the history entry
    observeEvent(run_result(), {
      req(!is.null(resubmit_chunk_id()))
      id <- resubmit_chunk_id()
      ch$code_history[[id]]$error         <- code_error()
      ch$code_history[[id]]$error_message <- run_result()$error_message
      resubmit_chunk_id(NULL)
    })


    ###  Selecting Chunk  ###

    # Update the selectInput choices when number of chunks changes
    observe({
      req(chunk_selection$chunk_choices)
      req(chunk_selection$selected_chunk)

      updateSelectInput(
        session = session,
        inputId = "selected_chunk",
        choices = chunk_selection$chunk_choices,
        selected = chunk_selection$selected_chunk
      )
    })

    # Sync dropdown selection to chunk_selection$selected_chunk
    # Skip if value is unchanged to avoid an echo loop
    observeEvent(input$selected_chunk, {
      if (!isTRUE(as.character(chunk_selection$selected_chunk) == input$selected_chunk)) {
        # Set past_prompt before selected_chunk so mod_03 reads the correct prompt
        # when it reacts to the selected_chunk change
        new_id <- as.integer(input$selected_chunk)
        if (new_id >= 1 && new_id <= length(ch$code_history)) {
          chunk_selection$past_prompt <- ch$code_history[[new_id]]$prompt
        }
        chunk_selection$selected_chunk <- input$selected_chunk
      }
    })

    # Apply rename — JS sends this only when the user confirms (Enter / ✓ button)
    observeEvent(input$chunk_rename_result, {
      req(input$chunk_rename_result)
      chunk_id <- as.integer(input$chunk_rename_result$id)
      new_name <- trimws(input$chunk_rename_result$name)
      req(nchar(new_name) > 0)
      req(chunk_id >= 1 && chunk_id <= length(ch$code_history))

      ch$code_history[[chunk_id]]$name <- new_name
      ch$code_history[[chunk_id]]$rmd  <- sub(
        "### [0-9]+\\. [^\n]*",
        paste0("### ", chunk_id, ". ", new_name),
        ch$code_history[[chunk_id]]$rmd
      )

      choices <- seq_along(ch$code_history)
      names(choices) <- sapply(choices, function(i) {
        nm <- ch$code_history[[i]]$name
        if (!is.null(nm)) nm else paste0("Chunk #", i)
      })
      chunk_selection$chunk_choices <- choices
      updateSelectInput(session, "selected_chunk",
        choices  = choices,
        selected = chunk_selection$selected_chunk
      )
    })


    ###  Print Results or Error  ###

    # Print code chunk
    output$code_results <- renderUI({
      req(logs$raw)

      results <- gsub("```", "", logs$raw)

      # Calculate height based on number of lines
      num_lines <- lengths(regmatches(results, gregexpr("\n", results))) + 1
      height_px <- max(120, min(600, num_lines * 18))
      height <- sprintf("%dpx", height_px)

      # use shinyAce to print with syntax coloring (editable when focused)
      shinyAce::aceEditor(
        ns("code_display"),
        value = results,  # code results
        mode = "r",
        theme = "xcode",  # change syntax color theme here
        height = height,
        fontSize = 14,
        readOnly = TRUE,   # JS toggles this to FALSE on focus, TRUE on blur
        showPrintMargin = FALSE  # remove vertical line at 80 chars
      )
    })

    # Print results
    output$console_output <- renderText({
      req(!code_error())
      paste(run_result()$console_output, collapse = "\n")
    })

    # Display error messages
    output$error_message <- renderUI({
      req(code_error())
      req(logs$code)
      if(code_error()) {
        h4(paste("Error!", run_result()$error_message), style = "color:red")
      } else {
        return(NULL)
      }
    })


    ###  Plotting  ###

    # Plot results
    output$result_plot <- renderPlot({
      req(!code_error())
      req(logs$code)
      req(!is.null(run_result()$result) || !is.null(run_result()$console_output))
      # Check if the result is not a ggplot or a known plot type
      if (inherits(run_result()$result, "ggplot") || is.null(run_result()$console_output)) {
        return(run_result()$result)
      } else {
        # If the result is not a ggplot (e.g., corrplot), re-evaluate the command_string,
        # under the parent environment of the run_env()
        tmp_env <- list2env(run_env_start())
        tryCatch({
          eval_result <- eval(
            parse(text = clean_cmd(logs$code, selected_dataset_name(), file.exists(on_server))),
            envir = tmp_env
          )
        })
      }
    })

    # Plot results - plotly
    output$result_plotly <- plotly::renderPlotly({
      req(!code_error())
      req(!use_python())
      req(!is.null(run_result()$result))
      req(
        is_interactive_plot() ||   # natively interactive
          turned_on(input$make_ggplot_interactive)
      )

      g <- run_result()$result
      # still errors some times, when the returned list is not a plot
      if (is.character(g) || is.data.frame(g) || is.numeric(g)) {
        return(NULL)
      } else {
        return(g)
      }
    })

    # Plot results - canvasXpress
    output$result_CanvasXpress <- canvasXpress::renderCanvasXpress({
      req(!code_error())
      req(!use_python())
      req(!is.null(run_result()$result))

      g <- run_result()$result
      if (
        turned_on(input$make_cx_interactive) &&
          !is.character(g) &&
          !is.data.frame(g) &&
          !is.numeric(g)
      ) {
        g <- canvasXpress::canvasXpress(g)
      } else {
        g <- canvasXpress::canvasXpress(destroy = TRUE)
      }
      return(g)
    })

    # Checks to render plotly or canvasXpress if applicable
    output$plot_ui <- renderUI({
      req(submit_button())
      req(!use_python())
      req(!code_error())
      req(logs$code)

      if (
        is_interactive_plot() ||   # natively interactive
          turned_on(input$make_ggplot_interactive) # converted
      ) {
        plotly::plotlyOutput(ns("result_plotly"))
      } else if (
        turned_on(input$make_cx_interactive) # converted
      ) {
        canvasXpress::canvasXpressOutput(ns("result_CanvasXpress"))
      } else {
        plotOutput(ns("result_plot"))
      }
    })

    # Display tips for interactive plots
    output$tips_interactive <- renderUI({
      req(submit_button())

      if (is_interactive_plot() ||   # natively interactive
          turned_on(input$make_ggplot_interactive) # converted
      ) {
        tagList(
          p("Mouse over to see values. Select a region to zoom.
          Click on the legends to deselect a group.
          Double click a category to hide all others.
          Use the menu on the top right for other functions."
          )
        )
      } else if (turned_on(input$make_cx_interactive)) {
        tagList(
          p("To reset, press ESC. Or mouse over the top,
          then click the reset button on the top left.
          Mouse over to see values. Select a region to zoom.
          Click on the legends to deselect a group.
          Double click a category to hide all others.
          Use the menu on the top right for other functions.
          Right click for more options."
          )
        )
      }
    })

    # Check if plot is interactive
    is_interactive_plot <- reactive({
      # only true if the plot is interactive, natively.
      req(submit_button())
      req(logs$code)
      req(!code_error())
      if (inherits(run_result()$result, "plotly")) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })

    # Reminder for user to uncheck interactive plot
    observe({
      req(input$make_cx_interactive)
      req(tabs() == "Home")
      showNotification(
        ui = paste("Please uncheck the CanvasXpress
        box before proceeding to the next request."),
        id = "uncheck_canvasXpress",
        duration = 10,
        type = "error"
      )
    })

    # Remove reminder messages if the tab changes
    observe({
      if (is.null(input$make_cx_interactive) || tabs() != "Home") {
        removeNotification("uncheck_canvasXpress")
      }
    })

    # Hide interactive checkbox initially
    observe({
      # hide it by default
      shinyjs::hideElement(id = "make_ggplot_interactive")
      updateCheckboxInput(
        session = session,
        inputId = "make_ggplot_interactive",
        label = "Interactive via plotly",
        value = FALSE
      )

      req(!code_error())
      req(logs$code)
      txt <- paste(llm_response()$cmd, collapse = " ")

      # if not a dataframe, create dummy data
      if ("data.frame" %in% class(current_data())) {
        df <- current_data()
      } else {
        df <- data.frame(value = rep(1, 3))
      }

      if (inherits(run_result()$result, "ggplot") && # if ggplot2, and it is
          !is_interactive_plot() && # not already an interactive plot, show
          # if there are too many data points, don't do the interactive
          !(dim(df)[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
      ) {
        shinyjs::showElement(id = "make_ggplot_interactive")
      }
    })

    # Hide interactive checkbox initially
    observe({
      # hide it by default
      shinyjs::hideElement(id = "make_cx_interactive")
      updateCheckboxInput(
        session = session,
        inputId = "make_cx_interactive",
        label = "Interactive via CanvasXpress",
        value = FALSE
      )

      req(!code_error())
      req(logs$code)
      txt <- paste(llm_response()$cmd, collapse = " ")

      # if not a dataframe, create dummy data
      if ("data.frame" %in% class(current_data())) {
        df <- current_data()
      } else {
        df <- data.frame(value = rep(1, 3))
      }

      if (inherits(run_result()$result, "ggplot") && # if canvasXpress, and it is
         !is_interactive_plot() && # not already an interactive plot, show
         # if there are too many data points, don't do the interactive
         !(dim(df)[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
      ) {
        shinyjs::showElement(id = "make_cx_interactive")
      }
    })

    # First Dataset Table
    output$data_table_DT <- DT::renderDataTable({
      req(current_data())
      DT::datatable(
        current_data(),
        options = list(
          lengthMenu = c(5, 20, 50, 100),
          pageLength = 10,
          dom = "ftp",
          scrollX = "400px"
        ),
        rownames = FALSE
      )
    })

    output$data_size <- renderUI({
      req(!is.null(current_data()))
      tagList(
        hr(class = "custom-hr-thick"),
        h4("Selected Dataset"),
        paste(
          dim(current_data())[1], "rows X",
          dim(current_data())[2], "columns"
        )
      )
    })

    # Second Dataset Table
    output$data_table_DT_2 <- DT::renderDataTable({
      req(current_data_2())
      DT::datatable(
        current_data_2(),
        options = list(
          lengthMenu = c(5, 20, 50, 100),
          pageLength = 10,
          dom = "ftp",
          scrollX = "400px"
        ),
        rownames = FALSE
      )
    })

    output$data_size_2 <- renderUI({
      req(!is.null(current_data_2()))
      tagList(
        hr(class = "custom-hr-thick"),
        h4("2nd Dataset (Must specify, e.g. 'create a piechart of X in df2.')"),
        paste(
          dim(current_data_2())[1], "rows X",
          dim(current_data_2())[2], "columns"
        )
      )
    })


    observeEvent(input$delete_chunk, {

      req(input$selected_chunk)
      shinyalert::shinyalert(
        title = paste0("Delete Code Chunk ", input$selected_chunk, "?"),
        text = NULL,
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes",
        cancelButtonText = "No",
        callbackR = function(isConfirmed) {
          if (isConfirmed) {
            # What current chunk is selected??
            id_pre <- as.integer(input$selected_chunk)
            ch$code_history[[id_pre]] <- NULL # R Automatically shifts list down

            max_id <- length(ch$code_history)

            if (max_id > 0){ # Order Operation MATTERS!!!!
              # Order Operation 1 (Reorder Code History ID's & rmd chunk numbering)
              ch$code_history <- lapply(1:max_id, function(i) {
                ch$code_history[[i]]$id = i #Reasign the id's
                substr(ch$code_history[[i]]$rmd,6,6) = as.character(i) #Updating the RMD number as well.
                ch$code_history[[i]]
              })

              # Order Operation 2 (Update current code info)
              logs$id <- ch$code_history[[max_id]]$id
              logs$code <- ch$code_history[[max_id]]$code
              logs$raw <- ch$code_history[[max_id]]$raw
              logs$last_code <- ch$code_history[[max_id]]$last_code
              logs$language <- ch$code_history[[max_id]]$language


              choices <- seq_along(ch$code_history)
              names(choices) <- sapply(choices, function(i) {
                if (!is.null(ch$code_history[[i]]$name)) ch$code_history[[i]]$name else paste0("Chunk #", i)
              })
              chunk_selection$chunk_choices <- choices

              # Update chunk choices
              updateSelectInput(
                session = session,
                inputId = "selected_chunk",
                choices = choices,
                selected = logs$id
              )

            } else {
              logs$id <- 0
              logs$code = ""
              logs$raw = ""
              logs$last_code = ""
              logs$language = ""
              ch$code_history = list()

              # update chunk choices
              updateSelectInput(
                session = session,
                inputId = "selected_chunk",
                choices = "",
                selected = NULL
              )
              
            }
          }
        }
      )
    })

  })
}