

#____________________________________________________________________________
#  Run the code, data prep, show code
#____________________________________________________________________________


mod_07_run_code_serv <- function(id, run_env, run_env_start, run_result, submit_button,
                                 reverted, logs, use_python, selected_dataset_name,
                                 current_data, current_data_2, code_error) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Run the code ###

    # stores the results after running the generated code
    # returns error indicator and message
    # Sometimes returns NULL, even when code runs fine. Especially when
    # a base R plot is generated

    observeEvent(
      eventExpr = list(
        submit_button(),  # when submit is clicked
        reverted(),       # or when a previous code chunk is selected
        logs$code
      ),{
      req(logs$code != "")
      req(!use_python())

      # --- Pre-execution security check ---
      validation <- validate_r_code(logs$code)
      if (!validation$safe) {
        issue_list <- paste0("<li>", validation$issues, "</li>", collapse = "")
        showNotification(
          ui = HTML(paste0(
            "<b>Code blocked — security violation:</b><ul>", issue_list, "</ul>"
          )),
          type = "error",
          duration = 20
        )
        code_error(TRUE)
        run_result(list(
          result       = NULL,
          console_output = NULL,
          error_message  = paste("Security violation:",
                                 paste(validation$issues, collapse = "; "))
        ))
        return()
      }

      result <- NULL
      console_output <- NULL
      error_message <- ""

      withProgress(message = "Running the code ...", {
        incProgress(0.4)

        run_env_start(as.list(run_env())) # keep a copy of the crime scene

        # Inject an S4-aware summary into run_env so packages that register summary
        # only as an S4 method (e.g. lavaan) dispatch correctly — no S3 bridge needed.
        s4_aware_summary <- function(object, ...) {
          cl <- class(object)[1L]
          s3 <- getS3method("summary", cl, optional = TRUE)
          if (!is.null(s3)) return(s3(object, ...))
          if (isS4(object)) {
            m <- tryCatch(
              methods::selectMethod("summary", cl, optional = TRUE),
              error = function(e) NULL
            )
            if (!is.null(m)) return(m(object, ...))
          }
          base::summary(object, ...)
        }
        had_summary_pre <- exists("summary", envir = run_env(), inherits = FALSE)
        if (!had_summary_pre) assign("summary", s4_aware_summary, envir = run_env())

        result <- tryCatch({
          cleaned_code <- clean_cmd(logs$code, selected_dataset_name(), file.exists(on_server))

          # Pre-attach packages used via 'pkg::fn()' so S3/S4 methods dispatch correctly.
          # Using pkg:: only loads the namespace; S3 methods require the package to be attached.
          ns_pkgs <- unique(regmatches(
            cleaned_code,
            gregexpr("[A-Za-z][A-Za-z0-9.]*(?=:::?)", cleaned_code, perl = TRUE)
          )[[1]])
          for (pkg in setdiff(ns_pkgs, c("base", "utils", "methods", "stats",
                                          "graphics", "grDevices", "datasets",
                                          "tools", "compiler"))) {
            if (!paste0("package:", pkg) %in% search()) {
              suppressMessages(suppressWarnings(tryCatch(
                require(pkg, character.only = TRUE, quietly = TRUE),
                error = function(e) NULL
              )))
            }
          }

          # Eval each expression individually, replicating R's top-level auto-print behavior.
          # This captures output from ALL expressions (not just the last one), and avoids
          # printing "NULL" for invisible returns like pie() / ggplot side-effect draws.
          parsed_exprs <- parse(text = cleaned_code)
          eval_result  <- NULL

          all_output <- capture.output({
            for (i in seq_along(parsed_exprs)) {
              vis        <- withVisible(eval(parsed_exprs[[i]], envir = run_env()))
              eval_result <- vis$value
              if (vis$visible) print(vis$value)
            }
          })

          # NULL signals "no text output — may be a plot" to mod_04's plot_ui condition
          console_output <- if (length(all_output) > 0) all_output else NULL

          eval_result                # without this, interactive plots don't work
        }, error = function(e) {
          list(error_message = e$message)  # won't work if not inside a list!
        })

        # Remove the temporary S4 summary shim if user code didn't redefine it
        if (!had_summary_pre &&
            exists("summary", envir = run_env(), inherits = FALSE) &&
            identical(get("summary", envir = run_env(), inherits = FALSE), s4_aware_summary)) {
          rm("summary", envir = run_env())
        }

        # update the error message, if any
        if (length(names(result)) != 0) {
          if (names(result)[1] == "error_message") {
            error_message <- result$error_message
          }
        }

        # Code was Run with error
        if (error_message != "") {
          run_env(list2env(run_env_start()))  # revert the environment
          showNotification(
            "Resubmit the same request to see if ChatGPT can resolve the error.
            If that fails, change the request.",
            duration = 10
          )
          code_error(TRUE)
        } else{
          code_error(FALSE)
        }

        # Check to see if df changed from running the code
        if (!is.null(current_data()) && !is.null(run_env()$df)) {
          row_check <- nrow(current_data()) == nrow(run_env()$df) # Check if # of rows are same
          col_check <- ncol(current_data()) == ncol(run_env()$df) # Check if # of columns are same
          if (row_check && col_check) {
            val_check <- tryCatch(
              length(which(current_data() != run_env()$df)),
              error = function(e) 1L  # treat uncomparable types as changed
            )
            if (val_check > 0) {
              current_data(run_env()$df)
            }
          } else {
            current_data(run_env()$df)
          }
        }

        # Check to see if df2 changed from running the code
        if (!is.null(current_data_2()) && !is.null(run_env()$df2)) {
          row_check <- nrow(current_data_2()) == nrow(run_env()$df2) # Check if # of rows are same
          col_check <- ncol(current_data_2()) == ncol(run_env()$df2) # Check if # of columns are same
          if (row_check && col_check) {
            val_check <- tryCatch(
              length(which(current_data_2() != run_env()$df2)),
              error = function(e) 1L  # treat uncomparable types as changed
            )
            if (val_check > 0) {
              current_data_2(run_env()$df2)
            }
          } else {
            current_data_2(run_env()$df2)
          }
        }

        run_result(
          list(
            result = result,
            console_output = console_output,
            error_message = error_message
          )
        )
      })
    })

  })
}