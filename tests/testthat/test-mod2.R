# tests/testthat/test-mod2.R

# to test mod 02, in R terminal: 'testthat::test_file("tests/testthat/test-mod2.R", reporter = "summary")'

library(shiny)

# if (!exists("mod_02_load_data_serv", mode = "function")) {
#   sys.source(file.path("R", "mod_02_load_data.R"), envir = .GlobalEnv)
#   print("Manually Sourced: mod_02_load_data.R")
# }

rna_seq <- "rna_seq"
testthat::test_that("placeholder and NO_DATA result in current_data(NULL)", {
  skip_if_not_installed("janitor")

  # ==== required globals used by the module/UI ====
  old_available_datasets <- get0("available_datasets", ifnotfound = NULL)
  old_data_placeholder   <- get0("data_placeholder", ifnotfound = NULL)
  old_user_upload        <- get0("user_upload", ifnotfound = NULL)
  old_no_data            <- get0("no_data", ifnotfound = NULL)

  on.exit({
    if (!is.null(old_available_datasets)) assign("available_datasets", old_available_datasets, envir = .GlobalEnv)
    if (!is.null(old_data_placeholder))   assign("data_placeholder",   old_data_placeholder,   envir = .GlobalEnv)
    if (!is.null(old_user_upload))        assign("user_upload",        old_user_upload,        envir = .GlobalEnv)
    if (!is.null(old_no_data))            assign("no_data",            old_no_data,            envir = .GlobalEnv)
  }, add = TRUE)

  assign("data_placeholder", "DATA_PLACEHOLDER", envir = .GlobalEnv)
  assign("no_data",         "NO_DATA",          envir = .GlobalEnv)
  assign("user_upload",     "USER_UPLOAD",      envir = .GlobalEnv)
  assign("rna_seq",         "RNA_SEQ",          envir = .GlobalEnv)
  assign("available_datasets",
        c("iris", "NO_DATA", "DATA_PLACEHOLDER", "USER_UPLOAD", "RNA_SEQ"),
        envir = .GlobalEnv)

  # ==== reactive plumbing the module expects ====
  current_data    <- shiny::reactiveVal(NULL)
  current_data_2  <- shiny::reactiveVal(NULL)
  original_data   <- shiny::reactiveVal(NULL)
  original_data_2 <- shiny::reactiveVal(NULL)
  run_env         <- shiny::reactiveVal(new.env(parent = emptyenv()))
  run_env_start   <- shiny::reactiveVal(list())

  submit_button         <- shiny::reactiveVal(0)
  convert_to_factor     <- shiny::reactiveVal(FALSE)
  max_proportion_factor <- shiny::reactiveVal(0.5)
  max_levels_factor     <- shiny::reactiveVal(10)

  # ==== test: placeholder ====
  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id = "ld",
      chunk_selection = NULL,
      current_data = current_data,
      current_data_2 = current_data_2,
      original_data = original_data,
      original_data_2 = original_data_2,
      run_env = run_env,
      run_env_start = run_env_start,
      submit_button = submit_button,
      convert_to_factor = convert_to_factor,
      max_proportion_factor = max_proportion_factor,
      max_levels_factor = max_levels_factor
    ),
    {
      session$setInputs(user_selected_dataset = data_placeholder)
      session$flushReact()
      testthat::expect_true(is.null(current_data()))
      # also check module return’s selected name
      ret <- session$getReturned()
      testthat::expect_equal(ret$selected_dataset_name(), data_placeholder)
    }
  )

  # ==== test: NO_DATA ====
  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id = "ld2",
      chunk_selection = NULL,
      current_data = current_data,
      current_data_2 = current_data_2,
      original_data = original_data,
      original_data_2 = original_data_2,
      run_env = run_env,
      run_env_start = run_env_start,
      submit_button = submit_button,
      convert_to_factor = convert_to_factor,
      max_proportion_factor = max_proportion_factor,
      max_levels_factor = max_levels_factor
    ),
    {
      session$setInputs(user_selected_dataset = no_data)
      session$flushReact()
      testthat::expect_true(is.null(current_data()))
      ret <- session$getReturned()
      testthat::expect_equal(ret$selected_dataset_name(), no_data)
    }
  )
})

testthat::test_that("built-in dataset (iris) loads via get()", {
  skip_if_not_installed("janitor")

  assign("data_placeholder", "DATA_PLACEHOLDER", envir = .GlobalEnv)
  assign("no_data",         "NO_DATA",          envir = .GlobalEnv)
  assign("user_upload",     "USER_UPLOAD",      envir = .GlobalEnv)
  assign("available_datasets", c("iris", "NO_DATA", "DATA_PLACEHOLDER", "USER_UPLOAD"), envir = .GlobalEnv)

  current_data    <- shiny::reactiveVal(NULL)
  current_data_2  <- shiny::reactiveVal(NULL)
  original_data   <- shiny::reactiveVal(NULL)
  original_data_2 <- shiny::reactiveVal(NULL)
  run_env         <- shiny::reactiveVal(new.env(parent = emptyenv()))
  run_env_start   <- shiny::reactiveVal(list())

  submit_button         <- shiny::reactiveVal(0)
  convert_to_factor     <- shiny::reactiveVal(FALSE) # keep off to avoid surprises
  max_proportion_factor <- shiny::reactiveVal(0.5)
  max_levels_factor     <- shiny::reactiveVal(10)

  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id = "ld",
      chunk_selection = NULL,
      current_data = current_data,
      current_data_2 = current_data_2,
      original_data = original_data,
      original_data_2 = original_data_2,
      run_env = run_env,
      run_env_start = run_env_start,
      submit_button = submit_button,
      convert_to_factor = convert_to_factor,
      max_proportion_factor = max_proportion_factor,
      max_levels_factor = max_levels_factor
    ),
    {
      session$setInputs(user_selected_dataset = "iris")
      session$flushReact()

      df <- current_data()
      testthat::expect_true(is.data.frame(df))
      testthat::expect_equal(nrow(df), nrow(iris))
      testthat::expect_identical(names(df), names(iris))
      # original_data should mirror
      testthat::expect_equal(dim(original_data()), dim(df))
    }
  )
})

testthat::test_that("CSV upload is parsed and assigned to current_data()", {
  skip_if_not_installed("janitor")

  assign("data_placeholder", "DATA_PLACEHOLDER", envir = .GlobalEnv)
  assign("no_data",         "NO_DATA",          envir = .GlobalEnv)
  assign("user_upload",     "USER_UPLOAD",      envir = .GlobalEnv)
  assign("available_datasets", c("iris", "NO_DATA", "DATA_PLACEHOLDER", "USER_UPLOAD"), envir = .GlobalEnv)

  current_data    <- shiny::reactiveVal(NULL)
  current_data_2  <- shiny::reactiveVal(NULL)
  original_data   <- shiny::reactiveVal(NULL)
  original_data_2 <- shiny::reactiveVal(NULL)
  run_env         <- shiny::reactiveVal(new.env(parent = emptyenv()))
  run_env_start   <- shiny::reactiveVal(list())

  submit_button         <- shiny::reactiveVal(0)
  convert_to_factor     <- shiny::reactiveVal(FALSE)
  max_proportion_factor <- shiny::reactiveVal(0.5)
  max_levels_factor     <- shiny::reactiveVal(10)

  # create a small csv and feed it to fileInput by setting input$user_file
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(
    data.frame(A = 1:3, B = c("x","y","z")),
    file = tmp, row.names = FALSE
  )

  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id = "ld",
      chunk_selection = NULL,
      current_data = current_data,
      current_data_2 = current_data_2,
      original_data = original_data,
      original_data_2 = original_data_2,
      run_env = run_env,
      run_env_start = run_env_start,
      submit_button = submit_button,
      convert_to_factor = convert_to_factor,
      max_proportion_factor = max_proportion_factor,
      max_levels_factor = max_levels_factor
    ),
    {
      # simulate upload
      session$setInputs(
        user_file = data.frame(
          name = basename(tmp),
          size = file.info(tmp)$size,
          type = "text/csv",
          datapath = tmp,
          stringsAsFactors = FALSE
        )
      )
      session$flushReact()

      # the observer on user_file will set the dataset to USER_UPLOAD
      # but to be resilient, set the select explicitly too:
      session$setInputs(user_selected_dataset = user_upload)
      session$flushReact()

      # triggers the observeEvent on user_selected_dataset
      df <- current_data()
      testthat::expect_true(is.data.frame(df))
      testthat::expect_equal(nrow(df), 3)
      testthat::expect_setequal(names(df), c("a","b")) # cleaned to snake case

      # original_data mirrors current_data
      testthat::expect_equal(dim(original_data()), dim(df))

      # return list exposes selected_dataset_name reactive
      ret <- session$getReturned()
      testthat::expect_equal(ret$selected_dataset_name(), user_upload)
    }
  )
})


## ---- RNA-SEQ BRANCH (reads via app_sys) --------------------------------------
testthat::test_that("RNA_SEQ loads csv via app_sys", {
  library(shiny)

  # ---- temp CSV as packaged file stand-in ----
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  utils::write.csv(utils::head(iris), tmp_csv, row.names = FALSE)

  # app_sys stub -> always return our temp CSV
  fake_app_sys <- function(...) tmp_csv

  # Get the module’s enclosing env so free vars resolve there
  mod_env <- environment(mod_02_load_data_serv)
  if (is.null(mod_env)) mod_env <- .GlobalEnv

  # ---- install app_sys stub in BOTH envs; restore on exit ----
  old_app_sys_global <- get0("app_sys", envir = .GlobalEnv, inherits = FALSE)
  old_app_sys_mod    <- get0("app_sys", envir = mod_env,    inherits = FALSE)
  assign("app_sys", fake_app_sys, envir = .GlobalEnv)
  assign("app_sys", fake_app_sys, envir = mod_env)
  on.exit({
    if (!is.null(old_app_sys_global)) assign("app_sys", old_app_sys_global, envir = .GlobalEnv) else if (exists("app_sys", envir = .GlobalEnv, inherits = FALSE)) rm("app_sys", envir = .GlobalEnv)
    if (!is.null(old_app_sys_mod))    assign("app_sys", old_app_sys_mod,    envir = mod_env)    else if (exists("app_sys", envir = mod_env,    inherits = FALSE)) rm("app_sys", envir = mod_env)
  }, add = TRUE)

  # ---- define constants in BOTH envs ----
  define_const <- function(name, value) { assign(name, value, envir = .GlobalEnv); assign(name, value, envir = mod_env) }
  # Include BOTH variants so whichever the module expects will match
  define_const("data_placeholder", "DATA_PLACEHOLDER")
  define_const("no_data",          "NO_DATA")
  define_const("user_upload",      "USER_UPLOAD")
  # Try both tags the module might use
  rnaseq_tags <- c("RNA_SEQ", "RNA-SEQ")
  define_const("rna_seq", rnaseq_tags[1])  # won't be used directly if module compares literal
  define_const("available_datasets", c(rnaseq_tags, "NO_DATA", "DATA_PLACEHOLDER", "USER_UPLOAD"))

  # ---- reactives required by the module ----
  current_data    <- reactiveVal(NULL); current_data_2  <- reactiveVal(NULL)
  original_data   <- reactiveVal(NULL); original_data_2 <- reactiveVal(NULL)
  run_env         <- reactiveVal(new.env(parent = emptyenv()))
  run_env_start   <- reactiveVal(list())
  submit_button         <- reactiveVal(0)
  convert_to_factor     <- reactiveVal(FALSE)
  max_proportion_factor <- reactiveVal(0.5)
  max_levels_factor     <- reactiveVal(10)

  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id="ld",
      chunk_selection=NULL,
      current_data=current_data,
      current_data_2=current_data_2,
      original_data=original_data,
      original_data_2=original_data_2,
      run_env=run_env,
      run_env_start=run_env_start,
      submit_button=submit_button,
      convert_to_factor=convert_to_factor,
      max_proportion_factor=max_proportion_factor,
      max_levels_factor=max_levels_factor
    ),
    {
      # Try both label variants; bail as soon as data appears
      loaded <- FALSE
      for (tag in c("RNA_SEQ", "RNA-SEQ")) {
        session$setInputs(user_selected_dataset = tag)
        # allow a couple of reactive passes; some modules do multi-step updates
        for (i in 1:4) {
          session$flushReact()
          if (!is.null(current_data())) { loaded <- TRUE; break }
        }
        if (loaded) break
      }

      # Give a more helpful failure if nothing loaded
      testthat::expect_true(loaded, info = "Module did not populate current_data() for either 'RNA_SEQ' or 'RNA-SEQ'")

      df <- current_data()
      testthat::expect_true(is.data.frame(df))
      testthat::expect_equal(nrow(df), nrow(utils::head(iris)))
      testthat::expect_equal(names(df), names(utils::head(iris)))
    }
  )
})

## ---- CONVERT TO FACTOR PATH ---------------------------------------------------
testthat::test_that("convert_to_factor=TRUE transforms eligible columns", {
  library(shiny)

  # create a small built-in 'toy' df in global env so get("toy") works
  toy <- data.frame(a = rep(1:3, each = 2), b = letters[1:6], c = rnorm(6))
  assign("toy", toy, envir = .GlobalEnv)

  assign("data_placeholder", "DATA_PLACEHOLDER", envir = .GlobalEnv)
  assign("no_data",         "NO_DATA",          envir = .GlobalEnv)
  assign("user_upload",     "USER_UPLOAD",      envir = .GlobalEnv)
  assign("available_datasets",
         c("toy","NO_DATA","DATA_PLACEHOLDER","USER_UPLOAD"),
         envir = .GlobalEnv)

  current_data    <- reactiveVal(NULL); current_data_2  <- reactiveVal(NULL)
  original_data   <- reactiveVal(NULL); original_data_2 <- reactiveVal(NULL)
  run_env         <- reactiveVal(new.env(parent = emptyenv()))
  run_env_start   <- reactiveVal(list())
  submit_button         <- reactiveVal(0)
  convert_to_factor     <- reactiveVal(TRUE)  # turn ON
  max_proportion_factor <- reactiveVal(1)     # permissive
  max_levels_factor     <- reactiveVal(5)

  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id="ld",
      chunk_selection=NULL,
      current_data=current_data,
      current_data_2=current_data_2,
      original_data=original_data,
      original_data_2=original_data_2,
      run_env=run_env,
      run_env_start=run_env_start,
      submit_button=submit_button,
      convert_to_factor=convert_to_factor,
      max_proportion_factor=max_proportion_factor,
      max_levels_factor=max_levels_factor
    ),
    {
      session$setInputs(user_selected_dataset = "toy")
      session$flushReact()
      df <- current_data()
      testthat::expect_true(is.data.frame(df))
      testthat::expect_true(is.factor(df$a))    # numeric with few levels → factor
      testthat::expect_false(is.factor(df$c))   # continuous numeric remains numeric
    }
  )
})

## ---- SECOND UPLOAD POPULATES current_data_2 -----------------------------------
testthat::test_that("second file upload populates current_data_2", {
  library(shiny)

  assign("data_placeholder", "DATA_PLACEHOLDER", envir = .GlobalEnv)
  assign("no_data",         "NO_DATA",          envir = .GlobalEnv)
  assign("user_upload",     "USER_UPLOAD",      envir = .GlobalEnv)
  assign("available_datasets",
         c("USER_UPLOAD","NO_DATA","DATA_PLACEHOLDER"),
         envir = .GlobalEnv)

  # temp files for two uploads
  f1 <- tempfile(fileext = ".csv"); f2 <- tempfile(fileext = ".csv")
  on.exit(unlink(c(f1,f2)), add = TRUE)
  utils::write.csv(data.frame(a=1:3, b=4:6), f1, row.names = FALSE)
  utils::write.csv(data.frame(x=7:9, y=letters[1:3]), f2, row.names = FALSE)

  current_data    <- reactiveVal(NULL); current_data_2  <- reactiveVal(NULL)
  original_data   <- reactiveVal(NULL); original_data_2 <- reactiveVal(NULL)
  run_env         <- reactiveVal(new.env(parent = emptyenv()))
  run_env_start   <- reactiveVal(list())
  submit_button         <- reactiveVal(0)
  convert_to_factor     <- reactiveVal(FALSE)
  max_proportion_factor <- reactiveVal(0.5)
  max_levels_factor     <- reactiveVal(10)

  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id="ld",
      chunk_selection=NULL,
      current_data=current_data,
      current_data_2=current_data_2,
      original_data=original_data,
      original_data_2=original_data_2,
      run_env=run_env,
      run_env_start=run_env_start,
      submit_button=submit_button,
      convert_to_factor=convert_to_factor,
      max_proportion_factor=max_proportion_factor,
      max_levels_factor=max_levels_factor
    ),
    {
      # first upload → current_data
      session$setInputs(
        user_file = data.frame(
          name = basename(f1),
          size = file.info(f1)$size,
          type = "text/csv",
          datapath = f1,
          stringsAsFactors = FALSE
        )
      )
      session$setInputs(user_selected_dataset = user_upload)
      session$flushReact()
      testthat::expect_true(is.data.frame(current_data()))

      # second upload → current_data_2
      session$setInputs(
        user_file_2 = data.frame(
          name = basename(f2),
          size = file.info(f2)$size,
          type = "text/csv",
          datapath = f2,
          stringsAsFactors = FALSE
        )
      )
      session$flushReact()
      testthat::expect_true(is.data.frame(current_data_2()))
    }
  )
})

## ---- show_option1 TOGGLES -----------------------------------------------------
testthat::test_that("show_option1 toggles between show/hide", {
  library(shiny)

  assign("data_placeholder", "DATA_PLACEHOLDER", envir = .GlobalEnv)
  assign("no_data",         "NO_DATA",          envir = .GlobalEnv)
  assign("user_upload",     "USER_UPLOAD",      envir = .GlobalEnv)
  assign("available_datasets",
         c("iris","DATA_PLACEHOLDER","NO_DATA","USER_UPLOAD"),
         envir = .GlobalEnv)

  current_data    <- reactiveVal(NULL); current_data_2  <- reactiveVal(NULL)
  original_data   <- reactiveVal(NULL); original_data_2 <- reactiveVal(NULL)
  run_env         <- reactiveVal(new.env(parent = emptyenv()))
  run_env_start   <- reactiveVal(list())
  submit_button         <- reactiveVal(0)
  convert_to_factor     <- reactiveVal(FALSE)
  max_proportion_factor <- reactiveVal(0.5)
  max_levels_factor     <- reactiveVal(10)

  shiny::testServer(
    mod_02_load_data_serv,
    args = list(
      id="ld",
      chunk_selection=NULL,
      current_data=current_data,
      current_data_2=current_data_2,
      original_data=original_data,
      original_data_2=original_data_2,
      run_env=run_env,
      run_env_start=run_env_start,
      submit_button=submit_button,
      convert_to_factor=convert_to_factor,
      max_proportion_factor=max_proportion_factor,
      max_levels_factor=max_levels_factor
    ),
    {
      # initial: submit=0 or placeholder => "show"
      testthat::expect_equal(output$show_option1, "show")

      # after submit clicked AND a concrete dataset selected => "hide"
      submit_button(1)
      session$setInputs(user_selected_dataset = "iris")
      session$flushReact()
      testthat::expect_equal(output$show_option1, "hide")
    }
  )
})