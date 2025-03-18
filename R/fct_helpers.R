###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################


###################################################
# Global Variables
###################################################


release <- "2.00" # RTutor
no_data <- "no_data" #'No Data' # no data is uploaded or selected
user_upload <- "Upload" # data is uploaded by user, used to be called uploaded_data
rna_seq <- "rna_seq"
min_query_length <- 6  # minimum # of characters
max_query_length <- 2000 # max # of characters
language_models <- c("o3-mini", "gpt-4o", "gpt-4o-mini")
names(language_models) <- c("o3 mini", "GPT-4o", "GPT-4o mini")
default_model <- "o3 mini"  # "GPT-4 Turbo"   # "ChatGPT"   # "GPT-4 (03/23)"
api_versions <- list(  # API version list corresponding to selected model, may need adjusting
  "o3-mini" = "2025-01-01",
  "gpt-4o" = "2024-08-01",
  "gpt-4o-mini" = "2024-08-01"
)
max_content_length <- 3000 # max tokens:  Change according to model !!!!
max_content_length_ask <- 3000 # max tokens:  Change according to model !!!!
default_temperature <- 0.2
pre_text <- "Write correct, efficient R code to analyze data."
pre_text_python <- "Write correct, efficient Python code."
after_text <- "Use the df data frame."
max_char_question <- 1000 # max n. of characters in the Q&A
max_data_points <- 10000  # max number of data points for interactive plot
max_levels_factor_conversion <- 5 # Numeric columns will be converted to factor if less than or equal to this many levels
# if a column is numeric but only have a few unique values, treat as categorical
unique_ratio <- 0.05   # number of unique values / total # of rows
max_eda_levels <- 12 # max number of levels in categorical varaible for EDA, ggairs
max_eda_var <- 20 # maximum num of variables in EDA
sqlitePath <- "../../data/usage_data.db" # folder to store the user queries, generated R code, and running results
sqltable <- "usage"
sqltable2 <- "feedback"

# additional prompts to send to ChatGPT
system_role <- "Act as a experienced data scientist and statistician. You will write code following instructions. Do not provide explanation. 
If the goal can be achieved by showing quantitative results, do not produce a plot. When a plot is required, ggplot2 is preferred. 
If multiple plots are generated, try to combine them into one."
system_role_tutor <- "Act as a professor of statistics, computer science, coding, and math. You will respond like answering questions by students.
If the question is in languages other than English, respond in that language. If the question is not remotely related to your expertise, respond with 'No comment'.
Response should be structured HTML with proper formatting and spacing."

# If this file exists, running on the server. Otherwise local. This is used to change app behavior.
on_server <- "on_server.txt"



###################################################################
# Load Data & Demo Prompts
###################################################################

######### Load Built-In Data with Base R #########

# Prepare a list of available built-in datasets
available_datasets <- data()$results[, 3] # name of datasets
available_datasets <- sort(gsub(" .*", "", available_datasets)) # clean and sort

# Filter to include only data frames & matrices
available_datasets <- Filter(function(x)
  is.data.frame(get(x, envir = .GlobalEnv)) ||
  is.matrix(get(x, envir = .GlobalEnv)),
  available_datasets
)

# Add diamonds and mpg df to list
available_datasets <- c(available_datasets, "diamonds", "mpg", rna_seq)

# Order the datasets
order <- c("mpg", "iris", "diamonds", rna_seq, "airquality", "CO2",
           "ToothGrowth", "pressure", "ChickWeight")
available_datasets <- c(
  intersect(order, available_datasets),
  setdiff(available_datasets, order)
)
data_placeholder <- "Demo:"
# Append dummy values for user-uploaded data & no data
available_datasets <- c(data_placeholder, no_data, available_datasets,
  user_upload
)

# Define the datasets to rename & their desired display names
rename_map <- c(
  "no_data" = "No Data",
  "iris" = "Iris",
  "mpg" = "MPG",
  "diamonds" = "Diamonds",
  "airquality" = "Air Quality",
  "CO2" = "CO2",
  "ToothGrowth" = "Tooth Growth",
  "pressure" = "Pressure",
  "ChickWeight" = "Chick Weights",
  "rna_seq" = "RNA Seq",
  "Upload" = "Upload"
)

# Update the names of `available_datasets` using the mapping
available_datasets <- setNames(
  available_datasets,
  ifelse(available_datasets %in% names(rename_map),
         rename_map[available_datasets],
         available_datasets)  # Keep the original name if not in rename_map
)


# load demo requests for different datasets (demo questions)
demo <- read.csv(app_sys("app", "www", "demo_questions.csv"))

# load jokes
jokes <- demo[
  which(demo$data == "jokes"),
  "requests"
]


###################################################################
# Prepare User Input & Command & API Key
###################################################################


#' Prepare User input.
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param txt A string that stores the user input.
#' @param selected_data Name of the dataset.
#' @param df the data frame
#' @param use_python  whether or not using python instead of R
#' @param chunk_id  first or not? First chunk add data description
#' @param send_head  send 5 rows of data (& data desc.) to LLM? default of TRUE
#' @param df2 the second data frame
#'
#' @return Returns a cleaned up version, so that it could be sent to GPT.
prep_input <- function(txt, selected_data, df, use_python, chunk_id, send_head, df2) {

  if (is.null(txt) || is.null(selected_data)) {
    return(NULL)
  }
  # if too short, do not send.
  if (nchar(txt) < min_query_length || nchar(txt) > max_query_length) {
    return(NULL)
  }

  # remove extra space at the end.
  txt <- gsub(" *$|\n*$", "", txt)
  # some times it is like " \n "
  txt <- gsub(" *$|\n*$", "", txt)
  # if last character is not a period. Add it. Otherwise,
  # Davinci will try to complete a sentence.
  if (!grepl("\\.$|?", txt)) {
    txt <- paste(txt, ".", sep = "")
  }

  if (!is.null(selected_data)) {
    if (selected_data != no_data) {
      # variables mentioned in request
      relevant_var <- sapply(
        colnames(df),
        function(x) {
          # hwy. class
          grepl(
            tolower(
              paste0(
                " ", # proceeding space
                x,
                "[ |\\.|,|?]" # ending space, comma, period, or question mark
              )
            ),
            txt
          )
        }
      )
      relevant_var <- names(relevant_var)[relevant_var]

      data_info <- describe_df(
        df,
        list_levels = TRUE,
        relevant_var = relevant_var,
        send_head = send_head
      )

      txt <- paste(txt, after_text)  # Always add 'use the df data frame.'

      # in a session, sometimes the first chunk has the id of 0. sometimes 1???

      # add data description
      # if it is not the first chunk and data description is long, do not add
      n_words <- tokens(data_info)
      more_info <- chunk_id <= 1 || n_words < 200
      if (more_info && !(chunk_id > 1 && n_words > 600)) {
        txt <- paste(txt, data_info)
      }

      # if there is a second data frame, add that too.
      if (!is.null(df2)) {
        df2_name <- "df2"

        # 2nd data must be specifically called
        if (grepl(df2_name, txt)) {
          data_info_2 <- describe_df(
            df2,
            list_levels = TRUE,
            relevant_var = relevant_var,
            send_head = send_head
          )
          data_info_2 <- gsub("df data frame", paste0(df2_name, " data frame"), data_info_2)

          n_words <- tokens(data_info_2)
          if (more_info && !(chunk_id > 1 && n_words > 600)) {
            txt <- paste(txt, data_info_2)
          }
        }
      }

    }
  }

  txt <- paste(
    ifelse(
      use_python,
      pre_text_python,
      pre_text
    ),
    txt
  )

  # replace newline with space.
  txt <- gsub("\n", " ", txt)
  return(txt)
}



#' Describe data frame
#'
#' Returns information on data frame describing columns
#'
#' @param df a data frame
#' @param list_levels whether to list levels for factors
#' @param relevant_var  a list of variables mentioned by the user
#' @param send_head logical; whether to include a preview of the data frame
#' @return Returns a cleaned up version, so that it can be executed as an R command
describe_df <- function(df, list_levels = FALSE, relevant_var = NULL, send_head = TRUE) {
  # Identify column types
  is_numeric <- sapply(df, is.numeric)
  is_factor <- sapply(df, is.factor)

  numeric_var <- names(df)[is_numeric]
  categorical_var <- names(df)[is_factor]

  # Initialize description
  data_info <- c()

  # Numeric variables
  if (length(numeric_var) > 0) {
    data_info <- c(data_info, sprintf(
      "The df data frame %s %s.",
      if (length(numeric_var) == 1) 
        "has a column that contains a numeric variable"
      else "contains these numeric variables:",
      paste(numeric_var, collapse = ", ")
    ))
  }

  # Categorical variables
  if (length(categorical_var) > 0) {
    data_info <- c(data_info, sprintf(
      "The df data frame %s %s.",
      if (length(categorical_var) == 1)
        "has a column that contains a categorical variable"
      else "contains these categorical variables:",
      paste(categorical_var, collapse = ", ")
    ))
  }

  # List levels for categorical variables if requested
  if (list_levels && length(relevant_var) > 0) {
    relevant_cat_var <- intersect(relevant_var, categorical_var)
    for (var in relevant_cat_var) {
      levels_freq <- sort(table(df[[var]]), decreasing = TRUE)
      max_levels <- min(length(levels_freq), 4)
      levels_str <- paste(names(levels_freq)[1:max_levels], collapse = "', '")

      data_info <- c(data_info, sprintf(
        "The categorical variable %s has these levels: '%s'%s.",
        var,
        levels_str,
        if (length(levels_freq) > 4) ", etc" else ""
      ))
    }
  }

  # Add sample rows if requested
  if (send_head && (nrow(df) >= 5)) {
    n_samples <- 5
    sample_rows <- capture.output(as.data.frame(df[sample(nrow(df), n_samples), , drop = FALSE]))

    # Reduce samples if output is too long
    if (sum(nchar(sample_rows)) > 1000) {
      n_samples <- 2
      sample_rows <- capture.output(as.data.frame(df[sample(nrow(df), n_samples), , drop = FALSE]))
    }

    # Only add if not too long
    if (sum(nchar(sample_rows)) <= 2000) {
      data_info <- c(data_info,
                     "The df data frame looks like this: \n",
                     paste(sample_rows, collapse = "\n"))
    }
  }

  paste(data_info, collapse = " ")
}



#' Clean up R commands generated by GTP
#'
#' The response from GTP3 sometimes contains strings that are not R commands.
#'
#' @param cmd A string that stores the completion from GTP3.
#' @param selected_data, name of the selected dataset.
#' @param on_server, whether or not running on the server.
#' @return Returns a cleaned up version, so it can be executed as an R command.
clean_cmd <- function(cmd, selected_data, on_server = FALSE) {
  req(cmd)
  # simple way to check
  if (grepl("That model is currently overloaded with other requests.|Error:", cmd)) {
    return(NULL)
  }
  # Use cat to converts \n to newline
  # use capture.output to get the string
  cmd <- capture.output(
    cat(cmd)
  )
  cmd <- polish_cmd(cmd)

  # replace install.packages by "#install.packages"
  cmd <- gsub("install.packages", "#install.packages", cmd)

  # prevent running system commands, malicious
  # system("...")  --> #system("...")
  cmd <- gsub("system *\\(", "#system\\()", cmd)
  cmd <- gsub("source *\\(", "#source\\()", cmd)
  cmd <- gsub("unlink *\\(", "#unlink\\()", cmd)
  cmd <- gsub(
    "(link|dir|link)_(create|delete|chmod|chown|move) *\\(",
    "#MASKED_FILE_OPERATION\\(", cmd
  )

  # use pacman, load if installed; otherwise install it first then load.
  if (!on_server) {
    cmd <- gsub("library\\(", "pacman::p_load\\(", cmd)
  }
  #if (selected_data != no_data) {
  #  cmd <- c("df <- as.data.frame(current_data())", cmd)
  #}

  return(cmd)

}


#' Remove Markdown and explanation
#'
#' The response from GTP3 sometimes contains strings that are not R commands.
#'
#' @param cmd A string that stores the completion from GTP3.
#' @return Returns a cleaned up version, so that it could be executed as R command.
polish_cmd <- function(cmd) {

  cmd <- gsub("``` *", "```", cmd)
  #                    ```{python} ```{r}               ```python                    ^```
  cmd <- gsub(".*(```\\{(PYTHON|Python|python|R|r|bash|sql|js|rcpp|css)\\}|```(PYTHON|Python|python|R|r|bash|sql|js|rcpp|css)|^```)", "", cmd)

  # remove anything after ```
  cmd <- gsub("```.*", "", cmd)

  # sometimes ChatGPT returns \r\n as new lines. The \r causes error.
  cmd <- gsub("\r", "", cmd)

  return(paste0("\n", cmd))
}


#' Estimate tokens from text
#'
#'
#' @param text a string
#'
#' @return a number
#'
tokens <- function(text) {
  # Approximate tokenization by splitting on spaces and punctuations
  tokens <- unlist(strsplit(text, "[[:space:]]|[[:punct:]]"))

  # Filter out empty tokens
  tokens <- tokens[nchar(tokens) > 0]

  # Further split longer tokens (this is a very crude approximation)
  long_tokens <- tokens[nchar(tokens) > 3]
  additional_tokens <- sum(nchar(long_tokens) %/% 4)

  total_tokens <- length(tokens) + additional_tokens

  return(total_tokens)
}


#' Estimate API cost
#'
#'
#' @param prompt_tokens a number
#' @param completion_tokens a number
#' @param selected_model a string
#'
#' @return a number
#'
api_cost <- function(prompt_tokens, completion_tokens, selected_model) {
  if (grepl("gpt-4", selected_model)) { # gpt4
    # input token $0.03 / 1k token, Output is $0.06 / 1k for GPT-4
    completion_tokens * 6e-5 + prompt_tokens  * 3e-5
  } else {
    # ChatGPT
    completion_tokens * 2e-6 + prompt_tokens  * 1.5e-6
  }
}


###################################################################
# Prepare Data
###################################################################


#' Returns true only defined and has a value of true
#'
#' This is used when some the input variables are not defined globally,
#' But could be turned on. if you use if(input$selected), it will
#' give an error.
#'
#' @param x
#'
#' @return Returns TRUE or FALSE
turned_on <- function(x) {

  # length = 0 when NULL, length = 3 if vector
  if (length(x) != 1) {
    return(FALSE)
  } else {

    # contain logical value?
    if (!is.logical(x)) {
      return(FALSE)
    } else {
      # return the logical value.
      return(x)
    }
  }
}


#' Returns a data frame with some numeric columns with fewer levels
#' converted as factors
#'
#'
#' @param df a data frame
#' @param max_levels_factor  max levels, defaults to max_levels
#' @param max_proportion_factor max proportion
#'
#' @return Returns a data frame
numeric_to_factor <- function(df, max_levels_factor, max_proportion_factor) {
  # Identify columns to convert
  convert_index <- sapply(df, function(x) {
    # Check if numeric or character
    if ((is.numeric(x) || is.character(x)) &&
          # Few unique values compared to total values
          length(unique(x)) / length(x) < max_proportion_factor &&
          # Less than specified max levels
          length(unique(x)) <= max_levels_factor) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  # Convert identified columns to factors
  convert_var <- names(df)[convert_index]
  for (var in convert_var) {
    df[[var]] <- factor(df[[var]])
  }

  return(df)
}


###################################################################
# Miscellaneous
###################################################################


#' Plot missing values
#'
#'
#' @param df a dataframe
#'
#' @return a plot
#'
#ploting missing values
missing_values_plot <- function(df) {
  req(!is.null(df))

  # Calculate the total number of missing values per column
  missing_values <- sapply(df, function(x) sum(is.na(x)))

  # Calculate the number of cases with at least one missing value
  cases_with_missing <- sum(apply(df, 1, function(x) any(is.na(x))))

  # Check if there are any missing values
  if (all(missing_values == 0)) {
    return(NULL)
  } else {
    # Create a data frame for plotting
    missing_data_df <- data.frame(
      Column = c(names(missing_values), "At Least One Missing"),
      MissingValues = c(missing_values, cases_with_missing)
    )

    # Plot the number of missing values for all columns with labels
    ggplot(missing_data_df, aes(x = Column, y = MissingValues, fill = Column)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.0f%%", MissingValues / nrow(df) * 100)),
                hjust = -5) + # Bar labels
      coord_flip() + # Makes the bars horizontal
      labs(
        title = "Number of Missing Values by Column",
        x = "Column",
        y = "Number of Missing Values"
      ) +
      scale_fill_brewer(palette = "Set3") + # Color palette for different bars
      theme(
        legend.position = "none",
        axis.title.y = element_blank()
      ) + # No legend
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) # Extend the y-axis limits by 10%
  }
}


# RMarkdown file's Header for knit python chunks
Rmd_script_python <-
  "---
  output: html_fragment
  params:
    df:
  printcode:
    label: \"Display Code\"
    value: TRUE
    input: checkbox
  ---

  ```{r, echo=FALSE, message=FALSE, warning=FALSE}
  library(reticulate)
  df <- params$df
  ```

  ```{python, echo = FALSE, message=FALSE}
  df = r.df
  ```

  #### Results:"


#' Generate html file from Python code
#'
#'
#' @param python_code, a chunk of code
#' @param html_file file name for output
#' @param select_data selected_dataset_name
#' @param current_data   current_data()
#'
#' @return -1 if failed. If success, the the designated html file is written
#'
python_html <- function(python_code, select_data, current_data) {
  withProgress(message = "Running Python...", {
    incProgress(0.2)
    temp_rmd <- paste0(tempfile(), "_temp.Rmd")

    # html file is generated using the same file name except the extension
    html_file <- gsub("Rmd$", "html", temp_rmd)

    Rmd_script <- paste0(
      Rmd_script_python,
      "\n```{python, echo=FALSE}\n",
      python_code,
      "\n```\n"
    )
    write(
      Rmd_script,
      file = temp_rmd,
      append = FALSE
    )

    # Set up parameters to pass to Rmd document
    params <- list(df = iris) # dummy

    # if uploaded, use that data
    req(select_data)
    if (select_data != no_data) {
      params <- list(
        df = current_data
      )
    }

    req(params)
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    try(
      rmarkdown::render(
        input = temp_rmd, # markdown_location,
        params = params,
        envir = new.env(parent = globalenv())
      )
    )
  })  # progress bar

  if (file.exists(html_file)) {
    return(html_file)
  } else {
    return(-1)
  }
}


### Azure API Calling ###

#' Create Chat Completion Azure
#'
#' Creates a completion for the chat message. See [this
#' page](https://platform.openai.com/docs/api-reference/chat/create) for
#' details.
#'
#' For arguments description please refer to the [official
#' documentation](https://platform.openai.com/docs/api-reference/chat/create).
#'
#' @param model required; a length one character vector.
#' @param messages required; defaults to `NULL`; a list in the following
#'   format: `list(list("role" = "user", "content" = "Hey! How old are you?")`
#' @param temperature required; defaults to `1`; a length one numeric vector
#'   with the value between `0` and `2`.
#' @param top_p required; defaults to `1`; a length one numeric vector with the
#'   value between `0` and `1`.
#' @param n required; defaults to `1`; a length one numeric vector with the
#'   integer value greater than `0`.
#' @param stream required; defaults to `FALSE`; a length one logical vector.
#'   **Currently is not implemented.**
#' @param stop optional; defaults to `NULL`; a character vector of length
#'   between one and four.
#' @param max_tokens required; defaults to `(4096 - prompt tokens)`; a length
#'   one numeric vector with the integer value greater than `0`.
#' @param presence_penalty required; defaults to `0`; a length one numeric
#'   vector with a value between `-2` and `2`.
#' @param frequency_penalty required; defaults to `0`; a length one numeric
#'   vector with a value between `-2` and `2`.
#' @param logit_bias optional; defaults to `NULL`; a named list.
#' @param user optional; defaults to `NULL`; a length one character vector.
#' @param openai_api_key required; defaults to `Sys.getenv("OPENAI_API_KEY")`
#'   (i.e., the value is retrieved from the `.Renviron` file); a length one
#'   character vector. Specifies OpenAI API key.
#' @param openai_organization optional; defaults to `NULL`; a length one
#'   character vector. Specifies OpenAI organization.
#' @param seed optional; defaults to `NULL`; a length one
#'   numeric vector with integer values. Attempts deterministic sampling.
#' @return Returns a list, elements of which contain chat completion(s) and
#'   supplementary information.
#' @examples \dontrun{
#' create_chat_completion_azure(
#'    model = "gpt-4o-mini",
#'    api_version = "2023-03-15",
#'    messages = list(
#'        list(
#'            "role" = "system",
#'            "content" = "You are a helpful assistant."
#'        ),
#'        list(
#'            "role" = "user",
#'            "content" = "Who won the world series in 2020?"
#'        ),
#'        list(
#'            "role" = "assistant",
#'            "content" = "The LA Dodgers won the World Series in 2020."
#'        ),
#'        list(
#'            "role" = "user",
#'            "content" = "Where was it played?"
#'        )
#'    )
#' )
#' }
#' #export
create_chat_completion_azure <- function(
  model,
  api_version,
  messages = NULL,
  temperature = 0.2,
  top_p = 0.95,
  n = 1,
  stream = FALSE,
  stop = NULL,
  max_tokens = NULL,
  presence_penalty = 0,
  frequency_penalty = 0,
  logit_bias = NULL,
  user = NULL,
  openai_api_key = Sys.getenv("AZURE_OPENAI_API_KEY"),
  endpoint = Sys.getenv("AZURE_OPENAI_API_ENDPOINT"),
  seed = NULL
) {

  #---------------------------------------------------------------------------
  # Validate Arguments

  if (is.null(openai_api_key) || openai_api_key == "") {
    stop("Error: AZURE_OPENAI_API_KEY is missing. Set it in your environment variables.", call. = FALSE)
  }
  if (is.null(endpoint) || endpoint == "") {
    stop("Error: AZURE_OPENAI_API_ENDPOINT is missing. Set it in your environment variables.", call. = FALSE)
  }

  #---------------------------------------------------------------------------
  # Build path parameters/API URL

  api_call <- glue::glue("{endpoint}openai/deployments/{model}/chat/completions?api-version={api_version}-preview")

  headers <- httr::add_headers(
    `Content-Type` = "application/json",
    `api-key` = openai_api_key
  )

  #---------------------------------------------------------------------------
  # Build Request Body Based on Model

  if (model == "o3-mini") {
    # Use max_completion_tokens instead of max_tokens & exclude temperature
    body <- list(
      model = model,
      messages = messages,
      n = n,
      stream = stream,
      stop = stop,
      max_completion_tokens = max_tokens,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      logit_bias = logit_bias,
      user = user,
      seed = seed
    )
  } else {
    # Default case: Include temperature & max_tokens
    body <- list(
      model = model,
      messages = messages,
      temperature = temperature,
      top_p = top_p,
      n = n,
      stream = stream,
      stop = stop,
      max_tokens = max_tokens,
      presence_penalty = presence_penalty,
      frequency_penalty = frequency_penalty,
      logit_bias = logit_bias,
      user = user,
      seed = seed
    )
  }

  #---------------------------------------------------------------------------
  # Make a request and parse it
  response <- httr::POST(
    api_call,
    headers,
    body = body,
    encode = "json"
  )

  parsed <- response %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  #---------------------------------------------------------------------------
  # Check whether request failed and return parsed
  if (httr::http_error(response)) {
    stop(paste0(
      "OpenAI API request failed [",
      httr::status_code(response),
      "]:\n\n",
      parsed$error$message
    ), call. = FALSE)
  }
  parsed
}



### Save User Data and Feedback ###

# #' Creates a SQLite database file for collecting user data
# #'
# #' The data file should be stored in the ../../data folder inside
# #' the container. From outside in the RTutor_server folder,
# #' it is in data folder.
# #'  Only works on local machines. Not on linux.
# #' @return nothing
# create_usage_db <- function() {
#   # if db does not exist, create one
#   if(!file.exists(sqlitePath)) {
#     db <- RSQLite::dbConnect(RSQLite::SQLite(), gsub(".*/", "", sqlitePath))
#     txt <- sprintf(
#       paste0(
#       "CREATE TABLE ",
#         sqltable,
#         "(\n",
#         "date DATE NOT NULL,
#         time TIME NOT NULL,
#         request varchar(5000),
#         code varchar(5000),
#         error int ,
#         data_str varchar(5000))"
#       )
#     )
#       # Submit the update query and disconnect
#       RSQLite::dbExecute(db, txt)
#       RSQLite::dbDisconnect(db)
#   }
# }
# # To create a database under Ubuntu
# # sudo apt update
# # sudo apt install sqlite3
# # cd ~/Rtutor_server/data
# # sudo  sqlite3 usage_data.db
# # CREATE TABLE usage (
# #        date DATE NOT NULL,
# #        time TIME NOT NULL,
# #        request varchar(5000),
# #        code varchar(5000),
# #        error int,
# #        data_str varchar(5000),
# #       dataset varchar(100));
# # sudo chmod a+w usage_data.db
# # note that error column, 1 means error, 0 means no error, success.
#' Saves user queries, code, and error status
#'
#' @param date Date in the format of "2023-01-04"
#' @param time Time "13:05:12"
#' @param request, user request
#' @param code AI generated code
#' @param error status, TRUE, error
#' @param chunk, id, from 1, 2, ...
#' @param api_time  time in seconds for API response
#' @param tokens  total completion tokens
#' @param filename name of the uploaded file
#' @param filesize size
#'
#' @return nothing
save_data <- function(
  date, time, request, code, error_status,
  data_str, dataset, session, filename,
  filesize, chunk, api_time, tokens, language
) {
  # if db does not exist, create one
  if (file.exists(sqlitePath)) {
    # Connect to the database
    db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlitePath, flags = RSQLite::SQLITE_RW)
    # Construct the update query by looping over the data fields
    txt <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      sqltable,
      "date, time, request, code, error, data_str, dataset, session, filename, filesize, chunk, api_time, tokens, language",
      paste(
        c(
          as.character(date),
          as.character(time),
          clean_txt(request),
          clean_txt(code),
          as.integer(error_status),
          clean_txt(data_str),
          dataset,
          session,
          filename,
          filesize,
          chunk,
          api_time,
          tokens,
          language
        ),
        collapse = "', '"
      )
    )
    # Submit the update query and disconnect
    try(
      RSQLite::dbExecute(db, txt)
    )
    RSQLite::dbDisconnect(db)
  }
}
# SQLite command to create feedback table
# "CREATE TABLE feedback (
#        date DATE NOT NULL,
#        time TIME NOT NULL,
#        helpfulness varchar(50),
#        experience varchar(50),
#        comments varchar(5000)); "


#' Clean up text strings for inserting into SQL
#'
#'
#' @param x a string that can contain ' or "
#'
#' @return nothing
clean_txt <- function(x) {
  return(gsub("\'|\"", "", x))
}


#' Save user feedback
#'
#'
#' @param date Date in the format of "2023-01-04"
#' @param time Time "13:05:12"
#' @param comments, user request
#' @param helpfulness rating
#' @param experience  R experience
#'
#' @return nothing
save_comments <- function(date, time, comments, helpfulness, experience) {
  # if db does not exist, create one
  if (file.exists(sqlitePath)) {
    # Connect to the database
    db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlitePath, flags = RSQLite::SQLITE_RW)
    # Construct the update query by looping over the data fields
    txt <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      sqltable2,
      "date, time, comments, helpfulness, experience",
      paste(
        c(
          as.character(date),
          as.character(time),
          clean_txt(comments),
          helpfulness,
          experience
        ),
        collapse = "', '"
      )
    )
    # Submit the update query and disconnect
    try(
      RSQLite::dbExecute(db, txt)
    )
    RSQLite::dbDisconnect(db)
  }
}


### Lists - FAQ & Updates ###

# Create a data frame with questions and answers for FAQ section
# Used in faq_list component
faqs <- data.frame(
  question = c(
    "What is RTutor.ai?",
    "How does RTutor.ai work?",
    "Is my data uploaded to OpenAI?",
    "Who is it for?",
    "How do you make sure the results are correct?",
    "Can you use RTutor to do R coding homework?",
    "Can private companies use RTutor?",
    "Can you run RTutor locally?",
    "Why do I get different results with the same request?",
    "Can people without R coding experience use RTutor for statistical analysis?",
    "Can this replace statisticians or data scientists?",
    "How do I write my request effectively?",
    "Can I install R packages in the AI generated code?",
    "Can I upload big files to the site?",
    "Voice input does not work!"
  ),
  answer = c(
    "RTutor.ai is an artificial intelligence (AI)-based app that enables users to interact with their data via natural language. Users ask questions about or request analyses in English. The app generates and runs R code to answer that question with plots and numeric results.",  #After uploading a dataset, users ask questions about or request analyses in English. The app generates and runs R code to answer that question with plots and numeric results.",
    "The requests are structured and sent to OpenAI’s AI system, which returns R code. The R code is cleaned up and executed in a Shiny environment, showing results or error messages. Multiple requests are logged to produce an R Markdown file, which can be knitted into an HTML report. This enables record keeping and reproducibility.",
    "By default, 5 randomly selected rows are sent to OpenAI to provide precise code results. You may opt out of this in the settings tab. All of the column names of your data are sent to OpenAI as a prompt to generate R code as well. Your data is not stored on our server after the session.",
    "The primary goal is to help people with some R experience to learn R or be more productive. RTutor can be used to quickly speed up the coding process using R. It gives you a draft code to test and refine. Be wary of bugs and errors.",
    "Try to word your question differently and try the same request several times. Then users can double-check to see if they get the same results from different runs.",  #A higher temperature parameter will give diverse choices. Then users can double-check to see if they get the same results from different runs.",
    "No. That would defeat the purpose. You need to learn R coding properly to be able to tell if the generated R coding is correct.",
    "No. It can be tried as a demo. RTutor website and source code are freely available for non-profit organizations only and distributed using the CC NC 3.0 license.",
    "Yes. Download the R package and install it locally. Then you need to obtain an API key from OpenAI.",
    "OpenAI’s language model has a certain degree of randomness when giving results, controlled by a 'temperature' parameter. Though this is set low, the app still may produce varying results.", #"OpenAI’s language model has a certain degree of randomness that could be adjusted by parameters called 'temperature'. Set this in Settings.",
    "Not entirely. This is because the generated code can be wrong. However, it could be used to quickly conduct data visualization and exploratory data analysis (EDA). Just be mindful of this experimental technology.",
    "No. But RTutor can make them more efficient.",
    "Imagine you have a summer intern, a college student who took one semester of statistics and R. You send the intern emails with instructions, and he/she sends back code and results. The intern is not experienced, thus error-prone, but is hard-working. Thanks to AI, this intern is lightning-fast and nearly free.",
    "No. But we are working to pre-install all the top 5000 most frequently used R packages on the server. Chances are that your favorite package is already installed.",
    "Not if it is more than 10MB. Try to get a small portion of your data. Upload it to the site to get the code, which can be run locally on your laptop. Alternatively, download the RTutor R package and use it from your computer.",
    "One of the main reasons is that your browser blocks the website from accessing the microphone. Make sure you access the site using https://RTutor.ai. With http, microphone access is automatically blocked in Chrome. Speak closer to the mic. Make sure there is only one browser tab using the mic."
  ),
  stringsAsFactors = FALSE
)


# Create a data frame with update versions and descriptions
# Used in site_updates_table component
site_updates_df <- data.frame(
  Version = c(
    "V2.00", "V1.02",
    "V1.01", "V1.0", "V0.99",
    "V0.98.3", "V0.98.2", "V0.98",
    "V0.97", "V0.96", "V0.95",
    "V0.94", "V0.93", "V0.92",
    "V0.91", "V0.90", "V0.8.6",
    "V0.8.5", "V0.8.4", "V0.8.3",
    "V0.8.2", "V0.8.1", "V0.8.0",
    "V0.7.6", "V0.7.5", "V0.7",
    "V0.6", "V0.5", "V0.4",
    "V0.3", "V0.2", "V0.1"
  ),
  Date = c(
    "12/18/2024", "10/8/2024",
    "8/30/2024","8/20/2024", "7/30/2024",
    "11/1/2023","11/1/2023","10/28/2023",
           "10/23/2023","9/26/2023","6/11/2023",
           "4/21/2023","3/26/2023","3/8/2023",
           "2/6/2023","1/15/2023","1/8/2023",
           "1/6/2023","1/5/2023","1/5/2023",
           "1/4/2023","1/3/2023","1/3/2023",
           "12/31/2022","12/31/2022","12/27/2022",
           "12/27/2022","12/24/2022","12/23/2022",
           "12/20/2022","12/16/2022","12/11/2022"),
  Description = c(
    "Modularize Backend",
    "Add option to delete code chunks",
    "Bug Fixes: API Key Validation, EDA Report Download",
    "Redesign UI; Create Privacy Policy, Terms & Conditions; Fix Data Types Bug; Add Data Revert Option",
    "Fix Rplots.pdf error",
    "Fix issue with EDA report when the target variable is categorical or not specified.",
    "Comprehensive EDA report!",
    "Ask questions about code, error. Second data file upload.",
    "GPT-4 becomes the default. Make ggplot2 a preferred method for plotting. Use R environment to enable successive data manipulation.",
    "Include column names in all requests. GPT-4 is available.",
    "ChatGPT(gpt-3.5-turbo) becomes default model.",
    "Interactive plots using CanvasXpress.",
    "Change data types. Add data description. Improve voice input.",
    "Includes description of data structure in prompt.",
    "Voice input is improved. Just enable microphone and say Tutor...",
    "Generates and runs Python code in addition to R!",
    "Add description of the levels in factors.",
    "Demo in many foreign languages.",
    "Collect user feedback.",
    "Collect some user data for improvement.",
    "Auto-convert first column as row names.",
    "Option to convert some numeric columns with few unique levels to factors.",
    "Add description of columns (numeric vs. categorical).",
    "Add RNA-seq data and example requests.",
    "Redesigned UI.",
    "Add EDA tab.",
    "Keeps record of all code chunks for reuse and report.",
    "Keep current code and continue.",
    "Interactive plot. Voice input optional.",
    "Add voice recognition.",
    "Add temperature control. Server reboot reminder.",
    "Initial launch"
  )
)


#' Map R Column Classes to User-Friendly Types
#'
#' This function takes a column from a dataset and maps its R class
#' to one of the predefined user-friendly categories:
#' "character", "numeric", "integer", "factor", or "Date".
#'
#' @param col A column vector from a dataframe (e.g., `df$column_name`).
#'
#' @return A character string representing the mapped data type.
#' Possible values: `"character"`, `"numeric"`, `"integer"`, `"factor"`, `"Date"`, `"Datetime"`.
#'
#' @export
#'
#' @examples
#' map_class_to_type(Sys.Date())   # Returns "Date"
#' map_class_to_type(42)           # Returns "numeric"
#' map_class_to_type(factor("A"))  # Returns "factor"
#'
map_class_to_type <- function(col) {
  col_class <- class(col)
  
  if ("character" %in% col_class) {
    return("character")
  } else if ("factor" %in% col_class) {
    return("factor")
  } else if ("integer" %in% col_class) {
    return("integer")
  } else if ("numeric" %in% col_class) {
    return("numeric")
  } else if ("Date" %in% col_class) {
    return("Date")  # Pure dates
  } else if (any(c("POSIXct", "POSIXt") %in% col_class)) {
    return("Datetime")  # Preserve Datetime
  } else {
    return("character")  # Default to character if unrecognized
  }
}

