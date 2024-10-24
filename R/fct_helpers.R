###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################



###################################################
# Global variables
###################################################

release <- "1.02" # RTutor LLC
uploaded_data <- "User Upload" # used for drop down
no_data <- "no_data" # no data is uploaded or selected
names(no_data) <- "No data (examples)"
rna_seq <- "rna_seq"  # RNA-Seq read counts
names(rna_seq) <- "RNA-Seq"
min_query_length <- 6  # minimum # of characters
max_query_length <- 2000 # max # of characters
#language_model <- "code-davinci-002	"# "text-davinci-003"
language_models <- c("gpt-4o-2024-08-06",  "gpt-4o-mini", "gpt-3.5-turbo")
names(language_models) <- c("GPT-4o", "GPT-4o mini", "GPT-3.5 Turbo" )
default_model <- "GPT-4o" #"GPT-4 Turbo"    # "ChatGPT" #   "GPT-4 (03/23)"
max_content_length <- 3000 # max tokens:  Change according to model !!!!
max_content_length_ask <- 3000 # max tokens:  Change according to model !!!!
default_temperature <- 0.2
pre_text <- "Write correct, efficient R code to analyze data."
pre_text_python <- "Write correct, efficient Python code."
after_text <- "Use the df data frame."
max_char_question <- 1000 # max n. of characters in the Q&A
max_eda_levels <- 12 # max number of levels in categorical varaible for EDA, ggairs
max_eda_var <- 20 # maximum num of variables in EDA
max_data_points <- 10000  # max number of data points for interactive plot
max_levels_factor_conversion <- 5 # Numeric columns will be converted to factor if less than or equal to this many levels
# if a column is numeric but only have a few unique values, treat as categorical
unique_ratio <- 0.05   # number of unique values / total # of rows
sqlitePath <- "../../data/usage_data.db" # folder to store the user queries, generated R code, and running results
sqltable <- "usage"
system_role <- "Act as a experienced data scientist and statistician. You will write code following instructions. Do not provide explanation. 
If the goal can be achieved by showing quantitative results, do not produce a plot. When a plot is required, ggplot2 is preferred. 
If multiple plots are generated, try to combine them into one."
system_role_tutor <- "Act as a professor of statistics, computer science and mathematics. 
You will respond like answering questions by students. If the question is in languages other than English, respond in that language. 
If the question is not remotely related to your expertise, respond with 'No comment'."
# voice input parameters
wake_word <- "Tutor" #Tutor, Emma, Note that "Hey Cox" does not work very well.
# this triggers the submit button
action_verbs <- c(
  "now", 
  "over",
  "do it", 
  "do it now", 
  "go ahead", 
  "submit", 
  "what are you waiting for"
)

#RMarkdown file's Header for knit python chunks
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

# if this file exists, running on the server. Otherwise local.
# this is used to change app behavior.
on_server <- "on_server.txt"


#' Move an element to the front of a vector
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param v is the vector
#' @param e is the element
#'
#' @return Returns a reordered vector
move_front <- function(v, e){
  ix <- which(v == e)

  # if found, move to the beginning.
  if(length(ix) != 0) {
    v <- v[-ix]
    v <- c(e, v)
  }
  return(v)
}


#' Prepare User input.
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param txt A string that stores the user input.
#' @param selected_data Name of the dataset.
#' @param df the data frame
#' @param use_python  whether or not using python instead of R
#' @param chunk_id  first or not? First chunk add data description
#'
#' @return Returns a cleaned up version, so that it could be sent to GPT.
prep_input <- function(txt, selected_data, df, use_python, chunk_id, selected_model, send_head, df2 = NULL, df2_name = NULL) {

  if(is.null(txt) || is.null(selected_data)) {
    return(NULL)
  } 
  # if too short, do not send. 
  if(nchar(txt) < min_query_length || nchar(txt) > max_query_length) {
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
            paste0(
              " ", # proceeding space
              x,
              "[ |\\.|,|?]" # ending space, comma, period, or question mark
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
      # Always add 'use the df data frame.'
      txt <- paste(txt, after_text)

      n_words <- tokens(data_info)
      #if it is the first chunk;  always do this when Davinci model; or if data description is short
      more_info <- chunk_id <= 1 || selected_model == "text-davinci-003" || n_words < 200

      # in a session, sometimes the first chunk has the id of 0. sometimes 1?????

      # add data descrdiption
      # if it is not the first chunk and data description is long, do not add.
      if (more_info && !(chunk_id > 1 && n_words > 600)) {
        txt <- paste(txt, data_info)
      }
      
      # if there is a second data frame, add that too.
      if(!is.null(df2)) {
        if(is.null(df2_name)) {
          df2_name <- "df2"
        } 

        # 2nd data must be specificall called
        if(grepl(df2_name, txt)) {
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
  #cat("\n", txt)
  return(txt)
}



#' Describe data frame
#'
#' Returns information on data frame describing columns.
#'
#' @param df a data frame
#' @param list_levels whether to list levels for factors
#' @param relevant_var  a list of variables mentioned by the user
#' @return Returns a cleaned up version, so that it could be executed as R command.
describe_df <- function(df, list_levels = FALSE, relevant_var = NULL, send_head = TRUE) {

  data_info <- ""
  numeric_index <- sapply(
    df,
    function(x) {
      if (is.numeric(x)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )

  numeric_var <- colnames(df)[numeric_index]
  cat_var <- colnames(df)[!numeric_index]

  # calculate total number of unique levels
  total_levels <- sapply(cat_var, function(x) {length(unique(df[, x]))})
  # remove columns that are names, strings, etc
  cat_var <- cat_var[total_levels < nrow(df) * 0.8]

  # numeric variables
  if (length(numeric_var) == 1) {
    data_info <- paste0(
      data_info,
      "The df data frame has a column ",
      numeric_var,
      " that contains a numeric variable. "
    )
  } else if (length(numeric_var) > 1) {
    data_info <- paste0(
      data_info,
      "The df data frame contains these numeric variables: ",
      paste0(
        numeric_var[1:(length(numeric_var) - 1)],
        collapse = ", "
      ),
      ", and ",
      numeric_var[length(numeric_var)],
      ". "
    )
  }
  # Categorical variables-----------------------------
  # numeric variables
  if (length(cat_var) == 1) {
    data_info <- paste0(
      data_info,
      "The df data frame has a column ",
      cat_var,
      " that contains a categorical variable. "
    )
  } else if (length(cat_var) > 1) {
    data_info <- paste0(
      data_info,
      "The df data frame contains these categorical variables: ",
      paste0(
        cat_var[1:(length(cat_var) - 1)],
        collapse = ", "
      ),
      ", and ",
      cat_var[length(cat_var)],
      ". "
    )
  }
  
  if(list_levels & length(relevant_var) > 0) {

    # only list for categorical variables specified in user prompt
    relevant_cat_var <- intersect(relevant_var, cat_var)
  # describe the levels in categorical variable
    for (var in relevant_cat_var) {
      max_lelvels_description <- 4
      ix <- match(var, colnames(df))
      factor_levels <- sort(table(df[, ix]), decreasing = TRUE)
      factor_levels <- names(factor_levels)

      # have more than 6 levels?
      many_levels <- FALSE

      if (length(factor_levels) > max_lelvels_description) {
        many_levels <- TRUE
        factor_levels <- factor_levels[1:max_lelvels_description]
      }

      last_level <- factor_levels[length(factor_levels)]
      factor_levels <- factor_levels[-1 * length(factor_levels)]
      tem <- paste0(
        factor_levels,
        collapse = "', '"
      )
      if (!many_levels) { # less than 6 levels
        factor_levels <- paste0("'", tem, "', and '", last_level, "'")
      } else { # more than 6 levels
        factor_levels <- paste0(
          "'",
          tem,
          "', '",
          last_level,
          "', etc"
        )
      }
      data_info <- paste0(
        data_info,
        "The categorical variable ",
        var,
        " has these levels: ",
        factor_levels,
        ". "
      )
    }
  }
  
  if(send_head) {
    #randomly select 5 rows, print out, convert to string
    sample_rows <- paste0(
      capture.output(as.data.frame(df[sample(nrow(df), 5),])), 
      collapse = "\n"
    )
    # if too long, use only 2 rows
    if(nchar(sample_rows) > 1000) {
      sample_rows <- paste0(
        capture.output(as.data.frame(df[sample(nrow(df), 2),])), 
        collapse = "\n"
      )
    }
    sample_rows <- paste(
      "The df data frame looks like this: \n",
      sample_rows
    )
    
    # if still too long, skip
    if(nchar(sample_rows) > 2000) {
      sample_rows <- ""
    }

    data_info <- paste0(
      data_info,
      sample_rows    
    )
  }


  return(data_info)
}


#' Clean up R commands generated by GTP
#'
#' The response from GTP3 sometimes contains strings that are not R commands.
#'
#' @param cmd A string that stores the completion from GTP3.
#' @param selected_data, name of the selected dataset. 
#' @param on_server, whether or not running on the server.
#' @return Returns a cleaned up version, so that it could be executed as R command.
clean_cmd <- function(cmd, selected_data, on_server = FALSE) {
  req(cmd)
  # simple way to check
  if(grepl("That model is currently overloaded with other requests.|Error:", cmd)) {
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
  if(!on_server) {
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



###################################################################
# Prepare data
###################################################################

# A file, demo requests for different datasets, demo questions,
demo <- read.csv(app_sys("app", "www", "demo_questions.csv"))

ix <- which(demo$data == "questions")
demo_questions <- demo$requests[ix]
names(demo_questions) <- demo$name[ix]

jokes <- demo[
  which(demo$data == "jokes"), 
  "requests"
]

# prepare a list of available data sets that are built-in
datasets <- data()$results[, 3] # name of datasets
datasets <- gsub(" .*", "", datasets)

datasets <- sort(datasets)

# if dataset is not data frame or matrix, remove.
ix <- sapply(
  datasets, 
  function(x) {
    eval(
      parse(
        text = paste(
          "is.data.frame(",
          x,
          ") | is.matrix(",
           x, 
           ")"
        )
      )
    )
  }
)
datasets <- datasets[which(ix)]

datasets <- move_front(datasets, "state.x77")
datasets <- move_front(datasets, "iris")
datasets <- move_front(datasets, "mtcars")


# append a dummy value, used when user upload their data.
datasets <- c(datasets, uploaded_data)
# move it to 2nd place
datasets <- move_front(datasets, uploaded_data)

# append a dummy value, used when user do not use any data
datasets <- c(datasets, rna_seq)
# move it to 2nd place
datasets <- move_front(datasets, rna_seq)

# append a dummy value, used when user do not use any data
datasets <- c(datasets, no_data)
# move it to 2nd place
datasets <- move_front(datasets, no_data)

datasets <- move_front(datasets, "diamonds")
# default
datasets <- move_front(datasets, "mpg")

datasets <- setNames(datasets, datasets)

names(datasets)[match("mpg", datasets)] <- "mpg (examples)"
names(datasets)[match("diamonds", datasets)] <- "diamonds (examples)"
names(datasets)[match(rna_seq, datasets)] <- "RNA-Seq (examples)"

colnames(mpg) <- c("maker", "model", "dis", "year", "cylinder", 
  "transmission", "drive", "city", "highway", "fuel", "type")

#' Clean up API key character
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param api_key is a character string
#'
#' @return Returns a string with api key.
clean_api_key <- function(api_key) {
  # remove spaces
  api_key <- gsub(" ", "", api_key)
  return(api_key)
}


# #' Validate API key character
# #'
# #' The response from GPT3 sometimes contains strings that are not R commands.
# #'
# #' @param api_key is a character string
# #'
# #' @return Returns TRUE or FALSE
# validate_api_key <- function(api_key) {
#   valid <- TRUE
#   # if 51 characters, use the one in the file
#   if (nchar(api_key) != 51) {
#     valid <- FALSE
#   }
#   return(valid)
# }


# get API key from environment variable.
api_key_global <- Sys.getenv("OPEN_API_KEY")
key_source <- "from OS environment variable."

# If there is an key file in the current folder, use that instead.
if (file.exists(file.path(getwd(), "api_key.txt"))) {
  api_key_file <- readLines(file.path(getwd(), "api_key.txt"))
  api_key <- clean_api_key(api_key_file)

  # # if valid, replace with file
  # if(validate_api_key(api_key_file)) {
  #   api_key_global <- api_key_file
  #   key_source <- "from file."
  # }
  api_key_global <- api_key_file
  key_source <- "from file."
}


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
numeric_to_factor <- function(df, max_levels_factor, max_proptortion_factor) {
  # some columns looks like numbers but have few levels
  # convert these to factors

  convert_index <- sapply(
    df,
    function(x) {
      if (
        (is.numeric(x) || is.character(x)) &&
        # if there are few unique values compared to total values
        length(unique(x)) / length(x) < max_proptortion_factor &&
        length(unique(x)) <= max_levels_factor  # less than 12 unique values
          # relcassify numeric variable as categorical
      ) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )

  convert_var <- colnames(df)[convert_index]
  for (var in convert_var) {
    eval(
      parse(   #df$cyl <- as.factor(df$cyl)
        text = paste0("df$", var, " <- as.factor(df$", var, ")")
      )
    )
  }
  return(df)

}


#' Creates a SQLite database file for collecting user data
#' 
#' The data file should be stored in the ../../data folder inside 
#' the container. From outside in the RTutor_server folder, 
#' it is in data folder.
#'  Only works on local machines. Not on linux.
#' @return nothing
create_usage_db <- function() {
  # if db does not exist, create one
  if(!file.exists(sqlitePath)) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), gsub(".*/", "", sqlitePath))
    txt <- sprintf(
      paste0(
      "CREATE TABLE ",
        sqltable,
        "(\n",
        "date DATE NOT NULL,
        time TIME NOT NULL,
        request varchar(5000),
        code varchar(5000),
        error int ,
        data_str varchar(5000))"
      )
    )
      # Submit the update query and disconnect
      RSQLite::dbExecute(db, txt)
      RSQLite::dbDisconnect(db)
  }
}

# To create a database under Ubuntu
# sudo apt update
# sudo apt install sqlite3
# cd ~/Rtutor_server/data
# sudo  sqlite3 usage_data.db


# CREATE TABLE usage (
#        date DATE NOT NULL,
#        time TIME NOT NULL,
#        request varchar(5000),
#        code varchar(5000),
#        error int,
#        data_str varchar(5000),
#       dataset varchar(100));

# sudo chmod a+w usage_data.db

# note that error column, 1 means error, 0 means no error, success.


#' Saves user queries, code, and error status
#' 
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
  date,
  time,
  request,
  code,
  error_status,
  data_str,
  dataset,
  session,
  filename,
  filesize,
  chunk,
  api_time,
  tokens,
  language
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

#' Clean up text strings for inserting into SQL
#' 
#'
#' @param x a string that can contain ' or "
#'
#' @return nothing
  clean_txt <- function(x) {
    return(gsub("\'|\"", "", x))
  }



# SQLite command to create feedbck table

# "CREATE TABLE feedback (
#        date DATE NOT NULL,
#        time TIME NOT NULL,
#        helpfulness varchar(50),
#        experience varchar(50),
#        comments varchar(5000)); "


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
        "feedback",
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



#' Generate html file from Python code
#' 
#'
#' @param python_code, a chunk of code 
#' @param html_file file name for output
#' @param select_data input$select data
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
    

    if(file.exists(html_file)) {
      return(html_file)       
    } else {
      return(-1)
    }
}

#' Describes the distribution of a column in a data frame, or a vector.
#' 
#'
#' @param x a vector
#'
#' @return a text string
#' 
distribution_description <- function(x) {

  if (is.numeric(x)) {
    desc <- paste0(" has a ",
                   " mean of ", round(mean(x, na.rm = TRUE), 2),
                   " and standard Deviation of ", round(sd(x, na.rm = TRUE), 2), ". ")
    
    skewness <- moments::skewness(x, na.rm = TRUE)
    if (abs(skewness) < 0.5) {
      desc <- paste0(desc, "The distribution is approximately symmetric.")
    } else {
      if (skewness > 0) {
        if (skewness > 1) {
          desc <- paste0(desc, "The distribution is highly right-skewed.")
        } else {
          desc <- paste0(desc, "The distribution is moderately right-skewed.")
        }
      } else {
        if (skewness < -1) {
          desc <- paste0(desc, "The distribution is highly left-skewed.")
        } else {
          desc <- paste0(desc, "The distribution is moderately left-skewed.")
        }
      }
    }
  } else if (is.factor(x)) {
    desc <- paste0(" has ")
    freq_table <- as.data.frame(table(x))
    top_levels <- freq_table %>% arrange(desc(Freq))
    if(nrow(top_levels) > 3) {
      top_levels <- top_levels[1:2,]
    }
    desc <- paste0(desc, " levels:")
    for (j in 1:nrow(top_levels)) {
      desc <- paste0(desc, " '", top_levels$x[j], "' (", round(100 * top_levels$Freq[j] / sum(freq_table$Freq), 0), "%), ")
    }
  } else if (is.character(x)) {
    desc <- ""
  } else {
    desc <- ""
  }
    
  desc <- gsub(",\\s*$", ".", desc)
  return(desc)
}



#' Generate a plain text about the distribution and correlations.
#' 
#'
#' @param df a data frame.
#'
#' @return a text string
#' 
describe_data <- function(df) {
  p_val_cutoff <- 1e-3
  R_cutoff <- 0.5
  numeric_vars <- sapply(df, is.numeric)
  character_vars <- sapply(df, is.character)

  # Convert character columns to factors if number of unique values is much less than total rows
  df[character_vars] <- lapply(df[character_vars], function(x) {
    if (length(unique(x)) / nrow(df) < 0.1) {
      factor(x)
    } else {
      x
    }
  })

  factor_vars <- sapply(df, is.factor)

  a <- ""

  for (i in 1:(ncol(df))) {
    b <- ""
    for (j in 1:ncol(df)) {
      if (i == j) {
        next
      }
      if (numeric_vars[i] && numeric_vars[j]) {
        cor_test <- cor.test(df[[i]], df[[j]])
        if (cor_test$p.value < p_val_cutoff & abs(cor_test$estimate) > R_cutoff) {
          b <- paste0(b, ", ", colnames(df)[j], " (R=", round(cor_test$estimate, 2), ")")
        }
      } else if (factor_vars[i] && factor_vars[j] && nlevels(df[[i]]) > 1 && nlevels(df[[j]]) > 1) {
        chi_test <- chisq.test(table(df[[i]], df[[j]]))
        if (!is.na(chi_test$p.value) && chi_test$p.value < p_val_cutoff) {
          b <- paste0(b, ", ", colnames(df)[j], " (P=", formatC(chi_test$p.value, format = "e", digits = 1), ")")
        }
      } else if (numeric_vars[i] && factor_vars[j] && nlevels(df[[j]]) > 1) {
        anova_test <- aov(df[[i]] ~ df[[j]])
        anova_p_value <- summary(anova_test)[[1]][["Pr(>F)"]][1]
        if (anova_p_value < p_val_cutoff) {
          b <- paste0(b, ", ", colnames(df)[j], " (P=", formatC(anova_p_value, format = "e", digits = 1), ")")
        }
      } else if (factor_vars[i] && numeric_vars[j] && nlevels(df[[i]]) > 1) {
        anova_test <- aov(df[[j]] ~ df[[i]])
        anova_p_value <- summary(anova_test)[[1]][["Pr(>F)"]][1]
        if (anova_p_value < p_val_cutoff) {
          b <- paste0(b, ", ", colnames(df)[j], " (P=", formatC(anova_p_value, format = "e", digits = 1), ")")
        }
      }
    }

    if (nchar(b) > 0) {
      a <- paste0(a, "The column \'", colnames(df)[i], "\'", distribution_description(df[[i]]), " It has significant correlation with:", b, ".\n\n")
    }
  }
  a <- gsub(":,", ": ", a)
  return(a)
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
  if(grepl("gpt-4", selected_model)) { # gpt4
    # input token $0.03 / 1k token, Output is $0.06 / 1k for GPT-4
    completion_tokens * 6e-5+ prompt_tokens  * 3e-5
  } else {
    # ChatGPT
    completion_tokens * 2e-6+ prompt_tokens  * 1.5e-6 
  }


}

#' Estimate API cost
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
    # Calculate the percentage of missing values per column
    # missing_percentage <- (missing_values / nrow(df)) * 100
    # Plot the number of missing values for all columns with labels
    ggplot(missing_data_df, aes(x = Column, y = MissingValues, fill = Column)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.0f%%", MissingValues / nrow(df) * 100)), hjust = -5) + # Add labels to the bars
      # geom_text(aes(label = sprintf("%.2f%%", MissingPercentage)), hjust = -0.3) +
      coord_flip() + # Makes the bars horizontal
      labs(title = "Number of Missing Values by Column", x = "Column", y = "Number of Missing Values") +
      scale_fill_brewer(palette = "Set3") + # Use a color palette for different bars
      theme(legend.position = "none", axis.title.y = element_blank()) + # Remove the legend
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) # Extend the y-axis limits by 10%
  }
}



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
    "V1.02",
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
  Date = c("10/8/2024",
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

terms_of_use_content <- function() {
  HTML('
    <div class="policy">
      <h1>Terms of Service</h1>
      <p style="font-weight: bold;">Updated Aug. 19th, 2024</p>

      <p>Welcome to Orditus LLC ("we", "us", or "our"). These Terms of Use ("Terms") govern your use of our website RTutor.ai and any services, content, or features provided by us (collectively, the "Services"). By accessing or using our Services, you agree to be bound by these Terms. If you do not agree to these Terms, you may not use our Services.</p>

      <h2>1. Description of Services</h2>
      <p>RTutor is designed to generate and run R and Python code. Using the power of ChatGPT, RTutor translates your natural language into scripts to analyze and interpret data.</p>

      <h2>2. Acceptance of Terms</h2>
      <p>By accessing and using our Services, you accept and agree to be bound by these Terms and our Privacy Policy. If you do not agree to these Terms, you should not use our Services.</p>

      <h2>3. Changes to Terms</h2>
      <p>We reserve the right to modify these Terms at any time. Any changes will be effective immediately upon posting of the revised Terms. Your continued use of the Services after the posting of any changes constitutes your acceptance of the revised Terms.</p>

      <h2>4. User Obligations and Restrictions</h2>
      <p>You agree to use the Services only for lawful purposes and in a manner that does not infringe the rights of, restrict, or inhibit anyone else\'s use and enjoyment of the Services. Prohibited behavior and use includes but is not limited to:</p>
      <p>You must be at least 13 years old to use this Service.<br>
      You must not use this Service commercially in any form without our written consent.<br>
      You agree not to transmit obscene or offensive content.<br>
      You agree not to use this Service to generate content that is misleading, harmful, or illegal.<br>
      You must not overuse the Services in a manner that adversely impacts the performance or availability of the Services for other users.<br>
      You must not upload data that is confidential or personally identifiable without proper anonymization.<br>
      You agree not to attempt to reverse-engineer or otherwise tamper with the AI models and algorithms.<br>
      You must not copy or scrape the content of the Service in any form without our written consent.</p>

      <h2>5. User Content</h2>
      <p>By submitting, posting, or displaying content, you grant us a worldwide, non-exclusive, royalty-free license to use, reproduce, adapt, modify, publish, and distribute such content in any and all media. You are responsible for any data you submit to our Services. By submitting data, you agree that:<br><br>
      You have the necessary rights and permissions to share the data.<br>
      You will not submit any data that violates the rights of others or any applicable laws.<br>
      We may use, store, and process basic information about your data as described in our Privacy Policy.</p>

      <h2>6. Intellectual Property</h2>
      <p>All content, trademarks, and data on this website, including but not limited to software, databases, text, graphics, icons, hyperlinks, private information, designs, and agreements, are the property of or licensed to Orditus LLC and as such are protected from infringement by local and international legislation and treaties. All content, AI models, algorithms, and data used or generated by our Services are the property of Orditus LLC or its licensors and are protected by intellectual property laws. By using our Services, you agree that:
      <br>You retain ownership of the data you submit to the AI assistant.
      <br>You grant us a license to use, reproduce, adapt, and modify your inputs as necessary to provide and improve our Services.
      <br>We retain ownership of all models, algorithms, and enhancements made to the Services based on aggregated and anonymized user data.</p>

      <h2>7. Limitation of Liability</h2>
      <p>To the fullest extent permitted by law, Orditus LLC shall not be liable for any indirect, incidental, special, consequential, or punitive damages arising from or related to your use of the Services, including but not limited to:<br><br>
      <br>Inaccuracies or errors in AI-generated results.
      <br>Loss or corruption of data.
      <br>Misuse of AI-generated insights for decision-making.
      <br>Unauthorized access to or use of your account or data.<br>
      In no event shall Orditus LLC nor its directors, employees, partners, or affiliates, be liable for any indirect, incidental, special, consequential, or punitive damages, including without limitation, loss of profits, data, use, goodwill, or other intangible losses, resulting from:<br>
      Your use or inability to use the Services.<br>
      Any unauthorized access to or use of our servers and/or any personal information stored therein.<br>
      Any breaks or stoppages in our service that affect the sending or receiving of data to or from the Services.</p>

      <h2>8. Termination</h2>
      <p>We may terminate or suspend your access to our Services immediately, without prior notice or liability, for any reason whatsoever, including without limitation if you breach the Terms. Upon termination, your right to use the Services will immediately cease.</p>

      <h2>9. Privacy Policy</h2>
      <p>Your privacy is important to us. Please review our Privacy Policy to understand how we collect, use, and protect your information. We may use this information to improve the Service. By using our Services, you consent to the data practices described in our Privacy Policy.</p>

      <h2>10. Governing Law</h2>
      <p>These Terms shall be governed and construed in accordance with the laws of South Dakota, USA without regard to its conflict of law provisions.</p>

      <h2>11. Contact Us</h2>
      <p style="padding-bottom: 90px;">If you have any questions about these Terms, please contact us at ge@orditus.com.</p>
    </div>
  ')
}

privacy_policy_content <- function() {
  HTML('
    <div class="policy">
      <h1>Privacy Policy</h1>
      <p style="font-weight: bold;">Updated Aug. 23rd, 2024</p>

      <p>We are committed to protecting your privacy. This Privacy Policy explains how ("we", "us", or "our"), Orditus LLC, collects, uses, discloses, and safeguards your information when you use our website RTutor.ai and our services (collectively, the "Services"). By accessing or using the Services, you agree to the terms of this Privacy Policy. If you do not agree with the terms of this Privacy Policy, please do not access or use the Services.</p>

      <h2>1. Information We Collect</h2>
      <h3>1.1 Personal Information</h3>
      <p>We may collect personal information that you provide directly to us when you use our Services or communicate with us. This information may include:<br><br>
      Name<br>
      Email address<br>
      Company name<br><br></p>
      <h3>1.2 Usage Information</h3>
      <p>We may collect information from third parties that your browser sends whenever you visit our website or use our Services. This data may include:<br><br>
      Browser type,<br>
      Browser version,<br>
      Pages of our Service that you visit,<br>
      City of user,<br>
      Time and date of your visit,<br>
      Time spent on those pages,<br>
      Other diagnostic data.<br><br>
      We do not collect IP addresses or demographic data.</p>
      <h3>1.3 Upload Information and Prompts</h3>
      <p>When you upload data to our Services, it is stored temporarily for the duration of your session. Once your session ends or you close your browser, all uploaded data is automatically deleted. We do not track or store any of the data you upload.
      <br>By default, 5 randomly selected rows from your uploaded data is sent to the selected language learning model to increase results accuracy. However, you may opt out of this using the bottom leftmost settings option in the "Settings" tab. We do not store or track this data, it is deleted when you reset your session. It is solely used to send to the selected LLM.
      <br>We do collect data structure information and the prompts you send while using our Services. If you prefer not to share this information, you can opt out through the settings in the "Settings" tab.
      <br>Additionally, column names and data type information from your uploaded data are sent to the selected language learning model to improve the functionality of the Services. This includes category names of all factor variables within your uploaded data.</p>

      <h2>2. How We Collect This Data</h2>
      <p>To collect data structure information and prompts, we utilize Google Analytics and an SQL database. Google Analytics helps us track and analyze user interactions with our Services, providing insights into how data is structured, and which prompts are most commonly used. This information is collected in real-time and stored securely in our SQLite database. The SQLite database allows us to manage and retrieve this data efficiently, ensuring that it is used to improve the quality and functionality of our Services.</p>

      <h2>3. Why We Collect This Data</h2>
      <p>We may use information from users for the following purposes:<br><br>
      To operate and improve the Services by providing you with more effective customer service.<br>
      To perform research and analysis aimed at improving the Services, or other products and technologies of Orditus LLC.<br>
      To diagnose or fix problems with the Services.<br>
      To communicate with you, especially through the sending of emails, information on technical Service issues, security announcements, information on new Services available, legal notices, response to your requests, or other information that we think might be relevant to you.<br>
      To protect against harm to the rights, property or safety of Orditus LLC, our users, yourself, or the public.<br>
      To enforce any terms of use, including investigations of potential violations of the terms.<br>
      To comply with any law, regulation, legal process, or governmental requests.</p>

      <h2>4. How We Use This Data</h2>
      <p>Orditus LLC will use the information collected from you to pursue legitimate interests such as legal or regulatory compliance, security control, business operations, scientific research, or any other interest reasonably held as legitimate.</p>

      <h2>5. Sharing This Data</h2>
      <p>We will not lease, sell, or rent your personal, usage, or upload information except in the following instances described in this policy.<br></p>
      <h3>5.1 Third Party Service Providers</h3>
      <p>We may occasionally hire other companies to provide limited services on our behalf, such as providing customer support, hosting websites, or performing statistical analysis of our Services. Those companies will be allowed to obtain only the information they need to deliver the relevant service. They will be required to maintain the confidentiality of the information and are prohibited from using it for any other purpose.<br><br>
      Third Party Service Providers:<br>
      Google Analytics<br>
      SQLite<br>
      AWS<br></p>
      <h3>5.2 With your Consent</h3>
      <p>At any time during your use of our Services, or upon explicit request from us, you may consent to the disclosure of your information.<br></p>
      <h3>5.3 For Security and Safety Purposes</h3>
      <p>In the event of any fraud, security threats, or incidents, we reserve the right to disclose your information without your consent for the purposes of maintaining security on our website and for our users and addressing fraud or security issues. We reserve the right to disclose your information without your consent for the purpose of protecting against harm to the rights, property, or safety of Orditus LLC, our users, yourself, or the public.<br></p>
      <h3>5.4 For Legal or Regulatory Purposes</h3>
      <p>We reserve the right to disclose your information without your consent to comply with any applicable law, regulation, legal process, or governmental requests.</p>

      <h2>6. Opting Out of Sharing Your Data</h2>
      <h3>6.1 Access your Information</h3>
      <p>You may be entitled under data protection laws to access and review personal information that Orditus LLC holds related to you. You may access, modify, or delete the information we collected by controlling the content that you share or upload at any time.<br><br>
      Any other request should be addressed to: ge@orditus.com. Such inquiries should be clearly marked as data protection queries, and you should indicate if the request is time sensitive.<br></p>
      <h3>6.2 Data Retention</h3>
      <p>We retain your Information for as long as necessary to deliver our Services, to comply with any applicable legal requirements, to maintain security and prevent incidents, and, in general, to pursue our legitimate interests. If you wish to request the erasure of all your personal information that we process, you may do so by sending a written request to ge@orditus.com.</p>
      <h3>6.3 Opting Out</h3>
      <p>You may opt out of sending information relevant to your data uploads and prompt requests at any time by switching off the checkbox in the RTutor.ai settings.</p>

      <h2>7. Data Security</h2>
      <p>All interactions with our Services occur over HTTPS which ensures that data transmitted between your device and our servers is encrypted and secure.</p>

      <h2>8. Children’s Privacy</h2>
      <p>Orditus.com is neither directed to nor structured to attract children under the age of 13 years. Accordingly, Orditus LLC does not intend to collect personal information from anyone it knows to be under 13 years of age. Orditus LLC will direct potential users under 13 years of age not to use the Services. If the Company learns that personal information of persons less than 13 years of age has been collected without verifiable parental consent, Orditus LLC will take the appropriate steps to delete this information.<br><br>
      To make such a request, please contact Orditus LLC at: ge@orditus.com.</p>

      <h2>9. Contact Us</h2>
      <p style="padding-bottom: 90px;">If you have questions about this policy, please email us at: ge@orditus.com.</p>
  </div>
  ')
}
