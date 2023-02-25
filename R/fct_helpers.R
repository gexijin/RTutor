###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################



###################################################
# Global variables
###################################################

release <- "0.91" # RTutor
uploaded_data <- "User Upload" # used for drop down
no_data <- "no_data" # no data is uploaded or selected
names(no_data) <- "No data (examples)"
rna_seq <- "rna_seq"  # RNA-Seq read counts
names(rna_seq) <- "RNA-Seq"
min_query_length <- 6  # minimum # of characters
max_query_length <- 500 # max # of characters
#language_model <- "code-davinci-002	"# "text-davinci-003"
language_model <- "text-davinci-003"
default_temperature <- 0.1
pre_text <- "Generate R code. The demography data frame contains information 
about patient demographics information. It a numeric column called age. Other columns
include patient, site, investigator, Date_of_birth, sex, race, ethnicity and country. 
In the sex column, male is denoted as M. 
The events data frame contains information 
about adverse events in a clinical trial. 
All datasets have been read in and contain a common column called patient. "
pre_text_python <- "Generate Python code. "
after_text <- " Use the df data frame, which is already read from a file. "
max_char_question <- 280 # max n. of characters in the Q&A
max_levels <- 12 # max number of levels in categorical varaible for EDA, ggairs
max_data_points <- 10000  # max number of data points for interactive plot
max_levels_factor_conversion <- 4 # Numeric columns will be converted to factor if less than or equal to this many levels
# if a column is numeric but only have a few unique values, treat as categorical
unique_ratio <- 0.1   # number of unique values / total # of rows
sqlitePath <- "../../data/usage_data.db" # folder to store the user queries, generated R code, and running results
sqltable <- "usage"

# voice input parameters
wake_word <- "Tutor" #Tutor, Emma, Note that "Hey Cox" does not work very well.
# this triggers the submit button
action_verbs <- c(
  "now", 
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


#' Read built-in datasets
#'
#' Read user data files
#' @return Returns a cleaned up version, so that it could be sent to GPT.
read_additional_data <- function() {

  # Running on laptop, specify absolute path to the folder
  data_path <- "G:/My Drive/personal/UK/clean/"

  # data file should be stored in 
  if(file.exists(on_server)) {
    data_path <- "../../data/"
  }

  demography <<- readr::read_delim(
    paste0(data_path, "Demography.txt"), 
    delim = "\t", 
    escape_double = FALSE, 
    col_types = readr::cols(
      patient = readr::col_character(),
      enrolled_date = readr::col_date(format = "%d/%m/%Y"), 
      site = readr::col_character(), 
      investigator = readr::col_character(), 
      Date_of_birth = readr::col_date(format = "%d/%m/%Y"), 
      age = readr::col_integer()
    ), 
    trim_ws = TRUE
  )
  demography$sex <<- as.factor(demography$sex)
  demography$race <<- as.factor(demography$race)
  demography$ethnicity <<- as.factor(demography$ethnicity)
  demography$country <<- as.factor(demography$country)
  demography <<- as.data.frame(demography)

  if(0) { # disable temporarily
  lab <<- readr::read_delim(
    paste0(data_path, "Lab Results.txt"), 
    delim = "\t", 
    escape_double = FALSE, 
    col_types = readr::cols(
      patient = readr::col_character(), 
      result = readr::col_number()
    ), 
    trim_ws = TRUE
  )

  lab <<- na.omit(lab)

  lab$test <<- as.factor(lab$test)
  lab$category <<- as.factor(lab$category)
  lab$indicator <<- as.factor(lab$indicator)
  lab <<- as.data.frame(lab)
  }

  events <<- readr::read_delim(
    paste0(data_path, "Adverse Events.txt"), 
    delim = "\t", 
    escape_double = FALSE, 
    col_types = readr::cols(
      patient = readr::col_character(), 
      Start = readr::col_date(format = "%d/%m/%Y"), 
      End = readr::col_date(format = "%d/%m/%Y")
    ), 
    trim_ws = TRUE
  )
  events$Serious <<- as.factor(events$Serious)
  events$Status <<- as.factor(events$Status)
  events <<- as.data.frame(events)

  medications <<- readr::read_delim(
    paste0(data_path, "Medications.txt"), 
      delim = "\t", 
      escape_double = FALSE, 
      col_types = readr::cols(
        patient = readr::col_character(), 
        Start = readr::col_date(format = "%d/%m/%Y"), 
        End = readr::col_date(format = "%d/%m/%Y")
      ), 
    trim_ws = TRUE
  )
  medications$medications <<- as.factor(medications$medications)
  medications <<- as.data.frame(medications)

}


#' Prepare User input.
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param txt A string that stores the user input.
#' @param selected_data Name of the dataset.
#' @param df the data frame
#' @param use_python  whether or not using python instead of R
#'
#' @return Returns a cleaned up version, so that it could be sent to GPT.
prep_input <- function(txt, selected_data, df, use_python) {

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
   if (!grepl("\\.$", txt)) {
     txt <- paste(txt, ".", sep = "")
   }

  if (!is.null(selected_data)) {
    if (selected_data != no_data) {

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
      none_numeric_var <- colnames(df)[!numeric_index]

      # variables mentioned in request
      relevant_var <- sapply(
        colnames(df),
        function(x) {
          # hwy. class
          grepl(
            paste0(
              " ", # proceeding space
              x,
              "[ |\\.|,]" # ending space, comma, or period
            ),
          txt
          )
        }
      )

      relevant_var <- colnames(df)[relevant_var]

      if (length(relevant_var) > 0) {

        # numeric variables-----------------------------
        relevant_var_numeric <- intersect(relevant_var, numeric_var)
        if (length(relevant_var_numeric) == 1) {
          data_info <- paste0(
            data_info,
            "Note that ",
            relevant_var_numeric,
            " is a numeric variable. "
          )
        } else if (length(relevant_var_numeric) > 1) {
          data_info <- paste0(
            data_info,
            "Note that ",
            paste0(
              relevant_var_numeric[1:(length(relevant_var_numeric) - 1)],
              collapse = ", "
            ),
            " and ",
            relevant_var_numeric[length(relevant_var_numeric)],
            " are numeric variables. "
          )
        }

        # Categorical variables-----------------------------
        all_relevant_var_categorical <- intersect(
          relevant_var,
          none_numeric_var
        )

        for (relevant_var_categorical in all_relevant_var_categorical) {
          ix <- match(relevant_var_categorical, colnames(df))
          factor_levels <- sort(table(df[, ix]), decreasing = TRUE)
          factor_levels <- names(factor_levels)

          # have more than 6 levels?
          many_levels <- FALSE

          if (length(factor_levels) > 6) {
            many_levels <- TRUE
            factor_levels <- factor_levels[1:6]
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
            "The column ",
            relevant_var_categorical,
            " contains a categorical variable with these levels: ",
            factor_levels,
            ". "
          )
        }
      }

      #txt <- paste(txt, after_text)

      # if user is not trying to convert data
      #if (!grepl("Convert |convert ", txt)) {
      #  txt <- paste(txt, data_info)
      #}

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
#' Returns information on data frame describing columns.
#'
#' @param df a data frame
#' @return Returns a cleaned up version, so that it could be executed as R command.

 describe_df <- function(df) {

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
      none_numeric_var <- colnames(df)[!numeric_index]

      relevant_var <- colnames(df)



      # numeric variables-----------------------------
      relevant_var_numeric <- intersect(relevant_var, numeric_var)
      if (length(relevant_var_numeric) == 1) {
        data_info <- paste0(
          data_info,
          "Note that ",
          relevant_var_numeric,
          " is a numeric variable. "
        )
      } else if (length(relevant_var_numeric) > 1) {
        data_info <- paste0(
          data_info,
          "Note that ",
          paste0(
            relevant_var_numeric[1:(length(relevant_var_numeric) - 1)],
            collapse = ", "
          ),
          " and ",
          relevant_var_numeric[length(relevant_var_numeric)],
          " are numeric variables. "
        )
      }

      # Categorical variables-----------------------------
      all_relevant_var_categorical <- intersect(
        relevant_var,
        none_numeric_var
      )

      for (relevant_var_categorical in all_relevant_var_categorical) {
        ix <- match(relevant_var_categorical, colnames(df))
        factor_levels <- sort(table(df[, ix]), decreasing = TRUE)
        factor_levels <- names(factor_levels)

        # have more than 6 levels?
        many_levels <- FALSE

        if (length(factor_levels) > 6) {
          many_levels <- TRUE
          factor_levels <- factor_levels[1:6]
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
          "The column ",
          relevant_var_categorical,
          " contains a categorical variable with these levels: ",
          factor_levels,
          ". "
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
#' @return Returns a cleaned up version, so that it could be executed as R command.
clean_cmd <- function(cmd, selected_data) {
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

  #cmd is a vector. Each element is a line.

  # sometimes it returns RMarkdown code.
  cmd <- gsub("```", "", cmd)

  # remove empty lines
  cmd <- cmd[cmd != ""]

  # replace install.packages by "#install.packages"
  cmd <- gsub("install.packages", "#install.packages", cmd)

  if (selected_data != no_data) {
    cmd <- c("df <- as.data.frame(current_data())", cmd)
  }

  return(cmd)

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

#datasets <- c(datasets, "demography", "lab")
datasets <- c("mpg", "demography", "lab", "events", "medications")
# move it to 2nd place
datasets <- move_front(datasets, no_data)

datasets <- move_front(datasets, "diamonds")
# default
datasets <- move_front(datasets, "mpg")

datasets <- move_front(datasets, "lab")
datasets <- move_front(datasets, "demography")

datasets <- setNames(datasets, datasets)

names(datasets)[match("mpg", datasets)] <- "mpg (examples)"
names(datasets)[match("diamonds", datasets)] <- "diamonds (examples)"
names(datasets)[match(rna_seq, datasets)] <- "RNA-Seq (examples)"


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


#' Validate API key character
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param api_key is a character string
#'
#' @return Returns TRUE or FALSE
validate_api_key <- function(api_key) {
  valid <- TRUE
  # if 51 characters, use the one in the file
  if (nchar(api_key) != 51) {
    valid <- FALSE
  }
  return(valid)
}


# get API key from environment variable.
api_key_global <- Sys.getenv("OPEN_API_KEY")
key_source <- "from OS environment variable."

# If there is an key file in the current folder, use that instead.
if (file.exists(file.path(getwd(), "api_key.txt"))) {
  api_key_file <- readLines(file.path(getwd(), "api_key.txt"))
  api_key <- clean_api_key(api_key_file)

  # if valid, replace with file
  if(validate_api_key(api_key_file)) {
    api_key_global <- api_key_file
    key_source <- "from file."
  }
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
        is.numeric(x) &&
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


