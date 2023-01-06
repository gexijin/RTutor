###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################



###################################################
# Global variables
###################################################

release <- "0.8.4" # RTutor
uploaded_data <- "User Upload" # used for drop down
no_data <- "No data (examples)" # no data is uploaded or selected
rna_seq <- "RNA-Seq"  # RNA-Seq read counts
min_query_length <- 6  # minimum # of characters
max_query_length <- 500 # max # of characters
#language_model <- "code-davinci-002	"# "text-davinci-003"
language_model <- "text-davinci-003"
default_temperature <- 0.1
pre_text <- "Generate R code. "
after_text <- " Use the df data frame. "
max_char_question <- 280 # max n. of characters in the Q&A
max_levels <- 12 # max number of levels in categorical varaible for EDA, ggairs
max_data_points <- 10000  # max number of data points for interactive plot
# if a column is numeric but only have a few unique values, treat as categorical
unique_ratio <- 0.1   # number of unique values / total # of rows
sqlitePath <- "../../data/usage_data.db" # folder to store the user queries, generated R code, and running results
sqltable <- "usage"



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
#'
#' @return Returns a cleaned up version, so that it could be sent to GPT.
prep_input <- function(txt, selected_data, df) {

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
        relevant_var_categorical <- intersect(relevant_var, none_numeric_var)
        if (length(relevant_var_categorical) == 1) {
          data_info <- paste0(
            data_info,
            "Note that ",
            relevant_var_categorical,
            " is a categorical variable. "
          )
        } else if (length(relevant_var_categorical) > 1) {
          data_info <- paste0(
            data_info,
            "Note that ",
            paste0(
              relevant_var_categorical[1:(length(relevant_var_categorical) - 1)],
              collapse = ", "
            ),
            " and ",
            relevant_var_categorical[length(relevant_var_categorical)],
            " are categorical variables. "
          )
        }
      }

      txt <- paste(txt, after_text)
      # if user is not trying to convert data
      if(!grepl("Convert |convert ", txt)) {
        txt <- paste(txt, data_info)
      }

    }
  }
  txt <- paste(pre_text, txt)

  return(txt)
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

# demo requests for mpg dataset
demos_mpg <- c(
  ' ... ' = 'Example requests',

  'Boxplot, ggplot2' = "Use ggplot2 to create a boxplot of hwy vs. class. 
 Color by class. 
 Add jitter.",
  'Correlation' = "Calculate the correlation coefficient of cty vs hwy. 
Repeat that after log transformation. 
Collect these results and show them.",

  'ANOVA, after log' = "Conduct ANOVA of log-transformed hwy by class and drv.",

  "Barplot, summarized" = "Calculate average cty by year and class. Then use ggplot2 to create a barplot of average mpg by class, colored by year. The bars for different years should be side by side.",

  "Analysis, step by step" = "Only keep cars with hwy bigger than 15, but less than 40. 
Add 0.5 to cty. 
Perform log transformation on cty. 
Raise hwy to the second power. 
Calculate correlation coefficient of transformed hwy and cty.",

  "Correlation heatmap" = "Create a correlation map of all the columns that contain numbers.",

  'Regression, specific' = "Build a regression model of hwy based on cyl, displ, drv, and class. 
Give me diagnostic plots.",

  "Analysis, complex" = "hwy and cty represent miles per gallon (MPG) on the highway and in the city, respectively. 
Only keep cars more efficient than 15 MPG, but less than 40, on the highway. 
Add 0.5 to city MPG for correction. 
Perform log transformation on city MPG. 
Raise highway MPG to the second power. 
Calculate correlation coefficient of  the two transformed variables.",

  "Neural network" = "Build a neural network model to predict  
hwy based on displ, cyl, and class.   
Use the nnet package. Plot the distribution of residuals.",

"High level Qs, 1" = "Are hwy increasing over the years?",
"High level Qs, 2" = "Are drv and cyl independent?",
"High level Qs, 3" = "Is hwy normally distributed?",

#"Ask for info, demo" = "Show me how to do model-based clustering.",

  'Scatter plot, interactive' = "Plot hwy vs. displ group by cyl. Make it interactive with ggplotly.",

  "Scatter plot, refined" = "Use ggplot2. Plot hwy vs. cty, colored by class. 
Change shape by drv. Change size by displ. 
Change x label to 'Highway mpg'. 
Change y label to 'City mpg'. 
Change background to white. 
Increase font for labels to 15. 
Remove all grids.",

"Hierarchical clustering" = "Conduct hierarchical clustering. ",
"Density plot, panels" = "Only keep 4, 6, and 8 cylinders. 
Create a density plot of cty, colored by year. Split into panels with one column  by cyl.",
 "Pie chart" = "Create an pie chart based on  class. "
)

# demo requests when no dataset is selected.
demos_no_data <- c(
  'Random number' = "Generate 100 random numbers. Plot their distribution.",
  'Hierarchical tree' = "Provide a demo for hierarchical clustering tree.",
  'Heat map' = "Create a heatmap with hierarchical clustering tree.",
  "Ridge regression" = "Provide a demo for ridge regression.",
  'PCA' = "Create a matrix with 100 columns and 20 rows. Fill it with random numbers from the normal distribution. 
The mean is 1 for the first 10 rows, but 3 for the rest. 
Conduct PCA. Plot using the first two principal components.",
'Map' = "Create an world map. ",
'Map, US' = "Create a US map.",
'Map, state' = "Crate a county level map of South Dakota.",
'Bioinformatics, genes' = "Use the biomaRt package to retrieve all human genes on Chr.Y.",
'Bioinformatics, position' = "Use the biomaRt package to retrieve the gene symbol, the start and end position of all human genes on Chr.Y.",
'Bioinformatics, length' = "Use the biomaRt package to retrieve the gene symbol, the start and end position of all human genes on Chr.Y. Calculate the length as the absolute difference between the start and end positions. Create a density plot of the length after log10 transformation.",
'Financial, stocks' = "Retrieve and plot the stock price of Apple in 2022. Add 20 day moving average."
)


demos_diamond <- c(
  'Distribution, cut' = "Plot the distribution of cut using a pie chart.",
  'Distribution, price' = "Plot the distribution of price after log transformation.",
  'Scatter, price vs. carat' = "Plot price vs. carat. Change color by clarity. ",
  'Combinations' = "Plot the combinations of cut and clarity.",
  'Modelling' = "Remove diamonds larger than 3 carat. 
Build a model of price vs. carat. 
Create a violin plot of the residuals by clarity. 
Limit ploting to -10000 to 10000."

)

demos_rna_seq <- c(
  'Total Counts' = "Convert the first column as row names and delete it. Plot the column sums.",
  'Boxplot, raw' = "Convert the first column as row names and delete it. Create a bloxplot of all columns.",
  'Boxplot, log' = "Convert the first column as row names and delete it. Add 1 to all numbers and then conduct log transformation. Create a bloxplot of all columns.",
  'Heatmap of variable genes' = "Each row represents a gene. Each column is a sample. Make the first column as row names, then delete it. Remove genes with sum less than 10. Add 1 to all numbers. Log transform using base 2. Rank genes by standard deviations in descending order. Convert data as matrix. Subtract row means from all rows. Create a heatmap of the top 50 genes using red and green colors.",
'DESeq2' = "Each row represents a gene. Each column is a sample. Make the first column as row names, then delete it. Remove rows with sum less than 10. Then use DESeq2 to identify differentially expressed genes. The first three columns are control samples. The last three are mutant. Show me the numbers of up and down-regulated genes based on FDR < 0.05 and log fold change > 1 or less than -1."
)




demo_questions <- c(
  'Example questions:' = "Example questions:",
  'Lookup R package' = "List popular R packages for time-series forecast.",
  'Explain concepts' = "What is Moran's I in sptatial statistics?",
  'Find statistic method' = "What statistics test should I use to examine the 
  correlation of two categorical variable?",
  'How to, situation' = "How do you do regression when there are
predictor variables that are highly correlated?",
  'How to, methods' = "How do you choose k in k-means clustering?",
  'How to, evaluation' = "How do you assess linear regression models?",
  'How to, outliers' = "How to deal with outliers?",
  'How to, interpretation' = "What does P value = 0.02 mean in ANOVA?",
  'Vague question, rejected' = "How does k-means clustering work?",
  'Vague question, w/ context' = "How does k-means clustering work in statistics?"
  )




# prepare a list of available data sets.
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



# Generated by ChatGPT
jokes <- c(
  "Why did the tomato turn red? Because it saw the salad dressing. - ChatGPT",
  "Why was the belt arrested? Because it held up a pair of pants. - ChatGPT",
  "Why was the math book sad? Because it had too many problems. - ChatGPT",
  "Why was the statistician's favorite method of transportation a bus? Because it had a lot of data. - ChatGPT",
  "Why did the tomato turn red? Because it saw the salad dressing. - ChatGPT",
  "Why was the math book upset? Because it was having a negative number of pages. - ChatGPT",
  "Why was the math book happy? Because it had lots of solutions. - ChatGPT",
  "Why couldn't the bicycle stand up by itself? Because it was two-tired. - ChatGPT",
  "Why do statisticians enjoy spending time with their data? Because they're data-driven individuals. - ChatGPT",
  "Why do statisticians like playing with their data? Because they have mean jokes. - ChatGPT",
  "Why do statisticians have a tough time at parties? Because they're always trying to fit in. - ChatGPT",
  "Believe in yourself and all that you are. Know that there is something inside you that is greater than any obstacle. -ChatGPT",
  "Don't let yesterday take up too much of today. -ChatGPT",
  "If you want to live a happy life, tie it to a goal, not to people or things. -ChatGPT",
  "If you can dream it, you can do it.",
  "Success is not final, failure is not fatal: it is the courage to continue that counts. -ChatGPT",
  "The only way to do great work is to love what you do. -ChatGPT",
  "The only limit to our realization of tomorrow will be our doubts of today. -ChatGPT",
  "You miss 100% of the shots you don't take. -ChatGPT",
  "Your time is limited, don't waste it living someone else's life. -ChatGPT",
  "Hardships often prepare ordinary people for an extraordinary destiny. -ChatGPT",
  "Success is not how high you have climbed, but how you make a positive difference to the world. -ChatGPT",
  "Happiness is not something ready-made. It comes from your own actions. -ChatGPT",
  "The only way to do great work is to be passionate about what you do. -ChatGPT",
  "Believe you can and you're halfway there. -ChatGPT",
  "The only way to achieve the impossible is to believe it is possible. -ChatGPT",
  "Do what you can, with what you have, where you are. -ChatGPT",
  "The greatest glory in living lies not in never falling, but in rising every time we fall. -ChatGPT",
  "Life is not about waiting for the storm to pass, but learning to dance in the rain. -ChatGPT",
  "The biggest adventure you can take is to live the life of your dreams. -ChatGPT",
  "If opportunity doesn't knock, build a door. -ChatGPT",
  "The only limit to our realization of tomorrow will be our doubts of today. -ChatGPT",
  "The only true limit to our realization of tomorrow is in today's doubts and fears. -ChatGPT",
  "Don't watch the clock; do what it does. Keep going. -ChatGPT",
  "Successful people do what unsuccessful people are not willing to do. Don't wish it were easier; wish you were better. -ChatGPT",
  "You may be the only person left who believes in you, but it's enough. It takes just one star to pierce a universe of darkness. Never give up. -ChatGPT",
  "Be the change you wish to see in the world. -ChatGPT",
  "The only way to discover the limits of the possible is to go beyond them into the impossible. -ChatGPT",
  "Success is not in what you have, but who you are. -ChatGPT",
  "Success is not how much money you make, but the difference you make in people's lives. -ChatGPT",
  "The only limit to our realization of tomorrow will be the doubts of today. -ChatGPT",
  "Statistics are like a bikini. What they reveal is suggestive, but what they conceal is vital. - Aaron Levenstein",
  "There are three kinds of lies: lies, damned lies, and statistics. - Benjamin Disraeli",
  "In God we trust, all others must bring data. - W. Edwards Deming",
  "Statistics is the grammar of science. - Karl Pearson",
  "A single death is a tragedy; a million deaths is a statistic. - Joseph Stalin",
  "Chance is the pseudonym of God when he does not want to sign his work. - Anatole France",
  "Statistics are like a lamp that illuminates the past and the present, but only reveals the future. - George E. P. Box",
  "Statistics are human beings with the tears wiped off. - Paul Brodeur",
  "The only thing more dangerous than ignorance is arrogance. - Albert Einstein",
  "The best thing about being a statistician is that you get to play in everyone's backyard. - John Tukey"
)



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
if (file.exists("api_key.txt")) {
  api_key_file <- readLines("api_key.txt")
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
#        data_str varchar(5000));


#' Saves user queries, code, and error status
#' 
#'
#' @param date Date in the format of "2023-01-04"
#' @param time Time "13:05:12"
#' @param request, user request
#' @param code AI generated code
#' @param error status, TRUE, error
#'
#' @return nothing
  save_data <- function(date, time, request, code, error_status, data_str) {
    # if db does not exist, create one
    if (file.exists(sqlitePath)) {
      # Connect to the database
      db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlitePath, flags = RSQLite::SQLITE_RW)
      # Construct the update query by looping over the data fields
      txt <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        sqltable,
        "date, time, request, code, error, data_str",
        paste(
          c(
            as.character(date),
            as.character(time),
            clean_txt(request),
            clean_txt(code),
            as.integer(error_status),
            clean_txt(data_str)
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


