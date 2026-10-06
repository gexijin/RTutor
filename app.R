# Shinylive entry point (GitHub Pages). run_app() is still the entry point elsewhere.
# Shiny sources every file in R/ before this file runs (shinylive can't install
# the package itself), so app_ui/app_server are already defined here.
library(shiny)
library(golem)

# The class's OpenAI key, written into the site by dev/build_shinylive.R from the
# DOG_LOVER secret. create_response() reads only OPENAI_API_KEY.
if (file.exists("openai_key.txt")) Sys.setenv(OPENAI_API_KEY = readLines("openai_key.txt", warn = FALSE)[1])

server <- function(input, output, session) {
  app_server(input, output, session)
  # Attaching the tidyverse takes ~5s in the browser and the app's own startup
  # doesn't need it, only students' code does. So attach it after the page shows,
  # while the student picks a dataset. later() yields to the event loop first;
  # while R is busy, nothing reaches the page.
  session$onFlushed(function() {
    later::later(function() suppressPackageStartupMessages(library(tidyverse)), delay = 1)
  }, once = TRUE)
}

shinyApp(app_ui, server)
