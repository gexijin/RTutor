# bootstrap.R: started by main.js as `Rscript --vanilla bootstrap.R`.
# All configuration arrives as environment variables set by main.js.
# OPENAI_API_KEY and RTUTOR_DESKTOP are read by the RTutor package itself.

data_dir <- Sys.getenv("RTUTOR_DATA_DIR", unset = getwd())
lib_dir  <- Sys.getenv("R_LIBS_USER")
host     <- Sys.getenv("RTUTOR_HOST", unset = "127.0.0.1")
port     <- as.integer(Sys.getenv("RTUTOR_PORT", unset = "7777"))

# Bundled library first; empty when main.js falls back to a developer's own R.
if (nzchar(lib_dir)) .libPaths(normalizePath(lib_dir, winslash = "/", mustWork = FALSE))
options(shiny.launch.browser = FALSE, golem.app.prod = TRUE)

# No sink(): main.js logs R's stdout and stderr, and it waits for Shiny's "Listening on"
# message on stderr, which a message sink would swallow.
setwd(data_dir)
message("[bootstrap] ", format(Sys.time()), "  R ", getRversion(), "  libPaths: ",
        paste(.libPaths(), collapse = " | "))

ok <- tryCatch({
  shiny::runApp(RTutor::run_app(), host = host, port = port, launch.browser = FALSE)
  TRUE
}, error = function(e) {
  message("FATAL: ", conditionMessage(e))
  FALSE
})

quit(status = if (ok) 0L else 1L, save = "no")
