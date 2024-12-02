{
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
}


# for testing R package
remove.packages("RTutor")
#remotes::install_github("gexijin/RTutor", upgrade = "never")
# install from current folder
install.packages(".", repos = NULL, type = "source")
RTutor::run_app()
