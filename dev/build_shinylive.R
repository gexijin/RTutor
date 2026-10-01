# Build the static shinylive site into site/. Run from the repo root:
#   Rscript dev/build_shinylive.R
#   Rscript -e 'httpuv::runStaticServer("site")'
# shinylive has no ignore list: it ships every file in the app folder and the
# browser installs every package those files mention. So copy only the app itself.
app <- file.path(tempdir(), "rtutor_app")
unlink(app, recursive = TRUE)
dir.create(app)
file.copy(c("app.R", "R", "inst"), app, recursive = TRUE)

shinylive::export(app, "site")

# Replace shinylive's default loading animation with our progress screen
index <- readLines("site/index.html")
index <- sub("<title>Shiny App</title>",
             '<title>UIUC RTutor</title><link rel="icon" type="image/png" href="icon.png">',
             index, fixed = TRUE)
body_end <- grep("</body>", index, fixed = TRUE)
index <- append(index, readLines("dev/shinylive_loading.html"), after = body_end - 1)
writeLines(index, "site/index.html")
file.copy(c("inst/app/www/logo_no_bckgrd.png", "inst/app/www/icon.png"), "site/", overwrite = TRUE)
