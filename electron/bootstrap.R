# bootstrap.R: started by main.js as `Rscript --vanilla bootstrap.R`.
# All configuration arrives as environment variables set by main.js.
# OPENAI_API_KEY and RTUTOR_DESKTOP are read by the RTutor package itself.

# Repository for packages installed while the app runs. Pinned to the same snapshot the
# bundled library was built from, so a package downloaded today fits dependencies frozen
# months ago. Keep in sync with CRAN_SNAPSHOT_DATE in scripts/install_packages.R -
# tests/testthat/test-desktop.R fails if the two drift apart.
CRAN_SNAPSHOT_DATE <- "2026-10-01"

data_dir <- Sys.getenv("RTUTOR_DATA_DIR", unset = getwd())
lib_dir  <- Sys.getenv("R_LIBS_USER")
host     <- Sys.getenv("RTUTOR_HOST", unset = "127.0.0.1")
port     <- as.integer(Sys.getenv("RTUTOR_PORT", unset = "7777"))

# Bundled library first; empty when main.js falls back to a developer's own R.
if (nzchar(lib_dir)) .libPaths(normalizePath(lib_dir, winslash = "/", mustWork = FALSE))
options(shiny.launch.browser = FALSE, golem.app.prod = TRUE)

# Packages installed while the app runs go into the bundled library, alongside the ones that
# shipped - one library, not two. Windows only, matching desktop_can_install() in
# R/fct_helpers.R: the per-user install folder is writable without admin rights, and CRAN has
# prebuilt binaries for this R so no toolchain is needed. The macOS bundle stays read-only -
# its package binaries need the install-name relocation that only runs at build time (see
# README). The .libPaths() call above already put the bundled library first, and that is
# where install.packages() and pacman write by default, so all that is left is the repo.
if (nzchar(lib_dir) && .Platform$OS.type == "windows") {
  snapshot <- paste0("https://packagemanager.posit.co/cran/", CRAN_SNAPSHOT_DATE)
  options(
    # CRANextra is pinned to the same snapshot on purpose: pacman adds a long-dead Oxford
    # mirror under that name on Windows whenever it is missing, which costs a failed
    # request and a warning on every install. It only sets it if absent, so this wins.
    repos = c(CRAN = snapshot, CRANextra = snapshot),
    pkgType = "binary",   # no Rtools on a student's machine, so never build from source
    timeout = 600
  )
}

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
