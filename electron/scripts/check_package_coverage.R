# check_package_coverage.R: which packages does LLM-generated code load that the desktop
# bundle (install_packages.R) does not include?
#
# Calls the real LLM: about 250 short code-generation calls, roughly $2-3 on the class key.
# Run by hand from the repo root, with OPENAI_API_KEY set (e.g. in .Renviron):
#   Rscript electron/scripts/check_package_coverage.R [results.csv]
# Run it before a release and whenever the package list or the model changes.
# Add any package it reports as missing to course_list in install_packages.R.

suppressMessages(devtools::load_all(quiet = TRUE))
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) stop("Set OPENAI_API_KEY to the sk-... key first.")
out_file <- commandArgs(TRUE)[1]

# ---- What the bundle contains: listed packages plus all their dependencies ----
src <- readLines("electron/scripts/install_packages.R")
cfg <- new.env()
eval(parse(text = src[seq_len(grep("^# =+ Library path", src) - 1)]), cfg)
desc <- read.dcf("DESCRIPTION", c("Imports", "Depends"))
desc_pkgs <- trimws(sub("\\(.*", "", unlist(strsplit(paste(desc[!is.na(desc)], collapse = ","), ","))))
listed <- unique(c(cfg$server_list, cfg$course_list, desc_pkgs))
listed <- listed[nzchar(listed)]

snapshot <- paste0("https://packagemanager.posit.co/cran/", cfg$CRAN_SNAPSHOT_DATE)
db <- available.packages(repos = snapshot, type = "source")
deps <- unlist(tools::package_dependencies(listed, db = db, recursive = TRUE,
                                           which = c("Depends", "Imports", "LinkingTo")))
base_and_recommended <- rownames(installed.packages(priority = c("base", "recommended")))
bundle <- unique(c(listed, deps, base_and_recommended))
cat(length(bundle), "packages in the bundle (including dependencies)\n")

# ---- Prompts ----
# 1. The example requests shipped with RTutor, on their own datasets.
demo <- read.csv(app_sys("app", "www", "demo_questions.csv"), check.names = FALSE)
names(demo)[1] <- "data"  # strip a byte-order mark from the first header
demo_data <- c(
  "MPG (examples)" = "mpg", "Iris (examples)" = "iris", "Diamonds (examples)" = "diamonds",
  "Air Quality (examples)" = "airquality", "CO2 (examples)" = "CO2",
  "Chick Weights (examples)" = "ChickWeight", "Pressure (examples)" = "pressure",
  "Tooth Growth (examples)" = "ToothGrowth", "No Data" = no_data
)
demo <- demo[demo$data %in% names(demo_data) & nzchar(demo$requests), ]
cases <- data.frame(dataset = unname(demo_data[demo$data]), prompt = demo$requests)

# 2. Typical stats-course requests on every UIUC course dataset (the datasets students see).
templates <- c(
  "Show summary statistics for every variable.",
  "Plot the distribution of each numeric variable.",
  "Make a boxplot comparing a numeric variable across groups.",
  "Test whether the mean of a numeric variable differs between groups.",
  "Run an ANOVA followed by a post-hoc test.",
  "Fit a linear regression model and show the diagnostic plots.",
  "Compute the correlations between the numeric variables and visualize them.",
  "Fit a model with an interaction between two predictors and plot the interaction.",
  "Create a nicely formatted summary table of the data.",
  "Check whether the residuals of a regression model are normally distributed."
)
cases <- rbind(cases, expand.grid(dataset = uiuc_datasets, prompt = templates, stringsAsFactors = FALSE))
cat(nrow(cases), "prompts\n\n")

load_df <- function(name) {
  if (name == no_data) return(NULL)
  if (name %in% uiuc_datasets) {
    return(read.csv(app_sys("app", "www", "demo_data", paste0(name, ".csv")), na.strings = c("NA", "")))
  }
  get(name)
}

pat <- "(?:library|require|requireNamespace|p_load)\\(\\s*['\"]?([A-Za-z][A-Za-z0-9.]*)|([A-Za-z][A-Za-z0-9.]*):::?"
packages_in <- function(code) {
  m <- regmatches(code, gregexpr(pat, code, perl = TRUE))[[1]]
  unique(sub(pat, "\\1\\2", m, perl = TRUE))
}

# ---- Generate code the same way the app does (mod_05: system_role + prep_input) ----
found <- vector("list", nrow(cases))
failed <- 0
for (i in seq_len(nrow(cases))) {
  df <- load_df(cases$dataset[i])
  request <- prep_input(cases$prompt[i], cases$dataset[i], df, FALSE, 1, TRUE, NULL)
  messages <- list(list(role = "system", content = system_role), list(role = "user", content = request))
  code <- tryCatch(
    create_response(language_models[[default_model]], messages)$choices[[1, "message.content"]],
    error = function(e) { message("  call failed: ", conditionMessage(e)); NA }
  )
  if (is.na(code)) { failed <- failed + 1; next }
  found[[i]] <- packages_in(code)
  cat(sprintf("[%3d/%d] %-18s %s\n", i, nrow(cases), cases$dataset[i], paste(found[[i]], collapse = " ")))
}

# ---- Report ----
used <- unlist(found)
if (length(used) == 0) stop("No packages found; check that the LLM calls succeeded.")
tab <- as.data.frame(table(package = used), stringsAsFactors = FALSE)
tab$in_bundle <- tab$package %in% bundle
tab <- tab[order(tab$in_bundle, -tab$Freq), ]
cat("\n", failed, "calls failed\n")
missing <- tab[!tab$in_bundle, ]
if (nrow(missing) == 0) {
  cat("All", nrow(tab), "packages used by generated code are in the bundle.\n")
} else {
  cat("NOT in the bundle (package, times used):\n")
  print(missing[, c("package", "Freq")], row.names = FALSE)
}
if (!is.na(out_file)) write.csv(tab, out_file, row.names = FALSE)
