# install_packages.R: fills the bundled R library for the desktop app.
# Called by .github/workflows/build-desktop.yml on macOS and Windows.
#
# Usage:  Rscript install_packages.R <library_path>
#
# Installs RTutor's own dependencies (DESCRIPTION) plus the packages that LLM-generated
# code is expected to use. The desktop app never installs packages at runtime, so anything
# generated code needs must be here. electron/scripts/check_package_coverage.R reports gaps.
#
# All packages come as prebuilt binaries from one dated Posit Package Manager snapshot, so
# Mac and Windows get identical versions and every rebuild is reproducible.

# ==================== Configuration ====================
# Bump to pick up newer package versions. Changing this invalidates the CI cache.
CRAN_SNAPSHOT_DATE <- "2026-10-01"

# (b) Curated server list (~/RTutor_server/classes/librarySetup.R on rtutor8core), cleaned up:
# dropped PCA (not a package), PCAExplorer/pcaMethods/ComplexHeatmap (Bioconductor),
# d3heatmap/bootPCA (removed from CRAN), liblinear (misnamed), mlr (heavy, superseded).
server_list <- c(
  "tidyverse", "readxl", "gridExtra", "DataExplorer",
  "ggfortify", "corrplot", "ggcorrplot", "GGally", "corrr",
  "pheatmap", "RColorBrewer", "gplots", "dendextend", "ggalluvial",
  "kernlab", "cluster", "fpc", "mclust", "dbscan", "factoextra", "ggdendro", "NbClust", "clusterGeneration",
  "ggbiplot", "psych", "qgraph",
  "mgcv", "nlme", "caret",
  "rpart", "randomForest", "e1071", "nnet", "gbm", "ROCR", "klaR", "class", "neuralnet", "xgboost",
  "forecast", "tseries", "TSA", "xts", "lubridate"
)

# (c) Common stats-course packages, ordered MOST to LEAST commonly used.
# If the installer is over the size limit, remove packages from the END of this list.
course_list <- c(
  "ggpubr", "car", "broom", "scales", "emmeans", "rstatix", "lme4", "survival", "MASS",
  "patchwork", "ggrepel", "Hmisc", "data.table", "skimr", "effectsize", "gtsummary",
  "kableExtra", "haven", "viridis", "cowplot", "ggthemes", "lmerTest", "multcomp",
  "nortest", "pwr", "boot", "lmtest", "sandwich", "glmnet", "ranger", "pROC",
  "rpart.plot", "DescTools", "survminer", "ggridges", "vcd", "FactoMineR", "zoo",
  "reshape2", "writexl", "coin", "ordinal", "ggbeeswarm", "treemapify", "Rtsne", "lattice"
)

# ==================== Library path ====================
lib <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(lib) || !nzchar(lib)) stop("Usage: Rscript install_packages.R <library_path>")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
cat("Installing to library:", lib, "\n")

# ==================== Repo root (for DESCRIPTION) ====================
# This script lives at <repo>/electron/scripts/install_packages.R.
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..", ".."))
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) stop("No DESCRIPTION at ", repo_root)

# ==================== Install ====================
options(
  repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/", CRAN_SNAPSHOT_DATE)),
  timeout = 600,
  warn = 1
)
cat("CRAN snapshot:", getOption("repos"), "\n\n")

if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak", lib = lib)

cat("Installing RTutor's dependencies from DESCRIPTION ...\n")
pak::local_install_deps(root = repo_root, lib = lib, upgrade = FALSE, ask = FALSE)

wanted <- unique(c(server_list, course_list))
cat("\nInstalling", length(wanted), "packages for generated code ...\n")
pak::pkg_install(wanted, lib = lib, upgrade = FALSE, ask = FALSE)

# ==================== Verify ====================
# Fail loudly: a silently missing package is exactly what the bundled set exists to prevent.
missing <- setdiff(wanted, rownames(installed.packages(lib.loc = c(lib, .Library))))
if (length(missing) > 0) stop("Not installed: ", paste(missing, collapse = ", "))

cat("\n", length(list.dirs(lib, recursive = FALSE)), "packages in", lib, "\n")
