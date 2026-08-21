# =============================================================================
# 00_packages.R
# Purpose: Load all R packages used in the wearable comparison analysis.
# Run this file first before any other scripts in the R/ folder.
# =============================================================================

# Use user-level library when system library is not writable (e.g. cloud VM).
# Check several common user-lib locations so packages installed by R, this
# script, or a previous session are all visible.
user_lib_candidates <- unique(c(
  Sys.getenv("R_LIBS_USER"),
  file.path(
    Sys.getenv("HOME"), "R",
    paste0(R.version$platform, "-library"),
    paste0(R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][[1]])
  ),
  file.path(
    Sys.getenv("HOME"), "R", R.version$platform, "library",
    paste0(R.version$major, ".", R.version$minor)
  )
))
user_lib_candidates <- user_lib_candidates[nzchar(user_lib_candidates)]
for (lib in user_lib_candidates) {
  if (dir.exists(lib) || dir.create(lib, recursive = TRUE, showWarnings = FALSE)) {
    .libPaths(c(lib, .libPaths()))
  }
}

required_packages <- c(
  "dplyr",        # Data manipulation (core tidyverse component)
  "tidyr",        # Pivot and reshape data
  "readr",        # Fast CSV reading
  "ggplot2",      # Plotting
  "purrr",        # Functional programming helpers
  "tibble",       # Modern data frames
  "stringr",      # String operations (Whoop RTF parsing)
  "lubridate",    # Date and timezone handling
  "janitor",      # Clean column names
  "striprtf",     # Parse Whoop manual steps from RTF file
  "knitr",        # Tables in the report
  "irr",          # Intraclass correlation coefficient (ICC)
  "psych",        # Lin's concordance correlation coefficient (CCC)
  "patchwork",    # Combine multiple ggplot figures
  "scales",       # Axis formatting helpers
  "ggridges",     # Ridgeline density plots
  "deming",       # Deming regression (errors on both axes)
  "zoo"           # Rolling-window calculations
)

# Install any package that is not yet on the system.
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
}

# Load each package into the current R session.
invisible(lapply(required_packages, library, character.only = TRUE))
