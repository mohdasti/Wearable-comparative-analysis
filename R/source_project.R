# =============================================================================
# source_project.R
# Purpose: Locate the project root and source another R script from R/.
# Used by all pipeline scripts so they work from repo root or reports/.
# =============================================================================

source_project_file <- function(filename) {
  # Search likely locations for R/<filename>.
  candidates <- c(
    file.path(getwd(), "R", filename),
    file.path(getwd(), "..", "R", filename),
    file.path(dirname(getwd()), "R", filename)
  )
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path)) {
    stop("Could not find R/", filename, " — run from project root or reports/.", call. = FALSE)
  }
  source(path, local = parent.frame())
}

# Load config (defines PROJECT_ROOT, TZ, paths, QC rules) if not already loaded.
if (!exists("PROJECT_ROOT")) {
  source_project_file("01_config.R")
}
