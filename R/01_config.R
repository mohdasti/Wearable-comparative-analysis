# =============================================================================
# 01_config.R
# Purpose: Central configuration — paths, timezone, analysis window, QC rules.
# Edit values here if file locations or thresholds change.
# =============================================================================

# Locate project root by searching upward for physiological_cycles.csv.
find_project_root <- function(start = getwd()) {
  dir <- normalizePath(start, mustWork = FALSE)
  for (i in 1:5) {
    if (file.exists(file.path(dir, "physiological_cycles.csv"))) return(dir)
    parent <- dirname(dir)
    if (parent == dir) break
    dir <- parent
  }
  normalizePath(start, mustWork = FALSE)
}
PROJECT_ROOT <- find_project_root()

# All timestamps are interpreted in US Pacific time.
TZ <- "America/Los_Angeles"
Sys.setenv(TZ = TZ)

# Whoop-anchored analysis window (inclusive).
WINDOW_START <- as.Date("2026-06-20")
WINDOW_END   <- as.Date("2026-07-22")

# Input file paths (repo root).
PATHS <- list(
  whoop_cycles = file.path(PROJECT_ROOT, "physiological_cycles.csv"),
  whoop_steps    = file.path(PROJECT_ROOT, "whoop.rtf"),
  oura_trends    = file.path(PROJECT_ROOT, "oura_2026-05-22_2026-07-23_trends.csv"),
  withings_steps = file.path(PROJECT_ROOT, "aggregates_steps.csv"),
  withings_sleep = file.path(PROJECT_ROOT, "sleep.csv")
)

# Output folder for cleaned / harmonized data.
PROCESSED_DIR <- file.path(PROJECT_ROOT, "data", "processed")
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

# Device display names used in plots and tables.
DEVICE_LABELS <- c(
  whoop     = "Whoop",
  oura      = "Oura Ring 4",
  withings  = "Withings ScanWatch"
)

# Metrics we compare across devices.
METRICS <- c("rhr", "hrv", "sleep_min", "steps")

METRIC_LABELS <- c(
  rhr       = "Resting heart rate (bpm)",
  hrv       = "Heart rate variability (ms)",
  sleep_min = "Sleep duration (min)",
  steps     = "Daily steps"
)

# QC thresholds — values outside hard bounds are excluded; soft bounds are flagged.
QC_RULES <- list(
  rhr = list(hard_min = 30,  hard_max = 200, soft_min = 35,  soft_max = 120, jump = 25),
  hrv = list(hard_min = 5,   hard_max = 200, soft_min = 10,  soft_max = 150, jump = 40),
  sleep_min = list(hard_min = 60, hard_max = 900, soft_min = 120, soft_max = 840, jump = 180),
  steps = list(hard_min = 0, hard_max = 40000, soft_min = 500, soft_max = 35000, jump_pct = 0.8)
)

# Pre-identified corrupt dates from manual data inspection.
KNOWN_BAD <- list(
  withings_sleep_exclude = as.Date(c("2026-06-20")),
  withings_sleep_dup     = as.Date("2026-06-26")
)
