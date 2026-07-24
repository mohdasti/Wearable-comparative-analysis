# =============================================================================
# 01_config.R
# Purpose: Central configuration — paths, timezone, analysis window, QC rules.
# Edit values here if file locations or thresholds change.
# =============================================================================

# Locate project root by searching upward for R/01_config.R.
find_project_root <- function(start = getwd()) {
  dir <- normalizePath(start, mustWork = FALSE)
  for (i in 1:5) {
    if (file.exists(file.path(dir, "R", "01_config.R"))) return(dir)
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

# Input file paths (raw_data/ subfolders).
PATHS <- list(
  whoop_cycles   = file.path(PROJECT_ROOT, "raw_data", "whoop", "physiological_cycles.csv"),
  whoop_steps    = file.path(PROJECT_ROOT, "raw_data", "whoop", "whoop.rtf"),
  whoop_workouts = file.path(PROJECT_ROOT, "raw_data", "whoop", "workouts.csv"),
  oura_trends    = file.path(PROJECT_ROOT, "raw_data", "oura", "oura_2026-05-22_2026-07-23_trends.csv"),
  withings_steps = file.path(PROJECT_ROOT, "raw_data", "withings", "aggregates_steps.csv"),
  withings_sleep = file.path(PROJECT_ROOT, "raw_data", "withings", "sleep.csv"),
  withings_hr    = file.path(PROJECT_ROOT, "raw_data", "withings", "raw_hr_hr.csv")
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

# Core metrics compared across devices (the headline four).
METRICS <- c("rhr", "hrv", "sleep_min", "steps")

# All metrics harmonized in the long table (core + extended).
# rhr_rest = "resting" definition (Whoop RHR / Oura lowest RHR / Withings min sleeping HR)
#            used for a like-for-like resting HR comparison.
ALL_METRICS <- c(
  "rhr", "rhr_rest", "hrv", "sleep_min", "steps",
  "deep_min", "rem_min", "light_min",
  "sleep_eff", "resp_rate",
  "bedtime_hr", "waketime_hr"
)

METRIC_LABELS <- c(
  rhr         = "Resting heart rate (bpm)",
  rhr_rest    = "Resting HR, like-for-like (bpm)",
  hrv         = "Heart rate variability (ms)",
  sleep_min   = "Sleep duration (min)",
  steps       = "Daily steps",
  deep_min    = "Deep sleep (min)",
  rem_min     = "REM sleep (min)",
  light_min   = "Light sleep (min)",
  sleep_eff   = "Sleep efficiency (%)",
  resp_rate   = "Respiratory rate (breaths/min)",
  bedtime_hr  = "Bedtime (hours after noon)",
  waketime_hr = "Wake time (hours after midnight)"
)

# Sleep stages compared as a group (all three devices report these).
SLEEP_STAGE_METRICS <- c("deep_min", "rem_min", "light_min")

# QC thresholds — values outside hard bounds are excluded; soft bounds are flagged.
QC_RULES <- list(
  rhr       = list(hard_min = 30, hard_max = 200, soft_min = 35,  soft_max = 120, jump = 25),
  rhr_rest  = list(hard_min = 30, hard_max = 200, soft_min = 35,  soft_max = 120, jump = 25),
  hrv       = list(hard_min = 5,  hard_max = 200, soft_min = 10,  soft_max = 150, jump = 40),
  sleep_min = list(hard_min = 60, hard_max = 900, soft_min = 120, soft_max = 840, jump = 180),
  steps     = list(hard_min = 0,  hard_max = 40000, soft_min = 500, soft_max = 35000, jump_pct = 0.8),
  deep_min  = list(hard_min = 0,  hard_max = 300, soft_min = 15,  soft_max = 200, jump = 120),
  rem_min   = list(hard_min = 0,  hard_max = 300, soft_min = 15,  soft_max = 200, jump = 120),
  light_min = list(hard_min = 0,  hard_max = 600, soft_min = 60,  soft_max = 450, jump = 180),
  sleep_eff = list(hard_min = 30, hard_max = 100, soft_min = 60,  soft_max = 100, jump = 30),
  resp_rate = list(hard_min = 5,  hard_max = 40,  soft_min = 10,  soft_max = 25,  jump = 6),
  bedtime_hr  = list(hard_min = -6, hard_max = 18, soft_min = 6, soft_max = 15, jump = 6),
  waketime_hr = list(hard_min = 0,  hard_max = 18, soft_min = 4, soft_max = 13, jump = 6)
)

# Pre-identified corrupt dates from manual data inspection.
KNOWN_BAD <- list(
  withings_sleep_exclude = as.Date(c("2026-06-20")),
  withings_sleep_dup     = as.Date("2026-06-26")
)
