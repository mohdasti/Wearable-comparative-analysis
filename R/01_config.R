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
  oura_trends    = file.path(PROJECT_ROOT, "raw_data", "oura", "oura_2026-05-22_2026-07-23_trends.csv"),
  withings_steps = file.path(PROJECT_ROOT, "raw_data", "withings", "aggregates_steps.csv"),
  withings_sleep = file.path(PROJECT_ROOT, "raw_data", "withings", "sleep.csv")
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
# *_pct    = stage share of total sleep. Comparing stage minutes alone confounds
#            stage detection with total-sleep differences; percentages separate them.
ALL_METRICS <- c(
  "rhr", "rhr_rest", "hrv", "sleep_min", "steps",
  "deep_min", "rem_min", "light_min",
  "deep_pct", "rem_pct", "light_pct",
  "awake_min", "tib_min",
  "sleep_eff", "resp_rate",
  "bedtime_hr", "waketime_hr",
  "readiness"
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
  deep_pct    = "Deep sleep (% of sleep)",
  rem_pct     = "REM sleep (% of sleep)",
  light_pct   = "Light sleep (% of sleep)",
  awake_min   = "Awake time during sleep (min)",
  tib_min     = "Time in bed (min)",
  sleep_eff   = "Sleep efficiency (%)",
  resp_rate   = "Respiratory rate (breaths/min)",
  bedtime_hr  = "Bedtime (hours after noon)",
  waketime_hr = "Wake time (hours after midnight)",
  readiness   = "Daily readiness score (0-100)"
)

# Sleep stages compared as a group (all three devices report these).
SLEEP_STAGE_METRICS <- c("deep_min", "rem_min", "light_min")
SLEEP_STAGE_PCT_METRICS <- c("deep_pct", "rem_pct", "light_pct")

# Largest device-vs-device bias we would still call practically interchangeable.
# Set from what a user could act on, not from statistics: e.g. a 2 bpm resting-HR
# offset is within normal day-to-day noise, so it would not change a decision.
# Used by the equivalence check — a bias whose 95% CI sits entirely inside
# +/- margin is "equivalent"; one whose CI extends past it is not.
EQUIV_MARGINS <- c(
  rhr = 2, rhr_rest = 2, hrv = 5, sleep_min = 30, steps = 1000,
  deep_min = 20, rem_min = 20, light_min = 30,
  deep_pct = 5, rem_pct = 5, light_pct = 5,
  awake_min = 20, tib_min = 30,
  sleep_eff = 5, resp_rate = 1,
  bedtime_hr = 0.5, waketime_hr = 0.5, readiness = 10
)

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
  deep_pct  = list(hard_min = 0,  hard_max = 100, soft_min = 3,   soft_max = 45,  jump = 30),
  rem_pct   = list(hard_min = 0,  hard_max = 100, soft_min = 3,   soft_max = 45,  jump = 30),
  light_pct = list(hard_min = 0,  hard_max = 100, soft_min = 20,  soft_max = 85,  jump = 35),
  awake_min = list(hard_min = 0,  hard_max = 400, soft_min = 2,   soft_max = 180, jump = 120),
  tib_min   = list(hard_min = 60, hard_max = 960, soft_min = 150, soft_max = 900, jump = 200),
  sleep_eff = list(hard_min = 30, hard_max = 100, soft_min = 60,  soft_max = 100, jump = 30),
  readiness = list(hard_min = 0,  hard_max = 100, soft_min = 10,  soft_max = 100, jump = 40),
  resp_rate = list(hard_min = 5,  hard_max = 40,  soft_min = 10,  soft_max = 25,  jump = 6),
  bedtime_hr  = list(hard_min = -6, hard_max = 18, soft_min = 6, soft_max = 15, jump = 6),
  waketime_hr = list(hard_min = 0,  hard_max = 18, soft_min = 4, soft_max = 13, jump = 6)
)

# Pre-identified corrupt dates from manual data inspection.
KNOWN_BAD <- list(
  withings_sleep_exclude = as.Date(c("2026-06-20")),
  withings_sleep_dup     = as.Date("2026-06-26")
)
