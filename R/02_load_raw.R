# =============================================================================
# 02_load_raw.R
# Purpose: Read raw exports from Whoop, Oura, and Withings and return a
#          single long-format tibble with one row per device / metric / day.
# =============================================================================

# Bootstrap path helper if not yet loaded.
if (!exists("source_project_file")) {
  .src <- c(
    file.path(getwd(), "R", "source_project.R"),
    file.path(getwd(), "..", "R", "source_project.R")
  )
  source(.src[file.exists(.src)][1])
}

source_project_file("01_config.R")

# -----------------------------------------------------------------------------
# parse_pacific_datetime()
# Converts a character datetime string to a POSIXct object in Pacific time.
# Input:  x — character vector of datetimes (may include timezone offsets)
# Output: POSIXct vector in TZ (America/Los_Angeles)
# -----------------------------------------------------------------------------
parse_pacific_datetime <- function(x) {
  # If readr already parsed the column to POSIXct (offset-aware), the instant is
  # correct; just express it in Pacific time.
  if (inherits(x, "POSIXct")) return(lubridate::with_tz(x, tzone = TZ))

  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_

  # Strings with an explicit offset (e.g. "...-07:00") mark a true instant:
  # parse respecting the offset, then convert to Pacific.
  has_offset <- grepl("[+-][0-9]{2}:?[0-9]{2}$", x)
  parsed <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = TZ)

  if (any(has_offset, na.rm = TRUE)) {
    p <- suppressWarnings(lubridate::parse_date_time(
      x[has_offset], orders = c("YmdHMSz", "YmdHMz"), tz = TZ, quiet = TRUE
    ))
    parsed[has_offset] <- lubridate::with_tz(p, tzone = TZ)
  }

  # Naive strings ("2026-06-20 01:41:18") are already local Pacific wall time.
  naive <- !has_offset & !is.na(x)
  if (any(naive)) {
    parsed[naive] <- suppressWarnings(lubridate::ymd_hms(x[naive], tz = TZ, quiet = TRUE))
  }
  parsed
}

# -----------------------------------------------------------------------------
# wake_date_from_pacific()
# Extracts the calendar wake date from a Pacific timestamp.
# Input:  dt — POSIXct datetime
# Output: Date vector
# -----------------------------------------------------------------------------
wake_date_from_pacific <- function(dt) {
  as.Date(lubridate::with_tz(dt, tzone = TZ))
}

# -----------------------------------------------------------------------------
# hours_after_noon()
# Converts a bedtime timestamp to "hours after 12:00 noon" so evening and
# early-morning bedtimes stay on one continuous scale (noon = 0, midnight = 12,
# 2am = 14). Makes bedtimes comparable and averageable across devices.
# -----------------------------------------------------------------------------
hours_after_noon <- function(dt) {
  dt <- lubridate::with_tz(dt, tzone = TZ)
  h <- lubridate::hour(dt) + lubridate::minute(dt) / 60 + lubridate::second(dt) / 3600
  ifelse(h >= 12, h - 12, h + 12)
}

# -----------------------------------------------------------------------------
# hours_after_midnight()
# Converts a wake timestamp to decimal hours after midnight (7:48 -> 7.8).
# -----------------------------------------------------------------------------
hours_after_midnight <- function(dt) {
  dt <- lubridate::with_tz(dt, tzone = TZ)
  lubridate::hour(dt) + lubridate::minute(dt) / 60 + lubridate::second(dt) / 3600
}

# -----------------------------------------------------------------------------
# make_metric_rows()
# Small helper to build a tidy block of rows for one device / metric.
# Keeps the parsers short and readable.
# -----------------------------------------------------------------------------
make_metric_rows <- function(df, device, metric, date, value, unit, datetime, source_file) {
  tibble::tibble(
    device = device, metric = metric, date = date,
    datetime = datetime, value = value, unit = unit, source_file = source_file
  )
}

# -----------------------------------------------------------------------------
# parse_whoop_cycles()
# Reads Whoop physiological_cycles.csv for RHR, HRV, and sleep duration.
# Sleep/recovery metrics are keyed to the wake date (Wake onset).
# -----------------------------------------------------------------------------
parse_whoop_cycles <- function() {
  # Whoop writes naive local wall-clock strings ("2026-07-22 02:25:47") with the
  # zone in a separate column. Forcing these to character stops readr from
  # parsing them as UTC, which would shift every time-of-day by the UTC offset.
  raw <- readr::read_csv(
    PATHS$whoop_cycles, show_col_types = FALSE,
    col_types = readr::cols(
      `Cycle start time` = readr::col_character(),
      `Cycle end time`   = readr::col_character(),
      `Sleep onset`      = readr::col_character(),
      `Wake onset`       = readr::col_character(),
      .default = readr::col_guess()
    )
  ) |>
    janitor::clean_names()

  raw <- raw |>
    dplyr::mutate(
      wake_onset_dt  = parse_pacific_datetime(wake_onset),
      sleep_onset_dt = parse_pacific_datetime(sleep_onset),
      cycle_end_dt   = parse_pacific_datetime(cycle_end_time),
      date = dplyr::if_else(
        !is.na(wake_onset_dt),
        wake_date_from_pacific(wake_onset_dt),
        wake_date_from_pacific(cycle_end_dt)
      )
    ) |>
    dplyr::filter(!is.na(date), date >= WINDOW_START, date <= WINDOW_END)

  src <- basename(PATHS$whoop_cycles)
  dt  <- raw$wake_onset_dt
  total <- raw$asleep_duration_min

  dplyr::bind_rows(
    make_metric_rows(raw, "whoop", "rhr",       raw$date, raw$resting_heart_rate_bpm,     "bpm", dt, src),
    # Whoop already reports a true resting HR, so rhr_rest == rhr.
    make_metric_rows(raw, "whoop", "rhr_rest",  raw$date, raw$resting_heart_rate_bpm,     "bpm", dt, src),
    make_metric_rows(raw, "whoop", "hrv",       raw$date, raw$heart_rate_variability_ms,  "ms",  dt, src),
    make_metric_rows(raw, "whoop", "sleep_min", raw$date, total,                          "min", dt, src),
    make_metric_rows(raw, "whoop", "deep_min",  raw$date, raw$deep_sws_duration_min,      "min", dt, src),
    make_metric_rows(raw, "whoop", "rem_min",   raw$date, raw$rem_duration_min,           "min", dt, src),
    make_metric_rows(raw, "whoop", "light_min", raw$date, raw$light_sleep_duration_min,   "min", dt, src),
    make_metric_rows(raw, "whoop", "deep_pct",  raw$date, raw$deep_sws_duration_min / total * 100,    "%", dt, src),
    make_metric_rows(raw, "whoop", "rem_pct",   raw$date, raw$rem_duration_min / total * 100,         "%", dt, src),
    make_metric_rows(raw, "whoop", "light_pct", raw$date, raw$light_sleep_duration_min / total * 100, "%", dt, src),
    make_metric_rows(raw, "whoop", "awake_min", raw$date, raw$awake_duration_min,         "min", dt, src),
    make_metric_rows(raw, "whoop", "tib_min",   raw$date, raw$in_bed_duration_min,        "min", dt, src),
    make_metric_rows(raw, "whoop", "sleep_eff", raw$date, raw$sleep_efficiency_percent,   "%",   dt, src),
    make_metric_rows(raw, "whoop", "resp_rate", raw$date, raw$respiratory_rate_rpm,       "brpm", dt, src),
    # Whoop "Recovery score" and Oura "Readiness score" are both 0-100 daily
    # readiness ratings, so they are harmonized under one metric name.
    make_metric_rows(raw, "whoop", "readiness", raw$date, raw$recovery_score_percent,     "score", dt, src),
    make_metric_rows(raw, "whoop", "bedtime_hr",  raw$date, hours_after_noon(raw$sleep_onset_dt), "h", raw$sleep_onset_dt, src),
    make_metric_rows(raw, "whoop", "waketime_hr", raw$date, hours_after_midnight(raw$wake_onset_dt), "h", dt, src)
  )
}

# -----------------------------------------------------------------------------
# parse_whoop_steps_rtf()
# Extracts manually logged daily step counts from whoop.rtf.
# Whoop does not export steps; the user typed them into an RTF note.
# Input pattern: "June20: 4718" or "July 5: 1087"; gaps marked with "-".
# -----------------------------------------------------------------------------
parse_whoop_steps_rtf <- function() {
  rtf_lines <- readLines(PATHS$whoop_steps, warn = FALSE)
  plain <- striprtf::read_rtf(PATHS$whoop_steps)

  # Match patterns like June20, June 20, July1, July 22
  matches <- gregexpr(
    "(June|July)[ ]?(\\d{1,2})[: ]+(-|\\d+)",
    plain, ignore.case = TRUE, perl = TRUE
  )
  reg <- regmatches(plain, matches)[[1]]

  if (length(reg) == 0) {
    return(tibble::tibble(
      device = character(), metric = character(), date = as.Date(character()),
      datetime = as.POSIXct(character()), value = numeric(), unit = character(),
      source_file = character()
    ))
  }

  parsed <- lapply(reg, function(entry) {
    m <- stringr::str_match(entry, "(June|July)[ ]?(\\d{1,2})[: ]+(-|\\d+)")
    month <- ifelse(tolower(m[2]) == "june", 6L, 7L)
    day   <- as.integer(m[3])
    val   <- m[4]
    list(
      date = as.Date(sprintf("2026-%02d-%02d", month, day)),
      value = if (val == "-") NA_real_ else as.numeric(val)
    )
  })

  tibble::tibble(
    device = "whoop",
    metric = "steps",
    date = do.call(c, lapply(parsed, function(x) x$date)),
    datetime = as.POSIXct(rep(NA, length(parsed)), origin = "1970-01-01", tz = TZ),
    value = vapply(parsed, function(x) x$value, numeric(1)),
    unit = "steps",
    source_file = basename(PATHS$whoop_steps)
  ) |>
    dplyr::filter(date >= WINDOW_START, date <= WINDOW_END)
}

# -----------------------------------------------------------------------------
# parse_oura_trends()
# Reads Oura daily trends export for RHR, HRV, sleep, and steps.
# The 'date' column is the summary / wake calendar day.
# -----------------------------------------------------------------------------
parse_oura_trends <- function() {
  # Force bedtime timestamp columns to character so readr does not silently
  # convert the ISO strings to UTC POSIXct (which would corrupt time-of-day).
  raw <- readr::read_csv(
    PATHS$oura_trends, show_col_types = FALSE,
    col_types = readr::cols(
      `Bedtime Start` = readr::col_character(),
      `Bedtime End`   = readr::col_character(),
      .default = readr::col_guess()
    )
  ) |>
    janitor::clean_names()

  # Several numeric columns arrive as text (thousands separators); coerce them.
  num <- function(x) readr::parse_number(as.character(x))

  raw <- raw |>
    dplyr::mutate(
      date = as.Date(date),
      bedtime_start_dt = parse_pacific_datetime(bedtime_start),
      bedtime_end_dt   = parse_pacific_datetime(bedtime_end)
    ) |>
    dplyr::filter(date >= WINDOW_START, date <= WINDOW_END)

  src <- basename(PATHS$oura_trends)
  dt  <- as.POSIXct(raw$date, tz = TZ)
  total <- num(raw$total_sleep_duration) / 60

  dplyr::bind_rows(
    make_metric_rows(raw, "oura", "rhr",       raw$date, num(raw$average_resting_heart_rate), "bpm", dt, src),
    make_metric_rows(raw, "oura", "rhr_rest",  raw$date, num(raw$lowest_resting_heart_rate),  "bpm", dt, src),
    make_metric_rows(raw, "oura", "hrv",       raw$date, num(raw$average_hrv),                "ms",  dt, src),
    make_metric_rows(raw, "oura", "sleep_min", raw$date, total,                               "min", dt, src),
    make_metric_rows(raw, "oura", "deep_min",  raw$date, num(raw$deep_sleep_duration) / 60,   "min", dt, src),
    make_metric_rows(raw, "oura", "rem_min",   raw$date, num(raw$rem_sleep_duration) / 60,    "min", dt, src),
    make_metric_rows(raw, "oura", "light_min", raw$date, num(raw$light_sleep_duration) / 60,  "min", dt, src),
    make_metric_rows(raw, "oura", "deep_pct",  raw$date, num(raw$deep_sleep_duration) / 60 / total * 100,  "%", dt, src),
    make_metric_rows(raw, "oura", "rem_pct",   raw$date, num(raw$rem_sleep_duration) / 60 / total * 100,   "%", dt, src),
    make_metric_rows(raw, "oura", "light_pct", raw$date, num(raw$light_sleep_duration) / 60 / total * 100, "%", dt, src),
    make_metric_rows(raw, "oura", "awake_min", raw$date, num(raw$awake_time) / 60,            "min", dt, src),
    make_metric_rows(raw, "oura", "tib_min",   raw$date, num(raw$total_bedtime) / 60,         "min", dt, src),
    make_metric_rows(raw, "oura", "sleep_eff", raw$date, num(raw$sleep_efficiency),           "%",   dt, src),
    make_metric_rows(raw, "oura", "resp_rate", raw$date, num(raw$respiratory_rate),           "brpm", dt, src),
    make_metric_rows(raw, "oura", "readiness", raw$date, num(raw$readiness_score),            "score", dt, src),
    make_metric_rows(raw, "oura", "steps",     raw$date, num(raw$steps),                      "steps", dt, src),
    make_metric_rows(raw, "oura", "bedtime_hr",  raw$date, hours_after_noon(raw$bedtime_start_dt), "h", raw$bedtime_start_dt, src),
    make_metric_rows(raw, "oura", "waketime_hr", raw$date, hours_after_midnight(raw$bedtime_end_dt), "h", raw$bedtime_end_dt, src)
  )
}

# -----------------------------------------------------------------------------
# parse_withings_steps()
# Daily step totals from Withings Health Mate aggregates export.
# -----------------------------------------------------------------------------
parse_withings_steps <- function() {
  readr::read_csv(PATHS$withings_steps, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::transmute(
      device = "withings", metric = "steps",
      date = as.Date(date),
      datetime = as.POSIXct(date, tz = TZ),
      value = value, unit = "steps",
      source_file = basename(PATHS$withings_steps)
    ) |>
    dplyr::filter(date >= WINDOW_START, date <= WINDOW_END)
}

# -----------------------------------------------------------------------------
# parse_withings_sleep()
# Nightly sleep duration and average HR from Withings sleep.csv.
# Sleep is attributed to the wake date (the 'to' timestamp, Pacific).
# Handles duplicate sessions on the same wake date by keeping the longest sleep.
# -----------------------------------------------------------------------------
parse_withings_sleep <- function() {
  raw <- readr::read_csv(
    PATHS$withings_sleep, show_col_types = FALSE,
    col_types = readr::cols(
      from = readr::col_character(),
      to   = readr::col_character(),
      .default = readr::col_guess()
    )
  ) |>
    janitor::clean_names()

  num <- function(x) readr::parse_number(as.character(x))

  raw <- raw |>
    dplyr::mutate(
      from_dt = parse_pacific_datetime(from),
      to_dt   = parse_pacific_datetime(to),
      date    = wake_date_from_pacific(to_dt),
      light_s = num(light_s),
      deep_s  = num(deep_s),
      rem_s   = num(rem_s),
      awake_s = num(awake_s),
      total_sleep_min = (light_s + deep_s + rem_s) / 60,
      time_in_bed_s   = as.numeric(difftime(to_dt, from_dt, units = "secs")),
      # Sleep efficiency = asleep time / time in bed (%).
      sleep_eff = (light_s + deep_s + rem_s) / time_in_bed_s * 100,
      avg_hr = num(average_heart_rate),
      min_hr = num(heart_rate_min)
    ) |>
    dplyr::filter(date >= WINDOW_START, date <= WINDOW_END)

  # Drop pre-identified corrupt night (2026-06-20).
  raw <- raw |>
    dplyr::filter(!date %in% KNOWN_BAD$withings_sleep_exclude)

  # Resolve duplicate wake dates: keep the row with the longest total sleep.
  raw <- raw |>
    dplyr::group_by(date) |>
    dplyr::arrange(dplyr::desc(total_sleep_min), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  src <- basename(PATHS$withings_sleep)
  dt  <- raw$to_dt
  total <- raw$total_sleep_min

  dplyr::bind_rows(
    make_metric_rows(raw, "withings", "sleep_min", raw$date, total,               "min", dt, src),
    make_metric_rows(raw, "withings", "deep_min",  raw$date, raw$deep_s / 60,     "min", dt, src),
    make_metric_rows(raw, "withings", "rem_min",   raw$date, raw$rem_s / 60,      "min", dt, src),
    make_metric_rows(raw, "withings", "light_min", raw$date, raw$light_s / 60,    "min", dt, src),
    make_metric_rows(raw, "withings", "deep_pct",  raw$date, raw$deep_s / 60 / total * 100,  "%", dt, src),
    make_metric_rows(raw, "withings", "rem_pct",   raw$date, raw$rem_s / 60 / total * 100,   "%", dt, src),
    make_metric_rows(raw, "withings", "light_pct", raw$date, raw$light_s / 60 / total * 100, "%", dt, src),
    make_metric_rows(raw, "withings", "awake_min", raw$date, raw$awake_s / 60,    "min", dt, src),
    make_metric_rows(raw, "withings", "tib_min",   raw$date, raw$time_in_bed_s / 60, "min", dt, src),
    make_metric_rows(raw, "withings", "sleep_eff", raw$date, raw$sleep_eff,       "%",   dt, src),
    # Withings "average heart rate" during sleep (comparable to Oura average RHR).
    make_metric_rows(raw, "withings", "rhr",       raw$date, raw$avg_hr,          "bpm", dt, src),
    # Withings minimum sleeping HR (like-for-like resting HR proxy).
    make_metric_rows(raw, "withings", "rhr_rest",  raw$date, raw$min_hr,          "bpm", dt, src),
    make_metric_rows(raw, "withings", "bedtime_hr",  raw$date, hours_after_noon(raw$from_dt), "h", raw$from_dt, src),
    make_metric_rows(raw, "withings", "waketime_hr", raw$date, hours_after_midnight(raw$to_dt), "h", dt, src)
  )
}

# -----------------------------------------------------------------------------
# load_all_raw()
# Master loader: calls every parser and combines results.
# Output: long tibble with columns device, metric, date, datetime, value, unit
# -----------------------------------------------------------------------------
load_all_raw <- function() {
  dplyr::bind_rows(
    parse_whoop_cycles(),
    parse_whoop_steps_rtf(),
    parse_oura_trends(),
    parse_withings_steps(),
    parse_withings_sleep()
  ) |>
    dplyr::mutate(
      device = factor(device, levels = names(DEVICE_LABELS), labels = unname(DEVICE_LABELS)),
      metric = factor(metric, levels = ALL_METRICS)
    )
}
