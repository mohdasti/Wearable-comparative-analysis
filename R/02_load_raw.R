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
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = TZ, quiet = TRUE))
  if (any(is.na(parsed) & !is.na(x))) {
    parsed2 <- suppressWarnings(lubridate::ymd_hms(
      paste0(sub("([+-][0-9]{2}:[0-9]{2})$", "", x), " ", TZ),
      tz = TZ, quiet = TRUE
    ))
    parsed[is.na(parsed)] <- parsed2[is.na(parsed)]
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
# parse_whoop_cycles()
# Reads Whoop physiological_cycles.csv for RHR, HRV, and sleep duration.
# Sleep/recovery metrics are keyed to the wake date (Wake onset).
# -----------------------------------------------------------------------------
parse_whoop_cycles <- function() {
  raw <- readr::read_csv(PATHS$whoop_cycles, show_col_types = FALSE) |>
    janitor::clean_names()

  raw <- raw |>
    dplyr::mutate(
      wake_onset_dt = parse_pacific_datetime(wake_onset),
      cycle_end_dt  = parse_pacific_datetime(cycle_end_time),
      date = dplyr::if_else(
        !is.na(wake_onset_dt),
        wake_date_from_pacific(wake_onset_dt),
        wake_date_from_pacific(cycle_end_dt)
      )
    ) |>
    dplyr::filter(!is.na(date), date >= WINDOW_START, date <= WINDOW_END)

  # Build long format: one row per metric per day.
  rhr <- raw |>
    dplyr::transmute(
      device = "whoop", metric = "rhr", date,
      datetime = wake_onset_dt,
      value = resting_heart_rate_bpm, unit = "bpm",
      source_file = basename(PATHS$whoop_cycles)
    )

  hrv <- raw |>
    dplyr::transmute(
      device = "whoop", metric = "hrv", date,
      datetime = wake_onset_dt,
      value = heart_rate_variability_ms, unit = "ms",
      source_file = basename(PATHS$whoop_cycles)
    )

  sleep <- raw |>
    dplyr::transmute(
      device = "whoop", metric = "sleep_min", date,
      datetime = wake_onset_dt,
      value = asleep_duration_min, unit = "min",
      source_file = basename(PATHS$whoop_cycles)
    )

  dplyr::bind_rows(rhr, hrv, sleep)
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
  raw <- readr::read_csv(PATHS$oura_trends, show_col_types = FALSE) |>
    janitor::clean_names()

  raw <- raw |>
    dplyr::mutate(
      date = as.Date(date),
      total_sleep_duration = readr::parse_number(as.character(total_sleep_duration))
    ) |>
    dplyr::filter(date >= WINDOW_START, date <= WINDOW_END)

  rhr <- raw |>
    dplyr::transmute(device = "oura", metric = "rhr", date,
                     datetime = as.POSIXct(date, tz = TZ),
                     value = average_resting_heart_rate, unit = "bpm",
                     source_file = basename(PATHS$oura_trends))

  hrv <- raw |>
    dplyr::transmute(device = "oura", metric = "hrv", date,
                     datetime = as.POSIXct(date, tz = TZ),
                     value = average_hrv, unit = "ms",
                     source_file = basename(PATHS$oura_trends))

  sleep <- raw |>
    dplyr::transmute(device = "oura", metric = "sleep_min", date,
                     datetime = as.POSIXct(date, tz = TZ),
                     value = total_sleep_duration / 60, unit = "min",
                     source_file = basename(PATHS$oura_trends))

  steps <- raw |>
    dplyr::transmute(device = "oura", metric = "steps", date,
                     datetime = as.POSIXct(date, tz = TZ),
                     value = readr::parse_number(as.character(steps)), unit = "steps",
                     source_file = basename(PATHS$oura_trends))

  dplyr::bind_rows(rhr, hrv, sleep, steps)
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
  raw <- readr::read_csv(PATHS$withings_sleep, show_col_types = FALSE) |>
    janitor::clean_names()

  raw <- raw |>
    dplyr::mutate(
      from_dt = parse_pacific_datetime(from),
      to_dt   = parse_pacific_datetime(to),
      date    = wake_date_from_pacific(to_dt),
      light_s = readr::parse_number(as.character(light_s)),
      deep_s  = readr::parse_number(as.character(deep_s)),
      rem_s   = readr::parse_number(as.character(rem_s)),
      awake_s = readr::parse_number(as.character(awake_s)),
      total_sleep_min = (light_s + deep_s + rem_s) / 60,
      avg_hr = readr::parse_number(as.character(average_heart_rate))
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

  sleep <- raw |>
    dplyr::transmute(
      device = "withings", metric = "sleep_min", date,
      datetime = to_dt, value = total_sleep_min, unit = "min",
      source_file = basename(PATHS$withings_sleep)
    )

  rhr <- raw |>
    dplyr::transmute(
      device = "withings", metric = "rhr", date,
      datetime = to_dt, value = avg_hr, unit = "bpm",
      source_file = basename(PATHS$withings_sleep)
    )

  dplyr::bind_rows(sleep, rhr)
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
      metric = factor(metric, levels = METRICS)
    )
}
