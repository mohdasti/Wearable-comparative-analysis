# =============================================================================
# 04_harmonize.R
# Purpose: Build daily wide and long tables for analysis, anchored to the
#          Whoop window. Saves processed .rds files to data/processed/.
# =============================================================================

if (!exists("source_project_file")) {
  .src <- c(file.path(getwd(), "R", "source_project.R"), file.path(getwd(), "..", "R", "source_project.R"))
  source(.src[file.exists(.src)][1])
}
source_project_file("03_clean_qc.R")

# -----------------------------------------------------------------------------
# build_daily_long()
# Returns cleaned long-format data restricted to the analysis window.
# -----------------------------------------------------------------------------
build_daily_long <- function() {
  raw <- load_all_raw()
  cleaned <- clean_raw_data(raw)
  cleaned$data
}

# -----------------------------------------------------------------------------
# build_daily_wide()
# Pivots long data to wide format: one row per date, columns per device_metric.
# Example column names: whoop_rhr, oura_steps, withings_sleep_min
# -----------------------------------------------------------------------------
build_daily_wide <- function(long_df) {
  long_df |>
    dplyr::mutate(
      device_key = dplyr::case_when(
        grepl("Whoop", device) ~ "whoop",
        grepl("Oura", device) ~ "oura",
        grepl("Withings", device) ~ "withings",
        TRUE ~ as.character(device)
      ),
      col = paste0(device_key, "_", metric)
    ) |>
    dplyr::select(date, col, value) |>
    tidyr::pivot_wider(names_from = col, values_from = value, names_glue = "{col}")
}

# -----------------------------------------------------------------------------
# compute_overlap_summary()
# Counts how many days each device (and combination) has data per metric.
# -----------------------------------------------------------------------------
compute_overlap_summary <- function(long_df) {
  presence <- long_df |>
    dplyr::mutate(device_key = dplyr::case_when(
      grepl("Whoop", device) ~ "whoop",
      grepl("Oura", device) ~ "oura",
      grepl("Withings", device) ~ "withings"
    )) |>
    dplyr::select(date, device_key, metric) |>
    dplyr::distinct()

  expand_grid <- expand.grid(
    date = seq(WINDOW_START, WINDOW_END, by = "day"),
    metric = METRICS,
    stringsAsFactors = FALSE
  )

  dev_wide <- presence |>
    dplyr::mutate(val = device_key) |>
    tidyr::pivot_wider(names_from = device_key, values_from = val,
                       values_fn = list(val = ~ "yes"), values_fill = NA) |>
    dplyr::right_join(expand_grid, by = c("date", "metric"))

  for (d in c("whoop", "oura", "withings")) {
    if (!d %in% names(dev_wide)) dev_wide[[d]] <- NA_character_
  }

  dev_wide |>
    dplyr::mutate(
      n_devices = rowSums(!is.na(dplyr::across(dplyr::all_of(c("whoop", "oura", "withings"))))),
      combo = dplyr::case_when(
        !is.na(whoop) & !is.na(oura) & !is.na(withings) ~ "all_three",
        !is.na(whoop) & !is.na(oura) ~ "whoop_oura",
        !is.na(whoop) & !is.na(withings) ~ "whoop_withings",
        !is.na(oura) & !is.na(withings) ~ "oura_withings",
        !is.na(whoop) ~ "whoop_only",
        !is.na(oura) ~ "oura_only",
        !is.na(withings) ~ "withings_only",
        TRUE ~ "none"
      )
    )
}

# -----------------------------------------------------------------------------
# save_harmonized()
# Runs the full harmonization pipeline and writes output files.
# -----------------------------------------------------------------------------
save_harmonized <- function() {
  long_df <- build_daily_long()
  wide_df <- build_daily_wide(long_df)
  overlap <- compute_overlap_summary(long_df)
  qc <- clean_raw_data(load_all_raw())$qc_exclusions

  readr::write_rds(long_df, file.path(PROCESSED_DIR, "daily_long.rds"))
  readr::write_rds(wide_df, file.path(PROCESSED_DIR, "daily_wide.rds"))
  readr::write_rds(overlap, file.path(PROCESSED_DIR, "overlap.rds"))
  readr::write_csv(qc, file.path(PROCESSED_DIR, "qc_exclusions.csv"))

  list(long = long_df, wide = wide_df, overlap = overlap, qc = qc)
}
