# =============================================================================
# 03_clean_qc.R
# Purpose: Apply quality-control rules, flag outliers, and log exclusions.
# Input:   long-format raw tibble from load_all_raw()
# Output:  cleaned tibble + qc_exclusions log
# =============================================================================

if (!exists("source_project_file")) {
  .src <- c(file.path(getwd(), "R", "source_project.R"), file.path(getwd(), "..", "R", "source_project.R"))
  source(.src[file.exists(.src)][1])
}
source_project_file("02_load_raw.R")

# -----------------------------------------------------------------------------
# apply_hard_bounds()
# Removes values outside physiologically impossible ranges.
# Logs each removal to the exclusions table.
# -----------------------------------------------------------------------------
apply_hard_bounds <- function(df, exclusions = list()) {
  for (m in names(QC_RULES)) {
    rules <- QC_RULES[[m]]
    sub <- df |> dplyr::filter(metric == m, !is.na(value))
    if (nrow(sub) == 0) next

    bad <- sub |>
      dplyr::filter(value < rules$hard_min | value > rules$hard_max)

    if (nrow(bad) > 0) {
      exclusions <- c(exclusions, list(
        bad |>
          dplyr::mutate(
            rule = sprintf("hard_bounds [%.0f, %.0f]", rules$hard_min, rules$hard_max),
            action = "excluded"
          ) |>
          dplyr::select(device, metric, date, value, rule, action, source_file)
      ))
      df <- df |>
        dplyr::anti_join(bad |> dplyr::select(device, metric, date), by = c("device", "metric", "date"))
    }
  }
  list(data = df, exclusions = exclusions)
}

# -----------------------------------------------------------------------------
# apply_soft_flags()
# Flags values outside typical ranges or with large day-to-day jumps.
# Flagged rows are kept but marked qc_flag = TRUE for sensitivity analysis.
# -----------------------------------------------------------------------------
apply_soft_flags <- function(df, exclusions = list()) {
  df <- df |> dplyr::mutate(qc_flag = FALSE, qc_note = NA_character_)

  for (m in names(QC_RULES)) {
    rules <- QC_RULES[[m]]
    sub_idx <- which(df$metric == m & !is.na(df$value))
    if (length(sub_idx) == 0) next

    # Soft range flags
    soft_bad <- sub_idx[df$value[sub_idx] < rules$soft_min | df$value[sub_idx] > rules$soft_max]
    if (length(soft_bad) > 0) {
      df$qc_flag[soft_bad] <- TRUE
      df$qc_note[soft_bad] <- sprintf("outside_soft_range [%.0f, %.0f]", rules$soft_min, rules$soft_max)
    }

    # Day-to-day jump flags (within each device)
    jump <- df |>
      dplyr::filter(metric == m, !is.na(value)) |>
      dplyr::arrange(device, date) |>
      dplyr::group_by(device) |>
      dplyr::mutate(
        prev = dplyr::lag(value),
        jump_abs = abs(value - prev),
        jump_pct = if (m == "steps") jump_abs / pmax(prev, 1) else NA_real_
      ) |>
      dplyr::ungroup()

    if (m == "steps") {
      jump_rows <- jump |>
        dplyr::filter(!is.na(jump_pct), jump_pct > rules$jump_pct)
    } else {
      jump_rows <- jump |>
        dplyr::filter(!is.na(jump_abs), jump_abs > rules$jump)
    }

    if (nrow(jump_rows) > 0) {
      for (i in seq_len(nrow(jump_rows))) {
        idx <- which(df$device == jump_rows$device[i] &
                       df$metric == jump_rows$metric[i] &
                       df$date == jump_rows$date[i])
        df$qc_flag[idx] <- TRUE
        df$qc_note[idx] <- paste(df$qc_note[idx], "large_day_jump", sep = ";")
      }
    }
  }

  list(data = df, exclusions = exclusions)
}

# -----------------------------------------------------------------------------
# drop_missing_values()
# Removes rows where the metric value is NA (e.g. Whoop sleep gaps).
# -----------------------------------------------------------------------------
drop_missing_values <- function(df, exclusions = list()) {
  na_rows <- df |> dplyr::filter(is.na(value))
  if (nrow(na_rows) > 0) {
    exclusions <- c(exclusions, list(
      na_rows |>
        dplyr::mutate(rule = "missing_value", action = "excluded") |>
        dplyr::select(device, metric, date, value, rule, action, source_file)
    ))
  }
  list(data = df |> dplyr::filter(!is.na(value)), exclusions = exclusions)
}

# -----------------------------------------------------------------------------
# clean_raw_data()
# Runs the full QC pipeline and returns cleaned data + exclusion log.
# -----------------------------------------------------------------------------
clean_raw_data <- function(raw_df) {
  excl <- list()

  step1 <- drop_missing_values(raw_df, excl)
  excl  <- step1$exclusions

  step2 <- apply_hard_bounds(step1$data, excl)
  excl  <- step2$exclusions

  step3 <- apply_soft_flags(step2$data, excl)

  qc_exclusions <- if (length(step3$exclusions) > 0) {
    dplyr::bind_rows(step3$exclusions)
  } else {
    tibble::tibble(device = character(), metric = character(), date = as.Date(character()),
                   value = numeric(), rule = character(), action = character(),
                   source_file = character())
  }

  list(data = step3$data, qc_exclusions = qc_exclusions)
}
