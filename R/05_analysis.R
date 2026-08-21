# =============================================================================
# 05_analysis.R
# Purpose: Statistical comparison functions — pairwise agreement plus
#          simultaneous three-device tests (ICC, Friedman, OCCC, Kendall's W).
# Each function is documented for readers new to R.
# =============================================================================

if (!exists("source_project_file")) {
  .src <- c(file.path(getwd(), "R", "source_project.R"), file.path(getwd(), "..", "R", "source_project.R"))
  source(.src[file.exists(.src)][1])
}
source_project_file("04_harmonize.R")

# -----------------------------------------------------------------------------
# device_metric_col()
# Returns the wide-table column name for a given device and metric.
# Example: device_metric_col("whoop", "rhr") -> "whoop_rhr"
# -----------------------------------------------------------------------------
device_metric_col <- function(device, metric) {
  paste0(device, "_", metric)
}

# -----------------------------------------------------------------------------
# get_pairwise_data()
# Extracts paired non-missing values for two devices on one metric.
# Input:  wide_df, metric, device_a, device_b
# Output: tibble with columns date, a, b
# -----------------------------------------------------------------------------
get_pairwise_data <- function(wide_df, metric, device_a, device_b) {
  col_a <- device_metric_col(device_a, metric)
  col_b <- device_metric_col(device_b, metric)
  if (!all(c(col_a, col_b) %in% names(wide_df))) return(NULL)

  wide_df |>
    dplyr::select(date, a = dplyr::all_of(col_a), b = dplyr::all_of(col_b)) |>
    dplyr::filter(!is.na(a), !is.na(b))
}

# -----------------------------------------------------------------------------
# correlation_stats()
# Computes Pearson and Spearman correlation with p-values for a paired sample.
# -----------------------------------------------------------------------------
correlation_stats <- function(pair_df) {
  if (is.null(pair_df) || nrow(pair_df) < 3) return(NULL)

  pearson  <- stats::cor.test(pair_df$a, pair_df$b, method = "pearson")
  spearman <- stats::cor.test(pair_df$a, pair_df$b, method = "spearman", exact = FALSE)

  tibble::tibble(
    n = nrow(pair_df),
    pearson_r = unname(pearson$estimate),
    pearson_p = pearson$p.value,
    spearman_rho = unname(spearman$estimate),
    spearman_p = spearman$p.value
  )
}

# -----------------------------------------------------------------------------
# bland_altman_stats()
# Computes Bland-Altman agreement statistics.
# mean_diff = bias; loa = bias +/- 1.96 * SD of differences
# -----------------------------------------------------------------------------
bland_altman_stats <- function(pair_df) {
  if (is.null(pair_df) || nrow(pair_df) < 3) return(NULL)

  diff <- pair_df$b - pair_df$a
  mean_val <- (pair_df$a + pair_df$b) / 2
  bias <- mean(diff, na.rm = TRUE)
  sd_diff <- stats::sd(diff, na.rm = TRUE)

  tibble::tibble(
    n = nrow(pair_df),
    mean_bias = bias,
    sd_diff = sd_diff,
    loa_lower = bias - 1.96 * sd_diff,
    loa_upper = bias + 1.96 * sd_diff,
    mean_of_means = mean(mean_val, na.rm = TRUE)
  )
}

# -----------------------------------------------------------------------------
# error_metrics()
# MAE, RMSE, and MAPE between two devices.
# MAE  = mean absolute error
# RMSE = root mean squared error
# MAPE = mean absolute percentage error (relative to device A)
# -----------------------------------------------------------------------------
error_metrics <- function(pair_df) {
  if (is.null(pair_df) || nrow(pair_df) < 1) return(NULL)

  diff <- pair_df$b - pair_df$a
  tibble::tibble(
    n = nrow(pair_df),
    mae = mean(abs(diff), na.rm = TRUE),
    rmse = sqrt(mean(diff^2, na.rm = TRUE)),
    mape = mean(abs(diff) / pmax(abs(pair_df$a), 1e-6), na.rm = TRUE) * 100
  )
}

# -----------------------------------------------------------------------------
# icc_value()
# Intraclass correlation ICC(2,1) — absolute agreement between two measurements.
# Values near 1 indicate strong agreement; uses the irr package.
# -----------------------------------------------------------------------------
icc_value <- function(pair_df) {
  if (is.null(pair_df) || nrow(pair_df) < 3) return(NA_real_)
  mat <- as.matrix(pair_df[, c("a", "b")])
  tryCatch({
    res <- irr::icc(mat, model = "twoway", type = "agreement", unit = "single")
    res$value
  }, error = function(e) NA_real_)
}

# -----------------------------------------------------------------------------
# ccc_value()
# Lin's concordance correlation coefficient — combines precision and accuracy.
# Uses the psych package.
# -----------------------------------------------------------------------------
ccc_value <- function(pair_df) {
  if (is.null(pair_df) || nrow(pair_df) < 3) return(NA_real_)
  tryCatch({
    res <- psych::concordance(pair_df$a, pair_df$b)
    res$rho.c[1]
  }, error = function(e) NA_real_)
}

# -----------------------------------------------------------------------------
# run_pairwise_analysis()
# Runs all agreement statistics for one metric and device pair.
# -----------------------------------------------------------------------------
run_pairwise_analysis <- function(wide_df, metric, device_a, device_b, label_a, label_b) {
  pair <- get_pairwise_data(wide_df, metric, device_a, device_b)
  if (is.null(pair) || nrow(pair) < 3) return(NULL)

  cor_stats <- correlation_stats(pair)
  ba_stats  <- bland_altman_stats(pair)
  err       <- error_metrics(pair)

  tibble::tibble(
    metric = metric,
    device_a = label_a,
    device_b = label_b,
    n = cor_stats$n,
    pearson_r = cor_stats$pearson_r,
    pearson_p = cor_stats$pearson_p,
    spearman_rho = cor_stats$spearman_rho,
    spearman_p = cor_stats$spearman_p,
    mean_bias = ba_stats$mean_bias,
    loa_lower = ba_stats$loa_lower,
    loa_upper = ba_stats$loa_upper,
    mae = err$mae,
    rmse = err$rmse,
    mape = err$mape,
    icc = icc_value(pair),
    ccc = ccc_value(pair),
    deming_slope = deming_slope(pair),
    deming_intercept = deming_intercept(pair)
  )
}

# -----------------------------------------------------------------------------
# deming_fit()
# Deming regression treats BOTH devices as having measurement error (unlike
# ordinary least squares, which assumes the x-axis is error-free). A slope near
# 1 and intercept near 0 means no proportional or fixed bias between devices.
# -----------------------------------------------------------------------------
deming_fit <- function(pair_df) {
  if (is.null(pair_df) || nrow(pair_df) < 4) return(NULL)
  tryCatch(deming::deming(b ~ a, data = pair_df), error = function(e) NULL)
}

deming_slope <- function(pair_df) {
  fit <- deming_fit(pair_df)
  if (is.null(fit)) return(NA_real_)
  unname(fit$coefficients[2])
}

deming_intercept <- function(pair_df) {
  fit <- deming_fit(pair_df)
  if (is.null(fit)) return(NA_real_)
  unname(fit$coefficients[1])
}

# All three device pairs. get_pairwise_data() returns NULL when a device lacks
# a metric (e.g. Withings HRV), so pairs are skipped automatically.
DEVICE_PAIRS <- list(
  c("whoop", "oura", "Whoop", "Oura Ring 4"),
  c("whoop", "withings", "Whoop", "Withings ScanWatch"),
  c("oura", "withings", "Oura Ring 4", "Withings ScanWatch")
)

# -----------------------------------------------------------------------------
# run_all_agreement()
# Runs pairwise analysis for every metric and applicable device pair.
# `metrics` defaults to all comparable metrics.
# -----------------------------------------------------------------------------
run_all_agreement <- function(wide_df, metrics = ALL_METRICS) {
  results <- list()

  for (m in metrics) {
    for (p in DEVICE_PAIRS) {
      res <- run_pairwise_analysis(wide_df, m, p[1], p[2], p[3], p[4])
      if (!is.null(res)) results <- c(results, list(res))
    }
  }

  if (length(results) == 0) return(NULL)
  dplyr::bind_rows(results)
}

# -----------------------------------------------------------------------------
# descriptive_stats()
# Summary statistics (mean, SD, median, IQR, CV) per device and metric.
# CV = coefficient of variation (SD / mean * 100) — day-to-day stability proxy.
# -----------------------------------------------------------------------------
descriptive_stats <- function(long_df) {
  long_df |>
    dplyr::group_by(device, metric) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      q25 = stats::quantile(value, 0.25, na.rm = TRUE),
      q75 = stats::quantile(value, 0.75, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      cv_pct = sd / mean * 100,
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# overlap_counts()
# Summarises how many days fall into each device-combination category per metric.
# -----------------------------------------------------------------------------
overlap_counts <- function(overlap_df) {
  overlap_df |>
    dplyr::group_by(metric, combo) |>
    dplyr::summarise(n_days = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(metric, dplyr::desc(n_days))
}

# -----------------------------------------------------------------------------
# weekday_weekend_stats()
# Compares mean values on weekdays vs weekends per device and metric.
# -----------------------------------------------------------------------------
weekday_weekend_stats <- function(long_df) {
  long_df |>
    dplyr::mutate(
      day_type = ifelse(lubridate::wday(date, label = TRUE) %in% c("Sat", "Sun"),
                        "weekend", "weekday")
    ) |>
    dplyr::group_by(device, metric, day_type) |>
    dplyr::summarise(mean = mean(value, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
}

# -----------------------------------------------------------------------------
# rank_consistency()
# On days where all available devices have a metric, do they rank the day
# similarly? Reports Spearman correlation of within-day ranks across devices.
# -----------------------------------------------------------------------------
# Fix rank_consistency filter — use base subsetting for compatibility.
rank_consistency <- function(wide_df, metric, devices) {
  cols <- vapply(devices, device_metric_col, character(1), metric = metric)
  if (!all(cols %in% names(wide_df))) return(NULL)

  sub <- wide_df[, c("date", cols), drop = FALSE]
  complete <- complete.cases(sub[, cols, drop = FALSE])
  sub <- sub[complete, , drop = FALSE]

  if (nrow(sub) < 5) return(NULL)

  mat <- as.matrix(sub[, cols, drop = FALSE])
  ranks <- apply(mat, 2, rank)
  cors <- stats::cor(ranks, method = "spearman")

  as.data.frame(as.table(cors)) |>
    tibble::as_tibble() |>
    dplyr::rename(device_a = Var1, device_b = Var2, spearman = Freq) |>
    dplyr::mutate(metric = metric) |>
    dplyr::filter(device_a != device_b)
}

# -----------------------------------------------------------------------------
# rolling_correlation()
# Computes a 7-day rolling Pearson correlation between two devices for one
# metric. Shows whether agreement is stable or drifts over the study window.
# Input:  wide_df, metric, device_a, device_b, window (days)
# Output: tibble with date, rolling_r, n (days in window)
# -----------------------------------------------------------------------------
rolling_correlation <- function(wide_df, metric, device_a, device_b, window = 7) {
  pair <- get_pairwise_data(wide_df, metric, device_a, device_b)
  if (is.null(pair) || nrow(pair) < window) return(NULL)

  pair <- pair |> dplyr::arrange(date)
  n <- length(pair$a)
  rolls <- numeric(n)
  counts <- integer(n)

  for (i in seq_len(n)) {
    start <- max(1, i - window + 1)
    a_win <- pair$a[start:i]
    b_win <- pair$b[start:i]
    ok <- stats::complete.cases(a_win, b_win)
    counts[i] <- sum(ok)
    rolls[i] <- if (counts[i] >= 3) stats::cor(a_win[ok], b_win[ok]) else NA_real_
  }

  tibble::tibble(date = pair$date, rolling_r = rolls, n = counts)
}

# -----------------------------------------------------------------------------
# get_threeway_data()
# Days where ALL listed devices recorded the metric (complete-case matrix).
# Pairwise tests can use days where only two devices overlap; three-way tests
# require the same days for every device so the comparison is simultaneous.
# Output: tibble with date plus one column per device, or NULL if too few days.
# -----------------------------------------------------------------------------
get_threeway_data <- function(wide_df, metric, devices = c("whoop", "oura", "withings")) {
  cols <- vapply(devices, device_metric_col, character(1), metric = metric)
  if (!all(cols %in% names(wide_df))) return(NULL)

  sub <- wide_df[, c("date", cols), drop = FALSE]
  names(sub) <- c("date", devices)
  sub <- sub[stats::complete.cases(sub[, devices, drop = FALSE]), , drop = FALSE]
  if (nrow(sub) < 5) return(NULL)
  sub
}

# -----------------------------------------------------------------------------
# occc_value()
# Barnhart overall concordance correlation coefficient (OCCC) — the k-rater
# generalization of Lin's CCC. Uses all devices in one number instead of
# averaging pairwise CCCs. Near 1 = all devices agree on both trend and level.
# Formula: Barnhart, Haber & Lin (2002), Biometrics.
# -----------------------------------------------------------------------------
occc_value <- function(mat) {
  if (is.null(mat) || nrow(mat) < 3 || ncol(mat) < 2) return(NA_real_)
  k <- ncol(mat)
  mus <- colMeans(mat, na.rm = TRUE)
  sds <- apply(mat, 2, stats::sd, na.rm = TRUE)
  if (any(!is.finite(sds)) || any(sds == 0)) return(NA_real_)
  cors <- stats::cor(mat, use = "complete.obs")

  num <- 0
  bias <- 0
  for (j in seq_len(k - 1L)) {
    for (l in (j + 1L):k) {
      num <- num + cors[j, l] * sds[j] * sds[l]
      bias <- bias + (mus[j] - mus[l])^2
    }
  }
  den <- (k - 1) * sum(sds^2) + bias
  if (!is.finite(den) || den == 0) return(NA_real_)
  as.numeric(2 * num / den)
}

# -----------------------------------------------------------------------------
# threeway_analysis()
# Simultaneous three-device tests on complete-case days for one metric.
#
# Agreement (do the three devices track the same days?):
#   - ICC(2,1) absolute agreement with 95% CI and F-test p-value
#   - OCCC (overall concordance correlation)
#   - Kendall's W and its chi-square p-value
#   - Cronbach's alpha (internal consistency of the three readings)
#
# Location (do the three devices sit at different levels?):
#   - Friedman rank test (non-parametric; no normality assumption)
#   - Repeated-measures ANOVA (parametric companion; assumes sphericity)
#
# Returns NULL when a device is missing the metric (e.g. Withings HRV).
# -----------------------------------------------------------------------------
threeway_analysis <- function(wide_df, metric, devices = c("whoop", "oura", "withings")) {
  sub <- get_threeway_data(wide_df, metric, devices)
  if (is.null(sub)) return(NULL)

  mat <- as.matrix(sub[, devices, drop = FALSE])
  n <- nrow(mat)

  icc_res <- tryCatch(
    irr::icc(mat, model = "twoway", type = "agreement", unit = "single"),
    error = function(e) NULL
  )
  kendall_res <- tryCatch(
    irr::kendall(mat, correct = TRUE),
    error = function(e) NULL
  )
  friedman_res <- tryCatch(
    stats::friedman.test(mat),
    error = function(e) NULL
  )

  long <- tidyr::pivot_longer(sub, -date, names_to = "device", values_to = "value")
  long$date <- factor(long$date)
  long$device <- factor(long$device, levels = devices)
  anova_res <- tryCatch({
    fit <- stats::aov(value ~ device + Error(date), data = long)
    sm <- summary(fit)
    # Device effect lives in the within-date stratum.
    within <- sm[["Error: Within"]][[1]]
    list(F = unname(within["device", "F value"]), p = unname(within["device", "Pr(>F)"]))
  }, error = function(e) NULL)

  alpha_res <- tryCatch({
    psych::alpha(mat, check.keys = FALSE, warnings = FALSE)$total$raw_alpha
  }, error = function(e) NA_real_)

  tibble::tibble(
    metric = metric,
    n_days = n,
    n_devices = ncol(mat),
    icc = if (is.null(icc_res)) NA_real_ else icc_res$value,
    icc_lbound = if (is.null(icc_res)) NA_real_ else icc_res$lbound,
    icc_ubound = if (is.null(icc_res)) NA_real_ else icc_res$ubound,
    icc_p = if (is.null(icc_res)) NA_real_ else icc_res$p.value,
    occc = occc_value(mat),
    cronbach_alpha = if (length(alpha_res) == 1) as.numeric(alpha_res) else NA_real_,
    kendall_w = if (is.null(kendall_res)) NA_real_ else kendall_res$value,
    kendall_p = if (is.null(kendall_res)) NA_real_ else kendall_res$p.value,
    friedman_chi2 = if (is.null(friedman_res)) NA_real_ else unname(friedman_res$statistic),
    friedman_p = if (is.null(friedman_res)) NA_real_ else friedman_res$p.value,
    anova_f = if (is.null(anova_res)) NA_real_ else anova_res$F,
    anova_p = if (is.null(anova_res)) NA_real_ else anova_res$p
  )
}

# -----------------------------------------------------------------------------
# threeway_posthoc()
# If Friedman (or RM-ANOVA) finds a difference among the three devices, this
# says WHICH pair differs. Wilcoxon signed-rank tests on the same complete-case
# days, Bonferroni-adjusted for the three pairs.
# -----------------------------------------------------------------------------
threeway_posthoc <- function(wide_df, metric, devices = c("whoop", "oura", "withings")) {
  sub <- get_threeway_data(wide_df, metric, devices)
  if (is.null(sub)) return(NULL)

  pairs <- utils::combn(devices, 2, simplify = FALSE)
  n_pairs <- length(pairs)
  rows <- lapply(pairs, function(p) {
    wt <- stats::wilcox.test(sub[[p[1]]], sub[[p[2]]], paired = TRUE, exact = FALSE)
    tibble::tibble(
      metric = metric,
      device_a = DEVICE_LABELS[[p[1]]],
      device_b = DEVICE_LABELS[[p[2]]],
      n_days = nrow(sub),
      median_diff = stats::median(sub[[p[2]]] - sub[[p[1]]], na.rm = TRUE),
      wilcox_p_raw = wt$p.value,
      wilcox_p_bonferroni = min(1, wt$p.value * n_pairs)
    )
  })
  dplyr::bind_rows(rows)
}

# -----------------------------------------------------------------------------
# run_all_threeway()
# Three-device tests for every metric with enough complete-case overlap.
# -----------------------------------------------------------------------------
run_all_threeway <- function(wide_df, metrics = ALL_METRICS,
                             devices = c("whoop", "oura", "withings")) {
  results <- lapply(metrics, function(m) threeway_analysis(wide_df, m, devices))
  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) return(NULL)
  out
}

run_all_threeway_posthoc <- function(wide_df, metrics = ALL_METRICS,
                                     devices = c("whoop", "oura", "withings")) {
  results <- lapply(metrics, function(m) threeway_posthoc(wide_df, m, devices))
  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) return(NULL)
  out
}

# -----------------------------------------------------------------------------
# kendall_w() / kendall_w_table()
# Kept for backward compatibility; threeway_analysis() is the full three-device
# test suite. W near 1 = all three rank days similarly; near 0 = no ranking
# agreement. Only defined when all three devices have data on the same days.
# -----------------------------------------------------------------------------
kendall_w <- function(wide_df, metric, devices = c("whoop", "oura", "withings")) {
  row <- threeway_analysis(wide_df, metric, devices)
  if (is.null(row)) return(NA_real_)
  row$kendall_w
}

kendall_w_table <- function(wide_df, metrics = ALL_METRICS) {
  tbl <- run_all_threeway(wide_df, metrics = metrics)
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  tbl |> dplyr::select(metric, kendall_w, kendall_p, n_days)
}

# -----------------------------------------------------------------------------
# cross_metric_correlation()
# Within each device, correlates two metrics (e.g. RHR vs HRV) across days.
# Sanity check: do the metrics move together as expected within a device?
# -----------------------------------------------------------------------------
cross_metric_correlation <- function(long_df, metric_x, metric_y) {
  wide <- long_df |>
    dplyr::filter(metric %in% c(metric_x, metric_y)) |>
    dplyr::mutate(
      device_key = dplyr::case_when(
        grepl("Whoop", device) ~ "whoop",
        grepl("Oura", device) ~ "oura",
        grepl("Withings", device) ~ "withings"
      ),
      col = paste0(device_key, "_", metric)
    ) |>
    dplyr::select(date, col, value) |>
    tidyr::pivot_wider(names_from = col, values_from = value)

  col_x <- paste0(c("whoop", "oura", "withings"), "_", metric_x)
  col_y <- paste0(c("whoop", "oura", "withings"), "_", metric_y)

  results <- list()
  for (dev in c("whoop", "oura", "withings")) {
    cx <- paste0(dev, "_", metric_x)
    cy <- paste0(dev, "_", metric_y)
    if (!all(c(cx, cy) %in% names(wide))) next
    sub <- wide[, c(cx, cy)]
    sub <- sub[stats::complete.cases(sub), , drop = FALSE]
    if (nrow(sub) < 5) next
    ct <- stats::cor.test(sub[[cx]], sub[[cy]], method = "spearman", exact = FALSE)
    results <- c(results, list(tibble::tibble(
      device = DEVICE_LABELS[[dev]],
      metric_x = metric_x, metric_y = metric_y,
      n = nrow(sub), spearman_rho = unname(ct$estimate), p_value = ct$p.value
    )))
  }
  if (length(results) == 0) return(NULL)
  dplyr::bind_rows(results)
}

# -----------------------------------------------------------------------------
# lagged_correlation()
# Exploratory: does today's step count predict tomorrow's RHR or HRV?
# Positive lag means metric_y on day t+1 vs metric_x on day t.
# -----------------------------------------------------------------------------
lagged_correlation <- function(long_df, metric_x, metric_y, lag_days = 1) {
  wide <- long_df |>
    dplyr::filter(metric %in% c(metric_x, metric_y)) |>
    dplyr::mutate(
      device_key = dplyr::case_when(
        grepl("Whoop", device) ~ "whoop",
        grepl("Oura", device) ~ "oura",
        grepl("Withings", device) ~ "withings"
      )
    ) |>
    dplyr::select(device_key, date, metric, value) |>
    tidyr::pivot_wider(names_from = metric, values_from = value)

  results <- list()
  for (dev in c("whoop", "oura", "withings")) {
    sub <- wide |> dplyr::filter(device_key == dev) |> dplyr::arrange(date)
    if (!all(c(metric_x, metric_y) %in% names(sub))) next
    sub <- sub |>
      dplyr::mutate(y_lag = dplyr::lead(.data[[metric_y]], lag_days)) |>
      dplyr::filter(!is.na(.data[[metric_x]]), !is.na(y_lag))
    if (nrow(sub) < 5) next
    ct <- stats::cor.test(sub[[metric_x]], sub$y_lag, method = "spearman", exact = FALSE)
    results <- c(results, list(tibble::tibble(
      device = DEVICE_LABELS[[dev]],
      predictor = metric_x, outcome = metric_y,
      lag_days = lag_days, n = nrow(sub),
      spearman_rho = unname(ct$estimate), p_value = ct$p.value
    )))
  }
  if (length(results) == 0) return(NULL)
  dplyr::bind_rows(results)
}

# -----------------------------------------------------------------------------
# compliance_summary()
# For each device and metric, reports how many days in the analysis window
# had valid data (compliance %). Helps quantify which device "saw" more days.
# -----------------------------------------------------------------------------
compliance_summary <- function(long_df) {
  total_days <- as.integer(WINDOW_END - WINDOW_START) + 1L
  long_df |>
    dplyr::group_by(device, metric) |>
    dplyr::summarise(
      days_present = dplyr::n(),
      compliance_pct = days_present / total_days * 100,
      .groups = "drop"
    ) |>
    dplyr::arrange(metric, dplyr::desc(compliance_pct))
}

# -----------------------------------------------------------------------------
# sensitivity_agreement()
# Re-runs agreement stats excluding days flagged by QC (qc_flag == TRUE).
# Compares to the full-sample agreement to show robustness.
# Input:  long_df with qc_flag column, wide_df, metric list
# Output: agreement tibble with a 'sample' column ("full" or "clean")
# -----------------------------------------------------------------------------
sensitivity_agreement <- function(long_df, wide_df, metrics = METRICS) {
  flagged_dates <- long_df |>
    dplyr::filter(qc_flag == TRUE) |>
    dplyr::select(date, device, metric) |>
    dplyr::distinct()

  clean_long <- long_df |>
    dplyr::anti_join(flagged_dates, by = c("date", "device", "metric"))

  clean_wide <- build_daily_wide(clean_long)

  full  <- run_all_agreement(wide_df, metrics = metrics)  |> dplyr::mutate(sample = "full")
  clean <- run_all_agreement(clean_wide, metrics = metrics) |> dplyr::mutate(sample = "clean")

  dplyr::bind_rows(full, clean)
}

# -----------------------------------------------------------------------------
# executive_summary()
# One-row-per-metric verdict table: best-agreeing pair, Pearson r, bias, n.
# Plain-language summary for the report introduction.
# -----------------------------------------------------------------------------
executive_summary <- function(agreement_df) {
  if (is.null(agreement_df) || nrow(agreement_df) == 0) return(NULL)

  agreement_df |>
    dplyr::group_by(metric) |>
    dplyr::slice_max(order_by = abs(pearson_r), n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      metric = METRIC_LABELS[metric],
      best_pair = paste(device_a, "vs", device_b),
      pearson_r = round(pearson_r, 3),
      mean_bias = round(mean_bias, 2),
      n_days = n,
      verdict = dplyr::case_when(
        abs(pearson_r) >= 0.9 ~ "Strong agreement",
        abs(pearson_r) >= 0.7 ~ "Moderate agreement",
        abs(pearson_r) >= 0.5 ~ "Weak agreement",
        TRUE                    ~ "Poor agreement"
      )
    )
}

# -----------------------------------------------------------------------------
# sleep_architecture_wide()
# Builds a wide table of sleep-stage minutes per device per night, for stacked
# bar plots. Columns: date, device, deep_min, rem_min, light_min.
# -----------------------------------------------------------------------------
sleep_architecture_wide <- function(long_df) {
  long_df |>
    dplyr::filter(metric %in% SLEEP_STAGE_METRICS) |>
    dplyr::select(date, device, metric, value) |>
    tidyr::pivot_wider(names_from = metric, values_from = value)
}
