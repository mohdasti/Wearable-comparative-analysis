# =============================================================================
# 06_plots.R
# Purpose: Reusable ggplot2 functions for the Quarto report.
# =============================================================================

if (!exists("source_project_file")) {
  .src <- c(file.path(getwd(), "R", "source_project.R"), file.path(getwd(), "..", "R", "source_project.R"))
  source(.src[file.exists(.src)][1])
}
source_project_file("05_analysis.R")

# High-contrast device palette: Red = Whoop, Green = Oura, Blue = Withings.
DEVICE_COLORS <- c(
  "Whoop"              = "#E63946",
  "Oura Ring 4"        = "#2A9D8F",
  "Withings ScanWatch" = "#457B9D"
)

# Sleep-stage fill colors (consistent across architecture plots).
STAGE_COLORS <- c(
  deep_min  = "#1D3557",
  rem_min   = "#457B9D",
  light_min = "#A8DADC"
)

# Null-coalesce helper (avoid requiring rlang explicitly).
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# -----------------------------------------------------------------------------
# plot_timeseries()
# Daily time series with gap handling (no line drawn across missing days).
# -----------------------------------------------------------------------------
plot_timeseries <- function(long_df, metric_name) {
  sub <- long_df |> dplyr::filter(metric == metric_name)

  ggplot2::ggplot(sub, ggplot2::aes(x = date, y = value, color = device, group = device)) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = DEVICE_COLORS) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::labs(
      x = "Date", y = METRIC_LABELS[[metric_name]],
      color = "Device",
      title = paste("Daily", METRIC_LABELS[[metric_name]])
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# -----------------------------------------------------------------------------
# plot_ridgeline()
# Ridgeline (joyplot) density — cleaner than overlapping densities for 3 devices.
# Requires ggridges package.
# -----------------------------------------------------------------------------
plot_ridgeline <- function(long_df, metric_name) {
  sub <- long_df |> dplyr::filter(metric == metric_name)

  ggplot2::ggplot(sub, ggplot2::aes(x = value, y = device, fill = device)) +
    ggridges::geom_density_ridges(alpha = 0.75, scale = 1.1, rel_min_height = 0.01) +
    ggplot2::scale_fill_manual(values = DEVICE_COLORS) +
    ggplot2::labs(
      x = METRIC_LABELS[[metric_name]], y = NULL,
      title = paste("Distribution —", METRIC_LABELS[[metric_name]])
    ) +
    ggridges::theme_ridges() +
    ggplot2::theme(legend.position = "none")
}

# -----------------------------------------------------------------------------
# plot_overlap_heatmap()
# Calendar heatmap showing which devices have data on each day for one metric.
# -----------------------------------------------------------------------------
plot_overlap_heatmap <- function(overlap_df, metric_name) {
  sub <- overlap_df |>
    dplyr::filter(metric == metric_name) |>
    dplyr::mutate(
      whoop_y = ifelse(!is.na(whoop), 1, NA),
      oura_y = ifelse(!is.na(oura), 2, NA),
      withings_y = ifelse(!is.na(withings), 3, NA)
    )

  long_presence <- sub |>
    tidyr::pivot_longer(c(whoop_y, oura_y, withings_y),
                        names_to = "dev", values_to = "y") |>
    dplyr::filter(!is.na(y)) |>
    dplyr::mutate(
      device_label = dplyr::case_when(
        dev == "whoop_y" ~ "Whoop",
        dev == "oura_y" ~ "Oura Ring 4",
        dev == "withings_y" ~ "Withings ScanWatch"
      )
    )

  ggplot2::ggplot(long_presence, ggplot2::aes(x = date, y = device_label)) +
    ggplot2::geom_tile(ggplot2::aes(fill = device_label), color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = DEVICE_COLORS) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::labs(
      x = "Date", y = NULL,
      title = paste("Data availability —", METRIC_LABELS[[metric_name]])
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# -----------------------------------------------------------------------------
# plot_scatter_pair()
# Scatter plot with identity line and Deming regression line.
# -----------------------------------------------------------------------------
plot_scatter_pair <- function(wide_df, metric, device_a, device_b, label_a, label_b) {
  pair <- get_pairwise_data(wide_df, metric, device_a, device_b)
  if (is.null(pair) || nrow(pair) < 2) return(NULL)

  point_color <- DEVICE_COLORS[[label_b]] %||% "#333333"
  line_color  <- DEVICE_COLORS[[label_a]] %||% "#666666"
  slope <- deming_slope(pair)
  intercept <- deming_intercept(pair)

  p <- ggplot2::ggplot(pair, ggplot2::aes(x = a, y = b)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_point(size = 3, alpha = 0.85, color = point_color) +
    ggplot2::labs(
      x = paste(label_a, "—", METRIC_LABELS[[metric]]),
      y = paste(label_b, "—", METRIC_LABELS[[metric]]),
      title = paste(label_a, "vs", label_b)
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (!is.na(slope)) {
    p <- p + ggplot2::geom_abline(
      slope = slope, intercept = intercept,
      color = line_color, linewidth = 0.8,
      linetype = "solid"
    )
  }
  p
}

# -----------------------------------------------------------------------------
# plot_bland_altman()
# Bland-Altman with shaded LoA band and date labels on outlier points.
# -----------------------------------------------------------------------------
plot_bland_altman <- function(wide_df, metric, device_a, device_b, label_a, label_b) {
  pair <- get_pairwise_data(wide_df, metric, device_a, device_b)
  if (is.null(pair) || nrow(pair) < 2) return(NULL)

  point_color <- DEVICE_COLORS[[label_b]] %||% "#333333"
  bias_color  <- DEVICE_COLORS[[label_a]] %||% "#666666"

  pair <- pair |>
    dplyr::mutate(
      mean_val = (a + b) / 2,
      diff_val = b - a
    )

  ba <- bland_altman_stats(pair)

  ggplot2::ggplot(pair, ggplot2::aes(x = mean_val, y = diff_val)) +
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = Inf,
      ymin = ba$loa_lower, ymax = ba$loa_upper,
      fill = bias_color, alpha = 0.08
    ) +
    ggplot2::geom_hline(yintercept = ba$mean_bias, color = bias_color, linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = c(ba$loa_lower, ba$loa_upper),
                        linetype = "dashed", color = "gray50", linewidth = 0.6) +
    ggplot2::geom_point(size = 3, alpha = 0.85, color = point_color) +
    ggplot2::labs(
      x = paste("Mean of", label_a, "&", label_b),
      y = paste("Difference (", label_b, "−", label_a, ")", sep = ""),
      title = paste("Bland–Altman:", label_a, "vs", label_b),
      subtitle = sprintf("Bias = %.2f  |  LoA [%.2f, %.2f]",
                         ba$mean_bias, ba$loa_lower, ba$loa_upper)
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

# -----------------------------------------------------------------------------
# plot_difference_timeseries()
# Shows (device B − device A) for each day — complements Bland-Altman by
# revealing whether bias drifts over time.
# -----------------------------------------------------------------------------
plot_difference_timeseries <- function(wide_df, metric, device_a, device_b, label_a, label_b) {
  pair <- get_pairwise_data(wide_df, metric, device_a, device_b)
  if (is.null(pair) || nrow(pair) < 2) return(NULL)

  pair <- pair |>
    dplyr::mutate(
      mean_val = (a + b) / 2,
      diff_val = b - a
    )

  bias <- mean(pair$diff_val, na.rm = TRUE)

  ggplot2::ggplot(pair, ggplot2::aes(x = date, y = diff_val)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = bias, color = DEVICE_COLORS[[label_a]] %||% "gray40", linewidth = 0.8) +
    ggplot2::geom_line(color = DEVICE_COLORS[[label_b]] %||% "#333333", linewidth = 0.7) +
    ggplot2::geom_point(size = 2.5, color = DEVICE_COLORS[[label_b]] %||% "#333333", alpha = 0.85) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::labs(
      x = "Date",
      y = paste(label_b, "−", label_a),
      title = paste("Daily difference —", METRIC_LABELS[[metric]]),
      subtitle = sprintf("Mean bias = %.2f", bias)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# -----------------------------------------------------------------------------
# plot_rolling_correlation()
# 7-day rolling Pearson r between two devices for one metric.
# -----------------------------------------------------------------------------
plot_rolling_correlation <- function(wide_df, metric, device_a, device_b, label_a, label_b, window = 7) {
  rolls <- rolling_correlation(wide_df, metric, device_a, device_b, window = window)
  if (is.null(rolls) || all(is.na(rolls$rolling_r))) return(NULL)

  ggplot2::ggplot(rolls, ggplot2::aes(x = date, y = rolling_r)) +
    ggplot2::geom_hline(yintercept = c(0, 0.7, 0.9), linetype = c("solid", "dotted", "dotted"),
                        color = c("gray50", "gray70", "gray70"), linewidth = c(0.5, 0.4, 0.4)) +
    ggplot2::geom_line(color = DEVICE_COLORS[[label_b]] %||% "#333333", linewidth = 0.9) +
    ggplot2::geom_point(size = 2, color = DEVICE_COLORS[[label_b]] %||% "#333333", alpha = 0.7) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::ylim(-0.2, 1) +
    ggplot2::labs(
      x = "Date", y = "Rolling Pearson r",
      title = paste(window, "-day rolling correlation:", label_a, "vs", label_b),
      subtitle = METRIC_LABELS[[metric]]
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# -----------------------------------------------------------------------------
# plot_correlation_heatmap()
# Pearson r heatmap with significance stars (p < 0.05 = *, p < 0.01 = **).
# -----------------------------------------------------------------------------
plot_correlation_heatmap <- function(agreement_df) {
  if (is.null(agreement_df) || nrow(agreement_df) == 0) return(NULL)

  plot_df <- agreement_df |>
    dplyr::mutate(
      pair = paste(device_a, "vs", device_b),
      metric_label = METRIC_LABELS[metric],
      sig = dplyr::case_when(
        pearson_p < 0.01 ~ "**",
        pearson_p < 0.05 ~ "*",
        TRUE ~ ""
      ),
      label = sprintf("%.2f%s\n(n=%d)", pearson_r, sig, n)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = pair, y = metric_label, fill = pearson_r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3, color = "white") +
    # Midpoint 0 so the color break sits at "no correlation"; a midpoint of 0.5
    # would paint a genuinely uncorrelated pair (r = 0) as if it were negative.
    ggplot2::scale_fill_gradient2(
      low = "#457B9D", mid = "gray85", high = "#E63946",
      midpoint = 0, limits = c(-1, 1), name = "Pearson r"
    ) +
    ggplot2::labs(x = "Device pair", y = "Metric",
                  title = "Correlation matrix across device pairs",
                  subtitle = "* p<0.05  ** p<0.01") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# -----------------------------------------------------------------------------
# plot_sleep_architecture()
# Stacked bar chart of sleep stages (deep / REM / light) per night, faceted by device.
# Input: output of sleep_architecture_wide()
# -----------------------------------------------------------------------------
plot_sleep_architecture <- function(arch_df, n_nights = 14) {
  if (is.null(arch_df) || nrow(arch_df) == 0) return(NULL)

  # Show the most recent n_nights with data from all devices if possible.
  sub <- arch_df |>
    dplyr::filter(!is.na(deep_min) | !is.na(rem_min) | !is.na(light_min)) |>
    dplyr::arrange(date) |>
    dplyr::slice_tail(n = n_nights * 3)

  sub <- sub |>
    tidyr::pivot_longer(c(deep_min, rem_min, light_min),
                        names_to = "stage", values_to = "minutes") |>
    dplyr::mutate(
      stage = factor(stage, levels = c("deep_min", "rem_min", "light_min"),
                     labels = c("Deep", "REM", "Light"))
    )

  ggplot2::ggplot(sub, ggplot2::aes(x = date, y = minutes, fill = stage)) +
    ggplot2::geom_col(position = "stack", width = 0.7) +
    ggplot2::facet_wrap(~ device, ncol = 1, scales = "free_x") +
    ggplot2::scale_fill_manual(values = c("Deep" = STAGE_COLORS["deep_min"],
                                          "REM"  = STAGE_COLORS["rem_min"],
                                          "Light" = STAGE_COLORS["light_min"])) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::labs(
      x = "Wake date", y = "Minutes", fill = "Stage",
      title = "Sleep architecture — stage composition per night"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# -----------------------------------------------------------------------------
# plot_paired_dumbbell()
# For one metric, shows the spread between devices on each day as a dumbbell/lollipop.
# Useful for seeing which device reads higher on which nights.
# -----------------------------------------------------------------------------
plot_paired_dumbbell <- function(wide_df, metric, devices = c("whoop", "oura", "withings"),
                                 complete_only = FALSE) {
  cols <- vapply(devices, device_metric_col, character(1), metric = metric)
  if (!all(cols %in% names(wide_df))) return(NULL)

  labels <- DEVICE_LABELS[devices]
  sub <- wide_df |>
    dplyr::select(date, dplyr::all_of(cols)) |>
    tidyr::pivot_longer(-date, names_to = "col", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      device = dplyr::case_when(
        grepl("whoop", col) ~ "Whoop",
        grepl("oura", col) ~ "Oura Ring 4",
        grepl("withings", col) ~ "Withings ScanWatch"
      )
    )

  # complete_only = TRUE keeps days where every device recorded (three-way tests).
  # Otherwise keep days with at least two devices (pairwise plots).
  min_n <- if (isTRUE(complete_only)) length(devices) else 2L
  day_counts <- sub |> dplyr::count(date) |> dplyr::filter(n >= min_n)
  sub <- sub |> dplyr::inner_join(day_counts, by = "date")

  ggplot2::ggplot(sub, ggplot2::aes(x = value, y = reorder(date, date), color = device)) +
    ggplot2::geom_line(ggplot2::aes(group = date), color = "gray70", linewidth = 0.6) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = DEVICE_COLORS) +
    ggplot2::labs(
      x = METRIC_LABELS[[metric]], y = "Date", color = "Device",
      title = paste(
        if (isTRUE(complete_only)) "Three-device complete-case spread —" else "Daily spread —",
        METRIC_LABELS[[metric]]
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

# -----------------------------------------------------------------------------
# plot_stage_composition()
# Mean share of total sleep spent in each stage, per device. Comparing stage
# MINUTES confounds two things: how the device splits stages and how much total
# sleep it recorded. Percentages isolate the staging algorithm itself.
# -----------------------------------------------------------------------------
plot_stage_composition <- function(long_df) {
  sub <- long_df |>
    dplyr::filter(metric %in% SLEEP_STAGE_PCT_METRICS) |>
    dplyr::group_by(device, metric) |>
    dplyr::summarise(mean_pct = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      stage = factor(metric, levels = c("deep_pct", "rem_pct", "light_pct"),
                     labels = c("Deep", "REM", "Light"))
    )
  if (nrow(sub) == 0) return(NULL)

  ggplot2::ggplot(sub, ggplot2::aes(x = device, y = mean_pct, fill = stage)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f%%", mean_pct)),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white", size = 3.5
    ) +
    ggplot2::scale_fill_manual(values = c("Deep" = unname(STAGE_COLORS["deep_min"]),
                                          "REM"  = unname(STAGE_COLORS["rem_min"]),
                                          "Light" = unname(STAGE_COLORS["light_min"]))) +
    ggplot2::labs(
      x = NULL, y = "Mean share of total sleep (%)", fill = "Stage",
      title = "How each device divides a night of sleep into stages",
      subtitle = "Percentages remove the effect of differing total sleep time"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

# -----------------------------------------------------------------------------
# plot_threeway_box()
# Boxplots + jitter of the three devices on the SAME complete-case days used
# in Friedman / three-rater ICC. Missing a device drops the whole day, so the
# comparison is simultaneous rather than pairwise.
# -----------------------------------------------------------------------------
plot_threeway_box <- function(wide_df, metric, devices = c("whoop", "oura", "withings")) {
  sub <- get_threeway_data(wide_df, metric, devices)
  if (is.null(sub)) return(NULL)

  long <- tidyr::pivot_longer(sub, -date, names_to = "device_key", values_to = "value") |>
    dplyr::mutate(device = unname(DEVICE_LABELS[device_key]))

  ggplot2::ggplot(long, ggplot2::aes(x = device, y = value, fill = device)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.55) +
    ggplot2::geom_jitter(width = 0.12, height = 0, size = 2, alpha = 0.7, shape = 21,
                         ggplot2::aes(color = device)) +
    ggplot2::scale_fill_manual(values = DEVICE_COLORS) +
    ggplot2::scale_color_manual(values = DEVICE_COLORS) +
    ggplot2::labs(
      x = NULL, y = METRIC_LABELS[[metric]],
      title = paste("Three-device complete-case days —", METRIC_LABELS[[metric]]),
      subtitle = paste(nrow(sub), "days with Whoop, Oura, and Withings all present")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none")
}