# =============================================================================
# 06_plots.R
# Purpose: Reusable ggplot2 functions for the Quarto report.
# =============================================================================

if (!exists("source_project_file")) {
  .src <- c(file.path(getwd(), "R", "source_project.R"), file.path(getwd(), "..", "R", "source_project.R"))
  source(.src[file.exists(.src)][1])
}
source_project_file("05_analysis.R")

# Consistent, high-contrast palette for the three devices.
# Red = Whoop, Blue = Oura, Orange = Withings (easy to tell apart in all plot types).
DEVICE_COLORS <- c(
  "Whoop"              = "#D62828",
  "Oura Ring 4"        = "#1D4ED8",
  "Withings ScanWatch" = "#EA580C"
)

# -----------------------------------------------------------------------------
# plot_timeseries()
# Daily time series of a metric, faceted or colored by device.
# -----------------------------------------------------------------------------
plot_timeseries <- function(long_df, metric_name) {
  sub <- long_df |> dplyr::filter(metric == metric_name)

  ggplot2::ggplot(sub, ggplot2::aes(x = date, y = value, color = device, group = device)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = DEVICE_COLORS) +
    ggplot2::labs(
      x = "Date", y = METRIC_LABELS[[metric_name]],
      color = "Device",
      title = paste("Daily", METRIC_LABELS[[metric_name]])
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

# -----------------------------------------------------------------------------
# plot_density()
# Overlapping density distributions per device for one metric.
# -----------------------------------------------------------------------------
plot_density <- function(long_df, metric_name) {
  sub <- long_df |> dplyr::filter(metric == metric_name)

  ggplot2::ggplot(sub, ggplot2::aes(x = value, fill = device, color = device)) +
    ggplot2::geom_density(alpha = 0.25, linewidth = 0.8) +
    ggplot2::scale_fill_manual(values = DEVICE_COLORS) +
    ggplot2::scale_color_manual(values = DEVICE_COLORS) +
    ggplot2::labs(
      x = METRIC_LABELS[[metric_name]], y = "Density",
      title = paste("Distribution of", METRIC_LABELS[[metric_name]])
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
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
    ggplot2::labs(
      x = "Date", y = NULL,
      title = paste("Data availability —", METRIC_LABELS[[metric_name]])
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none")
}

# -----------------------------------------------------------------------------
# plot_scatter_pair()
# Scatter plot comparing two devices on one metric with identity line.
# -----------------------------------------------------------------------------
plot_scatter_pair <- function(wide_df, metric, device_a, device_b, label_a, label_b) {
  pair <- get_pairwise_data(wide_df, metric, device_a, device_b)
  if (is.null(pair) || nrow(pair) < 2) return(NULL)

  point_color <- DEVICE_COLORS[[label_b]] %||% "#333333"
  line_color  <- DEVICE_COLORS[[label_a]] %||% "#666666"

  ggplot2::ggplot(pair, ggplot2::aes(x = a, y = b)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(size = 3, alpha = 0.85, color = point_color) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = line_color, fill = line_color, alpha = 0.15) +
    ggplot2::labs(
      x = paste(label_a, "—", METRIC_LABELS[[metric]]),
      y = paste(label_b, "—", METRIC_LABELS[[metric]]),
      title = paste(label_a, "vs", label_b)
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

# -----------------------------------------------------------------------------
# plot_bland_altman()
# Bland-Altman plot: difference vs mean for two devices.
# Shows bias (solid line) and limits of agreement (dashed lines).
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
    ggplot2::geom_hline(yintercept = ba$mean_bias, color = bias_color, linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = c(ba$loa_lower, ba$loa_upper),
                        linetype = "dashed", color = "gray50") +
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
# plot_correlation_heatmap()
# Heatmap of Pearson correlations across device pairs for all metrics.
# -----------------------------------------------------------------------------
plot_correlation_heatmap <- function(agreement_df) {
  if (is.null(agreement_df) || nrow(agreement_df) == 0) return(NULL)

  plot_df <- agreement_df |>
    dplyr::mutate(
      pair = paste(device_a, "vs", device_b),
      metric_label = METRIC_LABELS[metric]
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = pair, y = metric_label, fill = pearson_r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f\n(n=%d)", pearson_r, n)),
                       size = 3.5, color = "white") +
    ggplot2::scale_fill_gradient2(low = "#93C5FD", mid = "gray85", high = "#D62828",
                                  midpoint = 0.5, limits = c(0, 1)) +
    ggplot2::labs(x = "Device pair", y = "Metric", fill = "Pearson r",
                  title = "Correlation matrix across device pairs") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}
