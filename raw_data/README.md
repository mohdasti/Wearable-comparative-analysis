# Raw device exports

Personal health data exports from Whoop, Oura, and Withings. These files are **not modified** by the analysis pipeline — only read.

## Layout

```text
raw_data/
  whoop/          # Whoop app exports + manual step log
  oura/           # Oura trends export
  withings/       # Primary Withings files used in analysis
  withings/supplementary/   # Other Health Mate exports (not used in main pipeline)
```

## Files used in analysis

| Device | File | Purpose |
|--------|------|---------|
| Whoop | `whoop/physiological_cycles.csv` | Daily RHR, HRV, sleep duration, stages, efficiency, respiratory rate |
| Whoop | `whoop/whoop.rtf` | Manually logged daily steps (not exported by Whoop app) |
| Whoop | `whoop/workouts.csv` | Workout summary (optional exploratory use) |
| Oura | `oura/oura_2026-05-22_2026-07-23_trends.csv` | Daily trends export |
| Withings | `withings/sleep.csv` | Nightly sleep sessions |
| Withings | `withings/aggregates_steps.csv` | Daily step totals |
| Withings | `withings/raw_hr_hr.csv` | Intraday heart rate (optional exploratory use) |

## Supplementary Withings exports

The `withings/supplementary/` folder holds the full Health Mate export (activities, weight, raw tracker streams, etc.). These are kept for reference but are **not** loaded by the main R pipeline. See `withings/README.txt` for Health Mate field definitions.

## Updating data

1. Export new files from each device app.
2. Replace the corresponding files above (keep filenames or update `R/01_config.R` paths).
3. Re-run `save_harmonized()` and `quarto render reports/wearable_comparison.qmd`.
