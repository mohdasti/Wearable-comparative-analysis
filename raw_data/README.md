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
| Whoop | `whoop/physiological_cycles.csv` | RHR, HRV, sleep duration, stages, awake time, time in bed, efficiency, respiratory rate, recovery score, sleep timing |
| Whoop | `whoop/whoop.rtf` | Manually logged daily steps (not exported by Whoop app) |
| Oura | `oura/oura_2026-05-22_2026-07-23_trends.csv` | Daily trends export (same metrics plus readiness score and steps) |
| Withings | `withings/sleep.csv` | Nightly sleep sessions, stages, awake time, average and minimum HR |
| Withings | `withings/aggregates_steps.csv` | Daily step totals |

## Files present but not used

`whoop/workouts.csv`, `whoop/sleeps.csv`, `withings/raw_hr_hr.csv`, and everything under `withings/supplementary/` (the full Health Mate export: activities, weight, raw tracker streams, and so on) are kept for reference but are **not** read by the pipeline. See `withings/README.txt` for Health Mate field definitions.

Skin temperature and blood oxygen are exported by some devices but on incompatible scales (Whoop reports absolute °C, Oura a deviation from baseline) or too sparsely to support a comparison.

## Updating data

1. Export new files from each device app.
2. Replace the corresponding files above (keep filenames or update `R/01_config.R` paths).
3. Re-run `save_harmonized()` and `quarto render reports/wearable_comparison.qmd`.
