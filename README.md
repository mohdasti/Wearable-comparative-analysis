# Wearable Comparative Analysis

Compare daily health metrics across **Whoop**, **Oura Ring 4**, and **Withings ScanWatch** using transparent R code and a Quarto report.

## Metrics

**Headline:** resting heart rate, HRV (Whoop vs Oura only — Withings exported none), sleep duration, daily steps (Whoop steps logged by hand).

**Also compared:** like-for-like resting HR, sleep stages in minutes *and* as a share of the night, awake time, time in bed, sleep efficiency, respiratory rate, bedtime and wake time, and each brand's daily readiness score.

## Analysis window

**2026-06-20 to 2026-07-22** (US Pacific, `America/Los_Angeles`), anchored to Whoop coverage.

## Statistical approach

Every comparison separates two questions that are easy to conflate:

1. **Do the devices move together?** Pearson/Spearman correlation, Kendall's W.
2. **Do they report the same number?** Bias with 95% CI, limits of agreement, ICC, CCC/OCCC.

On top of that: Bland–Altman with a proportional-bias test, Deming regression, smallest real difference (SRD), an equivalence check against pre-set practical margins, and simultaneous three-device tests (three-rater ICC, Friedman, RM-ANOVA, Bonferroni Wilcoxon post-hoc).

## Headline findings

- All three devices agree on **bedtime** almost exactly (three-device ICC = 0.98).
- **No** headline metric passed the equivalence check on every device pair.
- Oura and Withings are nearly interchangeable on *lowest* resting HR (r = 0.94, ICC = 0.93) — better than either agrees with Whoop.
- Withings assigns ~40% of the night to deep sleep vs 16–20% for the others, and detects almost no REM.
- Whoop's Recovery score and Oura's Readiness score correlate at only r = 0.24.

## Project structure

```text
R/                          # Commented analysis scripts
  00_packages.R             # Package loader
  01_config.R               # Paths, timezone, QC thresholds
  02_load_raw.R             # Device parsers
  03_clean_qc.R             # Cleaning and outlier rules
  04_harmonize.R            # Daily long/wide tables
  05_analysis.R             # Correlation and agreement stats
  06_plots.R                # ggplot helpers
reports/
  wearable_comparison.qmd   # Full report (narrative + code + results)
  wearable_comparison.html  # Rendered output (self-contained)
raw_data/                   # Device exports (see raw_data/README.md)
  whoop/
  oura/
  withings/
data/processed/             # Generated .rds and QC logs (gitignored)
```

## Render the report

From the project root (requires R 4.3+ and Quarto):

```bash
quarto render reports/wearable_comparison.qmd
```

Or run the pipeline in R:

```r
source("R/00_packages.R")
source("R/source_project.R")
source("R/06_plots.R")
harmonized <- save_harmonized()
agreement  <- run_all_agreement(harmonized$wide)
threeway   <- run_all_threeway(harmonized$wide)
```

## R packages

`dplyr`, `tidyr`, `readr`, `ggplot2`, `purrr`, `tibble`, `stringr`, `lubridate`, `janitor`, `striprtf`, `knitr`, `irr`, `psych`, `patchwork`, `scales`, `rmarkdown`, `deming`, `ggridges`

Install with:

```r
install.packages(c(
  "dplyr", "tidyr", "readr", "ggplot2", "purrr", "tibble", "stringr",
  "lubridate", "janitor", "striprtf", "knitr", "irr", "psych",
  "patchwork", "scales", "rmarkdown", "deming", "ggridges"
))
```

## Data sources

| Device | Folder | Key files |
|--------|--------|-----------|
| Whoop | `raw_data/whoop/` | `physiological_cycles.csv`, `whoop.rtf` (manual steps) |
| Oura | `raw_data/oura/` | `oura_2026-05-22_2026-07-23_trends.csv` |
| Withings | `raw_data/withings/` | `sleep.csv`, `aggregates_steps.csv` |

See [raw_data/README.md](raw_data/README.md) for the full file list. Withings field definitions are in `raw_data/withings/README.txt`.

## License

MIT — see [LICENSE](LICENSE).
