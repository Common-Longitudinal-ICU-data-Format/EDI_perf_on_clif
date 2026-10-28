# EDI Performance on CLIF

This project evaluates Epic Deterioration Index (EDI) performance against adverse clinical outcomes using CLIF-formatted data. The workflow is site-independent: input locations, score columns, cohort categories, analysis parameters, and disclosure limits are configured in `config/config.json`.

## Analysis

The cohort includes adult hospitalizations that:

1.  Meet the configured minimum age, default 18 years.
2.  Have at least one configured inpatient location, default `ward`.
3.  Start in a configured location, default `ed` or `ward`.
4.  Do not move directly from ED to a configured excluded location, default `icu` or `procedural`.
5.  Have at least one EDI score between admission and discharge.

The analysis evaluates:

1.  ICU transfer.
2.  Vasopressor initiation.
3.  Invasive mechanical ventilation (IMV).
4.  In-hospital mortality. An in-admission death or discharge disposition of `Expired` or `Hospice` is counted at the recorded death or discharge time.
5.  Composite adverse outcome, defined as the earliest of the four outcomes above.

Procedural/surgery exposure is descriptive and is included in Table 1. It is not part of the composite outcome.

Performance is calculated at the configured prediction horizons, default 4 and 12 hours, and at the hospitalization level. Additional outputs describe calibration, outcome intersections, pre-event score trajectories, threshold performance, threshold-crossing lead time, hospitalizations ever crossing each threshold, and time from admission to first crossing.

## Required Data

The configured `data_directory` must contain:

1.  `clif_hospitalization.{filetype}` with `patient_id`, `hospitalization_id`, `admission_dttm`, `discharge_dttm`, `age_at_admission`, and `discharge_category`.
2.  `clif_patient.{filetype}` with `patient_id`, `death_dttm`, `sex_category`, `race_category`, and `ethnicity_category`.
3.  `clif_adt.{filetype}` with `hospitalization_id`, `in_dttm`, `out_dttm`, and `location_category`.
4.  `clif_respiratory_support.{filetype}` with `hospitalization_id`, `recorded_dttm`, and `device_category`.
5.  `clif_medication_admin_continuous.{filetype}` with `hospitalization_id`, `admin_dttm`, and `med_group`.
6.  `clif_scores.{filetype}` with `model_id`, `hospitalization_id`, `recorded_dttm`, and `score_value`. Score filenames and columns can be configured.
7.  `clif_model_registry.{filetype}` with populated, unique `model_id` and populated `model_category` columns. The registry filename can be configured.

Input categories are trimmed and matched case-insensitively. Clinical events are restricted to the admission-to-discharge interval.

EDI is selected using `clif_model_registry.model_category == "epic_deterioration_index"`. All matching model IDs/versions are included through their score-table join keys. No institution-specific model ID or model-name matching is used. The workflow checks score-to-registry mappings and reports missing or duplicate mappings before analysis. `is_live` is not an inclusion criterion for this historical analysis.

Timezone-naive timestamps are interpreted as wall-clock times in the configured timezone, independently of the machine's timezone. Timezone-aware timestamps retain their actual instants. CSV date-only values are interpreted as local midnight. Prediction horizons and trajectory windows use elapsed-hour durations across daylight-saving transitions.

## Configuration

Create the ignored local configuration:

``` bash
cp config/config_template.json config/config.json
```

Update the following groups in `config/config.json`:

-   Site and storage: `site_name`, `data_directory`, `filetype`, `timezone`, and `output_directory`.
-   Score source: `score_file`, `model_registry_file`, score column mappings, and `score_model_category`, which must be `epic_deterioration_index`.
-   Cohort: minimum age and location category lists.
-   Events: ICU, IMV, vasopressor, procedural, and mortality-disposition categories.
-   Analysis: EDI threshold, prediction horizons, calibration bins, trajectory settings, threshold grid, lead-time minimum N, crossing threshold grid, first-crossing plot settings, and `minimum_cell_size`.

See `config/config_template.json` and `config/README.md` for the complete schema. The loader validates required fields, value ranges, timezone, categories, and supported input formats before analysis.

**Migrating an existing configuration:** remove `score_model_id_value`, `edi_model_id`, and `score_source_verified_edi_only`. Use model IDs that match the CLIF registry, normally by selecting `clif_scores` with the standard column mappings. The loader rejects obsolete selection settings rather than silently selecting a different cohort.

## Dependencies

R packages are not installed automatically. Install these packages before running:

``` r
install.packages(c(
  "arrow", "dplyr", "tidyr", "purrr", "lubridate",
  "ggplot2", "pROC", "jsonlite"
))
```

## Run

Run from the repository root:

``` bash
Rscript code/edi_analysis.R
```

To use a different local configuration without replacing `config/config.json`, set `EDI_CONFIG_PATH`:

``` bash
EDI_CONFIG_PATH=/path/to/config.json Rscript code/edi_analysis.R
```

The workflow validates input schemas and registry mappings, filters the score source to EDI models and eligible encounters with Arrow, writes protected checkpoints, and then generates aggregate results. CSV join keys are explicitly read as strings; numeric parquet keys are cast to strings in Arrow before matching to preserve their exact values.

Run the focused tests and synthetic end-to-end portability fixtures with:

```bash
Rscript tests/run_tests.R
```

After a site run, validate output counts, crossing identities, and public count boundaries:

```bash
Rscript tests/validate_outputs.R /path/to/output/root
```

## Outputs

The configured output root always contains two safety-separated directories.

`intermediate_phi/` contains identifiers, timestamps, score rows, encounter-level data, internal support counts, and the full validated configuration. It must remain at the originating site.

`final_no_phi/` contains only aggregate, disclosure-controlled files:

1.  `{site}_flowsheet_summary.csv`.
2.  `{site}_table1_data.csv`.
3.  `{site}_fig_1.png` and `{site}_fig_1.csv`: performance forest plot and metrics.
4.  `{site}_fig_2.png` and `{site}_fig_2.csv`: aggregate calibration.
5.  `{site}_fig_3.png` and `{site}_fig_3.csv`: disclosure-controlled outcome intersections.
6.  `{site}_fig_4.png` and `{site}_fig_4.csv`: score trajectories before outcomes.
7.  `{site}_fig_5.png` and `{site}_fig_5.csv`: hospitalization-level threshold curves.
8.  `{site}_fig_6.png` and `{site}_fig_6.csv`: threshold-crossing lead time in hours.
9.  `{site}_fig_7.png` and `{site}_fig_7.csv`: percentage and number of hospitalizations ever reaching each EDI threshold, default 0–100 by 1, using full-admission maximum scores.
10. `{site}_fig_8.png` and `{site}_fig_8.csv`: first threshold crossing from admission; CSV includes hourly counts and cumulative percentages, using `ceiling(hours_from_admission)`. The plot uses six-hour bins and displays the first 72 hours by default; the denominator includes all crossers, including later crossings.
11. `{site}_fig_8_summary.csv`: N, mean, Q1, median, and Q3 of hours to first crossing of `edi_threshold`, default 40.
12. `{site}_run_manifest.csv`: sanitized workflow, category selection, analysis settings, and package metadata.

Figures 7–8 use joint count-boundary suppression so adjacent thresholds, hourly/cumulative results, six-hour bins, and cohort totals cannot recover small positive count differences. If Table 1's crossing or noncrossing stratum is too small, both stratum rows and Figure 8's statistics are masked together to protect their denominators. Zero-count cells may be published when the cohort has adequate support. Suppressed values are blank in CSVs and omitted from plots; exact internal counts remain in `intermediate_phi/`. Empty/eventless or fully suppressed analyses still produce all output tables and figures, with explanatory placeholders where needed.

## Data Security

1.  Never share `intermediate_phi/` or source CLIF/score files.
2.  Do not move `patient_id`, `hospitalization_id`, exact timestamps, or row-level records into `final_no_phi/`.
3.  Review both CSV files and rendered figures before distribution.
4.  The workflow suppresses aggregate results with support below `minimum_cell_size`; internal support counts remain protected.
5.  Local configuration files and generated output files are gitignored.

See `guides/primer.md` for additional security guidance. Institution-specific source scripts are local references under the ignored `local_reference/` directory; `code/edi_analysis.R` is the runnable workflow.
