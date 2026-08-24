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

Performance is calculated at the configured prediction horizons, default 4 and 12 hours, and at the hospitalization level. Additional outputs describe calibration, outcome intersections, pre-event score trajectories, threshold performance, and threshold-crossing lead time.

## Required Data

The configured `data_directory` must contain:

1.  `clif_hospitalization.{filetype}` with `patient_id`, `hospitalization_id`, `admission_dttm`, `discharge_dttm`, `age_at_admission`, and `discharge_category`.
2.  `clif_patient.{filetype}` with `patient_id`, `death_dttm`, `sex_category`, `race_category`, and `ethnicity_category`.
3.  `clif_adt.{filetype}` with `hospitalization_id`, `in_dttm`, `out_dttm`, and `location_category`.
4.  `clif_respiratory_support.{filetype}` with `hospitalization_id`, `recorded_dttm`, and `device_category`.
5.  `clif_medication_admin_continuous.{filetype}` with `hospitalization_id`, `admin_dttm`, and `med_group`.
6.  An EDI score file with configurable hospitalization ID, timestamp, score value, and optional model-ID columns.

Input categories are trimmed and matched case-insensitively. Clinical events are restricted to the admission-to-discharge interval.

## Configuration

Create the ignored local configuration:

``` bash
cp config/config_template.json config/config.json
```

Update the following groups in `config/config.json`:

-   Site and storage: `site_name`, `data_directory`, `filetype`, `timezone`, and `output_directory`.
-   Score source: `score_file`, score column mappings, and optional model-ID column/value.
-   Cohort: minimum age and location category lists.
-   Events: ICU, IMV, vasopressor, procedural, and mortality-disposition categories.
-   Analysis: EDI threshold, prediction horizons, calibration bins, trajectory settings, threshold grid, lead-time minimum N, and `minimum_cell_size`.

See `config/config_template.json` and `config/README.md` for the complete schema. The loader validates required fields, value ranges, timezone, categories, and supported input formats before analysis.

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

The workflow validates input schemas, filters the score source to eligible encounters with Arrow, writes protected checkpoints, and then generates aggregate results.

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
9.  `{site}_run_manifest.csv`: sanitized workflow and package metadata.

## Data Security

1.  Never share `intermediate_phi/` or source CLIF/score files.
2.  Do not move `patient_id`, `hospitalization_id`, exact timestamps, or row-level records into `final_no_phi/`.
3.  Review both CSV files and rendered figures before distribution.
4.  The workflow suppresses aggregate results with support below `minimum_cell_size`; internal support counts remain protected.
5.  Local configuration files and generated output files are gitignored.

See `output/README.md` and `guides/primer.md` for additional security guidance.