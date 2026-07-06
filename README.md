# EDI Performance on CLIF

This project evaluates Epic Deterioration Index (EDI) performance against adverse clinical outcomes using CLIF-formatted data. It is intended to run at any CLIF site by updating the local configuration file to point to that site's data and EDI score file.

## Objective

Measure EDI performance in eligible adult hospitalizations with at least one EDI score recorded between hospital arrival and discharge. The analysis estimates performance for:

1. ICU transfer.
2. Vasopressor initiation.
3. Invasive mechanical ventilation.
4. 30-day mortality.
5. Composite adverse outcome, defined as any of the above.

## Required Data

Place the required input files in the directory configured by `data_directory` in `config/config.json`. Files may be Parquet or CSV, but `filetype` in the config must match the CLIF table extension.

Required CLIF tables:

1. `clif_hospitalization.{filetype}` with `patient_id`, `hospitalization_id`, `admission_dttm`, `discharge_dttm`, `age_at_admission`, `discharge_category`.
2. `clif_patient.{filetype}` with `patient_id`, `death_dttm`, `sex_category`, `race_category`, `ethnicity_category`.
3. `clif_adt.{filetype}` with `hospitalization_id`, `in_dttm`, `out_dttm`, `location_category`.
4. `clif_respiratory_support.{filetype}` with `hospitalization_id`, `recorded_dttm`, `device_category`.
5. `clif_medication_admin_continuous.{filetype}` with `hospitalization_id`, `admin_dttm`, `med_group`.

Required EDI score data:

1. One EDI score file, configured by `score_file` in `config/config.json`.
2. A hospitalization or encounter identifier column, configured by `score_hospitalization_id_column`.
3. A score timestamp column, configured by `score_time_column`.
4. A numeric EDI score column, configured by `score_value_column`.

If your EDI scores are in `clif_scores.{filetype}`, configure those columns directly. If your site stores EDI scores in a separate local file, set `score_file` to that file name or absolute path and map the relevant columns in `config/config.json`.

## Configuration

Create a local config file from the template:

```bash
cp config/config_template.json config/config.json
```

Edit `config/config.json` for your site. This file is ignored by git because it contains site-local paths.

Example:

```json
{
  "site_name": "your_site",
  "data_directory": "/path/to/your/clif/data",
  "filetype": "parquet",
  "timezone": "US/Eastern",
  "output_directory": "./output",
  "score_file": "clif_scores.parquet",
  "score_hospitalization_id_column": "hospitalization_id",
  "score_time_column": "recorded_dttm",
  "score_value_column": "score_value",
  "edi_model_id": "30434503",
  "edi_threshold": 40,
  "minimum_cell_size": 10
}
```

Config fields:

1. `site_name`: Short site label used in output file names.
2. `data_directory`: Directory containing the CLIF tables and, if relative, the configured score file.
3. `filetype`: `parquet` or `csv` for CLIF tables.
4. `timezone`: Site timezone. This is retained for compatibility with CLIF tooling.
5. `output_directory`: Directory where project outputs are written.
6. `score_file`: EDI score file name relative to `data_directory`, or an absolute path.
7. `score_hospitalization_id_column`: Score-file column that maps to CLIF `hospitalization_id`.
8. `score_time_column`: Score-file timestamp column.
9. `score_value_column`: Score-file numeric EDI value column.
10. `edi_model_id`: Optional. Used only if the score file has a `model_id` column and should be filtered to one EDI model.
11. `edi_threshold`: Score threshold used for binary performance metrics.
12. `minimum_cell_size`: Minimum aggregate cell size for shareable outputs.

## Cohort Definition

The main script applies these inclusion criteria:

1. Adult hospitalization, using `age_at_admission` from `clif_hospitalization`.
2. At least one ward location during hospitalization.
3. First observed location is ED or ward.
4. If the first observed location is ED, the next distinct location is not ICU or procedural/surgery.
5. At least one EDI score recorded after admission and before discharge.

Category matching is intentionally case-insensitive. The R script lower-cases CLIF category fields before matching values such as `icu`, `ward`, `ed`, `procedural`, `imv`, `expired`, and `hospice`.

## How To Run

Run these steps from the project root.

1. Install R if it is not already available.

2. Create your local config.

```bash
cp config/config_template.json config/config.json
```

3. Edit `config/config.json` so `data_directory`, `site_name`, `filetype`, and score column mappings match your site.

4. Confirm the required CLIF tables and EDI score file exist in the configured location.

5. Run the analysis.

```bash
Rscript "code/001_EDI_Pref.R"
```

6. Review the console output for cohort counts and performance metrics.

7. Review generated outputs under `output/`.

The script uses `pacman::p_load()` to load required R packages. If packages are missing and your R environment allows installation, `pacman` can install them. If your site uses a locked-down environment, install the required packages before running the script.

## Outputs

Patient-level and encounter-level intermediate files are written to `output/intermediate_phi/`. These files may contain PHI or row-level data and must remain local to the site.

Intermediate outputs include:

1. `{site_name}_demo_outcomes.parquet`.
2. `{site_name}_adt5.parquet`.
3. `{site_name}_edi4.parquet`.
4. `{site_name}_table1.parquet`.
5. `{site_name}_fig_1_data.parquet`.

Aggregate final outputs are written to `output/final_no_phi/` with the configured site prefix:

1. `{site_name}_fig_1.png` and `{site_name}_fig_1_df.csv`: forest plot and aggregate performance metrics.
2. `{site_name}_fig_2.png` and `{site_name}_fig_2_df.csv`: calibration plot and binned aggregate calibration data.
3. `{site_name}_fig_3.png` and `{site_name}_fig_3_df.csv`: adverse-outcome overlap figure and aggregate overlap counts.

Both `output/intermediate_phi/` and generated files in `output/final_no_phi/` are gitignored.

## Data Security

Only aggregate results should be shared outside the site.

1. Do not share `output/intermediate_phi/` files.
2. Do not include `patient_id`, `hospitalization_id`, or row-level records in shared outputs.
3. Do not share raw CLIF `.csv` or `.parquet` data files.
4. Suppress or withhold aggregate cells with `n < minimum_cell_size`.
5. Keep site-local configs and data paths out of committed shared artifacts.

See `output/README.md` and `guides/primer.md` for broader CLIF project data-security guidance.
