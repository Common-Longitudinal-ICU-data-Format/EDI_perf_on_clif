# Configuration

Copy `config_template.json` to the ignored `config.json`, then edit it for the local site. Run the workflow from the repository root so `utils/config.R` can load this file. Set `EDI_CONFIG_PATH` to use another configuration file without replacing the default local file.

## Site And Score Source

- `site_name`: Safe short label used in final filenames.
- `data_directory`: Directory containing CLIF tables and relative score files.
- `filetype`: `parquet` or `csv`.
- `timezone`: Olson timezone used for local clinical timestamps.
- `output_directory`: Root under which `intermediate_phi/` and `final_no_phi/` are created.
- `score_file`: Filename relative to `data_directory`, or an absolute path.
- `score_hospitalization_id_column`: Score encounter-ID column.
- `score_time_column`: Score timestamp column.
- `score_value_column`: Numeric EDI score column.
- `score_model_id_column` and `score_model_id_value`: Optional model filter. Set both to `null` when the score file already contains only EDI.
- `edi_model_id`: Legacy alias for `score_model_id_value`; prefer the explicit model fields for new configurations.

## Protocol

- `edi_threshold`: Main binary EDI threshold.
- `minimum_cell_size`: Minimum support for shareable aggregate results.
- `cohort`: Minimum age and normalized location lists used for eligibility.
- `events`: Normalized source categories for each clinical event.
- `analysis.horizons_hours`: Prediction horizons; defaults to 4 and 12 hours.
- `analysis.calibration_bins`: Number of encounter-level calibration groups.
- `trajectory`: Pre-event window, bin width, minimum encounter count, and random seed.
- `threshold_grid`: Inclusive threshold range and step.
- `lead_time.min_n`: Minimum encounters required to publish a lead-time estimate.

Category values are normalized to lowercase. Lists should contain the CLIF categories used by the local site.

The local file may contain protected filesystem paths and must not be committed or shared. A validated snapshot is written only to the run's `intermediate_phi/` directory.
