# Configuration

Copy `config_template.json` to the ignored `config.json`, then edit it for the local site. Run the workflow from the repository root so `utils/config.R` can load this file. Set `EDI_CONFIG_PATH` to use another configuration file without replacing the default local file.

## Site And Score Source

- `site_name`: Safe short label used in final filenames.
- `data_directory`: Directory containing CLIF tables and relative score files.
- `filetype`: `parquet` or `csv`.
- `timezone`: Olson timezone used for local clinical timestamps.
- `output_directory`: Root under which `intermediate_phi/` and `final_no_phi/` are created.
- `score_file`: Filename relative to `data_directory`, or an absolute path.
- `model_registry_file`: Registry filename relative to `data_directory`, or an absolute path. Requires the standard CLIF `model_id` and `model_category` columns.
- `score_hospitalization_id_column`: Score encounter-ID column.
- `score_time_column`: Score timestamp column.
- `score_value_column`: Numeric EDI score column.
- `score_model_id_column`: Score model join-key column; defaults to `model_id`. Its values must match `clif_model_registry.model_id`.
- `score_model_category`: Must be `epic_deterioration_index`. All registry model IDs matching this normalized category are selected, regardless of version or `is_live`.

The default filenames are `clif_scores.parquet` and `clif_model_registry.parquet`. For CSV input, explicitly set their `.csv` filenames along with `filetype: "csv"`.

## Migration From ID-Based Selection

Remove `score_model_id_value`, `edi_model_id`, and `score_source_verified_edi_only` from existing configurations, including null-valued keys. Set `score_file`, its column mappings, and `model_registry_file` to the CLIF tables. A configuration with obsolete selection keys fails with migration instructions. Numeric-ID literals and unfiltered score sources are no longer selection modes. CSV join keys are explicitly typed as strings to preserve leading zeros; numeric parquet keys are matched using exact Arrow string casts.

## Protocol

- `edi_threshold`: Main binary EDI threshold.
- `minimum_cell_size`: Positive integer minimum support for shareable aggregate results. Count/seed settings must be exact integers within R's supported integer range.
- `cohort`: Minimum age and normalized location lists used for eligibility.
- `events`: Normalized source categories for each clinical event.
- `analysis.horizons_hours`: Prediction horizons; defaults to 4 and 12 hours.
- `analysis.calibration_bins`: Number of encounter-level calibration groups.
- `trajectory`: Pre-event window, bin width, minimum encounter count, and random seed.
- `threshold_grid`: Inclusive threshold range and step.
- `lead_time.min_n`: Minimum encounters required to publish a lead-time estimate.
- `threshold_crossing.min`, `.max`, `.step`: Figure 7's inclusive threshold grid, default 0, 100, 1. This is independent of the Figure 5–6 grid.
- `first_crossing.bin_hours`: Figure 8 plot-bin width, default 6 hours. Crossings at admission remain in hour/bin 0; later crossings are rounded up to their hourly and plot-bin endpoints.
- `first_crossing.max_hours_plot`: Figure 8 displayed time range, default 72 hours. All crossers remain in the percentage denominator.

Figure 8 uses `edi_threshold`, default 40, for its first-crossing definition. New crossing outputs apply `minimum_cell_size` jointly across nested count boundaries; suppression can therefore also mask neighboring otherwise-large cells.

Category values are normalized to lowercase. Lists should contain the CLIF categories used by the local site.

The local file may contain protected filesystem paths and must not be committed or shared. A validated snapshot is written only to the run's `intermediate_phi/` directory.
