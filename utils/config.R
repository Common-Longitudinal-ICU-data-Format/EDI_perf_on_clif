config_path <- Sys.getenv(
  "EDI_CONFIG_PATH",
  unset = file.path("config", "config.json")
)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Required package 'jsonlite' is not installed.", call. = FALSE)
}
if (!file.exists(config_path)) {
  stop(
    "Missing configuration file: ", config_path,
    ". Run this workflow from the repository root.",
    call. = FALSE
  )
}

merge_config <- function(values, defaults) {
  for (name in names(defaults)) {
    if (is.null(values[[name]])) {
      values[[name]] <- defaults[[name]]
    } else if (is.list(defaults[[name]]) && is.list(values[[name]])) {
      values[[name]] <- merge_config(values[[name]], defaults[[name]])
    }
  }
  values
}

config_defaults <- list(
  filetype = "parquet",
  timezone = "UTC",
  output_directory = "./output",
  score_file = "clif_scores.parquet",
  model_registry_file = "clif_model_registry.parquet",
  score_hospitalization_id_column = "hospitalization_id",
  score_time_column = "recorded_dttm",
  score_value_column = "score_value",
  score_model_id_column = "model_id",
  score_model_category = "epic_deterioration_index",
  edi_threshold = 40,
  minimum_cell_size = 10,
  cohort = list(
    min_age = 18,
    ward_eligibility = c("ward"),
    allowed_initial_locations = c("ed", "ward"),
    ed_locations = c("ed"),
    excluded_second_locations = c("icu", "procedural")
  ),
  events = list(
    icu_locations = c("icu"),
    imv_devices = c("imv"),
    vasopressor_med_groups = c("vasoactives"),
    procedural_locations = c("procedural"),
    mortality_discharge_categories = c("expired", "hospice")
  ),
  analysis = list(horizons_hours = c(4, 12), calibration_bins = 10),
  trajectory = list(window_hours = 24, bin_width_hours = 1, min_n = 20, seed = 123),
  threshold_grid = list(min = 0, max = 100, step = 5),
  lead_time = list(min_n = 20),
  threshold_crossing = list(min = 0, max = 100, step = 1),
  first_crossing = list(bin_hours = 6, max_hours_plot = 72)
)

raw_config <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
obsolete <- c("score_model_id_value", "edi_model_id", "score_source_verified_edi_only")
obsolete <- intersect(obsolete, names(raw_config))
if (length(obsolete)) {
  stop("Migrate configuration to category-based selection: remove ", paste(obsolete, collapse = ", "),
       " and configure clif_scores, model_registry_file, and score_model_id_column. EDI IDs are resolved from the registry.", call. = FALSE)
}
config <- merge_config(raw_config, config_defaults)

required_text <- c("site_name", "data_directory", "filetype", "timezone", "output_directory",
                   "score_file", "score_hospitalization_id_column", "score_time_column",
                   "score_value_column", "model_registry_file", "score_model_id_column",
                   "score_model_category")
missing_text <- required_text[vapply(required_text, function(x) {
  value <- config[[x]]
  is.null(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))
}, logical(1))]
if (length(missing_text)) {
  stop("Missing or invalid configuration value(s): ", paste(missing_text, collapse = ", "), call. = FALSE)
}
config$score_model_category <- tolower(trimws(config$score_model_category))
if (config$score_model_category != "epic_deterioration_index") {
  stop("This EDI workflow requires score_model_category = epic_deterioration_index.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", config$site_name)) {
  stop("site_name may contain only letters, numbers, underscores, and hyphens.", call. = FALSE)
}
config$filetype <- tolower(config$filetype)
if (!config$filetype %in% c("parquet", "csv")) {
  stop("filetype must be 'parquet' or 'csv'.", call. = FALSE)
}
if (!config$timezone %in% OlsonNames()) {
  stop("timezone must be a valid Olson timezone.", call. = FALSE)
}

positive_scalar <- function(value, name, allow_zero = FALSE) {
  value <- suppressWarnings(as.numeric(value))
  lower_ok <- if (allow_zero) value >= 0 else value > 0
  if (length(value) != 1L || !is.finite(value) || !lower_ok) {
    stop(name, " must be a valid numeric scalar.", call. = FALSE)
  }
  value
}

integer_scalar <- function(value, name, allow_zero = FALSE) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value) || value != floor(value) ||
      value > .Machine$integer.max || value < if (allow_zero) 0 else 1) {
    stop(name, " must be an integer in the supported range.", call. = FALSE)
  }
  as.integer(value)
}

config$edi_threshold <- positive_scalar(config$edi_threshold, "edi_threshold", TRUE)
config$minimum_cell_size <- integer_scalar(config$minimum_cell_size, "minimum_cell_size")
config$cohort$min_age <- positive_scalar(config$cohort$min_age, "cohort.min_age", TRUE)
config$analysis$calibration_bins <- integer_scalar(config$analysis$calibration_bins, "analysis.calibration_bins")
config$trajectory$window_hours <- positive_scalar(config$trajectory$window_hours, "trajectory.window_hours")
config$trajectory$bin_width_hours <- positive_scalar(config$trajectory$bin_width_hours, "trajectory.bin_width_hours")
config$trajectory$min_n <- integer_scalar(config$trajectory$min_n, "trajectory.min_n")
config$trajectory$seed <- integer_scalar(config$trajectory$seed, "trajectory.seed", TRUE)
config$threshold_grid$min <- positive_scalar(config$threshold_grid$min, "threshold_grid.min", TRUE)
config$threshold_grid$max <- positive_scalar(config$threshold_grid$max, "threshold_grid.max", TRUE)
config$threshold_grid$step <- positive_scalar(config$threshold_grid$step, "threshold_grid.step")
config$lead_time$min_n <- integer_scalar(config$lead_time$min_n, "lead_time.min_n")
config$threshold_crossing$min <- positive_scalar(config$threshold_crossing$min, "threshold_crossing.min", TRUE)
config$threshold_crossing$max <- positive_scalar(config$threshold_crossing$max, "threshold_crossing.max", TRUE)
config$threshold_crossing$step <- positive_scalar(config$threshold_crossing$step, "threshold_crossing.step")
config$first_crossing$bin_hours <- positive_scalar(config$first_crossing$bin_hours, "first_crossing.bin_hours")
config$first_crossing$max_hours_plot <- positive_scalar(config$first_crossing$max_hours_plot, "first_crossing.max_hours_plot")

config$analysis$horizons_hours <- sort(unique(as.numeric(config$analysis$horizons_hours)))
if (!length(config$analysis$horizons_hours) || any(!is.finite(config$analysis$horizons_hours)) ||
    any(config$analysis$horizons_hours <= 0)) {
  stop("analysis.horizons_hours must contain positive numbers.", call. = FALSE)
}
if (config$threshold_grid$max < config$threshold_grid$min) {
  stop("threshold_grid.max must be greater than or equal to threshold_grid.min.", call. = FALSE)
}
if (config$threshold_crossing$max < config$threshold_crossing$min) {
  stop("threshold_crossing.max must be greater than or equal to threshold_crossing.min.", call. = FALSE)
}

category_paths <- list(
  c("cohort", "ward_eligibility"), c("cohort", "allowed_initial_locations"),
  c("cohort", "ed_locations"), c("cohort", "excluded_second_locations"),
  c("events", "icu_locations"), c("events", "imv_devices"),
  c("events", "vasopressor_med_groups"), c("events", "procedural_locations"),
  c("events", "mortality_discharge_categories")
)
for (path in category_paths) {
  values <- config[[path[[1]]]][[path[[2]]]]
  values <- unique(tolower(trimws(as.character(values))))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) stop(paste(path, collapse = "."), " must not be empty.", call. = FALSE)
  config[[path[[1]]]][[path[[2]]]] <- values
}
