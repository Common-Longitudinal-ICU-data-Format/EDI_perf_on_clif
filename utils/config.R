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
  score_hospitalization_id_column = "hospitalization_id",
  score_time_column = "recorded_dttm",
  score_value_column = "score_value",
  score_model_id_column = NULL,
  score_model_id_value = NULL,
  score_source_verified_edi_only = FALSE,
  edi_model_id = NULL,
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
  lead_time = list(min_n = 20)
)

config <- merge_config(
  jsonlite::fromJSON(config_path, simplifyVector = TRUE),
  config_defaults
)

# Preserve the original edi_model_id key while allowing an explicit source column.
if (is.null(config$score_model_id_value) && !is.null(config$edi_model_id)) {
  config$score_model_id_value <- config$edi_model_id
}
if (!is.null(config$score_model_id_value) && is.null(config$score_model_id_column)) {
  config$score_model_id_column <- "model_id"
}

has_model_id_column <- !is.null(config$score_model_id_column)
has_model_id_value <- !is.null(config$score_model_id_value)
if (xor(has_model_id_column, has_model_id_value)) {
  stop(
    "score_model_id_column and score_model_id_value must be configured together.",
    call. = FALSE
  )
}
if (has_model_id_column && (
  length(config$score_model_id_column) != 1L ||
    is.na(config$score_model_id_column) ||
    !nzchar(trimws(as.character(config$score_model_id_column)))
)) {
  stop("score_model_id_column must be a non-empty scalar when configured.", call. = FALSE)
}
if (has_model_id_value && (
  length(config$score_model_id_value) != 1L ||
    is.na(config$score_model_id_value) ||
    !nzchar(trimws(as.character(config$score_model_id_value)))
)) {
  stop("score_model_id_value must be a non-empty scalar when configured.", call. = FALSE)
}
if (!is.logical(config$score_source_verified_edi_only) ||
    length(config$score_source_verified_edi_only) != 1L ||
    is.na(config$score_source_verified_edi_only)) {
  stop("score_source_verified_edi_only must be true or false.", call. = FALSE)
}
if (!has_model_id_column && !config$score_source_verified_edi_only) {
  stop(
    paste0(
      "Configure score_model_id_column and score_model_id_value, or set ",
      "score_source_verified_edi_only to true after verifying the score source contains only EDI."
    ),
    call. = FALSE
  )
}
if (has_model_id_column && config$score_source_verified_edi_only) {
  stop(
    "score_source_verified_edi_only must be false when a model filter is configured.",
    call. = FALSE
  )
}

required_text <- c("site_name", "data_directory", "filetype", "timezone", "output_directory",
                   "score_file", "score_hospitalization_id_column", "score_time_column",
                   "score_value_column")
missing_text <- required_text[vapply(required_text, function(x) {
  value <- config[[x]]
  is.null(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))
}, logical(1))]
if (length(missing_text)) {
  stop("Missing or invalid configuration value(s): ", paste(missing_text, collapse = ", "), call. = FALSE)
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

config$edi_threshold <- positive_scalar(config$edi_threshold, "edi_threshold", TRUE)
config$minimum_cell_size <- as.integer(positive_scalar(config$minimum_cell_size, "minimum_cell_size"))
config$cohort$min_age <- positive_scalar(config$cohort$min_age, "cohort.min_age", TRUE)
config$analysis$calibration_bins <- as.integer(positive_scalar(config$analysis$calibration_bins, "analysis.calibration_bins"))
config$trajectory$window_hours <- positive_scalar(config$trajectory$window_hours, "trajectory.window_hours")
config$trajectory$bin_width_hours <- positive_scalar(config$trajectory$bin_width_hours, "trajectory.bin_width_hours")
config$trajectory$min_n <- as.integer(positive_scalar(config$trajectory$min_n, "trajectory.min_n"))
config$trajectory$seed <- as.integer(positive_scalar(config$trajectory$seed, "trajectory.seed", TRUE))
config$threshold_grid$min <- positive_scalar(config$threshold_grid$min, "threshold_grid.min", TRUE)
config$threshold_grid$max <- positive_scalar(config$threshold_grid$max, "threshold_grid.max", TRUE)
config$threshold_grid$step <- positive_scalar(config$threshold_grid$step, "threshold_grid.step")
config$lead_time$min_n <- as.integer(positive_scalar(config$lead_time$min_n, "lead_time.min_n"))

config$analysis$horizons_hours <- sort(unique(as.numeric(config$analysis$horizons_hours)))
if (!length(config$analysis$horizons_hours) || any(!is.finite(config$analysis$horizons_hours)) ||
    any(config$analysis$horizons_hours <= 0)) {
  stop("analysis.horizons_hours must contain positive numbers.", call. = FALSE)
}
if (config$threshold_grid$max < config$threshold_grid$min) {
  stop("threshold_grid.max must be greater than or equal to threshold_grid.min.", call. = FALSE)
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
