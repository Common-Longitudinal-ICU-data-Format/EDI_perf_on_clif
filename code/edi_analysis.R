if (!file.exists(file.path("utils", "config.R")) || !dir.exists("code")) {
  stop("Run code/edi_analysis.R from the repository root.", call. = FALSE)
}

required_packages <- c(
  "arrow", "dplyr", "tidyr", "purrr", "lubridate", "ggplot2", "pROC", "jsonlite"
)
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    ". Install them before running the workflow.",
    call. = FALSE
  )
}

source(file.path("utils", "config.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(ggplot2)
})

site_name <- config$site_name
timezone <- config$timezone
threshold <- config$edi_threshold
minimum_cell_size <- config$minimum_cell_size
horizons <- config$analysis$horizons_hours
output_root <- config$output_directory
phi_dir <- file.path(output_root, "intermediate_phi")
final_dir <- file.path(output_root, "final_no_phi")
dir.create(phi_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)

final_path <- function(stem, extension) {
  file.path(final_dir, paste0(site_name, "_", stem, ".", extension))
}
phi_path <- function(stem) file.path(phi_dir, paste0(stem, ".parquet"))

normalize_id <- function(x) {
  if (is.numeric(x) || inherits(x, "integer64")) {
    x <- format(x, scientific = FALSE, trim = TRUE, digits = 22)
  } else {
    x <- trimws(as.character(x))
  }
  x <- sub("\\.0+$", "", x)
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

normalize_time <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(lubridate::force_tz(as.POSIXct(x), tzone = timezone))
  }
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = timezone))
  suppressWarnings(lubridate::ymd_hms(as.character(x), tz = timezone, quiet = TRUE))
}

clean_category <- function(x) tolower(trimws(as.character(x)))

input_path <- function(name) {
  file.path(config$data_directory, paste0("clif_", name, ".", config$filetype))
}

open_input <- function(path) {
  if (!file.exists(path) && !dir.exists(path)) {
    stop("Missing required input: ", path, call. = FALSE)
  }
  arrow::open_dataset(path, format = config$filetype)
}

require_columns <- function(dataset, columns, label) {
  missing <- setdiff(columns, names(dataset))
  if (length(missing)) {
    stop(label, " is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

field_is_numeric <- function(dataset, column) {
  type <- tryCatch(
    dataset$schema$GetFieldByName(column)$type$ToString(),
    error = function(e) ""
  )
  grepl("int|float|double|decimal", tolower(type))
}

ids_for_field <- function(dataset, column, ids) {
  if (!field_is_numeric(dataset, column)) return(as.character(ids))
  numeric_ids <- suppressWarnings(as.numeric(ids))
  if (anyNA(numeric_ids)) {
    stop("Configured/source ID types are incompatible for column ", column, ".", call. = FALSE)
  }
  numeric_ids
}

filter_dataset_ids <- function(dataset, column, ids) {
  require_columns(dataset, column, "Input dataset")
  source_ids <- ids_for_field(dataset, column, ids)
  dataset %>% filter(.data[[column]] %in% source_ids)
}

safe_quantiles <- function(x) {
  if (!any(is.finite(x))) return(rep(NA_real_, 3))
  as.numeric(stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE))
}

safe_divide <- function(numerator, denominator) {
  ifelse(is.finite(denominator) & denominator > 0, numerator / denominator, NA_real_)
}

safe_prop_ci <- function(successes, total) {
  if (!is.finite(total) || total <= 0) return(c(NA_real_, NA_real_))
  tryCatch(
    as.numeric(suppressWarnings(stats::prop.test(successes, total)$conf.int)),
    error = function(e) c(NA_real_, NA_real_)
  )
}

write_final_csv <- function(data, stem) {
  utils::write.csv(data, final_path(stem, "csv"), row.names = FALSE, na = "")
}

save_plot <- function(plot, stem, width = 12, height = 7) {
  ggplot2::ggsave(
    final_path(stem, "png"), plot = plot, width = width, height = height,
    units = "in", dpi = 300
  )
}

theme_edi <- function() {
  theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

log_step <- function(message) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n")
}

# Store the path-bearing configuration only in the PHI output area.
jsonlite::write_json(config, file.path(phi_dir, "validated_config_snapshot.json"),
                     pretty = TRUE, auto_unbox = TRUE, null = "null")

# Cohort inputs ---------------------------------------------------------------
log_step("Loading cohort inputs")
hosp_ds <- open_input(input_path("hospitalization"))
hosp_columns <- c(
  "patient_id", "hospitalization_id", "admission_dttm", "discharge_dttm",
  "age_at_admission", "discharge_category"
)
require_columns(hosp_ds, hosp_columns, "clif_hospitalization")

all_hospitalizations <- hosp_ds %>%
  select(hospitalization_id) %>%
  distinct() %>%
  summarise(n = n()) %>%
  collect() %>%
  pull(n)

hosp <- hosp_ds %>%
  select(all_of(hosp_columns)) %>%
  filter(age_at_admission >= config$cohort$min_age) %>%
  collect() %>%
  transmute(
    patient_id = normalize_id(patient_id),
    hospitalization_id = normalize_id(hospitalization_id),
    admission_dttm = normalize_time(admission_dttm),
    discharge_dttm = normalize_time(discharge_dttm),
    age_at_admission = as.numeric(age_at_admission),
    discharge_category = clean_category(discharge_category)
  ) %>%
  filter(
    !is.na(patient_id), !is.na(hospitalization_id), !is.na(admission_dttm),
    !is.na(discharge_dttm), discharge_dttm >= admission_dttm
  ) %>%
  distinct(hospitalization_id, .keep_all = TRUE)
adult_hospitalizations <- nrow(hosp)
arrow::write_parquet(hosp, phi_path("adult_hospitalizations"))

adt_ds <- open_input(input_path("adt"))
adt_columns <- c("hospitalization_id", "in_dttm", "out_dttm", "location_category")
require_columns(adt_ds, adt_columns, "clif_adt")
adt <- filter_dataset_ids(adt_ds, "hospitalization_id", hosp$hospitalization_id) %>%
  select(all_of(adt_columns)) %>%
  collect() %>%
  transmute(
    hospitalization_id = normalize_id(hospitalization_id),
    in_dttm = normalize_time(in_dttm),
    out_dttm = normalize_time(out_dttm),
    location_category = clean_category(location_category)
  ) %>%
  inner_join(
    hosp %>% select(hospitalization_id, admission_dttm, discharge_dttm),
    by = "hospitalization_id"
  ) %>%
  mutate(
    interval_start = if_else(in_dttm < admission_dttm, admission_dttm, in_dttm),
    raw_end = coalesce(out_dttm, discharge_dttm),
    interval_end = if_else(raw_end > discharge_dttm, discharge_dttm, raw_end)
  ) %>%
  filter(
    !is.na(interval_start), !is.na(interval_end), interval_start < interval_end,
    nzchar(location_category)
  ) %>%
  select(hospitalization_id, interval_start, interval_end, location_category)

location_summary <- adt %>%
  arrange(hospitalization_id, interval_start, interval_end) %>%
  group_by(hospitalization_id) %>%
  summarise(
    ever_ward = any(location_category %in% config$cohort$ward_eligibility),
    first_location = first(location_category),
    second_location = {
      later <- location_category[location_category != first(location_category)]
      if (length(later)) later[[1]] else NA_character_
    },
    .groups = "drop"
  )

ward_ids <- location_summary %>% filter(ever_ward) %>% pull(hospitalization_id)
initial_ids <- location_summary %>%
  filter(ever_ward, first_location %in% config$cohort$allowed_initial_locations) %>%
  pull(hospitalization_id)
location_eligible <- location_summary %>%
  filter(ever_ward, first_location %in% config$cohort$allowed_initial_locations) %>%
  filter(!(
    first_location %in% config$cohort$ed_locations &
      !is.na(second_location) &
      second_location %in% config$cohort$excluded_second_locations
  ))

eligible_hosp <- hosp %>%
  inner_join(location_eligible, by = "hospitalization_id")
arrow::write_parquet(eligible_hosp, phi_path("location_eligible_cohort"))

# Demographics and in-admission outcomes -------------------------------------
patient_ds <- open_input(input_path("patient"))
patient_columns <- c(
  "patient_id", "death_dttm", "sex_category", "race_category", "ethnicity_category"
)
require_columns(patient_ds, patient_columns, "clif_patient")
patient <- filter_dataset_ids(patient_ds, "patient_id", eligible_hosp$patient_id) %>%
  select(all_of(patient_columns)) %>%
  collect() %>%
  transmute(
    patient_id = normalize_id(patient_id),
    death_dttm = normalize_time(death_dttm),
    sex_category = clean_category(sex_category),
    race_category = clean_category(race_category),
    ethnicity_category = clean_category(ethnicity_category)
  ) %>%
  distinct(patient_id, .keep_all = TRUE)

interval_union_days <- function(start, end) {
  keep <- !is.na(start) & !is.na(end) & end > start
  start <- start[keep]
  end <- end[keep]
  if (!length(start)) return(NA_real_)
  ordering <- order(start, end)
  start <- start[ordering]
  end <- end[ordering]
  current_start <- start[[1]]
  current_end <- end[[1]]
  seconds <- 0
  if (length(start) > 1L) {
    for (i in 2:length(start)) {
      if (start[[i]] <= current_end) {
        if (end[[i]] > current_end) current_end <- end[[i]]
      } else {
        seconds <- seconds + as.numeric(difftime(current_end, current_start, units = "secs"))
        current_start <- start[[i]]
        current_end <- end[[i]]
      }
    }
  }
  seconds <- seconds + as.numeric(difftime(current_end, current_start, units = "secs"))
  seconds / 86400
}

eligible_adt <- adt %>% semi_join(eligible_hosp, by = "hospitalization_id")
icu <- eligible_adt %>%
  filter(location_category %in% config$events$icu_locations) %>%
  group_by(hospitalization_id) %>%
  summarise(
    icu_time = min(interval_start),
    icu_los_days = interval_union_days(interval_start, interval_end),
    .groups = "drop"
  )
procedural <- eligible_adt %>%
  filter(location_category %in% config$events$procedural_locations) %>%
  distinct(hospitalization_id) %>%
  mutate(procedural = TRUE)

resp_ds <- open_input(input_path("respiratory_support"))
resp_columns <- c("hospitalization_id", "recorded_dttm", "device_category")
require_columns(resp_ds, resp_columns, "clif_respiratory_support")
imv <- filter_dataset_ids(resp_ds, "hospitalization_id", eligible_hosp$hospitalization_id) %>%
  select(all_of(resp_columns)) %>%
  filter(tolower(device_category) %in% config$events$imv_devices) %>%
  collect() %>%
  transmute(
    hospitalization_id = normalize_id(hospitalization_id),
    event_time = normalize_time(recorded_dttm),
    category = clean_category(device_category)
  ) %>%
  inner_join(
    eligible_hosp %>% select(hospitalization_id, admission_dttm, discharge_dttm),
    by = "hospitalization_id"
  ) %>%
  filter(
    category %in% config$events$imv_devices,
    event_time >= admission_dttm, event_time <= discharge_dttm
  ) %>%
  group_by(hospitalization_id) %>%
  summarise(imv_time = min(event_time), .groups = "drop")

med_ds <- open_input(input_path("medication_admin_continuous"))
med_columns <- c("hospitalization_id", "admin_dttm", "med_group")
require_columns(med_ds, med_columns, "clif_medication_admin_continuous")
vasopressor <- filter_dataset_ids(med_ds, "hospitalization_id", eligible_hosp$hospitalization_id) %>%
  select(all_of(med_columns)) %>%
  filter(tolower(med_group) %in% config$events$vasopressor_med_groups) %>%
  collect() %>%
  transmute(
    hospitalization_id = normalize_id(hospitalization_id),
    event_time = normalize_time(admin_dttm),
    category = clean_category(med_group)
  ) %>%
  inner_join(
    eligible_hosp %>% select(hospitalization_id, admission_dttm, discharge_dttm),
    by = "hospitalization_id"
  ) %>%
  filter(
    category %in% config$events$vasopressor_med_groups,
    event_time >= admission_dttm, event_time <= discharge_dttm
  ) %>%
  group_by(hospitalization_id) %>%
  summarise(vasopressor_time = min(event_time), .groups = "drop")

encounters <- eligible_hosp %>%
  left_join(patient, by = "patient_id") %>%
  mutate(
    death_during_admission = !is.na(death_dttm) & death_dttm >= admission_dttm &
      death_dttm <= discharge_dttm,
    mortality_disposition = discharge_category %in%
      config$events$mortality_discharge_categories,
    death_time = case_when(
      death_during_admission ~ death_dttm,
      mortality_disposition ~ discharge_dttm,
      TRUE ~ as.POSIXct(NA, tz = timezone)
    ),
    hospital_los_days = as.numeric(difftime(discharge_dttm, admission_dttm, units = "days")),
    race_category_clean = if_else(
      race_category %in% c(
        "american indian or alaska native", "native hawaiian or other pacific islander", "other"
      ),
      "other/multiracial", race_category
    )
  ) %>%
  left_join(icu, by = "hospitalization_id") %>%
  left_join(imv, by = "hospitalization_id") %>%
  left_join(vasopressor, by = "hospitalization_id") %>%
  left_join(procedural, by = "hospitalization_id") %>%
  mutate(procedural = coalesce(procedural, FALSE))

# Filter the very large score source before collection. The eligible result is
# checkpointed so downstream analyses never need to rescan the source file.
score_path <- if (grepl("^(/|[A-Za-z]:[/\\\\])", config$score_file)) {
  config$score_file
} else {
  file.path(config$data_directory, config$score_file)
}
score_ds <- open_input(score_path)
score_columns <- c(
  config$score_hospitalization_id_column,
  config$score_time_column,
  config$score_value_column
)
if (!is.null(config$score_model_id_column)) {
  score_columns <- c(score_columns, config$score_model_id_column)
}
require_columns(score_ds, unique(score_columns), "score source")

score_query <- score_ds
if (!is.null(config$score_model_id_column) && !is.null(config$score_model_id_value)) {
  model_value <- config$score_model_id_value
  if (field_is_numeric(score_ds, config$score_model_id_column)) {
    model_value <- suppressWarnings(as.numeric(model_value))
    if (!is.finite(model_value)) stop("score model ID is incompatible with its source column.", call. = FALSE)
  }
  score_query <- score_query %>%
    filter(.data[[config$score_model_id_column]] == model_value)
}
score_source_ids <- ids_for_field(
  score_ds, config$score_hospitalization_id_column, eligible_hosp$hospitalization_id
)
score_query <- score_query %>%
  filter(.data[[config$score_hospitalization_id_column]] %in% score_source_ids)

log_step("Collecting eligible score rows")
scores <- score_query %>%
  select(all_of(unique(score_columns[1:3]))) %>%
  collect() %>%
  transmute(
    hospitalization_id = normalize_id(.data[[config$score_hospitalization_id_column]]),
    recorded_dttm = normalize_time(.data[[config$score_time_column]]),
    score_value = suppressWarnings(as.numeric(.data[[config$score_value_column]]))
  ) %>%
  inner_join(
    eligible_hosp %>% select(hospitalization_id, admission_dttm, discharge_dttm),
    by = "hospitalization_id"
  ) %>%
  filter(
    !is.na(hospitalization_id), !is.na(recorded_dttm), is.finite(score_value),
    recorded_dttm >= admission_dttm, recorded_dttm <= discharge_dttm
  ) %>%
  select(hospitalization_id, recorded_dttm, score_value) %>%
  distinct()
if (!nrow(scores)) {
  stop("No valid in-admission scores matched the location-eligible cohort.", call. = FALSE)
}
arrow::write_parquet(scores, phi_path("eligible_in_admission_scores"))

encounters <- encounters %>%
  semi_join(scores, by = "hospitalization_id") %>%
  mutate(
    adverse_outcome = !is.na(icu_time) | !is.na(imv_time) |
      !is.na(vasopressor_time) | !is.na(death_time),
    adverse_time_num = pmin(
      as.numeric(icu_time), as.numeric(imv_time), as.numeric(vasopressor_time),
      as.numeric(death_time), na.rm = TRUE
    ),
    adverse_outcome_time = as.POSIXct(
      if_else(adverse_outcome, adverse_time_num, NA_real_),
      origin = "1970-01-01", tz = timezone
    )
  ) %>%
  select(-adverse_time_num)
scores <- scores %>% semi_join(encounters, by = "hospitalization_id")
analysis_rows <- scores %>%
  inner_join(
    encounters %>% select(
      hospitalization_id, icu_time, vasopressor_time, imv_time, death_time,
      adverse_outcome_time
    ),
    by = "hospitalization_id"
  )
arrow::write_parquet(encounters, phi_path("analysis_encounters"))
arrow::write_parquet(analysis_rows, phi_path("analysis_score_rows"))

flow_internal <- tibble(
  step = c(
    "All hospitalizations",
    paste0("Age >= ", config$cohort$min_age, " with valid admission dates"),
    "Ward eligible",
    "Allowed initial location",
    "ED second location not excluded",
    "At least one in-admission score"
  ),
  hospitalizations = as.numeric(c(
    all_hospitalizations, adult_hospitalizations, length(unique(ward_ids)),
    length(unique(initial_ids)), nrow(eligible_hosp), nrow(encounters)
  ))
) %>%
  mutate(attrition = lag(hospitalizations) - hospitalizations)
arrow::write_parquet(flow_internal, phi_path("flow_summary_internal"))
flow_public <- flow_internal %>%
  mutate(
    publishable = hospitalizations >= minimum_cell_size &
      (is.na(attrition) | attrition == 0 | attrition >= minimum_cell_size),
    hospitalizations = if_else(publishable, hospitalizations, NA_real_),
    status = if_else(publishable, "Published", "Suppressed")
  ) %>%
  select(step, hospitalizations, status)
write_final_csv(flow_public, "flowsheet_summary")

rm(hosp_ds, adt_ds, patient_ds, resp_ds, med_ds, score_ds, score_query, patient,
   hosp, adt, eligible_adt, eligible_hosp, location_eligible, location_summary,
   ward_ids, initial_ids, icu, imv, vasopressor, procedural)
invisible(gc(verbose = FALSE))

# Table 1 ---------------------------------------------------------------------
log_step("Generating Table 1")
max_scores <- scores %>%
  group_by(hospitalization_id) %>%
  summarise(max_score = max(score_value), .groups = "drop")
table1_encounters <- encounters %>% inner_join(max_scores, by = "hospitalization_id")
arrow::write_parquet(table1_encounters, phi_path("table1_encounters"))

summarize_table1 <- function(data, cohort_label) {
  age <- safe_quantiles(data$age_at_admission)
  los <- safe_quantiles(data$hospital_los_days)
  n <- nrow(data)
  is_hispanic <- data$ethnicity_category %in% c("hispanic", "hispanic or latino")
  sex_known_n <- sum(!is.na(data$sex_category))
  ethnicity_known_n <- sum(!is.na(data$ethnicity_category))
  race_known_n <- sum(!is.na(data$race_category_clean))
  tibble(
    cohort = cohort_label,
    N = n,
    age_q1 = age[[1]], age_median = age[[2]], age_q3 = age[[3]],
    female_n = sum(data$sex_category == "female", na.rm = TRUE),
    female_pct = 100 * safe_divide(sum(data$sex_category == "female", na.rm = TRUE), sex_known_n),
    hispanic_n = sum(is_hispanic, na.rm = TRUE),
    hispanic_pct = 100 * safe_divide(sum(is_hispanic, na.rm = TRUE), ethnicity_known_n),
    white_n = sum(data$race_category_clean == "white", na.rm = TRUE),
    white_pct = 100 * safe_divide(sum(data$race_category_clean == "white", na.rm = TRUE), race_known_n),
    black_n = sum(data$race_category_clean == "black or african american", na.rm = TRUE),
    black_pct = 100 * safe_divide(sum(data$race_category_clean == "black or african american", na.rm = TRUE), race_known_n),
    hospital_los_q1 = los[[1]], hospital_los_median = los[[2]], hospital_los_q3 = los[[3]],
    icu_n = sum(!is.na(data$icu_time)),
    icu_pct = 100 * safe_divide(sum(!is.na(data$icu_time)), n),
    vasopressor_n = sum(!is.na(data$vasopressor_time)),
    vasopressor_pct = 100 * safe_divide(sum(!is.na(data$vasopressor_time)), n),
    imv_n = sum(!is.na(data$imv_time)),
    imv_pct = 100 * safe_divide(sum(!is.na(data$imv_time)), n),
    mortality_n = sum(!is.na(data$death_time)),
    mortality_pct = 100 * safe_divide(sum(!is.na(data$death_time)), n),
    procedural_n = sum(data$procedural),
    procedural_pct = 100 * safe_divide(sum(data$procedural), n),
    sex_known_n = sex_known_n,
    ethnicity_known_n = ethnicity_known_n,
    race_known_n = race_known_n
  )
}

table1_internal <- bind_rows(
  summarize_table1(table1_encounters, "All encounters"),
  summarize_table1(
    table1_encounters %>% filter(max_score >= threshold),
    paste0("Max EDI >= ", threshold)
  ),
  summarize_table1(
    table1_encounters %>% filter(max_score < threshold),
    paste0("Max EDI < ", threshold)
  )
)
arrow::write_parquet(table1_internal, phi_path("table1_summary_internal"))

table1_public <- table1_internal
count_pct_pairs <- list(
  c("female_n", "female_pct", "sex_known_n"),
  c("hispanic_n", "hispanic_pct", "ethnicity_known_n"),
  c("white_n", "white_pct", "race_known_n"),
  c("black_n", "black_pct", "race_known_n"),
  c("icu_n", "icu_pct", "N"), c("vasopressor_n", "vasopressor_pct", "N"),
  c("imv_n", "imv_pct", "N"), c("mortality_n", "mortality_pct", "N"),
  c("procedural_n", "procedural_pct", "N")
)
for (pair in count_pct_pairs) {
  unsafe <- table1_public[[pair[[1]]]] < minimum_cell_size |
    (table1_public[[pair[[3]]]] - table1_public[[pair[[1]]]]) < minimum_cell_size
  # If a threshold stratum is suppressed, suppress the overall cell too so it
  # cannot be reconstructed by subtraction from the other stratum.
  if (any(unsafe[-1])) unsafe[[1]] <- TRUE
  table1_public[unsafe, pair[1:2]] <- NA_real_
}
unsafe_stratum <- table1_public$N[-1] < minimum_cell_size |
  (table1_public$N[[1]] - table1_public$N[-1]) < minimum_cell_size
if (any(unsafe_stratum)) table1_public$N[-1] <- NA_real_
small_rows <- table1_internal$N < minimum_cell_size
table1_public[small_rows, setdiff(names(table1_public), "cohort")] <- NA
table1_public <- table1_public %>%
  select(-sex_known_n, -ethnicity_known_n, -race_known_n)
write_final_csv(table1_public, "table1_data")

# Reusable performance calculations ------------------------------------------
log_step("Calculating performance metrics")
outcomes <- c(
  ICU = "icu_time",
  Vasopressors = "vasopressor_time",
  IMV = "imv_time",
  `In-hospital Mortality` = "death_time",
  Composite = "adverse_outcome_time"
)

performance_metrics <- function(data, outcome_label, horizon_label) {
  data <- data %>% filter(!is.na(outcome), !is.na(predicted), is.finite(score))
  tp <- sum(data$predicted == 1L & data$outcome == 1L)
  fn <- sum(data$predicted == 0L & data$outcome == 1L)
  tn <- sum(data$predicted == 0L & data$outcome == 0L)
  fp <- sum(data$predicted == 1L & data$outcome == 0L)
  tp_encounters <- n_distinct(data$hospitalization_id[data$predicted == 1L & data$outcome == 1L])
  fn_encounters <- n_distinct(data$hospitalization_id[data$predicted == 0L & data$outcome == 1L])
  tn_encounters <- n_distinct(data$hospitalization_id[data$predicted == 0L & data$outcome == 0L])
  fp_encounters <- n_distinct(data$hospitalization_id[data$predicted == 1L & data$outcome == 0L])
  positives <- tp + fn
  negatives <- tn + fp
  n_encounters <- n_distinct(data$hospitalization_id)

  incidence_ci <- safe_prop_ci(positives, positives + negatives)
  sensitivity_ci <- safe_prop_ci(tp, positives)
  specificity_ci <- safe_prop_ci(tn, negatives)
  ppv_ci <- safe_prop_ci(tp, tp + fp)
  npv_ci <- safe_prop_ci(tn, tn + fn)
  auc <- auc_ci <- c(NA_real_, NA_real_)
  auc_value <- NA_real_
  if (positives > 0 && negatives > 0) {
    roc_object <- tryCatch(
      pROC::roc(data$outcome, data$score, quiet = TRUE, direction = "<"),
      error = function(e) NULL
    )
    if (!is.null(roc_object)) {
      auc_value <- as.numeric(pROC::auc(roc_object))
      auc_ci <- tryCatch(
        as.numeric(pROC::ci.auc(roc_object, method = "delong"))[c(1, 3)],
        error = function(e) c(NA_real_, NA_real_)
      )
    }
  }
  ppv <- safe_divide(tp, tp + fp)
  estimates <- c(
    safe_divide(positives, positives + negatives), auc_value,
    safe_divide(tp, positives), safe_divide(tn, negatives), ppv,
    safe_divide(tn, tn + fn), ifelse(is.finite(ppv) && ppv > 0, 1 / ppv, NA_real_)
  )
  ci_lower <- c(incidence_ci[[1]], auc_ci[[1]], sensitivity_ci[[1]], specificity_ci[[1]],
                ppv_ci[[1]], npv_ci[[1]], NA_real_)
  ci_upper <- c(incidence_ci[[2]], auc_ci[[2]], sensitivity_ci[[2]], specificity_ci[[2]],
                ppv_ci[[2]], npv_ci[[2]], NA_real_)
  support_min <- c(
    min(tp_encounters + fn_encounters, tn_encounters + fp_encounters),
    min(tp_encounters + fn_encounters, tn_encounters + fp_encounters),
    min(tp_encounters, fn_encounters), min(tn_encounters, fp_encounters),
    min(tp_encounters, fp_encounters), min(tn_encounters, fn_encounters),
    min(tp_encounters, fp_encounters)
  )
  tibble(
    outcome_name = outcome_label,
    horizon = horizon_label,
    metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
    estimate = estimates,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_encounters = n_encounters,
    n_observations = nrow(data),
    tp = tp, fn = fn, tn = tn, fp = fp,
    tp_encounters = tp_encounters, fn_encounters = fn_encounters,
    tn_encounters = tn_encounters, fp_encounters = fp_encounters,
    support_min = support_min,
    publishable = n_encounters >= minimum_cell_size & support_min >= minimum_cell_size
  )
}

prepare_horizon_data <- function(data, event_column, horizon_hours) {
  data %>%
    transmute(
      hospitalization_id, recorded_dttm, score = score_value,
      event_time = .data[[event_column]]
    ) %>%
    filter(is.na(event_time) | recorded_dttm < event_time) %>%
    arrange(hospitalization_id, recorded_dttm) %>%
    group_by(hospitalization_id) %>%
    mutate(
      alert_time_num = if_else(score >= threshold, as.numeric(recorded_dttm), -Inf),
      last_alert_num = cummax(alert_time_num),
      predicted = as.integer(
        is.finite(last_alert_num) &
          as.numeric(recorded_dttm) - last_alert_num <= horizon_hours * 3600
      ),
      outcome = as.integer(
        !is.na(event_time) & event_time <= recorded_dttm + hours(horizon_hours)
      )
    ) %>%
    ungroup()
}

prepare_hospital_data <- function(data, event_column) {
  data %>%
    transmute(
      hospitalization_id, recorded_dttm, score = score_value,
      event_time = .data[[event_column]]
    ) %>%
    filter(is.na(event_time) | recorded_dttm < event_time) %>%
    group_by(hospitalization_id) %>%
    summarise(
      outcome = as.integer(any(!is.na(event_time))),
      score = max(score),
      .groups = "drop"
    ) %>%
    mutate(predicted = as.integer(score >= threshold))
}

hospital_data <- map(outcomes, ~ prepare_hospital_data(analysis_rows, .x))
performance_internal <- imap_dfr(outcomes, function(event_column, outcome_label) {
  horizon_results <- map_dfr(horizons, function(horizon) {
    horizon_data <- prepare_horizon_data(analysis_rows, event_column, horizon)
    performance_metrics(horizon_data, outcome_label, paste0(horizon, "h"))
  })
  hospital_result <- performance_metrics(
    hospital_data[[outcome_label]], outcome_label, "Hospital level"
  )
  bind_rows(horizon_results, hospital_result)
})
arrow::write_parquet(performance_internal, phi_path("performance_metrics_internal"))

fig_1_data <- performance_internal %>%
  transmute(
    site = site_name, outcome = outcome_name, horizon, metric,
    estimate = if_else(publishable, estimate, NA_real_),
    ci_lower = if_else(publishable, ci_lower, NA_real_),
    ci_upper = if_else(publishable, ci_upper, NA_real_),
    status = if_else(publishable, "Published", "Suppressed")
  ) %>%
  mutate(horizon = factor(
    horizon,
    levels = c(paste0(horizons, "h"), "Hospital level")
  ))
write_final_csv(fig_1_data, "fig_1")
fig_1_plot_data <- fig_1_data %>%
  filter(
    status == "Published",
    metric %in% c("AUC", "Sensitivity", "Specificity", "PPV")
  )
fig_1 <- ggplot(fig_1_plot_data, aes(estimate, outcome)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.2, orientation = "y") +
  facet_grid(metric ~ horizon) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_edi() +
  labs(
    title = paste("EDI Performance for Adverse Clinical Outcomes -", site_name),
    x = "Estimate (95% CI)", y = NULL
  )
save_plot(fig_1, "fig_1", 13, 8)

# Figure 2: aggregate calibration --------------------------------------------
log_step("Generating calibration and outcome intersections")
calibration_source <- hospital_data[["Composite"]]
calibration_aggregate <- tibble(
  bin = integer(), n_encounters = integer(),
  mean_predicted_risk = double(), observed_event_rate = double()
)
if (nrow(calibration_source) >= minimum_cell_size && n_distinct(calibration_source$outcome) == 2L) {
  calibration_model <- tryCatch(
    stats::glm(outcome ~ score, data = calibration_source, family = stats::binomial()),
    error = function(e) NULL
  )
  if (!is.null(calibration_model)) {
    calibration_aggregate <- calibration_source %>%
      mutate(
        predicted_risk = as.numeric(stats::predict(calibration_model, type = "response")),
        bin = ntile(predicted_risk, config$analysis$calibration_bins)
      ) %>%
      filter(is.finite(predicted_risk)) %>%
      group_by(bin) %>%
      summarise(
        n_encounters = n(), events = sum(outcome),
        mean_predicted_risk = mean(predicted_risk),
        observed_event_rate = mean(outcome),
        .groups = "drop"
      ) %>%
      filter(
        n_encounters >= minimum_cell_size,
        events >= minimum_cell_size,
        n_encounters - events >= minimum_cell_size
      )
  }
}
arrow::write_parquet(calibration_aggregate, phi_path("calibration_bins_internal"))
fig_2_data <- calibration_aggregate %>%
  select(-n_encounters, -events) %>%
  mutate(site = site_name, .before = 1)
write_final_csv(fig_2_data, "fig_2")
fig_2 <- ggplot(fig_2_data, aes(mean_predicted_risk, observed_event_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  coord_equal() +
  theme_edi() +
  labs(
    title = "Calibration of EDI for the Composite Outcome",
    x = "Mean predicted risk", y = "Observed outcome rate"
  )
save_plot(fig_2, "fig_2", 8, 7)

# Figure 3: aggregate overlap combinations -----------------------------------
overlap_flags <- encounters %>%
  transmute(
    ICU = !is.na(icu_time), Vasopressors = !is.na(vasopressor_time),
    IMV = !is.na(imv_time), Mortality = !is.na(death_time)
  )
flag_matrix <- as.matrix(overlap_flags)
combination <- apply(flag_matrix, 1, function(x) {
  labels <- colnames(flag_matrix)[x]
  if (length(labels)) paste(labels, collapse = " + ") else "None"
})
overlap_internal <- tibble(combination = combination) %>%
  count(combination, name = "N") %>%
  arrange(desc(N)) %>%
  mutate(suppressed = N < minimum_cell_size)
# Complementary suppression prevents the cohort total from revealing a single
# omitted low-count combination by subtraction.
if (any(overlap_internal$suppressed) && any(!overlap_internal$suppressed)) {
  secondary <- which(!overlap_internal$suppressed)[
    which.min(overlap_internal$N[!overlap_internal$suppressed])
  ]
  overlap_internal$suppressed[[secondary]] <- TRUE
}
arrow::write_parquet(overlap_internal, phi_path("outcome_overlap_internal"))
fig_3_data <- overlap_internal %>%
  filter(!suppressed) %>%
  select(-suppressed) %>%
  mutate(site = site_name, status = "Published", .before = 1)
write_final_csv(fig_3_data, "fig_3")
if (nrow(fig_3_data)) {
  fig_3 <- ggplot(fig_3_data, aes(N, reorder(combination, N))) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = N), hjust = -0.1, size = 3.5) +
    expand_limits(x = max(fig_3_data$N) * 1.12) +
    theme_edi() +
    labs(title = "Published Composite Outcome Intersections", x = "Encounters", y = NULL)
} else {
  fig_3 <- ggplot() + annotate("text", x = 0, y = 0, label = "No publishable combinations") +
    theme_void() + labs(title = "Composite Outcome Intersections")
}
save_plot(fig_3, "fig_3", 10, 7)

# Figure 4: score trajectories in the configured pre-event window ------------
log_step("Generating trajectories and threshold analyses")
trajectory_window <- config$trajectory$window_hours
trajectory_bin <- config$trajectory$bin_width_hours
trajectory_min_n <- max(minimum_cell_size, config$trajectory$min_n)

build_trajectory <- function(data, anchor_column, outcome_label) {
  data %>%
    transmute(
      hospitalization_id, recorded_dttm, score_value,
      anchor_time = .data[[anchor_column]]
    ) %>%
    filter(!is.na(anchor_time), recorded_dttm <= anchor_time) %>%
    mutate(hours_to_anchor = as.numeric(difftime(anchor_time, recorded_dttm, units = "hours"))) %>%
    filter(hours_to_anchor >= 0, hours_to_anchor <= trajectory_window) %>%
    mutate(
      hours_bin = -pmin(
        floor(hours_to_anchor / trajectory_bin),
        ceiling(trajectory_window / trajectory_bin) - 1
      ) * trajectory_bin,
      outcome = outcome_label
    ) %>%
    select(hospitalization_id, outcome, hours_bin, score_value)
}

event_trajectories <- imap_dfr(outcomes, function(event_column, outcome_label) {
  build_trajectory(analysis_rows, event_column, outcome_label)
})
no_event_rows <- analysis_rows %>%
  filter(
    is.na(icu_time), is.na(vasopressor_time), is.na(imv_time),
    is.na(death_time)
  )
set.seed(config$trajectory$seed)
no_event_anchors <- no_event_rows %>%
  group_by(hospitalization_id) %>%
  summarise(min_time = min(recorded_dttm), max_time = max(recorded_dttm), .groups = "drop") %>%
  mutate(earliest_anchor = min_time + hours(trajectory_window)) %>%
  filter(earliest_anchor <= max_time) %>%
  mutate(control_anchor = as.POSIXct(NA, tz = timezone))
if (nrow(no_event_anchors)) {
  random_seconds <- map2_dbl(
    no_event_anchors$earliest_anchor, no_event_anchors$max_time,
    ~ stats::runif(1, as.numeric(.x), as.numeric(.y))
  )
  no_event_anchors$control_anchor <- as.POSIXct(random_seconds, origin = "1970-01-01", tz = timezone)
}
control_trajectories <- no_event_rows %>%
  inner_join(
    no_event_anchors %>% select(hospitalization_id, control_anchor),
    by = "hospitalization_id"
  ) %>%
  build_trajectory("control_anchor", "No Event")

trajectory_per_encounter <- bind_rows(event_trajectories, control_trajectories) %>%
  group_by(hospitalization_id, outcome, hours_bin) %>%
  summarise(score_value = mean(score_value), .groups = "drop")
trajectory_aggregate <- trajectory_per_encounter %>%
  group_by(outcome, hours_bin) %>%
  summarise(
    n_encounters = n(), mean_score = mean(score_value),
    se_score = if_else(n() > 1, stats::sd(score_value) / sqrt(n()), NA_real_),
    .groups = "drop"
  )
arrow::write_parquet(trajectory_aggregate, phi_path("trajectory_bins_internal"))
fig_4_data <- trajectory_aggregate %>%
  filter(n_encounters >= trajectory_min_n) %>%
  select(-n_encounters) %>%
  mutate(
    ci_lower = mean_score - 1.96 * se_score,
    ci_upper = mean_score + 1.96 * se_score,
    site = site_name,
    .before = 1
  )
write_final_csv(fig_4_data, "fig_4")
fig_4 <- ggplot(fig_4_data, aes(hours_bin, mean_score, color = outcome, fill = outcome)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(limits = c(-trajectory_window, 0)) +
  theme_edi() +
  labs(
    title = paste0("Mean EDI Score in the ", trajectory_window, " Hours Before Outcomes"),
    x = "Hours before outcome", y = "Mean EDI score", color = "Outcome", fill = "Outcome"
  )
save_plot(fig_4, "fig_4", 12, 7)

# Figure 5: hospital-level threshold curves ----------------------------------
thresholds <- seq(
  config$threshold_grid$min, config$threshold_grid$max,
  by = config$threshold_grid$step
)
threshold_metrics <- imap_dfr(hospital_data, function(data, outcome_label) {
  map_dfr(thresholds, function(current_threshold) {
    predicted <- as.integer(data$score >= current_threshold)
    tp <- sum(predicted == 1L & data$outcome == 1L)
    fn <- sum(predicted == 0L & data$outcome == 1L)
    tn <- sum(predicted == 0L & data$outcome == 0L)
    fp <- sum(predicted == 1L & data$outcome == 0L)
    tibble(
      outcome = outcome_label, threshold = current_threshold,
      n_encounters = nrow(data), tp = tp, fn = fn, tn = tn, fp = fp,
      sensitivity = safe_divide(tp, tp + fn),
      specificity = safe_divide(tn, tn + fp),
      PPV = safe_divide(tp, tp + fp),
      NPV = safe_divide(tn, tn + fn)
    )
  })
})
arrow::write_parquet(threshold_metrics, phi_path("threshold_metrics_internal"))
threshold_long <- threshold_metrics %>%
  pivot_longer(
    c(sensitivity, specificity, PPV, NPV),
    names_to = "metric", values_to = "estimate"
  ) %>%
  mutate(
    support_min = case_when(
      metric == "sensitivity" ~ pmin(tp, fn),
      metric == "specificity" ~ pmin(tn, fp),
      metric == "PPV" ~ pmin(tp, fp),
      metric == "NPV" ~ pmin(tn, fn)
    ),
    publishable = n_encounters >= minimum_cell_size & support_min >= minimum_cell_size
  )
fig_5_data <- threshold_long %>%
  transmute(
    site = site_name, outcome, threshold, metric,
    estimate = if_else(publishable, estimate, NA_real_),
    status = if_else(publishable, "Published", "Suppressed")
  )
write_final_csv(fig_5_data, "fig_5")
fig_5 <- fig_5_data %>%
  filter(status == "Published") %>%
  ggplot(aes(threshold, estimate, color = outcome)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(vars(metric), ncol = 2) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_edi() +
  theme(legend.position = "bottom") +
  labs(
    title = "Hospital-Level EDI Performance by Threshold",
    x = "EDI threshold", y = "Estimate", color = "Outcome"
  )
save_plot(fig_5, "fig_5", 13, 9)

# Figure 6: lead time in hours ------------------------------------------------
compute_lead_time <- function(data, event_column, outcome_label) {
  event_rows <- data %>%
    transmute(
      hospitalization_id, recorded_dttm, score_value,
      event_time = .data[[event_column]]
  ) %>%
    filter(!is.na(event_time), recorded_dttm <= event_time)
  map_dfr(thresholds, function(current_threshold) {
    above_threshold <- event_rows %>% filter(score_value >= current_threshold)
    if (!nrow(above_threshold)) {
      return(tibble(
        outcome = outcome_label, threshold = current_threshold, n_encounters = 0L,
        mean_lead_time_hours = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_
      ))
    }
    crossings <- above_threshold %>%
      group_by(hospitalization_id) %>%
      summarise(
        first_crossing = min(recorded_dttm), event_time = first(event_time),
        .groups = "drop"
      ) %>%
      mutate(lead_time_hours = as.numeric(difftime(event_time, first_crossing, units = "hours"))) %>%
      filter(is.finite(lead_time_hours), lead_time_hours >= 0)
    n <- nrow(crossings)
    mean_lead <- if (n) mean(crossings$lead_time_hours) else NA_real_
    se_lead <- if (n > 1) stats::sd(crossings$lead_time_hours) / sqrt(n) else NA_real_
    tibble(
      outcome = outcome_label, threshold = current_threshold, n_encounters = n,
      mean_lead_time_hours = mean_lead,
      ci_lower = mean_lead - 1.96 * se_lead,
      ci_upper = mean_lead + 1.96 * se_lead
    )
  })
}

lead_time_internal <- imap_dfr(outcomes, function(event_column, outcome_label) {
  compute_lead_time(analysis_rows, event_column, outcome_label)
})
arrow::write_parquet(lead_time_internal, phi_path("lead_time_internal"))
lead_min_n <- max(minimum_cell_size, config$lead_time$min_n)
fig_6_data <- lead_time_internal %>%
  filter(n_encounters >= lead_min_n) %>%
  transmute(
    site = site_name, outcome, threshold, mean_lead_time_hours, ci_lower, ci_upper
  )
write_final_csv(fig_6_data, "fig_6")
fig_6 <- ggplot(
  fig_6_data,
  aes(threshold, mean_lead_time_hours, color = outcome, fill = outcome)
) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  theme_edi() +
  labs(
    title = "Mean Lead Time Among Threshold-Crossing Encounters",
    x = "EDI threshold", y = "Lead time (hours)", color = "Outcome", fill = "Outcome"
  )
save_plot(fig_6, "fig_6", 11, 7)

# Sanitized aggregate run metadata contains no local paths or row-level data.
package_versions <- vapply(
  required_packages,
  function(package) as.character(utils::packageVersion(package)),
  character(1)
)
manifest <- tibble(
  key = c(
    "site", "completed_utc", "workflow", "filetype", "timezone", "r_version",
    "threshold", "minimum_cell_size", "minimum_age", "horizons_hours",
    paste0("package_", names(package_versions))
  ),
  value = c(
    site_name, format(Sys.time(), tz = "UTC", usetz = TRUE), "canonical_config_driven_v1",
    config$filetype, timezone, paste(R.version$major, R.version$minor, sep = "."),
    as.character(threshold), as.character(minimum_cell_size),
    as.character(config$cohort$min_age), paste(horizons, collapse = ","),
    unname(package_versions)
  )
)
write_final_csv(manifest, "run_manifest")
log_step("Analysis complete")
