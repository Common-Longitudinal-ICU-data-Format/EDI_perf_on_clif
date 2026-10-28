make_workflow_fixture <- function(root, site, format, timezone, model_ids, custom_columns = FALSE) {
  dir.create(root)
  input <- file.path(root, "input"); dir.create(input)
  ids <- paste0("enc-", seq_len(40))
  patients <- paste0("patient-", seq_len(40))
  at <- as.POSIXct("2020-01-01", tz = "UTC")
  icu_indices <- c(1:3, 11:13, 21:23, 31:33)
  vent_indices <- c(4, 14, 24, 34)
  vaso_indices <- c(5, 15, 25, 35)
  death_indices <- c(6, 16, 26, 36)
  hosp <- tibble(
    patient_id = patients, hospitalization_id = ids, admission_dttm = at,
    discharge_dttm = at + 72 * 3600, age_at_admission = 30 + seq_len(40),
    discharge_category = "Home"
  )
  patient <- tibble(
    patient_id = patients,
    death_dttm = as.POSIXct(ifelse(seq_len(40) %in% death_indices, as.numeric(at) + 48 * 3600, NA_real_), origin = "1970-01-01", tz = "UTC"),
    sex_category = rep(c("Female", "Male"), 20),
    race_category = rep(c("White", "Black or African American"), 20),
    ethnicity_category = rep(c("Hispanic or Latino", "Not Hispanic or Latino"), 20)
  )
  adt <- bind_rows(
    tibble(hospitalization_id = ids, in_dttm = at, out_dttm = at + ifelse(seq_len(40) %in% icu_indices, 48, 72) * 3600, location_category = "ward"),
    tibble(hospitalization_id = ids[icu_indices], in_dttm = at + 48 * 3600, out_dttm = at + 72 * 3600, location_category = "icu")
  )
  resp <- tibble(hospitalization_id = ids[vent_indices], recorded_dttm = at + 48 * 3600, device_category = "IMV")
  med <- tibble(hospitalization_id = ids[vaso_indices], admin_dttm = at + 48 * 3600, med_group = "vasoactives")
  registry <- tibble(
    model_id = model_ids,
    model_category = c("epic_deterioration_index", "epic_deterioration_index", "epic_sepsis_model"),
    is_live = FALSE
  )
  base_scores <- rep(c(20, 35, 45, 60), each = 10)
  scores <- bind_rows(
    tibble(model_id = model_ids[1], hospitalization_id = ids, recorded_dttm = at + 6 * 3600, score_value = base_scores),
    tibble(model_id = model_ids[2], hospitalization_id = ids, recorded_dttm = at + 30 * 3600, score_value = base_scores + 5),
    tibble(model_id = model_ids[3], hospitalization_id = ids, recorded_dttm = at + 12 * 3600, score_value = 99)
  )
  cfg <- list(
    site_name = site, data_directory = input, filetype = format, timezone = timezone,
    output_directory = file.path(root, "output"), score_file = paste0("clif_scores.", format),
    model_registry_file = paste0("clif_model_registry.", format), minimum_cell_size = 1,
    analysis = list(calibration_bins = 2), trajectory = list(min_n = 1),
    lead_time = list(min_n = 1), threshold_grid = list(step = 20)
  )
  if (custom_columns) {
    names(scores) <- c("model_key", "encounter_key", "score_clock", "edi_value")
    cfg$score_model_id_column <- "model_key"
    cfg$score_hospitalization_id_column <- "encounter_key"
    cfg$score_time_column <- "score_clock"
    cfg$score_value_column <- "edi_value"
  }
  tables <- list(hospitalization = hosp, patient = patient, adt = adt, respiratory_support = resp,
                 medication_admin_continuous = med, model_registry = registry, scores = scores)
  for (name in names(tables)) {
    path <- file.path(input, paste0("clif_", name, ".", format))
    if (format == "parquet") arrow::write_parquet(tables[[name]], path) else utils::write.csv(tables[[name]], path, row.names = FALSE)
  }
  config_path <- file.path(root, "config.json")
  jsonlite::write_json(cfg, config_path, auto_unbox = TRUE)
  list(config = config_path, output = cfg$output_directory, site = site)
}

execute_workflow_fixture <- function(fixture) {
  log <- file.path(dirname(fixture$config), "run.log")
  previous <- Sys.getenv("EDI_CONFIG_PATH", unset = NA_character_)
  on.exit(if (is.na(previous)) Sys.unsetenv("EDI_CONFIG_PATH") else Sys.setenv(EDI_CONFIG_PATH = previous))
  Sys.setenv(EDI_CONFIG_PATH = fixture$config)
  status <- system2(file.path(R.home("bin"), "Rscript"), "code/edi_analysis.R", stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log), collapse = "\n"))
  invisible(fixture)
}

run_workflow_fixture <- function(fixture) {
  execute_workflow_fixture(fixture)
  phi <- file.path(fixture$output, "intermediate_phi")
  final <- file.path(fixture$output, "final_no_phi")
  rows <- arrow::read_parquet(file.path(phi, "eligible_in_admission_scores.parquet"))
  expect_equal(nrow(rows), 80)
  expect_equal(length(unique(rows$hospitalization_id)), 40)
  read_result <- function(stem) utils::read.csv(file.path(final, paste0(fixture$site, "_", stem, ".csv")))
  table1 <- read_result("table1_data")
  expect_equal(table1$N, c(40, 30, 10))
  fig7 <- read_result("fig_7")
  expect_equal(fig7$n_reached[fig7$threshold == 40], 30)
  fig8 <- read_result("fig_8")
  expect_equal(fig8$n_crossed_this_hour[fig8$hour == 6], 20)
  expect_equal(fig8$n_crossed_this_hour[fig8$hour == 30], 10)
  expect_equal(read_result("fig_8_summary")$N, 30)
  manifest <- read_result("run_manifest")
  expect_equal(manifest$value[manifest$key == "score_model_category"], "epic_deterioration_index")
  pngs <- list.files(final, pattern = "\\.png$", full.names = TRUE)
  expect_equal(length(pngs), 8)
  stopifnot(all(file.info(pngs)$size > 0))
  for (path in list.files(final, pattern = "\\.csv$", full.names = TRUE)) {
    columns <- names(utils::read.csv(path))
    stopifnot(!any(columns %in% c("hospitalization_id", "patient_id", "recorded_dttm", "first_crossing")))
  }
}

test("complete parquet workflow selects EDI by category and produces Figures 1–8", {
  root <- tempfile("edi-workflow-parquet-")
  fixture <- make_workflow_fixture(root, "site_a", "parquet", "UTC", c("version-one", "version-two", "non-edi"))
  run_workflow_fixture(fixture)
  unlink(root, recursive = TRUE)
})
test("complete CSV workflow supports another site, timezone, model IDs and score column mappings", {
  root <- tempfile("edi-workflow-csv-")
  fixture <- make_workflow_fixture(root, "site_b", "csv", "Asia/Tokyo", c("new-one", "new-two", "other"), custom_columns = TRUE)
  run_workflow_fixture(fixture)
  unlink(root, recursive = TRUE)
})

make_small_complement_fixture <- function() {
  root <- tempfile("edi-small-complement-")
  fixture <- make_workflow_fixture(root, "synthetic", "parquet", "UTC", c("edi-a", "edi-b", "other"))
  cfg <- jsonlite::fromJSON(fixture$config)
  cfg$minimum_cell_size <- 10
  jsonlite::write_json(cfg, fixture$config, auto_unbox = TRUE)
  path <- file.path(root, "input", "clif_scores.parquet")
  rows <- arrow::read_parquet(path)
  rows$score_value[rows$model_id == "edi-a" & rows$hospitalization_id %in% paste0("enc-", 1:7)] <- 45
  arrow::write_parquet(rows, path)
  execute_workflow_fixture(fixture)
  fixture
}
test("suppressed Table 1 stratum counts cannot be recovered from counts and percentages", {
  fixture <- make_small_complement_fixture()
  result <- utils::read.csv(file.path(fixture$output, "final_no_phi", "synthetic_table1_data.csv"))
  stopifnot(all(is.na(as.matrix(result[-1, setdiff(names(result), "cohort")]))))
  unlink(dirname(fixture$config), recursive = TRUE)
})
test("Figure 8 does not reveal the Table 1 suppressed crossing total or small complement", {
  fixture <- make_small_complement_fixture()
  root <- file.path(fixture$output, "final_no_phi")
  summary <- utils::read.csv(file.path(root, "synthetic_fig_8_summary.csv"))
  hourly <- utils::read.csv(file.path(root, "synthetic_fig_8.csv"))
  stopifnot(is.na(summary$N), all(is.na(hourly$n_total_crossers)), all(is.na(hourly$pct_crossed_this_hour)),
            all(is.na(hourly$n_crossed_cumulative)))
  unlink(dirname(fixture$config), recursive = TRUE)
})
test("numeric int64 model IDs above double precision do not select a non-EDI model", {
  root <- tempfile("edi-int64-model-")
  fixture <- make_workflow_fixture(root, "synthetic", "parquet", "UTC",
                                   c("9007199254740993", "9007199254740994", "9007199254740992"))
  path <- file.path(root, "input", "clif_scores.parquet")
  rows <- arrow::read_parquet(path)
  rows$model_id <- bit64::as.integer64(rows$model_id)
  arrow::write_parquet(rows, path)
  run_workflow_fixture(fixture)
  unlink(root, recursive = TRUE)
})
test("CSV model IDs retain leading zeros when only EDI models have scores", {
  root <- tempfile("edi-leading-zero-")
  fixture <- make_workflow_fixture(root, "synthetic", "csv", "UTC", c("001", "002", "other"))
  path <- file.path(root, "input", "clif_scores.csv")
  rows <- utils::read.csv(path, colClasses = c(model_id = "character"))
  rows <- rows[rows$model_id != "other", ]
  utils::write.csv(rows, path, row.names = FALSE)
  run_workflow_fixture(fixture)
  unlink(root, recursive = TRUE)
})
test("eventless zero-crosser cohort still produces every analysis output", {
  root <- tempfile("edi-eventless-")
  fixture <- make_workflow_fixture(root, "synthetic", "parquet", "UTC", c("edi-a", "edi-b", "other"))
  cfg <- jsonlite::fromJSON(fixture$config)
  cfg$edi_threshold <- 100
  jsonlite::write_json(cfg, fixture$config, auto_unbox = TRUE)
  input <- file.path(root, "input")
  patient <- arrow::read_parquet(file.path(input, "clif_patient.parquet"))
  patient$death_dttm[] <- NA
  arrow::write_parquet(patient, file.path(input, "clif_patient.parquet"))
  adt <- arrow::read_parquet(file.path(input, "clif_adt.parquet")) %>% filter(location_category == "ward")
  arrow::write_parquet(adt, file.path(input, "clif_adt.parquet"))
  for (name in c("respiratory_support", "medication_admin_continuous")) {
    path <- file.path(input, paste0("clif_", name, ".parquet"))
    rows <- arrow::read_parquet(path)
    arrow::write_parquet(rows[0, ], path)
  }
  execute_workflow_fixture(fixture)
  result <- arrow::read_parquet(file.path(fixture$output, "intermediate_phi", "first_crossing_summary_internal.parquet"))
  expect_equal(result$N, 0)
  expect_equal(length(list.files(file.path(fixture$output, "final_no_phi"), pattern = "\\.png$")), 8)
  unlink(root, recursive = TRUE)
})
test("output validation accepts correctly suppressed small crossing totals", {
  root <- tempfile("edi-small-crossing-")
  fixture <- make_workflow_fixture(root, "synthetic", "parquet", "UTC", c("edi-a", "edi-b", "other"))
  cfg <- jsonlite::fromJSON(fixture$config)
  cfg$minimum_cell_size <- 10
  jsonlite::write_json(cfg, fixture$config, auto_unbox = TRUE)
  path <- file.path(root, "input", "clif_scores.parquet")
  rows <- arrow::read_parquet(path)
  rows$score_value[rows$model_id != "other"] <- 25
  rows$score_value[rows$model_id == "edi-a" & rows$hospitalization_id %in% paste0("enc-", 1:3)] <- 45
  arrow::write_parquet(rows, path)
  execute_workflow_fixture(fixture)
  log <- file.path(root, "validation.log")
  status <- system2(file.path(R.home("bin"), "Rscript"), c("tests/validate_outputs.R", shQuote(fixture$output)), stdout = log, stderr = log)
  if (status != 0L) stop(paste(readLines(log), collapse = "\n"))
  unlink(root, recursive = TRUE)
})
test("fully suppressed small cohort still produces all eight figures", {
  root <- tempfile("edi-suppressed-")
  fixture <- make_workflow_fixture(root, "synthetic", "parquet", "UTC", c("edi-a", "edi-b", "other"))
  cfg <- jsonlite::fromJSON(fixture$config)
  cfg$minimum_cell_size <- 1000
  jsonlite::write_json(cfg, fixture$config, auto_unbox = TRUE)
  execute_workflow_fixture(fixture)
  expect_equal(length(list.files(file.path(fixture$output, "final_no_phi"), pattern = "\\.png$")), 8)
  unlink(root, recursive = TRUE)
})
