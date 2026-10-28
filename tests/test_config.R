load_test_config <- function(values) {
  path <- tempfile("edi-config-", fileext = ".json")
  jsonlite::write_json(values, path, auto_unbox = TRUE, null = "null")
  previous <- Sys.getenv("EDI_CONFIG_PATH", unset = NA_character_)
  on.exit({
    if (is.na(previous)) Sys.unsetenv("EDI_CONFIG_PATH") else Sys.setenv(EDI_CONFIG_PATH = previous)
    unlink(path)
  })
  Sys.setenv(EDI_CONFIG_PATH = path)
  environment <- new.env(parent = globalenv())
  sys.source("utils/config.R", envir = environment)
  environment$config
}
base_config <- list(site_name = "synthetic", data_directory = "/synthetic/input")

test("minimal configuration selects standard CLIF EDI tables", {
  cfg <- load_test_config(base_config)
  expect_equal(cfg$score_model_id_column, "model_id")
  expect_equal(cfg$score_model_category, "epic_deterioration_index")
  expect_equal(cfg$model_registry_file, "clif_model_registry.parquet")
  expect_equal(cfg$threshold_crossing$step, 1)
  expect_equal(cfg$first_crossing$bin_hours, 6)
})
test("obsolete model selection requires explicit migration", {
  for (key in c("score_model_id_value", "edi_model_id", "score_source_verified_edi_only")) {
    cfg <- base_config; cfg[[key]] <- if (key == "score_source_verified_edi_only") TRUE else "arbitrary-old-id"
    expect_error(load_test_config(cfg), "migrat")
  }
})
test("EDI category typo or another model category is rejected", {
  expect_error(load_test_config(c(base_config, list(score_model_category = "epic_sepsis_model"))), "epic_deterioration_index")
})
test("crossing grid and plot settings validate positive widths", {
  expect_error(load_test_config(c(base_config, list(first_crossing = list(bin_hours = 0)))), "bin_hours")
  expect_error(load_test_config(c(base_config, list(threshold_crossing = list(min = 100, max = 0)))), "threshold_crossing")
})
test("count settings reject fractional and out-of-range integers", {
  for (number in c(0.9, 10.9, 2147483648)) {
    expect_error(load_test_config(c(base_config, list(minimum_cell_size = number))), "integer")
  }
})
test("obsolete null selection fields also require migration", {
  legacy <- base_config
  legacy["score_model_id_value"] <- list(NULL)
  legacy["edi_model_id"] <- list(NULL)
  expect_error(load_test_config(legacy), "migrat")
})
