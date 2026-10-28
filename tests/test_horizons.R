# Load the actual production function without executing the whole workflow.
suppressPackageStartupMessages(library(lubridate))
horizon_environment <- new.env(parent = globalenv())
horizon_environment$threshold <- 40
definitions <- parse("code/edi_analysis.R")
for (definition in definitions) {
  if (is.call(definition) && identical(definition[[1]], as.name("<-")) &&
      identical(definition[[2]], as.name("prepare_horizon_data"))) {
    eval(definition, envir = horizon_environment)
  }
}
prepare_horizon <- horizon_environment$prepare_horizon_data
test("spring DST horizon includes events within four elapsed hours", {
  score_time <- as.POSIXct("2020-03-08 01:30:00", tz = "America/Chicago")
  data <- tibble(hospitalization_id = "example", recorded_dttm = score_time,
                 score_value = 45, event_time = score_time + 3.5 * 3600)
  expect_equal(prepare_horizon(data, "event_time", 4)$outcome, 1)
})
test("fall DST horizon excludes events beyond four elapsed hours", {
  score_time <- as.POSIXct("2020-11-01 00:30:00", tz = "America/Chicago")
  data <- tibble(hospitalization_id = "example", recorded_dttm = score_time,
                 score_value = 45, event_time = score_time + 4.5 * 3600)
  expect_equal(prepare_horizon(data, "event_time", 4)$outcome, 0)
})
