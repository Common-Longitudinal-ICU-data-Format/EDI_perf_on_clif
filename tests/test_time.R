test("timezone-naive timestamps use the configured wall clock regardless of host timezone", {
  previous <- Sys.getenv("TZ", unset = NA_character_)
  on.exit_restore <- function() {
    if (is.na(previous)) Sys.unsetenv("TZ") else Sys.setenv(TZ = previous)
  }
  tryCatch({
    Sys.setenv(TZ = "America/Los_Angeles")
    timestamp <- as.POSIXct("2020-01-01 06:00:00", tz = "UTC")
    attr(timestamp, "tzone") <- NULL
    result <- normalize_clif_time(timestamp, "Asia/Tokyo")
    expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2020-01-01 06:00:00")
  }, finally = on.exit_restore())
})
test("timezone-aware timestamps preserve their actual instant", {
  timestamp <- as.POSIXct("2020-01-01 06:00:00", tz = "UTC")
  result <- normalize_clif_time(timestamp, "Asia/Tokyo")
  expect_equal(as.numeric(result), as.numeric(timestamp))
  expect_equal(format(result, "%H:%M:%S"), "15:00:00")
})
test("CSV Date and date-only strings normalize to local midnight", {
  date <- normalize_clif_time(as.Date("2020-01-01"), "Asia/Tokyo")
  text <- normalize_clif_time("2020-01-01", "Asia/Tokyo")
  expect_equal(format(date, "%H:%M:%S"), "00:00:00")
  expect_equal(as.numeric(date), as.numeric(text))
})
