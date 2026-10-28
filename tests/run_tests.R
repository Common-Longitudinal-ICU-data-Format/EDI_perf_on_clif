suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

failures <- character()
tests_run <- 0L
test <- function(label, code) {
  filter <- Sys.getenv("EDI_TEST_FILTER", unset = "")
  if (nzchar(filter) && !grepl(filter, label)) return(invisible(NULL))
  tests_run <<- tests_run + 1L
  tryCatch({
    force(code)
    cat("PASS:", label, "\n")
  }, error = function(e) {
    failures <<- c(failures, paste(label, conditionMessage(e), sep = ": "))
    cat("FAIL:", label, "—", conditionMessage(e), "\n")
  })
}
expect_equal <- function(actual, expected, tolerance = 1e-8) {
  result <- all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE)
  if (!isTRUE(result)) stop(paste(result, collapse = "; "), call. = FALSE)
}
expect_error <- function(code, pattern) {
  message <- tryCatch({ force(code); NULL }, error = conditionMessage)
  if (is.null(message) || !grepl(pattern, message, ignore.case = TRUE)) {
    stop("Expected error matching '", pattern, "'; got: ", message, call. = FALSE)
  }
}

for (path in c("utils/model_registry.R", "utils/threshold_crossing.R", "utils/time.R")) {
  if (file.exists(path)) source(path)
}
files <- sort(list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE))
requested <- commandArgs(trailingOnly = TRUE)
if (length(requested)) files <- files[basename(files) %in% requested]
for (path in files) source(path)
cat("\n", tests_run - length(failures), "/", tests_run, " tests passed\n", sep = "")
if (length(failures)) stop(paste(failures, collapse = "\n"), call. = FALSE)
