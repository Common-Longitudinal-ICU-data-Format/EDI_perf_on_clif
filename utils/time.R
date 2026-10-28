normalize_clif_time <- function(x, timezone) {
  if (inherits(x, "POSIXt")) {
    source_timezone <- attr(x, "tzone")
    if (is.null(source_timezone) || !length(source_timezone) || !nzchar(source_timezone[1])) {
      # Arrow's timezone-naive timestamps encode their wall clock on a UTC axis.
      # Read that clock explicitly rather than using the machine's timezone.
      return(lubridate::force_tz(lubridate::with_tz(x, "UTC"), tzone = timezone))
    }
    return(lubridate::with_tz(as.POSIXct(x), tzone = timezone))
  }
  if (inherits(x, "Date")) {
    return(lubridate::force_tz(as.POSIXct(x, tz = "UTC"), tzone = timezone))
  }
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = timezone))
  suppressWarnings(lubridate::ymd_hms(as.character(x), tz = timezone, quiet = TRUE, truncated = 3))
}
