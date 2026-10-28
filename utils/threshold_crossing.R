# August analyses: full-hospitalization maxima and admission-to-first crossing.
summarize_threshold_crossings <- function(max_scores, thresholds) {
  total <- length(max_scores)
  n_reached <- vapply(thresholds, function(thr) sum(max_scores >= thr), numeric(1))
  tibble::tibble(
    threshold = thresholds, n_total = total, n_reached = n_reached,
    pct_reached = if (total) round(n_reached / total * 100, 2) else NA_real_
  )
}

first_threshold_crossings <- function(scores, encounters, threshold) {
  scores %>%
    filter(score_value >= threshold, !is.na(recorded_dttm)) %>%
    group_by(hospitalization_id) %>%
    summarise(first_crossing = min(recorded_dttm), .groups = "drop") %>%
    inner_join(encounters %>% select(hospitalization_id, admission_dttm), by = "hospitalization_id") %>%
    transmute(
      hospitalization_id,
      hours_from_admission = as.numeric(difftime(first_crossing, admission_dttm, units = "hours"))
    ) %>%
    filter(is.finite(hours_from_admission), hours_from_admission >= 0)
}

summarize_first_crossings <- function(crossings, bin_hours) {
  values <- crossings$hours_from_admission
  n_cross <- length(values)
  quartiles <- if (n_cross) as.numeric(stats::quantile(values, c(.25, .5, .75))) else rep(NA_real_, 3)
  summary <- tibble::tibble(
    N = n_cross, mean_hours = if (n_cross) round(mean(values), 1) else NA_real_,
    q1_hours = round(quartiles[1], 1), median_hours = round(quartiles[2], 1),
    q3_hours = round(quartiles[3], 1)
  )
  hourly <- tibble::tibble(hour = ceiling(values)) %>% count(hour, name = "n_crossed_this_hour")
  if (n_cross) {
    hourly <- hourly %>%
      complete(hour = 0:max(hour), fill = list(n_crossed_this_hour = 0L)) %>%
      arrange(hour)
  }
  hourly <- hourly %>% mutate(
    n_total_crossers = n_cross,
    pct_crossed_this_hour = round(n_crossed_this_hour / n_cross * 100, 2),
    n_crossed_cumulative = cumsum(n_crossed_this_hour),
    pct_crossed_cumulative = round(n_crossed_cumulative / n_cross * 100, 2)
  )
  binned <- hourly %>%
    mutate(hour_bin = ceiling(hour / bin_hours) * bin_hours) %>%
    group_by(hour_bin) %>%
    summarise(n_crossed = sum(n_crossed_this_hour), .groups = "drop") %>%
    mutate(n_total_crossers = n_cross, pct_crossed = round(n_crossed / n_cross * 100, 2))
  list(hourly = hourly, binned = binned, summary = summary)
}

# Release nested count endpoints only when their positive differences are large
# enough. Fixed anchors include cohort totals and the published Table 1 stratum.
# This prevents adjacent thresholds/cumulative endpoints revealing small cells.
safe_nested_count_mask <- function(counts, total, minimum_cell_size, anchor_counts = numeric()) {
  if (!length(counts) || total < minimum_cell_size) return(rep(FALSE, length(counts)))
  anchors <- sort(unique(c(0, total, anchor_counts)))
  anchors <- anchors[
    anchors >= 0 & anchors <= total &
      (anchors == 0 | anchors >= minimum_cell_size) &
      (anchors == total | total - anchors >= minimum_cell_size)
  ]
  released <- anchors
  for (i in seq_len(length(anchors) - 1L)) {
    lower <- anchors[i]
    upper <- anchors[i + 1L]
    candidates <- sort(unique(counts[counts > lower & counts < upper]))
    last <- lower
    for (candidate in candidates) {
      if (candidate - last >= minimum_cell_size && upper - candidate >= minimum_cell_size) {
        released <- c(released, candidate)
        last <- candidate
      }
    }
  }
  counts %in% released
}

publish_threshold_crossings <- function(data, minimum_cell_size, anchor_counts = numeric()) {
  total <- if (nrow(data)) data$n_total[1] else 0
  mask <- safe_nested_count_mask(data$n_reached, total, minimum_cell_size, anchor_counts)
  data %>% mutate(
    n_total = if (total >= minimum_cell_size) n_total else NA_real_,
    n_reached = if_else(mask, n_reached, NA_real_),
    pct_reached = if_else(mask, pct_reached, NA_real_),
    status = if_else(mask, "Published", "Suppressed")
  )
}

publish_first_crossings <- function(data, bin_hours, minimum_cell_size, cohort_total) {
  total <- data$summary$N[1]
  publish_total <- total >= minimum_cell_size && cohort_total - total >= minimum_cell_size
  hourly <- data$hourly
  ends <- safe_nested_count_mask(hourly$n_crossed_cumulative, total, minimum_cell_size)
  if (!publish_total) ends[] <- FALSE
  starts <- if (length(ends)) c(publish_total, ends[-length(ends)]) else logical()
  increments <- starts & ends
  released <- c(0, hourly$n_crossed_cumulative[ends])
  bin <- ceiling(hourly$hour / bin_hours) * bin_hours
  binned_mask <- vapply(data$binned$hour_bin, function(current_bin) {
    rows <- which(bin == current_bin)
    first <- min(rows)
    before <- if (first == 1L) 0 else hourly$n_crossed_cumulative[first - 1L]
    after <- hourly$n_crossed_cumulative[max(rows)]
    publish_total && before %in% released && after %in% released
  }, logical(1))
  public_hourly <- hourly %>% mutate(
    n_total_crossers = if (publish_total) n_total_crossers else NA_real_,
    n_crossed_this_hour = if_else(increments, n_crossed_this_hour, NA_real_),
    pct_crossed_this_hour = if_else(increments, pct_crossed_this_hour, NA_real_),
    n_crossed_cumulative = if_else(ends, n_crossed_cumulative, NA_real_),
    pct_crossed_cumulative = if_else(ends, pct_crossed_cumulative, NA_real_),
    status = if_else(increments, "Published", "Suppressed"),
    cumulative_status = if_else(ends, "Published", "Suppressed")
  )
  public_binned <- data$binned %>% mutate(
    n_total_crossers = if (publish_total) n_total_crossers else NA_real_,
    n_crossed = if_else(binned_mask, n_crossed, NA_real_),
    pct_crossed = if_else(binned_mask, pct_crossed, NA_real_),
    status = if_else(binned_mask, "Published", "Suppressed")
  )
  summary <- data$summary
  if (!publish_total) summary[,] <- NA_real_
  summary$status <- if (publish_total) "Published" else "Suppressed"
  list(hourly = public_hourly, binned = public_binned, summary = summary)
}
