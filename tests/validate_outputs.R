# Run after analysis: Rscript tests/validate_outputs.R /path/to/output/root
suppressPackageStartupMessages(library(dplyr))
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) stop("Provide the analysis output root.", call. = FALSE)
root <- arguments[1]
phi <- file.path(root, "intermediate_phi")
final <- file.path(root, "final_no_phi")
manifests <- list.files(final, pattern = "_run_manifest\\.csv$", full.names = TRUE)
if (length(manifests) != 1L) stop("Expected one run manifest in the output directory.", call. = FALSE)
manifest <- utils::read.csv(manifests[1])
value <- function(key) manifest$value[match(key, manifest$key)]
site <- value("site")
threshold <- as.numeric(value("threshold"))
minimum <- as.numeric(value("minimum_cell_size"))
stopifnot(value("workflow") == "canonical_category_driven_v2",
          value("score_model_category") == "epic_deterioration_index",
          value("score_selection_method") == "clif_model_registry_category",
          value("horizon_time_basis") == "elapsed_hours")
read_final <- function(stem) utils::read.csv(file.path(final, paste0(site, "_", stem, ".csv")))
read_phi <- function(stem) arrow::read_parquet(file.path(phi, paste0(stem, ".parquet")))

encounters <- read_phi("table1_encounters")
table1 <- read_phi("table1_summary_internal")
score_count <- arrow::open_dataset(file.path(phi, "eligible_in_admission_scores.parquet")) %>%
  summarise(n = n()) %>% collect() %>% pull(n)
cross_count <- sum(encounters$max_score >= threshold)
stopifnot(table1$N[1] == nrow(encounters), table1$N[2] == cross_count)
fig7 <- read_phi("threshold_crossing_internal")
stopifnot(all(fig7$n_total == nrow(encounters)), all(diff(fig7$n_reached) <= 0))
if (threshold %in% fig7$threshold) stopifnot(fig7$n_reached[fig7$threshold == threshold] == cross_count)
hourly <- read_phi("first_crossing_hourly_internal")
summary <- read_phi("first_crossing_summary_internal")
stopifnot(summary$N == cross_count, sum(hourly$n_crossed_this_hour) == cross_count)
if (cross_count) {
  stopifnot(tail(hourly$n_crossed_cumulative, 1) == cross_count,
            tail(hourly$pct_crossed_cumulative, 1) == 100)
}
public7 <- read_final("fig_7")
table1_public <- read_final("table1_data")
anchors <- c(0, table1_public$N[1:2], public7$n_total)
released7 <- sort(unique(c(anchors, public7$n_reached)), na.last = NA)
stopifnot(all(diff(released7) >= minimum))
public8 <- read_final("fig_8")
public8_summary <- read_final("fig_8_summary")
released8 <- sort(unique(c(0, public8_summary$N, public8$n_total_crossers, public8$n_crossed_cumulative)), na.last = NA)
stopifnot(all(diff(released8) >= minimum))
positive_hourly <- public8$n_crossed_this_hour[is.finite(public8$n_crossed_this_hour) & public8$n_crossed_this_hour > 0]
stopifnot(all(positive_hourly >= minimum))

for (number in seq_len(8)) {
  path <- file.path(final, paste0(site, "_fig_", number, ".png"))
  stopifnot(file.exists(path), file.info(path)$size > 0)
  stopifnot(file.exists(file.path(final, paste0(site, "_fig_", number, ".csv"))))
}
for (path in list.files(final, pattern = "\\.csv$", full.names = TRUE)) {
  columns <- names(utils::read.csv(path))
  stopifnot(!any(columns %in% c("patient_id", "hospitalization_id", "recorded_dttm", "first_crossing")))
}
cat("Validated:", score_count, "scores;", nrow(encounters), "encounters;", cross_count,
    "threshold crossers; Figures 1–8 and aggregate output checks passed.\n")
print(public8_summary, row.names = FALSE)

baseline_path <- file.path(phi, "category_v1_baseline.rds")
if (file.exists(baseline_path)) {
  baseline <- readRDS(baseline_path)
  current_metrics <- read_phi("performance_metrics_internal")
  metric_comparison <- baseline$metrics %>%
    select(outcome_name, horizon, metric, baseline_estimate = estimate) %>%
    inner_join(current_metrics %>% select(outcome_name, horizon, metric, estimate),
               by = c("outcome_name", "horizon", "metric")) %>%
    mutate(delta = estimate - baseline_estimate)
  utils::write.csv(metric_comparison, file.path(phi, "category_baseline_metric_comparison.csv"), row.names = FALSE)
  cat("Baseline comparison: score-row delta", score_count - baseline$score_rows,
      "; encounter delta", nrow(encounters) - nrow(baseline$encounters),
      "; max absolute metric-estimate delta", max(abs(metric_comparison$delta), na.rm = TRUE), "\n")
  print(metric_comparison %>% filter(horizon == "Hospital level", metric == "AUC"), n = Inf)
}
