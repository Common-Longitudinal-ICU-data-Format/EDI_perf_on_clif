# Model IDs are opaque join keys. Category, not an institution's ID, selects EDI.
validate_model_registry <- function(registry) {
  missing <- setdiff(c("model_id", "model_category"), names(registry))
  if (length(missing)) {
    stop("clif_model_registry is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  for (column in c("model_id", "model_category")) {
    values <- as.character(registry[[column]])
    if (any(is.na(values) | !nzchar(trimws(values)))) {
      stop("clif_model_registry contains empty ", column, " values.", call. = FALSE)
    }
  }
  if (anyDuplicated(as.character(registry$model_id))) {
    stop("clif_model_registry contains duplicate model_id values; each ID must map to one model.", call. = FALSE)
  }
  invisible(registry)
}

resolve_edi_model_ids <- function(registry, category = "epic_deterioration_index") {
  validate_model_registry(registry)
  categories <- tolower(trimws(as.character(registry$model_category)))
  ids <- as.character(registry$model_id[categories == tolower(trimws(category))])
  if (!length(ids)) {
    stop("No models categorized as ", category, " in clif_model_registry. Correct the registry mapping before running.", call. = FALSE)
  }
  ids
}

validate_score_model_ids <- function(score_ids, registry) {
  validate_model_registry(registry)
  score_ids <- as.character(score_ids)
  if (any(is.na(score_ids) | !nzchar(trimws(score_ids)))) {
    stop("Score source contains empty model_id values.", call. = FALSE)
  }
  missing <- setdiff(score_ids, as.character(registry$model_id))
  if (length(missing)) {
    stop("Score model IDs missing from clif_model_registry: ", paste(missing, collapse = ", "), ". Correct the tables before running.", call. = FALSE)
  }
  invisible(TRUE)
}
