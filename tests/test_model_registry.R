registry <- tibble(
  model_id = c("alpha_v1", "beta_v2", "other_v1"),
  model_category = c("epic_deterioration_index", " EPIC_DETERIORATION_INDEX ", "epic_sepsis_model"),
  is_live = c(FALSE, TRUE, FALSE)
)

test("EDI selection follows categories across arbitrary IDs and versions", {
  expect_equal(resolve_edi_model_ids(registry), c("alpha_v1", "beta_v2"))
  renamed <- registry %>% mutate(model_id = c("different-x", "different-y", "different-z"))
  expect_equal(resolve_edi_model_ids(renamed), c("different-x", "different-y"))
})
test("registry rejects missing EDI category", {
  expect_error(resolve_edi_model_ids(registry %>% filter(model_id == "other_v1")), "no.*epic_deterioration_index")
})
test("registry rejects missing columns and duplicate IDs", {
  expect_error(resolve_edi_model_ids(select(registry, -model_category)), "model_category")
  expect_error(resolve_edi_model_ids(bind_rows(registry, registry[1, ])), "duplicate.*model_id")
})
test("registry rejects empty IDs and missing categories", {
  broken <- registry; broken$model_id[1] <- " "
  expect_error(resolve_edi_model_ids(broken), "empty.*model_id")
  broken <- registry; broken$model_category[1] <- NA_character_
  expect_error(resolve_edi_model_ids(broken), "empty.*model_category")
})
test("score mapping rejects orphaned and missing model IDs", {
  validate_score_model_ids(c("alpha_v1", "other_v1"), registry)
  expect_error(validate_score_model_ids(c("alpha_v1", "orphan"), registry), "missing.*registry")
  expect_error(validate_score_model_ids(c("alpha_v1", NA_character_), registry), "empty.*model_id")
})
test("Arrow query excludes non-EDI scores with category-resolved IDs", {
  path <- tempfile("edi-scores-", fileext = ".parquet")
  rows <- tibble(model_id = c("alpha_v1", "beta_v2", "other_v1"), score_value = c(20, 45, 99))
  arrow::write_parquet(rows, path)
  ids <- resolve_edi_model_ids(registry)
  selected <- arrow::open_dataset(path) %>% filter(model_id %in% ids) %>% collect()
  unlink(path)
  expect_equal(selected$score_value, c(20, 45))
})
