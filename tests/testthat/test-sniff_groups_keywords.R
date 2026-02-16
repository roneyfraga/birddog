test_that("sniff_groups_keywords returns correct output columns", {
  skip_if_not_installed("tidytext")

  groups <- make_sniff_groups_output()
  result <- sniff_groups_keywords(groups)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("group", "term_freq", "term_tfidf") %in% names(result)))
})
