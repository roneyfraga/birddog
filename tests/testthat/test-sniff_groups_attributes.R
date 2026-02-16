test_that("sniff_groups_attributes returns correct structure", {
  skip_if_not_installed("gt")

  groups <- make_sniff_groups_output()
  result <- suppressWarnings(
    sniff_groups_attributes(groups,
      growth_rate_period = 2015:2020,
      horizon_plot = FALSE,
      show_results = FALSE
    )
  )

  expect_type(result, "list")
  expect_true("attributes_table" %in% names(result))
  expect_true("regression" %in% names(result))
  expect_s3_class(result$attributes_table, "gt_tbl")
})
