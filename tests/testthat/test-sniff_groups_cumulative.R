test_that("sniff_groups_cumulative returns named list by year", {
  comps <- make_sniff_components_output()
  result <- sniff_groups_cumulative(comps, time_span = 2017:2019, min_group_size = 2)

  expect_type(result, "list")
  expect_true(all(grepl("^network_until_", names(result))))
  expect_length(result, 3)
})

test_that("sniff_groups_cumulative per-year structure is correct", {
  comps <- make_sniff_components_output()
  result <- sniff_groups_cumulative(comps, time_span = 2018:2019, min_group_size = 2)

  entry <- result[[1]]
  expect_true(all(c("groups", "documents", "network") %in% names(entry)))
  expect_s3_class(entry$groups, "tbl_df")
  expect_s3_class(entry$documents, "tbl_df")
})
