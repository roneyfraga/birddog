test_that("sniff_groups_hubs returns correct structure and zone values", {
  groups <- make_sniff_groups_output()
  result <- sniff_groups_hubs(groups, min_citations = 1)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("group", "name", "TC", "Ki", "ki", "Zi", "Pi", "zone") %in%
    names(result)))
  expect_true(all(result$zone %in% c("noHub", "R5", "R6", "R7")))
})

test_that("sniff_groups_hubs Pi is in [0, 1]", {
  groups <- make_sniff_groups_output()
  result <- sniff_groups_hubs(groups, min_citations = 1)

  valid_pi <- result$Pi[!is.na(result$Pi)]
  if (length(valid_pi) > 0) {
    expect_true(all(valid_pi >= 0 & valid_pi <= 1))
  }
})
