test_that("sniff_entropy rejects NULL input", {
  expect_error(sniff_entropy(NULL), "Network data not found")
})

test_that("sniff_entropy returns correct structure for groups scope", {
  groups <- make_sniff_groups_output()
  result <- suppressMessages(sniff_entropy(groups, scope = "groups"))

  expect_type(result, "list")
  expect_named(result, c("data", "plots", "years_range"))
  expect_s3_class(result$data, "tbl_df")
  expect_true(all(c("group", "year", "index") %in% names(result$data)))
  expect_true("c1g1" %in% result$data$group)
  expect_true("c1g2" %in% result$data$group)
})

test_that("sniff_entropy values are in [0, 1] or NA", {
  groups <- make_sniff_groups_output()
  result <- suppressMessages(sniff_entropy(groups, scope = "groups"))
  valid_idx <- result$data$index[!is.na(result$data$index)]
  expect_true(all(valid_idx >= 0 & valid_idx <= 1))
})

test_that("sniff_entropy respects custom year range", {
  groups <- make_sniff_groups_output()
  result <- suppressMessages(
    sniff_entropy(groups, scope = "groups", start_year = 2016, end_year = 2019)
  )
  expect_equal(result$years_range, c(start_year = 2016, end_year = 2019))
  expect_true(all(result$data$year >= 2016 & result$data$year <= 2019))
})

test_that("sniff_entropy works with network scope", {
  net <- make_test_tbl_graph()
  result <- suppressMessages(sniff_entropy(net, scope = "network"))

  expect_s3_class(result$data, "tbl_df")
  expect_true(all(result$data$group == "full_network"))
})
