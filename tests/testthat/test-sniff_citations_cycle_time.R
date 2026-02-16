test_that("sniff_citations_cycle_time rejects NULL input", {
  expect_error(sniff_citations_cycle_time(NULL), "Network data not found")
})

test_that("sniff_citations_cycle_time returns correct structure", {
  groups <- make_sniff_groups_output()
  tracked <- make_tracked_cr_py()
  result <- suppressMessages(
    sniff_citations_cycle_time(groups, scope = "groups", tracked_cr_py = tracked)
  )

  expect_type(result, "list")
  expect_named(result, c("data", "plots", "years_range", "tracked_cr_py"))
  expect_s3_class(result$data, "tbl_df")
  expect_true(all(c("group", "year", "index") %in% names(result$data)))
  expect_s3_class(result$tracked_cr_py, "tbl_df")
  expect_true(nrow(result$tracked_cr_py) > 0)
})

test_that("sniff_citations_cycle_time returns non-negative index values", {
  groups <- make_sniff_groups_output()
  tracked <- make_tracked_cr_py()
  result <- suppressMessages(
    sniff_citations_cycle_time(groups, scope = "groups", tracked_cr_py = tracked)
  )
  valid_idx <- result$data$index[!is.na(result$data$index)]
  if (length(valid_idx) > 0) {
    expect_true(all(valid_idx >= 0))
  }
})

test_that("sniff_citations_cycle_time respects min_papers_per_year filtering", {
  groups <- make_sniff_groups_output()
  tracked <- make_tracked_cr_py()
  result_high <- suppressMessages(
    sniff_citations_cycle_time(groups, tracked_cr_py = tracked, min_papers_per_year = 100)
  )
  expect_true(all(is.na(result_high$data$index)))
})

test_that("sniff_citations_cycle_time rolling window preserves row count", {
  groups <- make_sniff_groups_output()
  tracked <- make_tracked_cr_py()
  result_plain <- suppressMessages(
    sniff_citations_cycle_time(groups, tracked_cr_py = tracked, min_papers_per_year = 1)
  )
  result_smooth <- suppressMessages(
    sniff_citations_cycle_time(groups, tracked_cr_py = tracked,
                               rolling_window = 3, min_papers_per_year = 1)
  )
  expect_equal(nrow(result_plain$data), nrow(result_smooth$data))
})

test_that("sniff_citations_cycle_time works with network scope", {
  net <- make_test_tbl_graph()
  tracked <- make_tracked_cr_py()
  result <- suppressMessages(
    sniff_citations_cycle_time(net, scope = "network", tracked_cr_py = tracked)
  )
  expect_s3_class(result$data, "tbl_df")
  expect_true(all(result$data$group == "full_network"))
})
