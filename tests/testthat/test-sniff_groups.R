test_that("sniff_groups rejects non-list input", {
  expect_error(sniff_groups("not_a_list"), "Input must be a list")
})

test_that("sniff_groups returns correct structure", {
  comps <- make_sniff_components_output()
  result <- sniff_groups(comps, min_group_size = 2)

  expect_named(result, c("aggregate", "network", "pubs_by_year"))
  expect_s3_class(result$aggregate, "tbl_df")
  expect_true(all(c("group", "quantity_papers", "average_age") %in% names(result$aggregate)))
  expect_s3_class(result$network, "tbl_graph")
  expect_s3_class(result$pubs_by_year, "tbl_df")
  expect_true(all(c("group", "year", "publications") %in% names(result$pubs_by_year)))
  expect_true(all(result$pubs_by_year$publications > 0))
})

test_that("sniff_groups respects min_group_size filtering", {
  comps <- make_sniff_components_output()
  result_low <- sniff_groups(comps, min_group_size = 2)
  result_high <- sniff_groups(comps, min_group_size = 100)

  expect_true(nrow(result_low$aggregate) >= nrow(result_high$aggregate))
})

test_that("sniff_groups works with multiple algorithms", {
  comps <- make_sniff_components_output()
  for (algo in c("fast_greedy", "louvain", "walktrap")) {
    result <- sniff_groups(comps, min_group_size = 2, algorithm = algo, seed = 42L)
    expect_true(nrow(result$aggregate) > 0)
  }
})
