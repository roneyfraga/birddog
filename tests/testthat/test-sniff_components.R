test_that("sniff_components rejects non-network input", {
  expect_error(sniff_components("not_a_graph"), "Input must be a network object")
})

test_that("sniff_components returns correct structure", {
  df <- make_minimal_openalex_dataframe()
  net <- sniff_network(df, type = "direct citation")
  result <- sniff_components(net)

  expect_named(result, c("components", "network"))
  expect_s3_class(result$components, "tbl_df")
  expect_true(all(c("component", "quantity_publications", "average_age") %in%
    names(result$components)))
  expect_s3_class(result$network, "tbl_graph")
})

test_that("sniff_components labels components correctly", {
  df <- make_minimal_openalex_dataframe()
  net <- sniff_network(df, type = "direct citation")
  result <- sniff_components(net)

  comp_labels <- result$components$component
  expect_true(all(grepl("^c\\d+$", comp_labels)))
  expect_equal(comp_labels[1], "c1")
})
