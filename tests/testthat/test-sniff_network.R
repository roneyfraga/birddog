test_that("sniff_network rejects non-data.frame input", {
  expect_error(sniff_network("not_a_df"), "Input must be a data frame")
})

test_that("sniff_network returns directed tbl_graph for direct citation", {
  df <- make_minimal_openalex_dataframe()
  net <- sniff_network(df, type = "direct citation")

  expect_s3_class(net, "tbl_graph")
  expect_true(igraph::is_directed(net))
  expect_true(all(igraph::V(net)$NT == "direct-citation"))
})

test_that("sniff_network creates bibliographic coupling network", {
  df <- make_minimal_openalex_dataframe()
  net <- sniff_network(df, type = "bibliographic coupling")

  expect_s3_class(net, "tbl_graph")
  expect_false(igraph::is_directed(net))
  expect_true(all(igraph::V(net)$NT == "bibliographic-coupling"))
})

test_that("sniff_network with external_references includes extra nodes", {
  df <- make_minimal_openalex_dataframe()
  net_internal <- sniff_network(df, type = "direct citation", external_references = FALSE)
  net_external <- sniff_network(df, type = "direct citation", external_references = TRUE)

  expect_true(igraph::vcount(net_external) >= igraph::vcount(net_internal))
})
