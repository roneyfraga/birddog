test_that("sniff_key_route rejects NULL input", {
  expect_error(sniff_key_route(NULL), "Network data not found")
})

test_that("sniff_key_route rejects undirected networks", {
  dag <- make_simple_dag()
  undir <- igraph::as.undirected(dag) |> tidygraph::as_tbl_graph()
  expect_error(sniff_key_route(undir, scope = "network"), "must be a directed network")
})

test_that("sniff_key_route returns correct structure from linear DAG", {
  skip_if_not_installed("ggrepel")

  dag <- make_simple_dag()
  result <- suppressMessages(sniff_key_route(dag, scope = "network"))

  first_entry <- result[[1]]
  expect_s3_class(first_entry$plot, "ggplot")
  expect_s3_class(first_entry$data, "tbl_df")
  expect_true(all(c("name", "name2", "TI") %in% names(first_entry$data)))
})

test_that("sniff_key_route finds correct path in linear DAG", {
  skip_if_not_installed("ggrepel")

  dag <- make_simple_dag()
  result <- suppressMessages(sniff_key_route(dag, scope = "network"))

  path_nodes <- result[[1]]$data$name
  expect_true(length(path_nodes) >= 3)
  expect_true(all(path_nodes %in% c("n1", "n2", "n3", "n4", "n5")))
})

test_that("sniff_key_route works with groups scope", {
  skip_if_not_installed("ggrepel")

  groups <- make_sniff_groups_output()
  result <- suppressMessages(sniff_key_route(groups, scope = "groups"))

  expect_type(result, "list")
})
