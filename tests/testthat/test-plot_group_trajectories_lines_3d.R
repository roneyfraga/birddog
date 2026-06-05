# Minimal fixture: two 2-node trajectories (tr1, tr2) in group c1g1.
make_lines3d_fixture <- function() {
  g <- igraph::make_graph(
    c("y2015c1g1", "y2016c1g1", "y2017c1g2", "y2018c1g2"), directed = TRUE
  )
  igraph::V(g)$quantity_papers <- c(40, 60, 100, 220)[
    match(igraph::V(g)$name, c("y2015c1g1", "y2016c1g1", "y2017c1g2", "y2018c1g2"))
  ]
  trs <- tibble::tibble(
    traj_id = c("tr1", "tr2"),
    nodes = list(c("y2015c1g1", "y2016c1g1"), c("y2017c1g2", "y2018c1g2"))
  )
  list(graph = g, trajectories = trs)
}

test_that("plot_group_trajectories_lines_3d returns a plotly object", {
  td <- make_lines3d_fixture()
  p <- plot_group_trajectories_lines_3d(
    traj_data = td, traj_filtered = td$trajectories, group_id = "c1g1"
  )
  expect_s3_class(p, "plotly")
})

test_that("manual descriptions appear in hover text when supplied", {
  td <- make_lines3d_fixture()
  desc <- data.frame(
    id = c("c1g1:tr1", "c1g1:tr2"),
    text = c("first traj desc", "second traj desc"),
    stringsAsFactors = FALSE
  )
  p <- plot_group_trajectories_lines_3d(
    traj_data = td, traj_filtered = td$trajectories,
    group_id = "c1g1", descriptions = desc
  )
  txts <- unlist(lapply(p$x$attrs, function(tr) tr$text))
  expect_true(any(grepl("first traj desc", txts)))
  expect_true(any(grepl("second traj desc", txts)))
})

test_that("descriptions accept the '::' key form too", {
  td <- make_lines3d_fixture()
  desc <- data.frame(
    id = "c1g1::tr1", text = "double colon works", stringsAsFactors = FALSE
  )
  p <- plot_group_trajectories_lines_3d(
    traj_data = td, traj_filtered = td$trajectories,
    group_id = "c1g1", descriptions = desc
  )
  txts <- unlist(lapply(p$x$attrs, function(tr) tr$text))
  expect_true(any(grepl("double colon works", txts)))
})

test_that("plot_group_trajectories_lines_3d errors on malformed descriptions", {
  td <- make_lines3d_fixture()
  expect_error(
    plot_group_trajectories_lines_3d(
      traj_data = td, traj_filtered = td$trajectories,
      group_id = "c1g1", descriptions = data.frame(id = "c1g1:tr1")
    ),
    "columns"
  )
})
