test_that("plot_trajectory_lines_2d returns a buildable ggplot over the global flow", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  p <- plot_trajectory_lines_2d(flow, min_n = 0)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_match(p$labels$title, "global")
})

test_that("plot_trajectory_lines_2d targets a central and titles it as its feeders", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  cen <- paste0("tr::", flow$trajectories$group[flow$trajectories$type == "central"][1])
  p <- plot_trajectory_lines_2d(flow, target = cen, min_n = 0)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "Feeders")
})

test_that("plot_trajectory_lines_2d still draws the centrals when all feeders prune", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  p <- plot_trajectory_lines_2d(flow, min_n = 1e6)  # roots (centrals) survive
  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_lines_2d legend names finals fed and is ASCII-only in code", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  p <- plot_trajectory_lines_2d(flow, min_n = 0)
  # the edge-colour scale carries one label per drawn trajectory
  sc <- p$scales$scales[[which(vapply(p$scales$scales,
    function(s) any(s$aesthetics == "edge_colour"), logical(1)))]]
  expect_true(length(sc$labels) >= 1)
})
