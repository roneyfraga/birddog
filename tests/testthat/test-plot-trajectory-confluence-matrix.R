test_that("flow matrix renders finals x intermediates", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  p <- plot_trajectory_confluence_matrix(fl, min_n = 0)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("flow matrix honours the fill measure and in-cell labels", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  for (f in c("n_shared", "prop_of_group", "prop_of_traj")) {
    p <- plot_trajectory_confluence_matrix(fl, fill = f, min_n = 0, show_values = TRUE, label_size = 2)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("flow matrix swaps orientation and accepts a contrast background", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  p <- plot_trajectory_confluence_matrix(fl, min_n = 0, orientation = "finals-cols",
                              bg = "grey20", legend_position = "bottom")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_equal(p$theme$legend.position, "bottom")
})

test_that("flow matrix columns are intermediate trajectories only", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  p <- plot_trajectory_confluence_matrix(fl, min_n = 0)
  cols <- as.character(unique(p$data$traj_id))
  centrals <- fl$trajectories$traj_id[fl$trajectories$type == "central"]
  expect_false(any(cols %in% centrals))   # no central appears as a column
})

test_that("flow matrix keeps every final trajectory on its axis", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  finals <- colnames(.coupling_incidence_flow(fl)$incidence)
  p <- plot_trajectory_confluence_matrix(fl, min_n = 0, min_target_n = 0)
  b <- ggplot2::ggplot_build(p)
  yl <- b$layout$panel_params[[1]]$y$get_labels()   # finals on y (default finals-cols)
  expect_setequal(sub("^tr::", "", yl), finals)      # all finals present, even empty ones
})

test_that("flow matrix order_by accepts every metric", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  for (o in c("terminal", "reach", "size", "alpha", "cluster")) {
    p <- plot_trajectory_confluence_matrix(fl, min_n = 0, order_by = o)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("flow matrix validates its input", {
  expect_error(plot_trajectory_confluence_matrix(list()), "sniff_trajectory_braid")
})
