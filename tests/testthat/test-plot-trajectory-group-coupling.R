test_that("flow coupling renders the trajectory x group heatmap", {
  fl <- sniff_trajectory_flow(make_flow_simple_dpg(), min_group_size = 1)
  p <- plot_trajectory_group_coupling(fl)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("flow coupling honours the fill measure", {
  fl <- sniff_trajectory_flow(make_flow_simple_dpg(), min_group_size = 1)
  for (f in c("n_shared", "prop_of_group", "prop_of_traj")) {
    p <- plot_trajectory_group_coupling(fl, fill = f)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("flow coupling validates its input", {
  expect_error(plot_trajectory_group_coupling(list()), "sniff_trajectory_flow")
})
