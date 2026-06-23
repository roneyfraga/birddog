test_that("plot_trajectory_dispersion returns a ggplot", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  absorbed <- f$trajectories$traj_id[f$trajectories$type == "absorbed"][1]
  d <- sniff_trajectory_destination(f, absorbed)
  expect_s3_class(plot_trajectory_dispersion(d), "ggplot")
})

test_that("plot_trajectory_dispersion renders -> as an arrow in the title", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  absorbed <- f$trajectories$traj_id[f$trajectories$type == "absorbed"][1]
  d <- sniff_trajectory_destination(f, absorbed)
  p <- plot_trajectory_dispersion(d, title = "a -> b")
  expect_true(grepl(intToUtf8(8594), p$labels$title, fixed = TRUE))
})
