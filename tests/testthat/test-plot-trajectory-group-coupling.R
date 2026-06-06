test_that("plot_trajectory_group_coupling returns a ggplot heatmap", {
  cg <- sniff_trajectory_group_contribution(make_contribution_fixture())
  p <- plot_trajectory_group_coupling(cg)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_trajectory_group_coupling honors the fill measure", {
  cg <- sniff_trajectory_group_contribution(make_contribution_fixture())
  for (f in c("n_shared", "prop_of_group", "prop_of_traj")) {
    p <- plot_trajectory_group_coupling(cg, fill = f)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("plot_trajectory_group_coupling validates its input", {
  expect_error(plot_trajectory_group_coupling(list()), "sniff_trajectory_group_contribution")
})
