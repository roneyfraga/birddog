test_that("plot_trajectory_group_bipartite returns a ggplot", {
  cg <- sniff_trajectory_group_contribution(make_contribution_fixture())
  p <- plot_trajectory_group_bipartite(cg)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_trajectory_group_bipartite respects max_edges", {
  cg <- sniff_trajectory_group_contribution(make_contribution_fixture())
  p <- plot_trajectory_group_bipartite(cg, max_edges = 2)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_trajectory_group_bipartite validates its input", {
  expect_error(plot_trajectory_group_bipartite(list()), "sniff_trajectory_group_contribution")
})
