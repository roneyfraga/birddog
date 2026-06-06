test_that("plot_group_formation_2d renders a group's confluence", {
  r <- detect_soft_trajectories(make_group_formation_dpg(), min_len = 3, min_group_size = 1)
  gf <- sniff_group_formation("c1g1", r, min_papers = 1, min_prop = 0)
  p <- plot_group_formation_2d(gf)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})
