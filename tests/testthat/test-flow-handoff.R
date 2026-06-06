test_that("flow destination carries handoff info and the plot renders", {
  fl <- sniff_trajectory_flow(make_flow_simple_dpg(), min_group_size = 1)
  d <- sniff_trajectory_destination(fl, "tr1")
  expect_false(is.null(d$source_info))
  expect_equal(d$continuation_info$traj_id, "tr::c1g1")   # absorbed into the central
  p <- plot_trajectory_handoff(d)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("a central trajectory has no handoff", {
  fl <- sniff_trajectory_flow(make_flow_simple_dpg(), min_group_size = 1)
  d <- sniff_trajectory_destination(fl, "tr::c1g1")
  expect_null(d$continuation_info)
  expect_error(plot_trajectory_handoff(d), "continuation")
})
