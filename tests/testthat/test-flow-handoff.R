test_that("flow destination carries handoff info and the plot renders", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  d <- sniff_trajectory_destination(fl, "tr1")
  expect_false(is.null(d$source_info))
  expect_equal(d$continuation_info$traj_id, "tr::c1g1")   # absorbed into the central
  p <- plot_trajectory_dispersion(d)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("a central trajectory has no handoff", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  d <- sniff_trajectory_destination(fl, "tr::c1g1")
  expect_null(d$continuation_info)
  expect_error(plot_trajectory_dispersion(d), "continuation")
})

test_that("plot_trajectory_dispersion fans out to every impacted final", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  abs_id <- fl$trajectories$traj_id[fl$trajectories$type == "absorbed"][1]
  d <- sniff_trajectory_destination(fl, abs_id)
  p <- plot_trajectory_dispersion(d)
  # one curved link per row of the destination table (all finals impacted)
  curve_layer <- p$layers[[which(vapply(p$layers,
    function(L) inherits(L$geom, "GeomCurve"), logical(1)))]]
  expect_equal(nrow(curve_layer$data), nrow(d$destination))
})
