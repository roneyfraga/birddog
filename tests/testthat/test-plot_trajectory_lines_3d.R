# 3D sibling of plot_trajectory_lines_2d: same flow/confluence selection, plotly
# rendering with x = year, y = route, z = cumulative documents. Uses the shared
# make_flow_tree_dpg() fixture (helper-trajectory-flow.R).

test_that("plot_trajectory_lines_3d returns a plotly object over the global flow", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  p <- plot_trajectory_lines_3d(flow, min_n = 0)
  expect_s3_class(p, "plotly")
})

test_that("plot_trajectory_lines_3d targets a central and titles it as its feeders", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  cen <- paste0("tr::", flow$trajectories$group[flow$trajectories$type == "central"][1])
  p <- plot_trajectory_lines_3d(flow, target = cen, min_n = 0)
  expect_s3_class(p, "plotly")
})

test_that("plot_trajectory_lines_3d still draws the centrals when all feeders prune", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  p <- plot_trajectory_lines_3d(flow, min_n = 1e6)  # roots (centrals) survive
  expect_s3_class(p, "plotly")
})

test_that("plot_trajectory_lines_3d accepts log_scale", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  p <- plot_trajectory_lines_3d(flow, min_n = 0, log_scale = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("plot_trajectory_lines_3d puts the legend text and labels_text in node hovers", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  grps <- unique(flow$trajectories$group); grps <- grps[!is.na(grps)]
  lt <- data.frame(id = grps, text = paste0("DESC-", grps))
  p <- plotly::plotly_build(plot_trajectory_lines_3d(flow, min_n = 0, labels_text = lt))
  hovers <- unlist(lapply(p$x$data, function(tr) c(tr$text, tr$hovertext)))
  expect_true(any(grepl("DESC-", hovers)))        # labels_text description reaches a hover
  expect_true(any(grepl("Documents:", hovers)))   # absolute per-year documents shown
  expect_false(any(grepl("Cumulative", hovers)))  # running cumulative not in the hover
})

test_that("plot_trajectory_lines_3d draws inter-trajectory links only with show_background", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  on  <- plotly::plotly_build(plot_trajectory_lines_3d(flow, min_n = 0, show_background = TRUE))
  off <- plotly::plotly_build(plot_trajectory_lines_3d(flow, min_n = 0, show_background = FALSE))
  expect_gt(length(on$x$data), length(off$x$data))
})

test_that("plot_trajectory_lines_3d positions / hides the legend", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  pb <- plotly::plotly_build(plot_trajectory_lines_3d(flow, min_n = 0, legend_position = "bottom"))
  expect_identical(pb$x$layout$legend$orientation, "h")
  pn <- plotly::plotly_build(plot_trajectory_lines_3d(flow, min_n = 0, legend_position = "none"))
  expect_false(isTRUE(pn$x$layout$showlegend))
})

test_that("plot_trajectory_lines_3d legend_ncol sets the entry width", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  pb <- plotly::plotly_build(
    plot_trajectory_lines_3d(flow, min_n = 0, legend_position = "bottom", legend_ncol = 2))
  expect_equal(pb$x$layout$legend$entrywidth, 0.5)
  expect_identical(pb$x$layout$legend$entrywidthmode, "fraction")
})

test_that("plot_trajectory_lines_3d errors on malformed labels_text", {
  flow <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  expect_error(
    plot_trajectory_lines_3d(flow, min_n = 0, labels_text = data.frame(id = "x")),
    "labels_text"
  )
})
