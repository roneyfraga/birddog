make_dyn_i <- function() {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  sniff_trajectory_dynamics(fl)
}

test_that("plot_trajectory_dynamics_interactive returns a built-able plotly", {
  p <- plot_trajectory_dynamics_interactive(make_dyn_i(), target = "finals")
  expect_s3_class(p, "plotly")
  b <- plotly::plotly_build(p)
  expect_gt(length(b$x$data), 0)
  expect_match(b$x$layout$title$text, "research fronts")
})

test_that("plot_trajectory_dynamics_interactive renders the intermediary view", {
  p <- plot_trajectory_dynamics_interactive(make_dyn_i(), target = "intermediary")
  expect_s3_class(p, "plotly")
  b <- plotly::plotly_build(p)
  expect_match(b$x$layout$title$text, "declining")
})

test_that("plot_trajectory_dynamics_interactive hover carries the indicators", {
  p <- plot_trajectory_dynamics_interactive(make_dyn_i(), target = "finals")
  b <- plotly::plotly_build(p)
  hov <- unlist(lapply(b$x$data, function(tr) tr$hovertext))
  expect_true(any(grepl("growth rate", hov, fixed = TRUE)))
  expect_true(any(grepl("novelty", hov, fixed = TRUE)))
})

test_that("plot_trajectory_dynamics_interactive keeps every region in view", {
  dyn <- make_dyn_i()
  th <- attr(dyn, "state_thresholds")
  b <- plotly::plotly_build(plot_trajectory_dynamics_interactive(dyn, target = "finals"))
  xr <- b$x$layout$xaxis$range; yr <- b$x$layout$yaxis$range
  expect_lte(yr[1], th$decline_growth);    expect_gte(yr[2], th$emergence_growth)
  expect_lte(xr[1], th$emergence_novelty); expect_gte(xr[2], th$emergence_novelty)
})

test_that("plot_trajectory_dynamics_interactive validates its input", {
  expect_error(plot_trajectory_dynamics_interactive(list()), "sniff_trajectory_dynamics")
})
