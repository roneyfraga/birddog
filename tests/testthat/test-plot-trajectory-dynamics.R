make_dyn <- function() {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  sniff_trajectory_dynamics(fl)
}

test_that("plot_trajectory_dynamics renders the finals map of living cores", {
  p <- plot_trajectory_dynamics(make_dyn(), target = "finals")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_true(all(p$data$type == "central"))   # finals view = living cores only
})

test_that("plot_trajectory_dynamics renders the intermediary map of absorbed lineages", {
  p <- plot_trajectory_dynamics(make_dyn(), target = "intermediary")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_true(all(p$data$type == "absorbed"))   # intermediary view = declining lineages only
})

test_that("plot_trajectory_dynamics labels living cores from labels_text", {
  descr <- data.frame(id = "c1g3", text = "Methanogenesis", stringsAsFactors = FALSE)
  p <- plot_trajectory_dynamics(make_dyn(), target = "finals", labels_text = descr)
  expect_true("Methanogenesis" %in% p$data$.lab)         # description only by default
})

test_that("plot_trajectory_dynamics can prefix the group id with label_id", {
  descr <- data.frame(id = "c1g3", text = "Methanogenesis", stringsAsFactors = FALSE)
  p <- plot_trajectory_dynamics(make_dyn(), target = "finals", labels_text = descr,
                                label_id = TRUE)
  expect_true("c1g3: Methanogenesis" %in% p$data$.lab)
})

test_that("plot_trajectory_dynamics honours a custom palette and legend position", {
  pal <- c(emergence = "red", maturity = "blue", dormancy = "black")
  p <- plot_trajectory_dynamics(make_dyn(), target = "finals", palette = pal,
                                legend_position = "bottom")
  expect_equal(p$theme$legend.position, "bottom")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_trajectory_dynamics keeps every finals region in view", {
  dyn <- make_dyn()
  th <- attr(dyn, "state_thresholds")
  p <- plot_trajectory_dynamics(dyn, target = "finals")
  yl <- p$coordinates$limits$y; xl <- p$coordinates$limits$x
  expect_false(is.null(yl)); expect_false(is.null(xl))
  expect_lte(yl[1], th$decline_growth)     # the dormancy band stays visible
  expect_gte(yl[2], th$emergence_growth)   # the emergence band stays visible
  expect_lte(xl[1], th$emergence_novelty)  # the novelty cut stays inside the view
  expect_gte(xl[2], th$emergence_novelty)
})

test_that("plot_trajectory_dynamics keeps every intermediary region in view", {
  dyn <- make_dyn()
  th <- attr(dyn, "state_thresholds")
  p <- plot_trajectory_dynamics(dyn, target = "intermediary")
  yl <- p$coordinates$limits$y; xl <- p$coordinates$limits$x
  expect_lte(xl[1], th$convergence_entropy); expect_gte(xl[2], th$convergence_entropy)
  expect_lte(yl[1], th$dormancy_share);      expect_gte(yl[2], th$dormancy_share)
})

test_that("plot_trajectory_dynamics validates its input", {
  expect_error(plot_trajectory_dynamics(list()), "sniff_trajectory_dynamics")
  expect_error(plot_trajectory_dynamics(tibble::tibble(a = 1)), "sniff_trajectory_dynamics")
})
