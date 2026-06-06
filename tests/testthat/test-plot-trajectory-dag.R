test_that("plot_trajectory_dag returns a ggplot for a small DAG", {
  dpg <- tibble::tibble(
    group_id = c(rep("y2000c1g1", 3), rep("y2001c1g1", 3), rep("y2002c1g1", 3)),
    document_id = rep(paste0("w", 1:3), 3),
    network_until = c(rep(2000L, 3), rep(2001L, 3), rep(2002L, 3)),
    group = rep("c1g1", 9)
  )
  d <- sniff_trajectory_dag(dpg, min_group_size = 1)
  p <- plot_trajectory_dag(d)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_trajectory_dag validates its input", {
  expect_error(plot_trajectory_dag(list()), "sniff_trajectory_dag")
})
