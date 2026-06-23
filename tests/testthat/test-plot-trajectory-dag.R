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

n_label_layers <- function(p) {
  sum(vapply(p$layers,
             function(l) inherits(l$geom, c("GeomText", "GeomTextRepel")),
             logical(1)))
}

label_layer_texts <- function(p) {
  b <- ggplot2::ggplot_build(p)
  lab <- Filter(function(d) "label" %in% names(d), b$data)
  unique(unlist(lapply(lab, function(d) as.character(d$label))))
}

test_that("label_terminal toggles the final-group label layer (default TRUE)", {
  d <- make_dag_fixture()
  expect_equal(n_label_layers(plot_trajectory_dag(d)), 1L)                          # default
  expect_equal(n_label_layers(plot_trajectory_dag(d, label_terminal = FALSE)), 0L)
})

colour_legend_labels <- function(p) {
  b <- ggplot2::ggplot_build(p)
  sc <- b$plot$scales$get_scales("colour")
  if (is.null(sc)) character(0) else as.character(sc$get_labels())
}

test_that("final-node labels are always the bare group id", {
  d <- make_dag_fixture()
  lab <- data.frame(id = "c1g1", text = "Alpha", stringsAsFactors = FALSE)
  txt <- label_layer_texts(plot_trajectory_dag(d, label_terminal = TRUE, labels_text = lab))
  expect_true(all(c("c1g1", "c1g2") %in% txt))   # ids on the vertices
  expect_false("Alpha" %in% txt)                 # description is NOT on the vertex
})

test_that("labels_text drives a bottom colour legend with 'id: text' keys", {
  d <- make_dag_fixture()
  lab <- data.frame(id = "c1g1", text = "Alpha", stringsAsFactors = FALSE)
  p <- plot_trajectory_dag(d, label_terminal = TRUE, labels_text = lab)
  labs <- colour_legend_labels(p)
  expect_true("c1g1: Alpha" %in% labs)   # mapped final group
  expect_true("c1g2" %in% labs)          # unmapped final falls back to the bare id
  expect_equal(p$theme$legend.position, "bottom")
})

test_that("without labels_text the legend keys stay bare ids", {
  d <- make_dag_fixture()
  labs <- colour_legend_labels(plot_trajectory_dag(d, label_terminal = TRUE, show_legend = TRUE))
  expect_false(any(grepl(": ", labs)))   # no 'id: text' relabelling
})

test_that("labels_text with no matching id warns", {
  d <- make_dag_fixture()
  bad <- data.frame(id = "zzz", text = "Nope", stringsAsFactors = FALSE)
  expect_warning(plot_trajectory_dag(d, label_terminal = TRUE, labels_text = bad), "match")
})

test_that("dormant lineages are kept out of the colour legend", {
  # c1g3 lives only in 2000 with disjoint docs, so it dead-ends (dormant); the
  # final 2002 groups are c1g1, c1g2.
  dpg <- tibble::tibble(
    group_id = c(
      rep("y2000c1g1", 3), rep("y2001c1g1", 4), rep("y2002c1g1", 5),
      rep("y2000c1g2", 3), rep("y2001c1g2", 4), rep("y2002c1g2", 5),
      rep("y2000c1g3", 3)
    ),
    document_id = c(
      paste0("w", 1:3), paste0("w", 1:4), paste0("w", 1:5),
      paste0("x", 1:3), paste0("x", 1:4), paste0("x", 1:5),
      paste0("z", 1:3)
    ),
    network_until = c(
      rep(2000L, 3), rep(2001L, 4), rep(2002L, 5),
      rep(2000L, 3), rep(2001L, 4), rep(2002L, 5),
      rep(2000L, 3)
    ),
    group = c(rep("c1g1", 12), rep("c1g2", 12), rep("c1g3", 3))
  )
  d <- sniff_trajectory_dag(dpg, min_group_size = 2)
  expect_true("c1g3" %in% d$nodes$terminal_group)   # it is a dormant sink
  lab <- data.frame(id = c("c1g1", "c1g2"), text = c("A", "B"), stringsAsFactors = FALSE)
  labs <- colour_legend_labels(plot_trajectory_dag(d, labels_text = lab))
  expect_false(any(grepl("c1g3", labs)))   # dormant lineage omitted from the legend
  expect_true("c1g1: A" %in% labs)
})

test_that("legend_text_size and legend_ncol style the bottom legend", {
  d <- make_dag_fixture()
  lab <- data.frame(id = c("c1g1", "c1g2"), text = c("Alpha", "Beta"), stringsAsFactors = FALSE)
  p <- plot_trajectory_dag(d, labels_text = lab, legend_text_size = 14, legend_ncol = 1)
  expect_equal(p$theme$legend.text$size, 14)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("axis_text_size sets the x-axis year label size", {
  d <- make_dag_fixture()
  expect_equal(plot_trajectory_dag(d, axis_text_size = 16)$theme$axis.text.x$size, 16)
  expect_null(plot_trajectory_dag(d)$theme$axis.text.x$size)
})

test_that("title controls the plot title and defaults to the cumulative-groups label", {
  d <- make_dag_fixture()
  bt <- function(p) ggplot2::ggplot_build(p)$plot$labels$title
  expect_equal(bt(plot_trajectory_dag(d)), "Cumulative network groups")
  expect_equal(bt(plot_trajectory_dag(d, title = "Custom")), "Custom")
  expect_null(bt(plot_trajectory_dag(d, title = NULL)))
})
