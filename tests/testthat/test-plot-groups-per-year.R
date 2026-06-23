# the geom_point layer's built data (has x, y, size, no label)
point_data <- function(p) {
  b <- ggplot2::ggplot_build(p)
  cand <- Filter(function(d) all(c("x", "y", "size") %in% names(d)) && !"label" %in% names(d),
                 b$data)
  cand[[1]]
}

test_that("plot_groups_per_year returns a ggplot and builds cleanly", {
  d <- make_dag_fixture()
  p <- plot_groups_per_year(d)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("within a year the largest group sits at the baseline (min y)", {
  d <- make_dag_fixture()
  pd <- point_data(plot_groups_per_year(d))
  y2002 <- pd[pd$x == 2002, ]
  expect_equal(y2002$y[which.max(y2002$size)], min(y2002$y))
})

test_that("groups-per-year draws no edge layer", {
  d <- make_dag_fixture()
  p <- plot_groups_per_year(d)
  expect_false(any(vapply(p$layers,
                          function(l) inherits(l$geom, "GeomEdgePath"),
                          logical(1))))
})

test_that("show_count toggles the per-year count layer", {
  d <- make_dag_fixture()
  has_count <- function(p) any(vapply(p$layers,
                                      function(l) inherits(l$geom, "GeomLabel"),
                                      logical(1)))
  expect_true(has_count(plot_groups_per_year(d, show_count = TRUE)))
  expect_false(has_count(plot_groups_per_year(d, show_count = FALSE)))
})

test_that("count_size sets the count-label font size", {
  d <- make_dag_fixture()
  p <- plot_groups_per_year(d, count_size = 6)
  lab <- Filter(function(l) inherits(l$geom, "GeomLabel"), p$layers)[[1]]
  expect_equal(lab$aes_params$size, 6)
})

test_that("count_colour and count_fill style the count label", {
  d <- make_dag_fixture()
  lab <- Filter(function(l) inherits(l$geom, "GeomLabel"), plot_groups_per_year(d)$layers)[[1]]
  expect_equal(lab$aes_params$fill, "grey45")   # default highlight chip
  expect_equal(lab$aes_params$colour, "white")
  lab2 <- Filter(function(l) inherits(l$geom, "GeomLabel"),
                 plot_groups_per_year(d, count_fill = "red", count_colour = "black")$layers)[[1]]
  expect_equal(lab2$aes_params$fill, "red")
  expect_equal(lab2$aes_params$colour, "black")
})

n_terminal_label_layers <- function(p) {
  sum(vapply(p$layers, function(l) inherits(l$geom, c("GeomText", "GeomTextRepel")),
             logical(1)))
}

test_that("label_terminal toggles the final-node labels (default TRUE)", {
  d <- make_dag_fixture()
  expect_equal(n_terminal_label_layers(plot_groups_per_year(d)), 1L)
  expect_equal(n_terminal_label_layers(plot_groups_per_year(d, label_terminal = FALSE)), 0L)
})

test_that("label_size sets the final-node label font size", {
  d <- make_dag_fixture()
  lab <- Filter(function(l) inherits(l$geom, c("GeomText", "GeomTextRepel")),
                plot_groups_per_year(d, label_size = 5)$layers)[[1]]
  expect_equal(lab$aes_params$size, 5)
})

test_that("color_terminal toggles per-group colouring", {
  d <- make_dag_fixture()
  expect_equal(length(unique(point_data(plot_groups_per_year(d))$colour)), 1L)
  expect_gte(length(unique(point_data(plot_groups_per_year(d, color_terminal = TRUE))$colour)), 2L)
})

test_that("axis_text_size sets the x-axis year label size", {
  d <- make_dag_fixture()
  expect_equal(plot_groups_per_year(d, axis_text_size = 16)$theme$axis.text.x$size, 16)
  expect_null(plot_groups_per_year(d)$theme$axis.text.x$size)
})

test_that("plot_groups_per_year validates its input", {
  expect_error(plot_groups_per_year(list()), "sniff_trajectory_dag")
})
