# A minimal groups_cumulative-like fixture: a small directed igraph whose
# vertices have a `name`, plus a `documents` frame mapping names to groups
# (one NA node, one single-node group g3), wrapped as one year element.
make_groups_cumulative_fixture <- function() {
  g <- igraph::graph_from_data_frame(
    d = data.frame(
      from = c("a", "b", "c", "d", "e", "f"),
      to   = c("b", "c", "a", "e", "f", "d")
    ),
    directed = TRUE,
    vertices = data.frame(name = c("a", "b", "c", "d", "e", "f", "u", "h"))
  )
  docs <- data.frame(
    name  = c("a", "b", "c", "d", "e", "f", "u", "h"),
    group = c("g1", "g1", "g1", "g2", "g2", "g2", NA, "g3"),
    stringsAsFactors = FALSE
  )
  list(network_until_2020 = list(network = g, documents = docs))
}

# count rows across point-like layers (x,y present, no label column)
n_points <- function(p) {
  b <- ggplot2::ggplot_build(p)
  sum(vapply(b$data, function(d) {
    if (all(c("x", "y") %in% names(d)) && !"label" %in% names(d)) nrow(d) else 0L
  }, integer(1)))
}

test_that("plot_groups_map renders a ggplot for the stock snapshot", {
  fx <- make_groups_cumulative_fixture()
  p <- plot_groups_map(fx)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("show_unassigned toggles the NA-group node", {
  fx <- make_groups_cumulative_fixture()
  with_na    <- plot_groups_map(fx, show_unassigned = TRUE)
  without_na <- plot_groups_map(fx, show_unassigned = FALSE)
  expect_equal(n_points(with_na) - n_points(without_na), 1L)
})

test_that("network_until selects the element and errors clearly when missing", {
  fx <- make_groups_cumulative_fixture()
  expect_s3_class(plot_groups_map(fx, network_until = 2020), "ggplot")
  expect_error(plot_groups_map(fx, network_until = 1999), "2020")
})

test_that("plot_groups_map validates its input", {
  expect_error(plot_groups_map(list()), "non-empty")
  fx <- make_groups_cumulative_fixture()
  expect_error(
    plot_groups_map(fx, labels_text = data.frame(id = "g1")),
    "labels_text"
  )
})

test_that("labels_text with no matching id warns and falls back to group ids", {
  fx <- make_groups_cumulative_fixture()
  bad <- data.frame(id = "zzz", text = "Nope", stringsAsFactors = FALSE)
  expect_warning(p <- plot_groups_map(fx, labels_text = bad), "match")
  expect_s3_class(p, "ggplot")
})

test_that("label_size sets the centroid label font size", {
  fx <- make_groups_cumulative_fixture()
  p <- plot_groups_map(fx, label_size = 7)
  tl <- Filter(function(l) inherits(l$geom, c("GeomText", "GeomTextRepel")), p$layers)
  expect_length(tl, 1)
  expect_equal(tl[[1]]$aes_params$size, 7)
})

# collect every label string across all layers of a built plot
label_texts <- function(p) {
  b <- ggplot2::ggplot_build(p)
  lab_layers <- Filter(function(d) "label" %in% names(d), b$data)
  unique(unlist(lapply(lab_layers, function(d) as.character(d$label))))
}

test_that("labels data.frame supplies the centroid text", {
  fx <- make_groups_cumulative_fixture()
  lab <- data.frame(id = "g1", text = "Alpha", stringsAsFactors = FALSE)
  txt <- label_texts(plot_groups_map(fx, labels_text = lab))
  expect_true("Alpha" %in% txt)   # mapped id
  expect_true("g2" %in% txt)      # unmapped id falls back to the bare group
})

test_that("without labels the centroid text is the group id", {
  fx <- make_groups_cumulative_fixture()
  txt <- label_texts(plot_groups_map(fx, labels_text = NULL))
  expect_true(all(c("g1", "g2", "g3") %in% txt))
})

test_that("label = FALSE draws no label layer", {
  fx <- make_groups_cumulative_fixture()
  expect_length(label_texts(plot_groups_map(fx, label = FALSE)), 0)
})

test_that("normalize is opt-in and centres/scales the layout to a unit window", {
  fx <- make_groups_cumulative_fixture()
  b_def  <- ggplot2::ggplot_build(plot_groups_map(fx))
  b_norm <- ggplot2::ggplot_build(plot_groups_map(fx, normalize = TRUE))
  # default keeps the free Cartesian coord; normalize switches to a fixed-ratio
  # coord. ggplot2 4.x dropped the "CoordFixed" class, so distinguish by `ratio`
  # (coord_fixed sets ratio = 1; the default Cartesian coord leaves it NULL).
  expect_null(b_def$layout$coord$ratio)
  expect_equal(b_norm$layout$coord$ratio, 1)
  expect_silent(ggplot2::ggplot_build(plot_groups_map(fx, normalize = TRUE)))
  # after normalisation, ~normalize_q of nodes sit inside the unit radius
  pts <- do.call(rbind, lapply(b_norm$data, function(d) {
    if (all(c("x", "y") %in% names(d)) && !"label" %in% names(d)) d[, c("x", "y")] else NULL
  }))
  rad <- sqrt(pts$x^2 + pts$y^2)
  expect_true(mean(rad <= 1.0001) >= 0.75)
})

test_that("title and title_size control the plot title", {
  fx <- make_groups_cumulative_fixture()
  bt <- function(p) ggplot2::ggplot_build(p)$plot
  expect_equal(bt(plot_groups_map(fx))$labels$title, "Group map")
  expect_equal(bt(plot_groups_map(fx, title = "Custom"))$labels$title, "Custom")
  expect_null(bt(plot_groups_map(fx, title = NULL))$labels$title)
  expect_equal(bt(plot_groups_map(fx, title_size = 22))$theme$plot.title$size, 22)
})
