# multi-year fixture: nodes enter in different years; the last year has an
# unassigned node (u). Each network is a path graph so create_layout works.
make_temporal_fixture <- function() {
  mk <- function(v) igraph::graph_from_data_frame(
    d = data.frame(from = v[-length(v)], to = v[-1]),
    directed = TRUE, vertices = data.frame(name = v))
  docs <- function(nm, grp) data.frame(name = nm, group = grp, stringsAsFactors = FALSE)
  list(
    network_until_2018 = list(network = mk(c("a", "b")),
                              documents = docs(c("a", "b"), c("g1", "g1"))),
    network_until_2019 = list(network = mk(c("a", "b", "c", "d")),
                              documents = docs(c("a", "b", "c", "d"),
                                               c("g1", "g1", "g2", "g2"))),
    network_until_2020 = list(network = mk(c("a", "b", "c", "d", "e", "f", "u")),
                              documents = docs(c("a", "b", "c", "d", "e", "f", "u"),
                                               c("g1", "g1", "g2", "g2", "g3", "g3", NA)))
  )
}

test_that(".groups_map_appearance returns the first year each node appears", {
  fx <- make_temporal_fixture()
  ap <- .groups_map_appearance(fx)
  expect_equal(unname(ap[c("a", "b")]), c(2018L, 2018L))
  expect_equal(unname(ap[c("c", "d")]), c(2019L, 2019L))
  expect_equal(unname(ap[c("e", "f", "u")]), c(2020L, 2020L, 2020L))
})

geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))

test_that("plot_groups_map_animation returns a gganim", {
  skip_if_not_installed("gganimate")
  fx <- make_temporal_fixture()
  p <- plot_groups_map_animation(fx)
  expect_s3_class(p, "gganim")
})

test_that("label toggles the centroid text layer", {
  skip_if_not_installed("gganimate")
  fx <- make_temporal_fixture()
  has_label <- function(p) any(geoms(p) %in% c("GeomText", "GeomTextRepel"))
  expect_true(has_label(plot_groups_map_animation(fx, label = TRUE)))
  expect_false(has_label(plot_groups_map_animation(fx, label = FALSE)))
})

test_that("show_unassigned toggles the grey point layer", {
  skip_if_not_installed("gganimate")
  fx <- make_temporal_fixture()
  n_point <- function(p) sum(geoms(p) == "GeomPoint")
  expect_equal(
    n_point(plot_groups_map_animation(fx, show_unassigned = TRUE)) -
      n_point(plot_groups_map_animation(fx, show_unassigned = FALSE)),
    1L
  )
})

test_that("animation inherits validation", {
  skip_if_not_installed("gganimate")
  expect_error(plot_groups_map_animation(list()), "non-empty")
  fx <- make_temporal_fixture()
  expect_error(plot_groups_map_animation(fx, labels_text = data.frame(id = "g1")), "labels_text")
})

test_that("animation keeps the Year title and title_size sets its font", {
  skip_if_not_installed("gganimate")
  fx <- make_temporal_fixture()
  p <- plot_groups_map_animation(fx, title_size = 22)
  expect_match(p$labels$title, "Year:")
  expect_equal(p$theme$plot.title$size, 22)
})
