# 8-node fixture with the tooltip fields (TI/PY/AU/DE) as vertex attributes.
make_interactive_fixture <- function() {
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = c("a", "b", "c", "d", "e", "f"),
                   to   = c("b", "c", "a", "e", "f", "d")),
    directed = TRUE,
    vertices = data.frame(
      name = c("a", "b", "c", "d", "e", "f", "u", "h"),
      TI   = paste("Title", c("a", "b", "c", "d", "e", "f", "u", "h")),
      PY   = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008),
      AU   = "Smith;Jones;Lee",
      DE   = "alpha;beta;gamma",
      stringsAsFactors = FALSE
    )
  )
  docs <- data.frame(
    name  = c("a", "b", "c", "d", "e", "f", "u", "h"),
    group = c("g1", "g1", "g1", "g2", "g2", "g2", NA, "g3"),
    stringsAsFactors = FALSE
  )
  list(network_until_2020 = list(network = g, documents = docs))
}

# every tooltip string across the hover ("text") traces of a built plot.
# NB: plotly_build recycles `hoverinfo` to a per-point vector on multi-point
# traces, so test for "all entries are 'text'", not scalar identity.
hover_texts <- function(p) {
  b <- plotly::plotly_build(p)
  is_hover <- function(t) length(t$hoverinfo) > 0 && all(t$hoverinfo == "text")
  unlist(lapply(b$x$data, function(t) if (is_hover(t)) t$text else NULL))
}

test_that("interactive map renders a plotly widget", {
  fx <- make_interactive_fixture()
  p <- plot_groups_map_interactive(fx)
  expect_s3_class(p, "plotly")
  expect_s3_class(plotly::plotly_build(p), "plotly")
})

test_that("tooltip carries title and final group", {
  fx <- make_interactive_fixture()
  txt <- hover_texts(plot_groups_map_interactive(fx))
  expect_true(any(grepl("Title a", txt)))
  expect_true(any(grepl("g1", txt)))
})

test_that("unassigned nodes have no hover and toggle off", {
  fx <- make_interactive_fixture()
  has_skip <- function(p) {
    b <- plotly::plotly_build(p)
    any(vapply(b$x$data,
               function(t) length(t$hoverinfo) > 0 && all(t$hoverinfo == "skip"),
               logical(1)))
  }
  expect_true(has_skip(plot_groups_map_interactive(fx, show_unassigned = TRUE)))
  expect_false(has_skip(plot_groups_map_interactive(fx, show_unassigned = FALSE)))
})

test_that("group annotations toggle with label", {
  fx <- make_interactive_fixture()
  b_yes <- plotly::plotly_build(plot_groups_map_interactive(fx, label = TRUE))
  b_no  <- plotly::plotly_build(plot_groups_map_interactive(fx, label = FALSE))
  expect_true(length(b_yes$x$layout$annotations) >= 1)
  expect_true(is.null(b_no$x$layout$annotations) ||
                length(b_no$x$layout$annotations) == 0)
})

test_that("normalize fixes the axis range", {
  fx <- make_interactive_fixture()
  b <- plotly::plotly_build(plot_groups_map_interactive(fx, normalize = TRUE))
  expect_equal(b$x$layout$xaxis$range, c(-1, 1))
})

test_that("interactive map inherits validation", {
  expect_error(plot_groups_map_interactive(list()), "non-empty")
  fx <- make_interactive_fixture()
  expect_error(plot_groups_map_interactive(fx, network_until = 1999), "2020")
  expect_error(plot_groups_map_interactive(fx, labels_text = data.frame(id = "g1")), "labels_text")
})

test_that("assigned nodes are one trace coloured from the birddog palette", {
  fx <- make_interactive_fixture()
  b <- plotly::plotly_build(plot_groups_map_interactive(fx))
  hover_traces <- Filter(
    function(t) length(t$hoverinfo) > 0 && all(t$hoverinfo == "text"), b$x$data)
  # one trace with explicit per-node marker colours -- NOT one trace per group,
  # which would let plotly re-derive its own palette and desync from the static
  # plot_groups_map() (and warn on >8 groups).
  expect_length(hover_traces, 1)
  cols <- unique(unlist(hover_traces[[1]]$marker$color))
  expect_true(length(cols) >= 1 && all(grepl("^(#|rgba)", cols)))
})

test_that("title and title_size control the plotly title", {
  fx <- make_interactive_fixture()
  tt <- function(p) plotly::plotly_build(p)$x$layout$title
  expect_equal(tt(plot_groups_map_interactive(fx))$text, "Group map")
  expect_equal(tt(plot_groups_map_interactive(fx, title = "Custom"))$text, "Custom")
  expect_equal(tt(plot_groups_map_interactive(fx, title = NULL))$text, "")
  expect_equal(tt(plot_groups_map_interactive(fx, title_size = 22))$font$size, 22)
})
