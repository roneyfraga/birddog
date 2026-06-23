build_traces <- function(p) plotly::plotly_build(p)$x$data

marker_texts <- function(p) {
  unlist(lapply(build_traces(p),
                function(t) if (identical(t$mode, "markers")) t$text else NULL))
}

# c1g3 lives only in 2000 with disjoint docs, so it dead-ends (dormant); the
# final 2002 groups are c1g1, c1g2.
make_dormant_dag <- function() {
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
  sniff_trajectory_dag(dpg, min_group_size = 2)
}

test_that("interactive dag returns a plotly widget and builds cleanly", {
  d <- make_dag_fixture()
  p <- plot_trajectory_dag_interactive(d)
  expect_s3_class(p, "plotly")
  expect_silent(plotly::plotly_build(p))
})

test_that("the interactive dag has no legend", {
  d <- make_dag_fixture()
  tr <- build_traces(plot_trajectory_dag_interactive(d))
  expect_false(any(vapply(tr, function(t) isTRUE(t$showlegend), logical(1))))
})

test_that("node marker sizes vary with paper count", {
  d <- make_dag_fixture()
  tr <- build_traces(plot_trajectory_dag_interactive(d))
  sizes <- unlist(lapply(tr, function(t) if (identical(t$mode, "markers")) t$marker$size else NULL))
  expect_gt(length(unique(sizes)), 1)
})

test_that("final-year nodes hover 'id: description'; others hover the raw id", {
  d <- make_dag_fixture()   # final year 2002; intermediate years 2000, 2001
  lab <- data.frame(id = "c1g1", text = "Alpha", stringsAsFactors = FALSE)
  txt <- marker_texts(plot_trajectory_dag_interactive(d, labels_text = lab))
  expect_true(any(grepl("c1g1: Alpha", txt, fixed = TRUE)))   # final-year, id + description
  expect_true(any(txt == "c1g2"))                            # unmapped final -> bare id
  expect_true(any(grepl("y2000c1g1", txt)))                  # intermediate -> raw id
})

test_that("dormant node hover shows the raw node id", {
  d <- make_dormant_dag()
  txt <- marker_texts(plot_trajectory_dag_interactive(d))
  expect_true(any(grepl("y2000c1g3", txt)))   # dormant node shows its raw id
})

test_that("edge_alpha sets the edge-trace opacity (default 0.3)", {
  d <- make_dag_fixture()
  edge_of <- function(p) {
    Filter(function(t) identical(t$mode, "lines"), build_traces(p))[[1]]$opacity
  }
  expect_equal(edge_of(plot_trajectory_dag_interactive(d)), 0.3)
  expect_equal(edge_of(plot_trajectory_dag_interactive(d, edge_alpha = 0.2)), 0.2)
})

test_that("label_terminal toggles a final-year text trace of bare group ids", {
  d <- make_dag_fixture()
  text_tr <- function(p) Filter(function(t) identical(t$mode, "text"), build_traces(p))
  tr <- text_tr(plot_trajectory_dag_interactive(d))
  expect_length(tr, 1)                                   # default TRUE
  expect_setequal(as.character(tr[[1]]$text), c("c1g1", "c1g2"))   # the 2002 finals
  expect_length(text_tr(plot_trajectory_dag_interactive(d, label_terminal = FALSE)), 0)
})

test_that(".spread_y enforces a minimum gap and keeps order", {
  y <- c(0, 0.1, 0.2, 5)   # first three crowded, last far
  out <- .spread_y(y, min_gap = 1)
  expect_equal(order(out), order(y))               # order preserved
  expect_true(all(diff(sort(out)) >= 1 - 1e-9))    # min gap enforced
})

test_that("label_terminal adds a leader-line trace", {
  d <- make_dag_fixture()
  n_lines <- function(p) sum(vapply(build_traces(p),
                                    function(t) identical(t$mode, "lines"), logical(1)))
  expect_equal(n_lines(plot_trajectory_dag_interactive(d)), 2L)                         # edges + leaders
  expect_equal(n_lines(plot_trajectory_dag_interactive(d, label_terminal = FALSE)), 1L) # edges only
})

test_that("terminal labels sit to the right of the final-year nodes", {
  d <- make_dag_fixture()
  tr <- build_traces(plot_trajectory_dag_interactive(d))
  text_x <- Filter(function(t) identical(t$mode, "text"), tr)[[1]]$x
  node_x <- unlist(lapply(tr, function(t) if (identical(t$mode, "markers")) t$x else NULL))
  expect_true(min(text_x) > max(node_x))   # labels nudged past every node
})

test_that("a fully mismatched labels_text warns exactly once", {
  d <- make_dag_fixture()
  bad <- data.frame(id = "zzz", text = "Nope", stringsAsFactors = FALSE)
  expect_warning(plot_trajectory_dag_interactive(d, labels_text = bad), "match")
  warns <- 0L
  withCallingHandlers(
    plot_trajectory_dag_interactive(d, labels_text = bad),
    warning = function(w) { warns <<- warns + 1L; invokeRestart("muffleWarning") }
  )
  expect_equal(warns, 1L)
})

test_that("interactive dag validates its input", {
  expect_error(plot_trajectory_dag_interactive(list()), "sniff_trajectory_dag")
})
