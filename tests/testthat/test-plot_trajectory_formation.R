# Flow formation of tr::c1g3 in make_flow_tree_dpg() (helper-trajectory-flow.R):
# one feeder, the B-line (tr1), contributing n = 4 papers at the 2002 handoff.

make_tree_formation <- function(...) {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  sniff_trajectory_formation(fl, "tr::c1g3", ...)
}

test_that("plot_trajectory_formation returns a ggplot", {
  f <- make_tree_formation(min_papers = 2)

  p <- plot_trajectory_formation(f)

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_formation errors when no feeders pass thresholds", {
  f <- make_tree_formation(min_papers = 50)

  expect_error(plot_trajectory_formation(f), "threshold")
})

test_that("plot_trajectory_formation renders -> as an arrow in the title", {
  f <- make_tree_formation(min_papers = 2)

  p <- plot_trajectory_formation(f, title = "a -> b")

  arrow_char <- intToUtf8(8594)
  expect_true(grepl(arrow_char, p$labels$title, fixed = TRUE))
})

test_that("plot_trajectory_formation accepts the 'size' feeder curve", {
  f <- make_tree_formation(min_papers = 1, min_prop = 0)

  p <- plot_trajectory_formation(f, feeder_curve = "size")

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_formation accepts the 'inflow' feeder curve", {
  f <- make_tree_formation(min_papers = 1, min_prop = 0)

  p <- plot_trajectory_formation(f, feeder_curve = "inflow")

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_formation errors on an invalid feeder_curve", {
  f <- make_tree_formation(min_papers = 1, min_prop = 0)

  expect_error(plot_trajectory_formation(f, feeder_curve = "nope"))
})

test_that("plot_trajectory_formation draws feeders that grow along their length", {
  f <- make_tree_formation(min_papers = 1, min_prop = 0)

  p <- plot_trajectory_formation(f, feeder_curve = "size")
  b <- ggplot2::ggplot_build(p)
  ribbon_layers <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)
  ))

  grew <- FALSE
  for (i in ribbon_layers) {
    d <- b$data[[i]]
    centre <- mean((d$ymin + d$ymax) / 2, na.rm = TRUE)
    if (abs(centre) < 0.1) next  # skip the river
    w <- d$ymax - d$ymin
    if (length(unique(round(w, 6))) > 1) grew <- TRUE
  }

  expect_true(grew)
})

test_that("plot_trajectory_formation labels feeders on the right with two lines", {
  f <- make_tree_formation(min_papers = 1, min_prop = 0)

  p <- plot_trajectory_formation(f)
  b <- ggplot2::ggplot_build(p)

  # the feeder-label layer: a GeomText layer whose data carries source_key
  li <- which(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomText") &&
      !is.null(l$data) && "source_key" %in% names(l$data),
    logical(1)
  ))
  expect_length(li, 1)
  d <- b$data[[li]]
  fk <- f$feeders[f$feeders$kept %in% TRUE, ]

  expect_true(all(grepl("\n", d$label, fixed = TRUE)))   # two-line, like the target
  expect_true(all(d$x >= min(fk$handoff_year)))          # on the right, at the handoff
})

test_that("'size' and 'inflow' feeder curves change the rendering", {
  f <- make_tree_formation(min_papers = 1, min_prop = 0)

  # The feeder ribbon traces its measured cluster size (ending at its 2001 node)
  # under "size", but the cumulative arrival of its contributed papers (extended
  # to the 2002 handoff) under "inflow", so the drawn ribbons differ.
  feeder_ribbons <- function(p) {
    b <- ggplot2::ggplot_build(p)
    ribbon_layers <- which(vapply(
      p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)
    ))
    out <- list()
    for (i in ribbon_layers) {
      d <- b$data[[i]]
      centre <- mean((d$ymin + d$ymax) / 2, na.rm = TRUE)
      if (abs(centre) < 0.1) next  # skip the river, centred at y = 0
      out[[length(out) + 1]] <- d[, c("x", "ymin", "ymax")]
    }
    out
  }

  r_size <- feeder_ribbons(plot_trajectory_formation(f, feeder_curve = "size"))
  r_inflow <- feeder_ribbons(plot_trajectory_formation(f, feeder_curve = "inflow"))

  expect_false(identical(r_size, r_inflow))
})
