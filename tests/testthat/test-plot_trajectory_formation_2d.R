test_that("plot_trajectory_formation_2d returns a ggplot", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 2
  )

  p <- plot_trajectory_formation_2d(f)

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_formation_2d errors when no feeders pass thresholds", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 50
  )

  expect_error(plot_trajectory_formation_2d(f), "threshold")
})

test_that("plot_trajectory_formation_2d renders -> as an arrow in the title", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 2
  )

  p <- plot_trajectory_formation_2d(f, title = "a -> b")

  arrow_char <- intToUtf8(8594)
  expect_true(grepl(arrow_char, p$labels$title, fixed = TRUE))
})

test_that("plot_trajectory_formation_2d accepts the 'size' feeder curve", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 1, min_prop = 0
  )

  p <- plot_trajectory_formation_2d(f, feeder_curve = "size")

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_formation_2d accepts the 'inflow' feeder curve", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 1, min_prop = 0
  )

  p <- plot_trajectory_formation_2d(f, feeder_curve = "inflow")

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_formation_2d errors on an invalid feeder_curve", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 1, min_prop = 0
  )

  expect_error(plot_trajectory_formation_2d(f, feeder_curve = "nope"))
})

# Helper: max half-height of the feeder ribbon (the GeomRibbon layer whose
# vertical centre sits on a lane, not on the river at y = 0).
feeder_ribbon_half <- function(p) {
  b <- ggplot2::ggplot_build(p)
  ribbon_layers <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)
  ))
  mh <- 0
  for (i in ribbon_layers) {
    d <- b$data[[i]]
    centre <- mean((d$ymin + d$ymax) / 2, na.rm = TRUE)
    if (abs(centre) < 0.1) next  # the river is centred at y = 0
    mh <- max(mh, max((d$ymax - d$ymin) / 2, na.rm = TRUE))
  }
  mh
}

# Helper: max half-height of the river ribbon (centred at y = 0).
river_ribbon_half <- function(p) {
  b <- ggplot2::ggplot_build(p)
  ribbon_layers <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)
  ))
  for (i in ribbon_layers) {
    d <- b$data[[i]]
    centre <- mean((d$ymin + d$ymax) / 2, na.rm = TRUE)
    if (abs(centre) < 0.1) return(max((d$ymax - d$ymin) / 2, na.rm = TRUE))
  }
  NA_real_
}

test_that("plot_trajectory_formation_2d draws feeders that grow along their length", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 1, min_prop = 0
  )

  p <- plot_trajectory_formation_2d(f, feeder_curve = "size")
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

test_that("plot_trajectory_formation_2d labels feeders on the right with two lines", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 1, min_prop = 0
  )

  p <- plot_trajectory_formation_2d(f)
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

  expect_true(all(grepl("\n", d$label, fixed = TRUE)))   # two-line, like tr1
  expect_true(all(d$x >= min(fk$handoff_year)))          # on the right, at the handoff
})

test_that("'size' and 'inflow' feeder curves change the rendering", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 1, min_prop = 0
  )

  # The feeder ends at cohort_size (4) under "size" but at n (2) under "inflow",
  # so on the shared proportional scale the river half-height differs.
  rh_size <- river_ribbon_half(plot_trajectory_formation_2d(f, feeder_curve = "size"))
  rh_inflow <- river_ribbon_half(plot_trajectory_formation_2d(f, feeder_curve = "inflow"))

  expect_false(isTRUE(all.equal(rh_size, rh_inflow)))
})
