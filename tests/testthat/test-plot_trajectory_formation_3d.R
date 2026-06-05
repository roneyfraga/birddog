# Uses make_destination_fixture() (helper-trajectory-destination.R). The
# formation of c1g16::tr1 has one feeder (c1g10::tr3, n = 2) when thresholds are
# permissive.

make_fx_formation <- function() {
  fx <- make_destination_fixture()
  sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group,
    min_papers = 1, min_prop = 0
  )
}

test_that("plot_trajectory_formation_3d returns a plotly object", {
  p <- plot_trajectory_formation_3d(make_fx_formation())
  expect_s3_class(p, "plotly")
})

test_that("plot_trajectory_formation_3d works with log_scale", {
  p <- plot_trajectory_formation_3d(make_fx_formation(), log_scale = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("plot_trajectory_formation_3d works with the 'size' feeder curve", {
  p <- plot_trajectory_formation_3d(make_fx_formation(), feeder_curve = "size")
  expect_s3_class(p, "plotly")
})

test_that("plot_trajectory_formation_3d errors when no feeders pass thresholds", {
  fx <- make_destination_fixture()
  f <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 50
  )
  expect_error(plot_trajectory_formation_3d(f), "threshold")
})

test_that("plot_trajectory_formation_3d errors on a non-formation input", {
  expect_error(
    plot_trajectory_formation_3d(list(a = 1)),
    "sniff_trajectory_formation"
  )
})

test_that("plot_trajectory_formation_3d shows manual descriptions on hover", {
  desc <- data.frame(
    id = c("c1g16::tr1", "c1g10:tr3"),       # target (::) and feeder (:) keys
    text = c("target description", "feeder description"),
    stringsAsFactors = FALSE
  )
  p <- plot_trajectory_formation_3d(make_fx_formation(), descriptions = desc)
  txts <- unlist(lapply(p$x$attrs, function(tr) tr$text))
  expect_true(any(grepl("target description", txts)))
  expect_true(any(grepl("feeder description", txts)))
})

test_that("plot_trajectory_formation_3d errors on malformed descriptions", {
  expect_error(
    plot_trajectory_formation_3d(make_fx_formation(), descriptions = data.frame(id = "x")),
    "columns"
  )
})
