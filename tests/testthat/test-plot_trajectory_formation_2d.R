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
