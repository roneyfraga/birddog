test_that("plot_trajectory_handoff returns a ggplot", {
  fx <- make_destination_fixture()
  d <- sniff_trajectory_destination(
    fx$detected, "tr3", fx$docs_per_group,
    all_detected = fx$all_detected, group = "c1g10"
  )

  p <- plot_trajectory_handoff(d)

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_handoff errors without trajectory info", {
  fx <- make_destination_fixture()
  d <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  expect_error(
    plot_trajectory_handoff(d),
    "all_detected"
  )
})

test_that("plot_trajectory_handoff renders -> as an arrow in the title", {
  fx <- make_destination_fixture()
  d <- sniff_trajectory_destination(
    fx$detected, "tr3", fx$docs_per_group,
    all_detected = fx$all_detected, group = "c1g10"
  )

  p <- plot_trajectory_handoff(d, title = "a -> b")

  arrow_char <- intToUtf8(8594)
  expect_true(grepl(arrow_char, p$labels$title, fixed = TRUE))
})
