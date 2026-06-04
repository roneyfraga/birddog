test_that("plot_trajectory_destination returns a ggplot", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  p <- plot_trajectory_destination(dest)

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_destination errors without a flow component", {
  expect_error(
    plot_trajectory_destination(list(destination = NULL)),
    "flow"
  )
})

test_that(".destination_paths builds a modal year-group path per destination", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  dp <- birddog:::.destination_paths(dest$flow, key_col = "final_group", id_col = "g_final")

  c16 <- dp[dp$dest == "c1g16", ]
  c16 <- c16[order(c16$year), ]
  expect_equal(c16$node, c("y2018c1g16", "y2019c1g16", "y2020c1g16"))
  expect_true(all(c16$papers == 2))
  expect_setequal(unique(dp$dest), c("c1g16", "c1g10", "(dropped)"))
})

test_that(".destination_paths merges sub-threshold destinations into (other)", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  dp <- birddog:::.destination_paths(
    dest$flow,
    key_col = "final_group", share_tbl = dest$destination,
    id_col = "g_final", min_prop = 0.3
  )

  expect_setequal(unique(dp$dest), c("c1g16", "(other)"))
})

test_that("plot_trajectory_destination accepts a min_prop threshold", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  p <- plot_trajectory_destination(dest, min_prop = 0.3)

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_destination can colour by destination trajectory", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(
    fx$detected, "tr3", fx$docs_per_group, all_detected = fx$all_detected
  )

  p <- plot_trajectory_destination(dest, color_by = "trajectory")

  expect_s3_class(p, "ggplot")
})

test_that("plot_trajectory_destination color_by trajectory needs dest_traj", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  expect_error(
    plot_trajectory_destination(dest, color_by = "trajectory"),
    "all_detected"
  )
})

test_that("plot_trajectory_destination default title names the source trajectory", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(
    fx$detected, "tr3", fx$docs_per_group,
    all_detected = fx$all_detected, group = "c1g10"
  )

  p <- plot_trajectory_destination(dest, color_by = "trajectory")

  expect_match(p$labels$title, "c1g10")
  expect_match(p$labels$title, "tr3")
  expect_false(grepl(dest$terminal_node, p$labels$title, fixed = TRUE))
})

test_that("plot_trajectory_destination renders -> as an arrow in the title", {
  fx <- make_destination_fixture()
  dest <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  p <- plot_trajectory_destination(dest, title = "a -> b")

  arrow_char <- intToUtf8(8594)
  expect_true(grepl(arrow_char, p$labels$title, fixed = TRUE))
  expect_false(grepl("->", p$labels$title, fixed = TRUE))
})
