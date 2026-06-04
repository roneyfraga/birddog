# Shared fixture make_destination_fixture() lives in
# helper-trajectory-destination.R

test_that("sniff_trajectory_destination resolves terminal node and cohort", {
  fx <- make_destination_fixture()

  res <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  expect_equal(res$terminal_node, "y2018c1g16")
  expect_equal(res$cohort_size, 4L)
})

test_that("sniff_trajectory_destination computes the final-year destiny vector", {
  fx <- make_destination_fixture()

  res <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)
  dest <- res$destination

  expect_equal(dest$n[dest$g_final == "c1g16"], 2L)
  expect_equal(dest$n[dest$g_final == "c1g10"], 1L)
  expect_equal(dest$n[dest$g_final == "(dropped)"], 1L)
  expect_equal(sum(dest$prop), 1)
  # sorted by descending n, dominant real destination first
  expect_equal(dest$g_final[1], "c1g16")
})

test_that("sniff_trajectory_destination reports dormancy and continuation", {
  fx <- make_destination_fixture()

  res <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  expect_equal(res$dormant_share, 0.25)
  expect_equal(res$continuation, "c1g16")
})

test_that("sniff_trajectory_destination builds a conservative per-year flow", {
  fx <- make_destination_fixture()

  res <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)
  fl <- res$flow

  expect_true(all(c(
    "from_id", "to_id", "from_year", "to_year",
    "from_group", "to_group", "final_group", "n"
  ) %in% names(fl)))

  # only consecutive-year transitions
  expect_true(all(fl$to_year == fl$from_year + 1))

  # every cohort paper is accounted for at each transition (attrition routed,
  # not dropped silently)
  first <- dplyr::filter(fl, from_year == 2018)
  second <- dplyr::filter(fl, from_year == 2019)
  expect_equal(sum(first$n), 4)
  expect_equal(sum(second$n), 4)

  # the 2018 -> 2019 split is visible
  expect_setequal(unique(first$to_group), c("c1g16", "c1g10", "c1g1"))
})

test_that("sniff_trajectory_destination errors on an unknown traj_id", {
  fx <- make_destination_fixture()

  expect_error(
    sniff_trajectory_destination(fx$detected, "trX", fx$docs_per_group),
    "trX"
  )
})

test_that(".assign_destination_trajectory routes papers to the carrying trajectory", {
  fx <- make_destination_fixture()
  cohort <- c("d1", "d2", "d3", "d4")

  a <- birddog:::.assign_destination_trajectory(
    cohort, terminal_year = 2018,
    all_detected = fx$all_detected, docs_per_group = fx$docs_per_group
  )

  expect_equal(a$dest_traj_key[a$document_id == "d1"], "c1g16::tr1")
  expect_equal(a$dest_traj_key[a$document_id == "d2"], "c1g16::tr1")
  expect_equal(a$dest_traj_key[a$document_id == "d3"], "c1g10::tr1")
  expect_equal(a$dest_traj_key[a$document_id == "d4"], "(none)")
})

test_that("sniff_trajectory_destination reports the absorbing trajectory", {
  fx <- make_destination_fixture()

  res <- sniff_trajectory_destination(
    fx$detected, "tr3", fx$docs_per_group,
    all_detected = fx$all_detected
  )

  expect_equal(res$continuation_traj, "c1g16::tr1")
  expect_true("dest_traj" %in% names(res$flow))

  dt <- res$destination_traj
  expect_equal(dt$n[dt$traj_key == "c1g16::tr1"], 2L)
  expect_equal(dt$n[dt$traj_key == "c1g10::tr1"], 1L)
  expect_equal(dt$n[dt$traj_key == "(none)"], 1L)
})

test_that("sniff_trajectory_destination omits trajectory output without all_detected", {
  fx <- make_destination_fixture()

  res <- sniff_trajectory_destination(fx$detected, "tr3", fx$docs_per_group)

  expect_null(res$destination_traj)
  expect_false("dest_traj" %in% names(res$flow))
})
