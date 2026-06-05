# Shared fixture make_destination_fixture() lives in
# helper-trajectory-destination.R. In it, last network year is 2020;
# c1g10::tr3 (terminal y2018c1g16, cohort d1-d4) feeds c1g16::tr1 with d1,d2
# and c1g10::tr1 with d3; d4 has no carrier.

test_that("sniff_trajectory_formation finds direct feeders of a target", {
  fx <- make_destination_fixture()

  f <- sniff_trajectory_formation("c1g16::tr1", fx$all_detected, fx$docs_per_group)

  expect_equal(f$target_info$group, "c1g16")
  expect_equal(f$target_info$traj_id, "tr1")
  # target's own size = terminal node y2020c1g16 (d1,d2)
  expect_equal(f$target_info$size, 2L)
  expect_true("c1g10::tr3" %in% f$feeders$source_key)

  fr <- f$feeders[f$feeders$source_key == "c1g10::tr3", ]
  expect_equal(fr$n, 2L)
  expect_equal(fr$handoff_year, 2018L)
  expect_equal(fr$prop_of_source, 0.5)
  expect_equal(f$total_inflow, 2L)
})

test_that("sniff_trajectory_formation excludes the target itself", {
  fx <- make_destination_fixture()

  f <- sniff_trajectory_formation("c1g16::tr1", fx$all_detected, fx$docs_per_group)

  expect_false("c1g16::tr1" %in% f$feeders$source_key)
})

test_that("sniff_trajectory_formation excludes living trajectories as feeders", {
  fx <- make_destination_fixture()

  # c1g10::tr1 ends in 2020 (= last year), so it cannot hand off into anything
  f <- sniff_trajectory_formation("c1g16::tr1", fx$all_detected, fx$docs_per_group)

  expect_false("c1g10::tr1" %in% f$feeders$source_key)
})

test_that("sniff_trajectory_formation applies min_papers / min_prop to 'kept'", {
  fx <- make_destination_fixture()

  keep <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group,
    min_papers = 2, min_prop = 0.05
  )
  fr_keep <- keep$feeders[keep$feeders$source_key == "c1g10::tr3", ]
  expect_true(fr_keep$kept)

  drop <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 3
  )
  fr_drop <- drop$feeders[drop$feeders$source_key == "c1g10::tr3", ]
  expect_false(fr_drop$kept)
})

test_that("sniff_trajectory_formation returns per-feeder size and inflow curves", {
  fx <- make_destination_fixture()

  f <- sniff_trajectory_formation("c1g16::tr1", fx$all_detected, fx$docs_per_group)

  expect_true(all(c("size_curve", "inflow_curve") %in% names(f$feeders)))

  fr <- f$feeders[f$feeders$source_key == "c1g10::tr3", ]

  # size_curve: cluster size per year, ending at cohort_size
  # c1g10::tr3 nodes y2017c1g3 (no docs) -> y2018c1g16 (d1-d4)
  sc <- fr$size_curve[[1]]
  expect_named(sc, c("year", "size"))
  expect_equal(sc$year, c(2017L, 2018L))
  expect_equal(sc$size, c(0L, 4L))
  expect_equal(sc$size[which.max(sc$year)], fr$cohort_size)

  # inflow_curve: arrival of the n contributed papers (d1,d2, first seen 2018),
  # ending at n in the handoff year
  ic <- fr$inflow_curve[[1]]
  expect_named(ic, c("year", "size"))
  expect_equal(ic$year, 2018L)
  expect_equal(ic$size, 2L)
  expect_equal(ic$size[nrow(ic)], fr$n)
  expect_equal(max(ic$year), fr$handoff_year)
})

test_that("sniff_trajectory_formation errors on an unknown or malformed target", {
  fx <- make_destination_fixture()

  expect_error(
    sniff_trajectory_formation("c1g99::tr9", fx$all_detected, fx$docs_per_group),
    "c1g99"
  )
  expect_error(
    sniff_trajectory_formation("c1g16-tr1", fx$all_detected, fx$docs_per_group),
    "group::traj_id"
  )
})

test_that(".contributed_arrival_series counts arrival within the feeder's nodes", {
  # Doc "x" appears in a 2015 cluster outside the feeder, then joins the feeder
  # node y2018c1g16 in 2018. Its arrival INTO the feeder is 2018, so the curve
  # must use 2018 (the feeder's lifespan), not 2015 (its corpus first-appearance,
  # a cumulative-clustering artifact).
  dpg <- tibble::tribble(
    ~group_id,    ~document_id, ~network_until, ~group,
    "y2015c1g7",  "x",          2015,           "c1g7",
    "y2018c1g16", "x",          2018,           "c1g16",
    "y2018c1g16", "y",          2018,           "c1g16"
  )
  nodes <- c("y2017c1g16", "y2018c1g16")

  out <- .contributed_arrival_series(c("x", "y"), nodes, dpg, 2018)

  expect_equal(out$year, 2018L)   # not 2015
  expect_equal(out$size, 2L)
})
