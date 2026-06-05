# Uses make_destination_fixture() (helper-trajectory-destination.R). last_year
# is 2020. c1g10::tr3's cohort (d1-d4) hands off d1,d2 to c1g16::tr1 (external,
# different group) and d3 to c1g10::tr1 (internal, same group); d4 has no carrier.
# Sizes (unique docs in nodes): c1g16::tr1 = {d1,d2,d3,d4} = 4; c1g10::tr1 = {d3}
# = 1; c1g10::tr3 = 4; c1g16::tr2 = 0 (its nodes hold no documents).

test_that("sniff_trajectory_self_sufficiency computes the index per trajectory", {
  fx <- make_destination_fixture()

  s <- sniff_trajectory_self_sufficiency(fx$all_detected, fx$docs_per_group, min_size = 1)

  r16 <- s[s$key == "c1g16::tr1", ]
  expect_equal(r16$size, 4L)
  expect_equal(r16$inflow_external, 2L)        # d1, d2 imported from c1g10
  expect_equal(r16$self_sufficiency, 0.5)      # 1 - 2/4
  expect_true(r16$living)                      # ends 2020 = last_year

  r10 <- s[s$key == "c1g10::tr1", ]
  expect_equal(r10$inflow_external, 0L)
  expect_equal(r10$self_sufficiency, 1)
})

test_that("sniff_trajectory_self_sufficiency separates internal from external inflow", {
  fx <- make_destination_fixture()

  s <- sniff_trajectory_self_sufficiency(fx$all_detected, fx$docs_per_group, min_size = 1)

  r10 <- s[s$key == "c1g10::tr1", ]
  expect_equal(r10$inflow_internal, 1L)        # d3 from same-group c1g10::tr3
})

test_that("sniff_trajectory_self_sufficiency drops trajectories below min_size", {
  fx <- make_destination_fixture()

  s <- sniff_trajectory_self_sufficiency(fx$all_detected, fx$docs_per_group, min_size = 1)

  expect_false("c1g16::tr2" %in% s$key)        # size 0
})

test_that("sniff_trajectory_self_sufficiency is sorted by descending index", {
  fx <- make_destination_fixture()

  s <- sniff_trajectory_self_sufficiency(fx$all_detected, fx$docs_per_group, min_size = 1)

  expect_equal(s$self_sufficiency, sort(s$self_sufficiency, decreasing = TRUE))
})

test_that("sniff_trajectory_self_sufficiency validates docs_per_group columns", {
  fx <- make_destination_fixture()

  expect_error(
    sniff_trajectory_self_sufficiency(fx$all_detected, fx$docs_per_group[, 1:2]),
    "docs_per_group"
  )
})
