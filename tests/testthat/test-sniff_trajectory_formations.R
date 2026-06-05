# Batch version: all targets' feeders in one pass. Uses make_destination_fixture()
# where c1g10::tr3 feeds c1g16::tr1 (d1,d2) and c1g10::tr1 (d3).

test_that("sniff_trajectory_formations returns one formation object per fed target", {
  fx <- make_destination_fixture()

  fs <- sniff_trajectory_formations(fx$all_detected, fx$docs_per_group)

  expect_true(is.list(fs))
  expect_true(all(c("c1g16::tr1", "c1g10::tr1") %in% names(fs)))

  f16 <- fs[["c1g16::tr1"]]
  expect_equal(f16$target_info$group, "c1g16")
  fr <- f16$feeders[f16$feeders$source_key == "c1g10::tr3", ]
  expect_equal(fr$n, 2L)
  expect_equal(fr$prop_of_source, 0.5)

  f10 <- fs[["c1g10::tr1"]]
  fr10 <- f10$feeders[f10$feeders$source_key == "c1g10::tr3", ]
  expect_equal(fr10$n, 1L)
})

test_that("sniff_trajectory_formations elements match sniff_trajectory_formation", {
  fx <- make_destination_fixture()

  fs <- sniff_trajectory_formations(fx$all_detected, fx$docs_per_group)
  one <- sniff_trajectory_formation(
    "c1g16::tr1", fx$all_detected, fx$docs_per_group, min_papers = 1, min_prop = 0
  )

  b <- fs[["c1g16::tr1"]]$feeders
  a <- one$feeders
  expect_setequal(a$source_key, b$source_key)
  expect_equal(
    a$n[order(a$source_key)], b$n[order(b$source_key)]
  )
})

test_that("sniff_trajectory_formations carries per-feeder size and inflow curves", {
  fx <- make_destination_fixture()

  fs <- sniff_trajectory_formations(fx$all_detected, fx$docs_per_group)

  fr <- fs[["c1g16::tr1"]]$feeders
  fr <- fr[fr$source_key == "c1g10::tr3", ]

  sc <- fr$size_curve[[1]]
  expect_named(sc, c("year", "size"))
  expect_equal(sc$year, c(2017L, 2018L))
  expect_equal(sc$size, c(0L, 4L))

  ic <- fr$inflow_curve[[1]]
  expect_named(ic, c("year", "size"))
  expect_equal(ic$year, 2018L)
  expect_equal(ic$size, 2L)
})

test_that("sniff_trajectory_formations output plots without error", {
  fx <- make_destination_fixture()

  fs <- sniff_trajectory_formations(fx$all_detected, fx$docs_per_group)
  p <- plot_trajectory_formation_2d(fs[["c1g16::tr1"]])

  expect_s3_class(p, "ggplot")
})
