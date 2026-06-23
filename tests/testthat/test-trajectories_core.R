test_that(".extract_year extracts year from node names", {
  expect_equal(birddog:::.extract_year("y2005g01"), 2005L)
  expect_equal(birddog:::.extract_year(c("y2010g01", "y2015g03")), c(2010L, 2015L))
})

test_that(".na_to_zero replaces NA with 0", {
  expect_equal(birddog:::.na_to_zero(c(1, NA, 3, NA)), c(1, 0, 3, 0))
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

  out <- birddog:::.contributed_arrival_series(c("x", "y"), nodes, dpg, 2018)

  expect_equal(out$year, 2018L)   # not 2015
  expect_equal(out$size, 2L)
})
