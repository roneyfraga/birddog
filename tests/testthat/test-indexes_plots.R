test_that("indexes_plots creates CCT plotly output", {
  data <- tibble::tibble(
    index = c(5, 4.5, 4, 3.5, 3),
    year = 2016:2020,
    group = rep("c1g1", 5)
  )
  p <- indexes_plots(data, group_name = "c1g1", start_year = 2016, end_year = 2020, method = "cct")
  expect_s3_class(p, "plotly")
})

test_that("indexes_plots creates entropy plotly output", {
  data <- tibble::tibble(
    index = c(0.7, 0.75, 0.8, 0.85, 0.9),
    year = 2016:2020,
    group = rep("c1g1", 5)
  )
  p <- indexes_plots(data, group_name = "c1g1", start_year = 2016, end_year = 2020, method = "entropy")
  expect_s3_class(p, "plotly")
})

test_that("indexes_plots errors on missing group", {
  data <- tibble::tibble(
    index = c(0.5, 0.6),
    year = c(2016, 2017),
    group = rep("c1g1", 2)
  )
  expect_error(
    indexes_plots(data, group_name = "nonexistent", start_year = 2016, end_year = 2017),
    "No data available"
  )
})

test_that("indexes_plots handles all-NA index values", {
  data <- tibble::tibble(
    index = c(NA_real_, NA_real_, NA_real_),
    year = 2016:2018,
    group = rep("c1g1", 3)
  )
  expect_error(
    indexes_plots(data, group_name = "c1g1", start_year = 2016, end_year = 2018),
    "No non-NA data"
  )
})
