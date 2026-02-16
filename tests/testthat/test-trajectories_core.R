test_that(".extract_year extracts year from node names", {
  expect_equal(birddog:::.extract_year("y2005g01"), 2005L)
  expect_equal(birddog:::.extract_year(c("y2010g01", "y2015g03")), c(2010L, 2015L))
})

test_that(".na_to_zero replaces NA with 0", {
  expect_equal(birddog:::.na_to_zero(c(1, NA, 3, NA)), c(1, 0, 3, 0))
})

test_that("filter_trajectories respects top_n", {
  tr <- tibble::tibble(
    traj_id = paste0("tr", 1:5),
    score = c(10, 8, 6, 4, 2),
    length = c(5L, 4L, 3L, 3L, 2L)
  )

  result <- filter_trajectories(tr, top_n = 2)
  expect_equal(nrow(result), 2)
  expect_equal(result$score, c(10, 8))
})

test_that("filter_trajectories applies min_score and min_length", {
  tr <- tibble::tibble(
    traj_id = paste0("tr", 1:5),
    score = c(10, 8, 6, 4, 2),
    length = c(5L, 4L, 3L, 3L, 2L)
  )

  result <- filter_trajectories(tr, top_n = NULL, min_score = 5, min_length = 3)
  expect_true(all(result$score >= 5))
  expect_true(all(result$length >= 3))
  expect_equal(nrow(result), 3)
})
