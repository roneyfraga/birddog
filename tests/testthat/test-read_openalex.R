test_that("read_openalex rejects invalid format", {
  expect_error(read_openalex("file.csv", format = "xyz"), "must be either 'csv' or 'api'")
})

test_that("read_openalex api rejects non-data.frame input", {
  expect_error(read_openalex("not_a_df", format = "api"), "must be a data frame")
})
