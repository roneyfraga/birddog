test_that("get_openalex_fields rejects numeric input", {
  expect_error(get_openalex_fields(123), "must be a character vector or data frame")
})
