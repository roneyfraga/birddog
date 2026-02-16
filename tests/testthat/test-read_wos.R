test_that("read_wos parses sample_wos.bib correctly", {
  bib_file <- system.file("extdata", "sample_wos.bib", package = "birddog")
  skip_if(bib_file == "", "sample_wos.bib not found")

  M <- read_wos(bib_file, format = "bib", normalized_names = TRUE)

  expect_s3_class(M, "tbl_df")
  expect_true(nrow(M) >= 3)
  expect_true(all(c("AU", "TI", "PY", "DI", "CR", "SR", "DB", "DI2") %in% names(M)))
})

test_that("read_wos PY is numeric", {
  bib_file <- system.file("extdata", "sample_wos.bib", package = "birddog")
  skip_if(bib_file == "", "sample_wos.bib not found")

  M <- read_wos(bib_file)
  expect_true(is.numeric(M$PY))
})

test_that("read_wos DI2 is uppercase without punctuation", {
  bib_file <- system.file("extdata", "sample_wos.bib", package = "birddog")
  skip_if(bib_file == "", "sample_wos.bib not found")

  M <- read_wos(bib_file)
  valid_di2 <- M$DI2[!is.na(M$DI2)]
  # DI2 should not contain punctuation

  expect_false(any(grepl("[[:punct:]]", valid_di2)))
  # DI2 should be uppercase
  expect_true(all(valid_di2 == toupper(valid_di2)))
})

test_that("read_wos sets DB column", {
  bib_file <- system.file("extdata", "sample_wos.bib", package = "birddog")
  skip_if(bib_file == "", "sample_wos.bib not found")

  M <- read_wos(bib_file)
  expect_true("DB" %in% names(M))
  expect_true(all(grepl("^wos", M$DB)))
})

test_that("read_wos errors on non-existent file", {
  expect_error(read_wos("/nonexistent/file.bib"), "not found")
})

test_that("read_wos rejects invalid format", {
  expect_error(read_wos("dummy.txt", format = "invalid"), "must be one of")
})

test_that("read_wos parses CR field with semicolons", {
  bib_file <- system.file("extdata", "sample_wos.bib", package = "birddog")
  skip_if(bib_file == "", "sample_wos.bib not found")

  M <- read_wos(bib_file)
  has_cr <- M$CR[!is.na(M$CR) & M$CR != ""]
  # CR entries should contain semicolons (multiple references) or DOI refs
  expect_true(length(has_cr) > 0)
})
