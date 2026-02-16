test_that("sniff_groups_terms rejects invalid algorithm", {
  groups <- make_sniff_groups_output()
  expect_error(sniff_groups_terms(groups, algorithm = "bogus"), "algorithm must be one of")
})
