test_that("exports follow the API grammar (design 2026-06-12, section 3)", {
  exports <- getNamespaceExports("birddog")

  # rule 1: three public verbs + helpers
  off <- exports[!grepl(
    "^(read|sniff|plot|is|validate|mixed|data|fixed|get)_", exports)]
  expect_true(length(off) == 0,
    info = paste("off-grammar exports:", paste(off, collapse = ", ")))

  # rule 8: the plural never survives in a function name
  plural <- exports[grepl("trajectories", exports)]
  expect_true(length(plural) == 0,
    info = paste("plural exports:", paste(plural, collapse = ", ")))

  # rule 2: stock plots are plot_groups_*, never plot_group_*
  expect_false(any(grepl("^plot_group_[^s]", exports)))
})
