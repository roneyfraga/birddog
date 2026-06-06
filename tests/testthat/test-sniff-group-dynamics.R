test_that("sniff_group_dynamics returns phase and (with contribution) formation", {
  fx <- make_contribution_fixture()
  cg <- sniff_trajectory_group_contribution(fx)
  gd <- sniff_group_dynamics(fx$docs_per_group, contribution = cg)
  expect_true(all(c("group", "size", "recent_growth", "phase") %in% names(gd)))
  expect_true(all(c("source_entropy", "n_sources", "formation") %in% names(gd)))
  expect_true(all(gd$phase %in% c("emergence", "maturity", "dormancy")))
  expect_true(all(gd$formation %in% c("convergent", "divergent")))
})

test_that("sniff_group_dynamics works without a contribution", {
  fx <- make_contribution_fixture()
  gd <- sniff_group_dynamics(fx$docs_per_group)
  expect_false("formation" %in% names(gd))
  expect_true("phase" %in% names(gd))
})
