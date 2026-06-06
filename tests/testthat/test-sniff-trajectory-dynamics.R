test_that("default_state_thresholds carries the v2 knobs", {
  th <- default_state_thresholds()
  expect_named(th, c("emergence_growth", "dormancy_growth", "formation_entropy"))
  expect_true(th$dormancy_growth < th$emergence_growth)
})

test_that(".classify_growth_phase splits emergence / maturity / dormancy", {
  expect_equal(.classify_growth_phase(0.30, 0.15, 0.02), "emergence")
  expect_equal(.classify_growth_phase(0.08, 0.15, 0.02), "maturity")
  expect_equal(.classify_growth_phase(0.01, 0.15, 0.02), "dormancy")
  expect_equal(.classify_growth_phase(NA_real_, 0.15, 0.02), "maturity")
})
