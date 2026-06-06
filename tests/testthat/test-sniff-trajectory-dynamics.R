test_that("sniff_trajectory_dynamics marks a growing solo lineage as emergence", {
  r <- detect_soft_trajectories(make_growth_dpg(), min_len = 3, min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(r)
  expect_equal(nrow(dyn), 1L)
  expect_equal(dyn$growth_phase, "emergence")     # size 1 -> 4 over the window
  expect_false(dyn$converges)                      # solo: no merge
  expect_true(is.na(dyn$merge_year))
  expect_equal(dyn$age, 3L)                         # 2002 - 2000 + 1
  expect_true(all(c("traj_id", "terminal_group", "age", "recent_growth",
                    "growth_phase", "converges", "emergence_score") %in% names(dyn)))
})

test_that("sniff_trajectory_dynamics flags converging lineages", {
  r <- detect_soft_trajectories(make_group_formation_dpg(), min_len = 3, min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(r)
  expect_true(all(dyn$converges))                  # tr1 and tr2 merge
  expect_true(all(dyn$merge_year == 2001L))
})

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
