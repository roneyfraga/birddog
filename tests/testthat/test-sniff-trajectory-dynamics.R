test_that("default_state_thresholds carries the flow knobs", {
  th <- default_state_thresholds()
  expect_true(all(c("emergence_growth", "dormancy_growth", "convergence_entropy",
                    "dormancy_share") %in% names(th)))
  expect_true(th$dormancy_growth < th$emergence_growth)
})

test_that(".classify_flow_state covers the five states", {
  th <- default_state_thresholds()
  d <- tibble::tibble(
    type = c("central", "central", "central", "absorbed", "absorbed", "absorbed"),
    group = c("c1g1", "c1g1", "c1g1", "c1g2", "c1g3", NA),
    recent_growth = c(0.30, 0.05, 0.00, NA, NA, NA),
    dest_entropy = c(NA, NA, NA, 0.20, 0.90, NA),
    dormant_share = c(NA, NA, NA, 0.10, 0.10, NA)
  )
  expect_equal(.classify_flow_state(d, th),
               c("emergence", "maturity", "dormancy", "convergence", "divergence", "dormancy"))
})

test_that("sniff_trajectory_dynamics runs on a flow object", {
  fl <- sniff_trajectory_flow(make_flow_simple_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  expect_true(all(c("traj_id", "type", "group", "age", "recent_growth",
                    "dest_entropy", "dormant_share", "emergence_score", "state") %in%
                  names(dyn)))
  expect_true(all(dyn$state %in% c("emergence", "maturity", "dormancy",
                                   "convergence", "divergence")))
  expect_equal(dyn$state[dyn$traj_id == "tr::c1g1"], "emergence")  # growing central
})

test_that("sniff_trajectory_dynamics rejects a non-flow object", {
  expect_error(sniff_trajectory_dynamics(list()), "sniff_trajectory_flow")
})
