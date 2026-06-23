# Flow-destination tests — Task 1 of Plan 3.
# make_flow_simple_dpg() lives in helper-trajectory-flow.R.

test_that("flow destination distributes a terminal cohort and finds the absorber", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  d <- sniff_trajectory_destination(fl, "tr1")
  expect_equal(d$target, "tr1")
  expect_equal(d$continuation_traj, "tr::c1g1")        # absorbed into the central
  expect_equal(d$cohort_size, 2L)                       # {w4, w5}
  expect_equal(d$dormant_share, 0.5)                    # w5 never reaches the final cluster
  expect_equal(d$destination$n[d$destination$g_final == "c1g1"], 1L)  # w4 lands in c1g1
})

test_that("flow destination errors on an unknown trajectory", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  expect_error(sniff_trajectory_destination(fl, "trX"), "present in the flow object")
})
