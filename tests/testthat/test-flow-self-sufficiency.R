# Flow-self-sufficiency tests — Task 2 of Plan 3.
# make_flow_simple_dpg() lives in helper-trajectory-destination.R.

test_that("flow self_sufficiency is the endogenous fraction per central", {
  fl <- sniff_trajectory_flow(make_flow_simple_dpg(), min_group_size = 1)
  ss <- sniff_trajectory_self_sufficiency(fl, min_size = 1)
  expect_equal(nrow(ss), 1L)
  expect_equal(ss$central, "tr::c1g1")
  expect_equal(ss$size, 4L)                 # spine papers {w1,w2,w3,w4}
  expect_equal(ss$inflow, 1L)               # w4 also carried by the tributary
  expect_equal(ss$self_sufficiency, 0.75)
})
