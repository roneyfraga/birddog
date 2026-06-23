# Flow-self-sufficiency tests — Task 2 of Plan 3.
# make_flow_simple_dpg() lives in helper-trajectory-flow.R.

test_that("flow self_sufficiency is the endogenous fraction of the final community", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  ss <- sniff_trajectory_self_sufficiency(fl, min_size = 1)
  expect_equal(nrow(ss), 1L)
  expect_equal(ss$central, "tr::c1g1")
  expect_equal(ss$final_size, 4L)           # final community {w1,w2,w3,w4}
  expect_equal(ss$inflow, 1L)               # w4 delivered by the tributary's cohort
  expect_equal(ss$self_sufficiency, 0.75)
})

test_that("flow self_sufficiency counts secondary inflow from non-owned tributaries", {
  # tr10 is dominantly assigned to c1g1 (group = "c1g1"), but its terminal cohort
  # {x1,x2,x3} seeds two finals: x1,x2 -> c1g1, x3 -> c1g2 (a secondary delivery).
  # The old ownership filter (group == g) gave c1g2 a false 1.0; the destination
  # measure must register the x3 it received.
  dpg <- tibble::tibble(
    group_id = c(rep("y2003c1g1", 4), rep("y2003c1g2", 3), rep("y2000c1g9", 3)),
    document_id = c("a1", "a2", "x1", "x2",  "b1", "b2", "x3",  "x1", "x2", "x3"),
    network_until = c(rep(2003L, 7), rep(2000L, 3)),
    group = c(rep("c1g1", 4), rep("c1g2", 3), rep("c1g9", 3))
  )
  flow <- list(
    trajectories = tibble::tibble(
      traj_id = c("tr::c1g1", "tr::c1g2", "tr10"),
      type = c("central", "central", "absorbed"),
      group = c("c1g1", "c1g2", "c1g1"),
      absorbed_into = c(NA, NA, "tr::c1g1"),
      nodes = list("y2003c1g1", "y2003c1g2", "y2000c1g9")
    ),
    docs_per_group = dpg,
    last_year = 2003L
  )
  ss <- sniff_trajectory_self_sufficiency(flow, min_size = 1)

  c2 <- ss[ss$group == "c1g2", ]
  expect_equal(c2$inflow, 1L)                 # x3, a secondary delivery
  expect_lt(c2$self_sufficiency, 1)           # NOT falsely endogenous
  expect_equal(c2$self_sufficiency, 1 - 1 / 3)
  c1 <- ss[ss$group == "c1g1", ]
  expect_equal(c1$inflow, 2L)                 # x1, x2
  expect_equal(c1$self_sufficiency, 0.5)
})
