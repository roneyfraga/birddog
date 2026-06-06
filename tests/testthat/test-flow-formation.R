# Flow-formation tests — Tasks 2 and 3 of Plan 2.
# make_flow_tree_dpg() lives in test-trajectory-flow.R (loaded as a helper
# because testthat sources all test files; but since it's defined in a test
# file rather than helper-*.R, we replicate the fixture inline so this file
# is self-contained when run in isolation).

# ---------------------------------------------------------------------------
# Task 2: .formation_from_flow via the dispatcher
# ---------------------------------------------------------------------------

test_that("flow formation gives a central's direct tributaries", {
  fl <- sniff_trajectory_flow(make_flow_tree_dpg(), min_group_size = 1)
  f <- sniff_trajectory_formation(fl, "tr::c1g3", min_papers = 1, min_prop = 0)
  expect_equal(f$target, "tr::c1g3")
  expect_equal(f$target_info$traj_id, "tr::c1g3")
  expect_true(is.na(f$target_info$group))
  expect_equal(nrow(f$feeders), 1L)              # the B-line (direct child)
  expect_equal(f$feeders$handoff_year, 2002L)
  expect_true(all(c("kept", "n", "source_key", "cohort_size", "start_year",
                    "handoff_year", "size_curve", "inflow_curve") %in%
                  names(f$feeders)))
})

test_that("flow formation recurses: an absorbed trajectory has its own tributaries", {
  fl <- sniff_trajectory_flow(make_flow_tree_dpg(), min_group_size = 1)
  bline <- sniff_trajectory_formation(fl, "tr::c1g3", min_papers = 1,
                                      min_prop = 0)$feeders$source_key[1]
  f2 <- sniff_trajectory_formation(fl, bline, min_papers = 1, min_prop = 0)
  expect_equal(nrow(f2$feeders), 1L)             # the A-line absorbed into the B-line
  expect_equal(f2$feeders$handoff_year, 2001L)
})

test_that("flow formation errors on an unknown target", {
  fl <- sniff_trajectory_flow(make_flow_tree_dpg(), min_group_size = 1)
  expect_error(sniff_trajectory_formation(fl, "tr::nope"), "present in the flow object")
})

# ---------------------------------------------------------------------------
# Task 3: plot_trajectory_formation_2d renders a flow formation cleanly
# ---------------------------------------------------------------------------

test_that("plot_trajectory_formation_2d renders a flow formation cleanly", {
  fl <- sniff_trajectory_flow(make_flow_tree_dpg(), min_group_size = 1)
  f <- sniff_trajectory_formation(fl, "tr::c1g3", min_papers = 1, min_prop = 0)
  p <- plot_trajectory_formation_2d(f)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  # label must be bare traj_id (not "NA::tr::c1g3")
  expect_equal(p$labels$title, "tr::c1g3")
})
