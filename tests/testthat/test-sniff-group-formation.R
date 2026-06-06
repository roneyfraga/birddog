# Two births (A {w1,w2}, B {w3,w4}) merge at y2001c1g1 into a shared tail ending
# at c1g1 in 2002. Each is a pre-merge tributary: A brings {w1,w2}, B brings
# {w3,w4}; together they partition c1g1's final papers {w1,w2,w3,w4}.
make_group_formation_dpg <- function() {
  tibble::tibble(
    group_id = c("y2000c1g1", "y2000c1g1", "y2000c1g2", "y2000c1g2",
                 rep("y2001c1g1", 4), rep("y2002c1g1", 4)),
    document_id = c("w1", "w2", "w3", "w4", "w1", "w2", "w3", "w4",
                    "w1", "w2", "w3", "w4"),
    network_until = c(2000L, 2000L, 2000L, 2000L,
                      rep(2001L, 4), rep(2002L, 4)),
    group = c("c1g1", "c1g1", "c1g2", "c1g2", rep("c1g1", 4), rep("c1g1", 4))
  )
}

test_that("sniff_group_formation builds the confluence in the formation contract", {
  r <- detect_soft_trajectories(make_group_formation_dpg(), min_len = 3, min_group_size = 1)
  gf <- sniff_group_formation("c1g1", r, min_papers = 1, min_prop = 0)

  expect_equal(gf$target, "c1g1")
  expect_equal(gf$target_info$group, "c1g1")
  expect_equal(gf$target_info$traj_id, "all")
  expect_equal(gf$target_info$size, 4L)            # |final(c1g1)|
  expect_setequal(gf$feeders$source_key, c("tr1", "tr2"))
  # pre-merge tributaries partition the 4 final papers -> 2 + 2, no double count
  expect_equal(sort(gf$feeders$n), c(2L, 2L))
  expect_equal(gf$total_inflow, 4L)
  expect_true(all(gf$feeders$handoff_year == 2001L))   # converge at y2001c1g1
  expect_true(all(gf$feeders$kept))
  # the field contract the formation plot consumes
  expect_true(all(c("kept", "n", "source_key", "cohort_size", "start_year",
                    "handoff_year", "size_curve", "inflow_curve") %in%
                  names(gf$feeders)))
  expect_s3_class(gf$feeders$size_curve[[1]], "tbl_df")
})

test_that("sniff_group_formation errors for a group no trajectory terminates in", {
  r <- detect_soft_trajectories(make_group_formation_dpg(), min_len = 3, min_group_size = 1)
  expect_error(sniff_group_formation("c1g99", r), "no trajectory terminates")
})
