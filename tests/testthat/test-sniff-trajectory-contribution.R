test_that("contribution flags the source year-node docs that reach the target", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  tr <- fl$trajectories
  # the absorbed B-line carries {p3,p4,p5,p1} at its 2001 node y2001c1g2
  src <- tr$traj_id[vapply(tr$nodes, function(n) "y2001c1g2" %in% n, logical(1))]
  res <- sniff_trajectory_contribution(fl, src, 2001, "tr::c1g3")

  expect_setequal(names(res), c("source", "target", "year", "document_id", "in_target"))
  expect_equal(nrow(res), 4)                                    # the full year-node cohort
  expect_setequal(res$document_id, c("p1", "p3", "p4", "p5"))
  # c1g3's final-year community is {q1..q4,p3,p4}: only p3,p4 of the cohort land there
  expect_setequal(res$document_id[res$in_target], c("p3", "p4"))
  expect_equal(sum(res$in_target), 2L)
  expect_true(all(res$source == src) && all(res$target == "tr::c1g3") && all(res$year == 2001L))
})

test_that("contribution returns one self-describing row per cohort document", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  src <- fl$trajectories$traj_id[
    vapply(fl$trajectories$nodes, function(n) "y2001c1g2" %in% n, logical(1))]
  res <- sniff_trajectory_contribution(fl, src, 2001, "tr::c1g3")
  expect_s3_class(res, "tbl_df")
  expect_type(res$in_target, "logical")
  # constant id columns make several triples row-bindable
  res2 <- sniff_trajectory_contribution(fl, src, 2000, "tr::c1g3")
  expect_equal(ncol(dplyr::bind_rows(res, res2)), 5L)
})

test_that("contribution validates flow, ids, and the requested year", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  src <- fl$trajectories$traj_id[
    vapply(fl$trajectories$nodes, function(n) "y2001c1g2" %in% n, logical(1))]
  expect_error(sniff_trajectory_contribution(list(), src, 2001, "tr::c1g3"),
               "sniff_trajectory_braid")
  expect_error(sniff_trajectory_contribution(fl, "tr_ghost", 2001, "tr::c1g3"),
               "'source' must be")
  expect_error(sniff_trajectory_contribution(fl, src, 2001, "tr_ghost"),
               "'target' must be")
  expect_error(sniff_trajectory_contribution(fl, src, 1990, "tr::c1g3"),
               "no node in year")
})
