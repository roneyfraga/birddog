test_that("subset filters by predicate and stays a valid flow", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  s <- subset(f, type == "central")
  expect_true(all(s$trajectories$type == "central"))
  expect_no_error(validate_flow(s))
  expect_match(attr(s, "pruned"), "type == \"central\"", fixed = TRUE)
})

test_that("subset target keeps the watershed (central + subtree)", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  central <- f$trajectories$traj_id[f$trajectories$type == "central"][1]
  s <- subset(f, target = central)
  expect_true(central %in% s$trajectories$traj_id)
  expect_true(all(s$tree$parent %in% s$trajectories$traj_id))
  expect_true(all(s$tree$child %in% s$trajectories$traj_id))
})

test_that("subset reattaches orphaned children by transitive bypass", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  # drop one intermediate absorber and check its child points to a kept ancestor
  inter <- f$tree$parent[f$tree$parent %in%
    f$trajectories$traj_id[f$trajectories$type == "absorbed"]][1]
  skip_if(is.na(inter) || length(inter) == 0)
  s <- subset(f, traj_id != inter)
  kid <- f$tree$child[f$tree$parent == inter][1]
  new_parent <- s$trajectories$absorbed_into[s$trajectories$traj_id == kid]
  expect_false(identical(new_parent, inter))
  expect_true(is.na(new_parent) || new_parent %in% s$trajectories$traj_id)
})

test_that("subset dispatches for future detectors inheriting the contract class", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  g <- structure(f, class = c("birddog_custom", class(f)))
  s <- subset(g, type == "central")
  expect_s3_class(s, "birddog_custom")
  expect_s3_class(s, "birddog_flow")
})

test_that("an empty subset errors clearly", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  expect_error(subset(f, size < 0), "keeps no trajectories")
})

test_that("subset recomputes depth and group after bypass reattachment", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  a_id <- f$trajectories$traj_id[f$trajectories$type == "absorbed" &
                                   f$trajectories$depth == 2L]
  b_id <- f$trajectories$traj_id[f$trajectories$type == "absorbed" &
                                   f$trajectories$depth == 1L]
  stopifnot(length(a_id) == 1L, length(b_id) == 1L)
  s <- subset(f, traj_id != b_id)
  a_row <- s$trajectories[s$trajectories$traj_id == a_id, ]
  expect_identical(a_row$depth, 1L)
  expect_identical(a_row$group, "c1g3")
  expect_no_error(validate_flow(s))
})

test_that("subset orphans get NA group/depth when no central survives upstream", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  a_id <- f$trajectories$traj_id[f$trajectories$type == "absorbed" &
                                   f$trajectories$depth == 2L]
  b_id <- f$trajectories$traj_id[f$trajectories$type == "absorbed" &
                                   f$trajectories$depth == 1L]
  stopifnot(length(a_id) == 1L, length(b_id) == 1L)
  s <- subset(f, type == "absorbed")
  a_row <- s$trajectories[s$trajectories$traj_id == a_id, ]
  b_row <- s$trajectories[s$trajectories$traj_id == b_id, ]
  expect_true(is.na(a_row$group))
  expect_true(is.na(a_row$depth))
  expect_true(is.na(b_row$group))
  expect_true(is.na(b_row$depth))
  expect_no_error(validate_flow(s))
})
