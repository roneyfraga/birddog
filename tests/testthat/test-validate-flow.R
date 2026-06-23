test_that("is_flow recognises a flow object by class and by structure", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  expect_true(is_flow(fl))
  expect_true(inherits(fl, "birddog_flow"))
  expect_true(is_flow(unclass(fl)))                       # structural fallback
  expect_false(is_flow(make_flow_tree_dpg()))             # a bare tibble
  expect_false(is_flow(list(trajectories = tibble::tibble(x = 1))))  # no absorbed_into
  expect_false(is_flow(42))
})

test_that("sniff_trajectory_braid output passes validate_flow and is returned", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  expect_identical(validate_flow(fl), fl)
})

test_that("validate_flow rejects a missing structural element or column", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  no_tree <- fl; no_tree$tree <- NULL
  expect_error(validate_flow(no_tree), "missing element")

  no_col <- fl; no_col$trajectories$group <- NULL
  expect_error(validate_flow(no_col), "missing column")

  expect_error(validate_flow(fl$trajectories), "expected a list")
})

test_that("validate_flow catches a node label absent from the triple key", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  bad <- fl
  bad$trajectories$nodes[[1]][1] <- "y9999c9g9"          # format ok, key missing
  expect_error(validate_flow(bad), "docs_per_group\\$group_id")
})

test_that("validate_flow catches a dangling absorbed_into reference", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  bad <- fl
  ab <- which(bad$trajectories$type == "absorbed")[1]
  bad$trajectories$absorbed_into[ab] <- "tr_ghost"
  expect_error(validate_flow(bad), "unknown traj_id")
})

test_that("validate_flow reports every violation in one error", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  bad <- fl
  bad$trajectories$nodes[[1]][1] <- "zzz"                # bad format + missing key
  ab <- which(bad$trajectories$type == "absorbed")[1]
  bad$trajectories$absorbed_into[ab] <- "tr_ghost"       # dangling reference
  err <- tryCatch(validate_flow(bad), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "node label")
  expect_match(conditionMessage(err), "unknown traj_id")
})

test_that("validate_flow rejects a tree missing an edge", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  f$tree <- f$tree[-1, , drop = FALSE]
  expect_error(validate_flow(f), "tree")
})

test_that("validate_flow rejects a rewired tree parent", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  other <- setdiff(f$trajectories$traj_id,
                   c(f$tree$child[1], f$tree$parent[1]))[1]
  f$tree$parent[1] <- other
  expect_error(validate_flow(f), "tree")
})

test_that("validate_flow rejects a tree without the contract columns", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  f$tree <- data.frame(a = 1)
  expect_error(validate_flow(f), "tree")
})

test_that(".flow_has_cycle detects a cycle and clears an acyclic forest", {
  cyc <- tibble::tibble(traj_id = c("a", "b", "c"), absorbed_into = c("b", "a", NA))
  acyc <- tibble::tibble(traj_id = c("a", "b", "c"), absorbed_into = c("b", "c", NA))
  expect_true(.flow_has_cycle(cyc))
  expect_false(.flow_has_cycle(acyc))
})
