test_that("as_trajectory_dag resolves dag, lineage object, and docs_per_group", {
  dpg <- make_flow_simple_dpg()
  dag <- sniff_trajectory_dag(dpg, min_group_size = 1)
  gct <- list(groups_similarity = list(), groups_attributes = list(),
              docs_per_group = dpg)

  expect_identical(as_trajectory_dag(dag), dag)

  d_gct <- as_trajectory_dag(gct, min_group_size = 1)
  d_dpg <- as_trajectory_dag(dpg, min_group_size = 1)
  for (d in list(d_gct, d_dpg)) {
    expect_named(d, names(dag))
    expect_identical(d$edges, dag$edges)
    expect_identical(d$nodes, dag$nodes)
    expect_identical(d$births, dag$births)
  }
})

test_that("as_trajectory_dag classes a 1.x dag list saved without the class", {
  dpg <- make_flow_simple_dpg()
  dag <- sniff_trajectory_dag(dpg, min_group_size = 1)
  old <- unclass(dag)                          # 1.x RDS: dag shape, no class
  expect_false(inherits(old, "birddog_dag"))
  coerced <- as_trajectory_dag(old)
  expect_s3_class(coerced, "birddog_dag")
  expect_identical(coerced$nodes, dag$nodes)
  expect_identical(coerced$edges, dag$edges)
})

test_that("as_trajectory_dag rejects garbage with the standard error", {
  expect_error(as_trajectory_dag(42), "sniff_trajectory_dag")
  expect_error(as_trajectory_dag(list(a = 1)), "docs_per_group")
})

test_that("sniff_trajectory_dag accepts gct and returns a classed dag", {
  dpg <- make_flow_simple_dpg()
  gct <- list(groups_similarity = list(), docs_per_group = dpg)
  d1 <- sniff_trajectory_dag(dpg, min_group_size = 1)
  d2 <- sniff_trajectory_dag(gct, min_group_size = 1)
  expect_s3_class(d1, "birddog_dag")
  expect_identical(d1$edges, d2$edges)
  expect_identical(d1$nodes, d2$nodes)
})

test_that("sniff_trajectory_braid accepts dag, gct, and docs_per_group equivalently", {
  dpg <- make_flow_tree_dpg()
  f1 <- sniff_trajectory_braid(sniff_trajectory_dag(dpg, min_group_size = 1),
                              min_group_size = 1)
  f2 <- sniff_trajectory_braid(dpg, min_group_size = 1)
  f3 <- sniff_trajectory_braid(list(docs_per_group = dpg), min_group_size = 1)
  expect_s3_class(f1, "birddog_flow")
  expect_identical(f1$trajectories, f2$trajectories)
  expect_identical(f1$trajectories, f3$trajectories)
})

test_that("a 1.x flow (no class attribute) still passes is_flow and analyses", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  f_old <- unclass(f)
  expect_true(is_flow(f_old))
  absorbed <- f$trajectories$traj_id[f$trajectories$type == "absorbed"][1]
  expect_no_error(sniff_trajectory_destination(f_old, absorbed))
})

test_that("trajectory analyses reject non-flow input with the standard error", {
  expect_error(sniff_trajectory_formation(list()), "sniff_trajectory_braid")
  expect_error(sniff_trajectory_destination(list(), "tr1"), "sniff_trajectory_braid")
  expect_error(sniff_trajectory_self_sufficiency(list()), "sniff_trajectory_braid")
})

test_that("print methods summarize dag and flow", {
  f <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  expect_output(print(sniff_trajectory_dag(make_flow_tree_dpg(), min_group_size = 1)),
                "<birddog_dag>", fixed = TRUE)
  expect_output(print(f), "<birddog_flow>", fixed = TRUE)
  expect_output(print(f), "central")
})
