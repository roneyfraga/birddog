test_that("confluence object has rivers + confluences with the right shape", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  conf <- sniff_trajectory_confluence(fl)

  expect_named(conf, c("rivers", "confluences", "destinations", "centrals",
                       "last_year"))
  expect_equal(conf$last_year, 2002L)
  expect_equal(conf$centrals, "c1g1")

  expect_equal(nrow(conf$rivers), nrow(fl$trajectories))
  expect_true(all(c("traj_id", "type", "central", "size", "start",
                    "handoff_year", "depth", "parent", "size_curve") %in%
                  names(conf$rivers)))
  expect_true(is.list(conf$rivers$size_curve))
  expect_s3_class(conf$rivers$size_curve[[1]], "data.frame")
  ce <- conf$rivers[conf$rivers$type == "central", ]
  expect_equal(ce$handoff_year, 2002L)

  expect_equal(nrow(conf$confluences), nrow(fl$tree))
  expect_true(all(c("child", "parent", "n", "cohort_size",
                    "first_feed_year", "handoff_year", "inflow_curve") %in%
                  names(conf$confluences)))
  cf <- conf$confluences
  expect_equal(cf$child, "tr1")
  expect_equal(cf$parent, "tr::c1g1")
  expect_true(all(cf$n <= cf$cohort_size))
  expect_true(all(cf$first_feed_year <= cf$handoff_year))
  expect_equal(cf$handoff_year, 2001L)
})

test_that("confluence captures nested edges (A->B->C) one row per tree edge", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  conf <- sniff_trajectory_confluence(fl)

  expect_equal(conf$centrals, "c1g3")
  expect_equal(nrow(conf$confluences), nrow(fl$tree))
  expect_setequal(conf$confluences$parent, unique(fl$tree$parent))
  key <- function(d) paste(d$child, d$parent)
  expect_true(all(key(conf$confluences) %in% key(fl$tree)))
  expect_true(all(conf$confluences$n <= conf$confluences$cohort_size))
  expect_true(all(conf$confluences$first_feed_year <= conf$confluences$handoff_year))
})

test_that("first_feed_year is the first positive inflow year", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  conf <- sniff_trajectory_confluence(fl)
  ic <- conf$confluences$inflow_curve[[1]]
  expect_equal(conf$confluences$first_feed_year[1], ic$year[1])
})

test_that("sniff_trajectory_confluence accepts a docs_per_group directly", {
  conf <- sniff_trajectory_confluence(make_flow_simple_dpg(), min_group_size = 1)
  expect_equal(conf$centrals, "c1g1")
  expect_equal(nrow(conf$confluences), 1L)
})

test_that("sniff_trajectory_confluence validates its input", {
  expect_error(sniff_trajectory_confluence(42), "flow|docs_per_group|sniff")
})

test_that("zero-edge flow produces empty typed confluences", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  fl$tree <- fl$tree[integer(0), ]
  conf <- sniff_trajectory_confluence(fl)
  expect_equal(nrow(conf$confluences), 0L)
  expect_equal(names(conf$confluences),
               c("child", "parent", "n", "cohort_size",
                 "first_feed_year", "handoff_year", "inflow_curve"))
})
