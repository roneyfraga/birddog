test_that("sniff_trajectory_comparison ranks methods and flags cross-signal agreement", {
  dpg <- make_two_central_dpg()
  fl <- sniff_trajectory_braid(dpg, min_group_size = 1)
  ch <- sniff_trajectory_channel(dpg, min_group_size = 1)
  content <- list(coupling = make_content_aligned(),
                  keywords = make_content_aligned())

  cmp <- sniff_trajectory_comparison(list(flow = fl, channel = ch), content)
  expect_s3_class(cmp, "sniff_trajectory_comparison")
  expect_setequal(unique(cmp$summary$method), c("flow", "channel"))
  expect_true(all(c("best_method", "delta") %in% names(cmp$verdict)))
  expect_true("agree" %in% names(cmp$agreement))
  # identical content on both signals -> they agree on the winner
  expect_true(all(cmp$agreement$agree))
})

test_that("sniff_trajectory_comparison reports contested (final_group-divergent) nodes", {
  # Build one real flow, then a second 'method' that assigns the c1g2 lineage to
  # a DIFFERENT final_group (c1g1). This deterministically forces a final_group divergence on
  # exactly the three c1g2 nodes, exercising the contested-node machinery without
  # relying on flow-vs-channel disagreeing (which needs >= 2 final groups to show
  # up at the final_group level).
  dpg <- make_two_central_dpg()
  fl  <- sniff_trajectory_braid(dpg, min_group_size = 1)
  alt <- fl
  alt$trajectories$group[alt$trajectories$group == "c1g2"] <- "c1g1"
  content <- list(coupling = make_content_aligned())

  cmp <- sniff_trajectory_comparison(list(flow = fl, alt = alt), content)
  expect_false(is.null(cmp$contested))
  expect_true("closer_method" %in% names(cmp$contested))
  # the c1g2 nodes are exactly the final_group-divergent ones
  expect_setequal(unique(cmp$contested$node),
                  c("y2000c1g2", "y2001c1g2", "y2002c1g2"))
})

test_that("sniff_trajectory_comparison keeps profiles separate in contested", {
  dpg <- make_two_central_dpg()
  fl  <- sniff_trajectory_braid(dpg, min_group_size = 1)
  alt <- fl
  alt$trajectories$group[alt$trajectories$group == "c1g2"] <- "c1g1"
  content <- list(coupling = make_content_aligned())

  cmp <- sniff_trajectory_comparison(list(flow = fl, alt = alt), content,
                       profile = c("incremental", "full"))
  expect_true("profile" %in% names(cmp$contested))
  expect_setequal(unique(cmp$contested$profile), c("incremental", "full"))
  # three divergent nodes x one signal x two profiles, no many-to-many explosion
  expect_equal(nrow(cmp$contested), 6L)
})

test_that("sniff_trajectory_comparison with one flow returns summary only, with a message", {
  fl <- sniff_trajectory_braid(make_two_central_dpg(), min_group_size = 1)
  content <- list(coupling = make_content_aligned())
  expect_message(
    cmp <- sniff_trajectory_comparison(list(flow = fl), content),
    "nothing to compare")
  expect_true(nrow(cmp$summary) > 0)
  expect_equal(nrow(cmp$verdict), 0L)
  expect_null(cmp$contested)
})
