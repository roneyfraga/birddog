test_that("sniff_trajectory_channel returns a valid flow object", {
  ch <- sniff_trajectory_channel(make_channel_reroute_dpg(), min_group_size = 1)
  expect_s3_class(ch, "birddog_flow")
  expect_true(is_flow(ch))
  expect_silent(validate_flow(ch))
  expect_true("latest_departure" %in% names(ch$trajectories))
  expect_equal(sum(ch$trajectories$type == "central"), 1L)   # one final group
  ce <- ch$trajectories[ch$trajectories$type == "central", ]
  expect_equal(ce$traj_id, "tr::c1g1")
  expect_equal(ce$end, 2002L)
})

test_that("channel reroutes the central where flow's local greed does not", {
  dpg <- make_channel_reroute_dpg()
  ch <- sniff_trajectory_channel(dpg, min_group_size = 1)
  fl <- sniff_trajectory_braid(dpg, min_group_size = 1)

  ch_central <- ch$trajectories$nodes[[which(ch$trajectories$type == "central")]]
  fl_central <- fl$trajectories$nodes[[which(fl$trajectories$type == "central")]]

  # channel takes the high-product B-line; flow takes the heavy-final A-line
  expect_true("y2001c1g2" %in% ch_central)
  expect_true("y2001c1g1" %in% fl_central)
  expect_false(setequal(ch_central, fl_central))
})

test_that("latest_departure reports the freshest birth reaching a central", {
  ch <- sniff_trajectory_channel(make_channel_latedep_dpg(), min_group_size = 1)
  ce <- ch$trajectories[ch$trajectories$type == "central", ]
  expect_equal(ce$start, 2000L)               # the trunk emerged in 2000
  expect_equal(ce$latest_departure, 2003L)    # a 2003-born tributary feeds it
  expect_true(ce$latest_departure > ce$start) # old trunk, fresh input
})

test_that("an isolated final-year birth is a singleton central", {
  ch <- sniff_trajectory_channel(make_channel_isolated_dpg(), min_group_size = 1)
  expect_silent(validate_flow(ch))
  iso <- ch$trajectories[ch$trajectories$traj_id == "tr::c1g2", ]
  expect_equal(nrow(iso), 1L)
  expect_equal(iso$start, 2002L)
  expect_equal(iso$end, 2002L)
  expect_equal(lengths(iso$nodes), 1L)
})

test_that("an extinct lineage is absorbed with group = NA and no absorber", {
  ch <- sniff_trajectory_channel(make_channel_extinct_dpg(), min_group_size = 1)
  expect_silent(validate_flow(ch))
  ext <- ch$trajectories[ch$trajectories$type == "absorbed" &
                           vapply(ch$trajectories$nodes,
                                  function(n) "y2001c1g2" %in% n, logical(1)), ]
  expect_equal(nrow(ext), 1L)
  expect_true(is.na(ext$group))
  expect_true(is.na(ext$absorbed_into))
})

test_that("a weight == 1 edge (cost 0) does not error", {
  dpg <- tibble::tibble(
    group_id = c(rep("y2000c1g1", 3), rep("y2001c1g1", 3)),
    document_id = c(paste0("w", 1:3), paste0("w", 1:3)),   # identical sets -> J = 1
    network_until = c(rep(2000L, 3), rep(2001L, 3)),
    group = rep("c1g1", 6)
  )
  ch <- sniff_trajectory_channel(dpg, min_group_size = 1)
  expect_silent(validate_flow(ch))
})

test_that("channel is deterministic across runs", {
  dpg <- make_channel_reroute_dpg()
  a <- sniff_trajectory_channel(dpg, min_group_size = 1)
  b <- sniff_trajectory_channel(dpg, min_group_size = 1)
  expect_equal(a$trajectories, b$trajectories)
})
