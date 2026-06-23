test_that(".heaviest_predecessor keeps one strongest predecessor per node", {
  e <- tibble::tibble(
    from = c("y2000c1g1", "y2000c1g2"), to = c("y2001c1g1", "y2001c1g1"),
    weight = c(0.7, 0.3), documents = c(7L, 3L)
  )
  s <- .heaviest_predecessor(e)
  expect_equal(unname(s["y2001c1g1"]), "y2000c1g1")
})

test_that("sniff_trajectory_braid splits one central and one absorbed", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  tr <- fl$trajectories
  expect_equal(sum(tr$type == "central"), 1L)
  expect_equal(sum(tr$type == "absorbed"), 1L)
  ce <- tr[tr$type == "central", ]
  expect_equal(ce$traj_id, "tr::c1g1")
  expect_equal(ce$end, 2002L)
  ab <- tr[tr$type == "absorbed", ]
  expect_equal(ab$traj_id, "tr1")
  expect_equal(ab$absorbed_into, "tr::c1g1")
  expect_equal(ab$absorption_year, 2001L)
  expect_equal(ab$group, "c1g1")          # ultimate destination
  expect_equal(ab$depth, 1L)
  expect_equal(nrow(fl$tree), 1L)
})

test_that("sniff_trajectory_braid captures transitive absorption (the tree)", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  tr <- fl$trajectories
  expect_equal(sum(tr$type == "central"), 1L)         # tr::c1g3
  expect_equal(tr$traj_id[tr$type == "central"], "tr::c1g3")
  expect_equal(sum(tr$type == "absorbed"), 2L)
  # the B-line (size 4: p1,p3,p4,p5) is tr1, absorbed into the central at 2002
  b <- tr[tr$absorption_year == 2002L & tr$type == "absorbed", ]
  expect_equal(b$absorbed_into, "tr::c1g3")
  expect_equal(b$depth, 1L)
  # the A-line is absorbed into the B-line (non-central) at 2001
  a <- tr[tr$absorption_year == 2001L & tr$type == "absorbed", ]
  expect_equal(a$absorbed_into, b$traj_id)            # absorbed by another absorbed
  expect_equal(a$depth, 2L)
  expect_equal(a$group, "c1g3")                       # ultimate destination
})

test_that("the canonical detector is sniff_trajectory_braid, not sniff_trajectory_flow", {
  expect_true(is.function(sniff_trajectory_braid))
  exports <- getNamespaceExports("birddog")
  expect_true("sniff_trajectory_braid" %in% exports)
  expect_false("sniff_trajectory_flow" %in% exports)
  # the object vocabulary is untouched
  expect_true(all(c("is_flow", "validate_flow") %in% exports))
})
