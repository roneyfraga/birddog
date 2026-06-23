test_that("sniff_trajectory_emergence_owners rolls trajectory emergence up to authors", {
  fl  <- sniff_trajectory_braid(make_flow_three_centrals_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  # AX writes 2 docs of c1g2 only; AY one of c1g1; AZ one of c1g3
  authors <- tibble::tibble(
    document_id = c("b1", "b2", "a1", "c1"),
    author      = c("AX", "AX", "AY", "AZ"))
  ow <- sniff_trajectory_emergence_owners(fl, dyn, authors)
  expect_true(all(c("author", "total", "ndocs", "norm") %in% names(ow)))
  ei_c1g2 <- dyn$emergence_index[dyn$traj_id == "tr::c1g2"]
  ax <- ow[ow$author == "AX", ]
  expect_equal(ax$ndocs, 2L)                       # 2 docs of c1g2
  expect_equal(ax$total, ei_c1g2 * 2)              # weighted by c1g2's emergence
  expect_equal(ax$norm, ei_c1g2 * 2 / sqrt(2))     # Garner normalization
  # sorted by descending total
  expect_false(is.unsorted(rev(ow$total)))
})

test_that("sniff_trajectory_emergence_owners ignores non-living trajectories and bad input", {
  fl  <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  # p2 sits only in the absorbed lineage c1g1 (emergence_index NA) -> author dropped
  authors <- tibble::tibble(document_id = c("q1", "p2"), author = c("AQ", "AP"))
  ow <- sniff_trajectory_emergence_owners(fl, dyn, authors)
  expect_true("AQ" %in% ow$author)
  expect_false("AP" %in% ow$author)
  expect_error(sniff_trajectory_emergence_owners(list(), dyn, authors),
               "sniff_trajectory_braid")
  expect_error(sniff_trajectory_emergence_owners(fl, dyn, tibble::tibble(x = 1)),
               "document_id")
})
