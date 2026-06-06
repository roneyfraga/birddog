test_that("sniff_trajectory_projection returns the two one-mode graphs", {
  cg <- sniff_trajectory_group_contribution(make_contribution_fixture())

  pt <- sniff_trajectory_projection(cg, "trajectory")
  pg <- sniff_trajectory_projection(cg, "group")

  expect_true(igraph::is_igraph(pt))
  expect_true(igraph::is_igraph(pg))
  expect_setequal(igraph::V(pt)$name, c("tr1", "tr2", "tr3"))
  expect_setequal(igraph::V(pg)$name, c("c1g1", "c1g2"))

  # tr1 (c1g1) and tr3 (c1g1, c1g2) share c1g1 -> adjacent; tr1, tr2 share none
  expect_true(igraph::are_adjacent(pt, "tr1", "tr3"))
  expect_false(igraph::are_adjacent(pt, "tr1", "tr2"))
})

test_that("sniff_trajectory_projection validates its input", {
  expect_error(sniff_trajectory_projection(list()), "sniff_trajectory_group_contribution")
})
