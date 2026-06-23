# Minimal fixture for plot_groups_lineage_2d(): one group c1g1 whose lineage
# is a 3-node path (y2000 -> y2001 -> y2002). docs_per_group is built so the
# final cluster (y2002c1g1 = d1-d4) is fed partially by the earlier nodes.
make_group_traj_fixture <- function() {
  docs_per_group <- tibble::tribble(
    ~group_id,   ~document_id, ~network_until, ~group,
    "y2000c1g1", "d1", 2000L, "c1g1",
    "y2000c1g1", "d6", 2000L, "c1g1",
    "y2001c1g1", "d1", 2001L, "c1g1",
    "y2001c1g1", "d2", 2001L, "c1g1",
    "y2001c1g1", "d5", 2001L, "c1g1",
    "y2002c1g1", "d1", 2002L, "c1g1",
    "y2002c1g1", "d2", 2002L, "c1g1",
    "y2002c1g1", "d3", 2002L, "c1g1",
    "y2002c1g1", "d4", 2002L, "c1g1",
    "y2001c1g2", "d7", 2001L, "c1g2"
  )
  groups_similarity <- list(c1g1 = tibble::tribble(
    ~from,       ~to,         ~weight,
    "y2000c1g1", "y2001c1g1", 0.5,
    "y2001c1g1", "y2002c1g1", 0.5
  ))
  groups_attributes <- list(c1g1 = tibble::tribble(
    ~group, ~quantity_papers, ~network_until, ~tracked_documents, ~prop_tracked_intra_group, ~PY.sd,
    "c1g1", 10L, 2000L, 3L, 0.5, 2.0,
    "c1g1", 20L, 2001L, 4L, 0.6, 3.0,
    "c1g1", 40L, 2002L, 5L, 0.8, 4.0
  ))
  list(groups_similarity = groups_similarity,
       groups_attributes = groups_attributes,
       docs_per_group = docs_per_group)
}

test_that(".final_group_share_lookup computes per-node share of the final cluster", {
  fx <- make_group_traj_fixture()
  s <- .final_group_share_lookup(fx$docs_per_group, "c1g1")
  expect_equal(unname(s["y2002c1g1"]), 1)       # final cluster: all in itself
  expect_equal(unname(s["y2001c1g1"]), 2 / 3)   # d1,d2 of {d1,d2,d5}
  expect_equal(unname(s["y2000c1g1"]), 0.5)     # d1 of {d1,d6}
  expect_equal(unname(s["y2001c1g2"]), 0)       # d7 not in final cluster
})

test_that(".final_group_share_lookup errors on an unknown group", {
  fx <- make_group_traj_fixture()
  expect_error(.final_group_share_lookup(fx$docs_per_group, "c1g99"), "c1g99")
})

test_that("plot_groups_lineage_2d returns a ggplot with default coloring", {
  fx <- make_group_traj_fixture()
  p <- plot_groups_lineage_2d(fx, group = "c1g1")
  expect_s3_class(p, "ggplot")
})

test_that("plot_groups_lineage_2d colors by final_group_share", {
  fx <- make_group_traj_fixture()
  p <- plot_groups_lineage_2d(fx, group = "c1g1", color_by = "final_group_share")
  expect_s3_class(p, "ggplot")
})
