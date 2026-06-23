# A two-lineage DAG fixture: groups c1g1 and c1g2 tracked across 2000-2002,
# c1g1 always the larger, lineages document-disjoint (no cross edges). Yields a
# sniff_trajectory_dag() object with two terminal groups.
make_dag_fixture <- function() {
  dpg <- tibble::tibble(
    group_id = c(
      rep("y2000c1g1", 3), rep("y2001c1g1", 4), rep("y2002c1g1", 5),
      rep("y2000c1g2", 2), rep("y2001c1g2", 3), rep("y2002c1g2", 4)
    ),
    document_id = c(
      paste0("w", 1:3), paste0("w", 1:4), paste0("w", 1:5),
      paste0("x", 1:2), paste0("x", 1:3), paste0("x", 1:4)
    ),
    network_until = c(
      rep(2000L, 3), rep(2001L, 4), rep(2002L, 5),
      rep(2000L, 2), rep(2001L, 3), rep(2002L, 4)
    ),
    group = c(rep("c1g1", 12), rep("c1g2", 9))
  )
  sniff_trajectory_dag(dpg, min_group_size = 2)
}

test_that("make_dag_fixture builds a two-terminal-group DAG", {
  d <- make_dag_fixture()
  expect_equal(nrow(d$nodes), 6)
  expect_setequal(unique(d$nodes$terminal_group), c("c1g1", "c1g2"))
  expect_equal(d$last_year, 2002)
})
