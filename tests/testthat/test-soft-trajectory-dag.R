test_that(".build_global_edges links consecutive-year clusters by Jaccard", {
  dpg <- tibble::tibble(
    group_id = c(rep("y2000c1g1", 4), rep("y2001c1g1", 4), rep("y2001c1g2", 3)),
    document_id = c(paste0("w", 1:4),          # y2000c1g1
                    paste0("w", c(1:3, 9)),    # y2001c1g1 shares w1-3 with 2000
                    paste0("w", 5:7)),         # y2001c1g2 shares nothing
    network_until = c(rep(2000L, 4), rep(2001L, 7)),
    group = c(rep("c1g1", 4), rep("c1g1", 4), rep("c1g2", 3))
  )
  e <- .build_global_edges(dpg, min_group_size = 1, jaccard_min = 0.05)
  expect_equal(e$from, "y2000c1g1")
  expect_equal(e$to, "y2001c1g1")          # only the sharing successor survives
  expect_equal(e$documents, 3L)
})

test_that(".heaviest_successor keeps one strongest successor per node", {
  e <- tibble::tibble(
    from = c("y2000c1g1", "y2000c1g1"), to = c("y2001c1g1", "y2001c1g2"),
    weight = c(0.6, 0.3), documents = c(6L, 3L)
  )
  s <- .heaviest_successor(e)
  expect_equal(unname(s["y2000c1g1"]), "y2001c1g1")
})

test_that(".forward_terminal_group walks to the sink", {
  s <- c(y2000c1g1 = "y2001c1g1", y2001c1g1 = "y2002c1g4")  # sink y2002c1g4
  expect_equal(.forward_terminal_group(c("y2000c1g1", "y2002c1g4"), s),
               c("c1g4", "c1g4"))
})

test_that("sniff_trajectory_dag assembles nodes, edges, births", {
  dpg <- tibble::tibble(
    group_id = c(rep("y2000c1g1", 3), rep("y2001c1g1", 3), rep("y2002c1g1", 3)),
    document_id = c(paste0("w", 1:3), paste0("w", 1:3), paste0("w", 1:3)),
    network_until = c(rep(2000L, 3), rep(2001L, 3), rep(2002L, 3)),
    group = rep("c1g1", 9)
  )
  d <- sniff_trajectory_dag(dpg, min_group_size = 1)
  expect_setequal(d$nodes$name, c("y2000c1g1", "y2001c1g1", "y2002c1g1"))
  expect_equal(d$births, "y2000c1g1")                 # only the first year is a birth
  expect_true(all(d$nodes$terminal_group == "c1g1"))  # every node funnels to c1g1
  expect_equal(d$last_year, 2002L)
  expect_s3_class(d$nodes, "tbl_df")
  expect_true(igraph::is_igraph(d$graph))
})

test_that("sniff_trajectory_dag accepts a sniff_groups_lineage-shaped list", {
  dpg <- tibble::tibble(
    group_id = c("y2000c1g1", "y2000c1g1"), document_id = c("w1", "w2"),
    network_until = c(2000L, 2000L), group = c("c1g1", "c1g1")
  )
  d <- sniff_trajectory_dag(list(docs_per_group = dpg), min_group_size = 1)
  expect_equal(d$births, "y2000c1g1")
})
