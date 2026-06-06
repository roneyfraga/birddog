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

test_that(".terminal_group reads the last-year node's label", {
  expect_equal(.terminal_group(c("y2000c1g3", "y2002c1g7", "y2001c1g5")), "c1g7")
})
