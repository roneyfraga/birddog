test_that(".principal_line follows heaviest successors to the sink", {
  succ <- c(y2000c1g1 = "y2001c1g1", y2001c1g1 = "y2002c1g4")  # sink y2002c1g4
  expect_equal(.principal_line("y2000c1g1", succ),
               c("y2000c1g1", "y2001c1g1", "y2002c1g4"))
  expect_equal(.principal_line("y2002c1g4", succ), "y2002c1g4")  # already a sink
})

# Single chain: one birth, one trajectory, no shared tail.
test_that("detect_soft_trajectories returns one line for a single chain", {
  dpg <- tibble::tibble(
    group_id = c(rep("y2000c1g1", 3), rep("y2001c1g1", 3), rep("y2002c1g1", 3)),
    document_id = rep(paste0("w", 1:3), 3),
    network_until = c(rep(2000L, 3), rep(2001L, 3), rep(2002L, 3)),
    group = rep("c1g1", 9)
  )
  r <- detect_soft_trajectories(dpg, min_len = 3, min_group_size = 1)
  expect_equal(nrow(r$trajectories), 1L)
  tr <- r$trajectories
  expect_equal(tr$traj_id, "tr1")
  expect_equal(tr$terminal_group, "c1g1")
  expect_equal(tr$birth, "y2000c1g1")
  expect_equal(tr$start, 2000L); expect_equal(tr$end, 2002L)
  expect_equal(tr$length, 3L); expect_equal(tr$size, 3L)
  expect_true(tr$living)                     # end == last_year (2002)
  expect_equal(tr$nodes[[1]], c("y2000c1g1", "y2001c1g1", "y2002c1g1"))
  expect_equal(length(tr$shares_tail_with[[1]]), 0L)
  expect_true(is.na(tr$merge_year))
})

# Two births merging: shared tail, merge_year at the convergence node.
test_that("detect_soft_trajectories shares the tail of converging births", {
  dpg <- tibble::tibble(
    group_id = c("y2000c1g1", "y2000c1g1",                 # birth A {w1,w2}
                 "y2000c1g2", "y2000c1g2",                 # birth B {w3,w4}
                 rep("y2001c1g1", 4),                      # merge {w1,w2,w3,w4}
                 rep("y2002c1g1", 4)),                     # terminal {w1,w2,w3,w4}
    document_id = c("w1", "w2", "w3", "w4",
                    "w1", "w2", "w3", "w4",
                    "w1", "w2", "w3", "w4"),
    network_until = c(2000L, 2000L, 2000L, 2000L,
                      rep(2001L, 4), rep(2002L, 4)),
    group = c("c1g1", "c1g1", "c1g2", "c1g2",
              rep("c1g1", 4), rep("c1g1", 4))
  )
  r <- detect_soft_trajectories(dpg, min_len = 3, min_group_size = 1)
  tr <- r$trajectories
  expect_equal(nrow(tr), 2L)
  # both reach the same terminal node, so each lists the other as a tail-mate
  expect_setequal(unlist(tr$shares_tail_with), c("tr1", "tr2"))
  expect_equal(tr$terminal_node, c("y2002c1g1", "y2002c1g1"))
  expect_true(all(tr$merge_year == 2001L))   # converge at y2001c1g1
  # node y2001c1g1 belongs to BOTH trajectories (genuine node sharing)
  shared_node <- "y2001c1g1"
  in_both <- vapply(tr$nodes, function(p) shared_node %in% p, logical(1))
  expect_true(all(in_both))
})

# min_len filters short lines.
test_that("detect_soft_trajectories drops lines shorter than min_len", {
  dpg <- tibble::tibble(
    group_id = c(rep("y2000c1g1", 2), rep("y2001c1g1", 2)),
    document_id = rep(c("w1", "w2"), 2),
    network_until = c(2000L, 2000L, 2001L, 2001L),
    group = rep("c1g1", 4)
  )
  r <- detect_soft_trajectories(dpg, min_len = 3, min_group_size = 1)
  expect_equal(nrow(r$trajectories), 0L)
  expect_s3_class(r$trajectories, "tbl_df")
})
