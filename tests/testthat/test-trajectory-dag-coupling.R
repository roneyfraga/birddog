# sniff_trajectory_dag(similarity = "coupling"): bibliographic-coupling edges

test_that("coupling links nodes with no shared documents but shared references", {
  dpg <- tibble::tibble(
    group_id      = c("y2010c1g1", "y2010c1g1", "y2011c1g2", "y2011c1g2"),
    document_id   = c("d1", "d2", "d3", "d4"),          # disjoint document sets
    network_until = c(2010, 2010, 2011, 2011),
    group         = c("c1g1", "c1g1", "c1g2", "c1g2")
  )
  refs <- tibble::tibble(
    document_id = rep(c("d1", "d2", "d3", "d4"), each = 2),
    feature     = rep(c("R1", "R2"), times = 4)         # every paper cites {R1, R2}
  )

  # overlap (default): the two nodes share no documents -> no edge
  ov <- sniff_trajectory_dag(dpg, min_group_size = 2)
  expect_equal(nrow(ov$edges), 0L)

  # coupling: both nodes cite {R1, R2}, Jaccard 1 -> one edge
  cp <- sniff_trajectory_dag(dpg, min_group_size = 2,
                             similarity = "coupling", references = refs)
  expect_s3_class(cp, "birddog_dag")
  expect_equal(nrow(cp$edges), 1L)
  expect_equal(cp$edges$from, "y2010c1g1")
  expect_equal(cp$edges$to, "y2011c1g2")
  expect_equal(cp$edges$weight, 1)
  expect_equal(cp$edges$documents, 2L)   # shared references, the tie-break count
})

test_that("similarity = 'coupling' errors without references", {
  dpg <- tibble::tibble(
    group_id = c("y2010c1g1", "y2010c1g1"), document_id = c("d1", "d2"),
    network_until = c(2010, 2010), group = c("c1g1", "c1g1"))
  expect_error(
    sniff_trajectory_dag(dpg, min_group_size = 2, similarity = "coupling"),
    "requires"
  )
})

test_that("a coupling DAG is a drop-in: braid routes it into a valid flow", {
  # one community over three years, disjoint docs each year (no carry-over),
  # so overlap would find no edges; coupling links them by shared references.
  dpg <- tibble::tibble(
    group_id      = rep(c("y2010c1g1", "y2011c1g1", "y2012c1g1"), each = 2),
    document_id   = paste0("d", 1:6),
    network_until = rep(c(2010, 2011, 2012), each = 2),
    group         = rep("c1g1", 6)
  )
  refs <- tibble::tibble(document_id = rep(paste0("d", 1:6), each = 2),
                         feature = rep(c("R1", "R2"), times = 6))

  expect_equal(nrow(sniff_trajectory_dag(dpg, min_group_size = 2)$edges), 0L)

  cp <- sniff_trajectory_dag(dpg, min_group_size = 2,
                             similarity = "coupling", references = refs)
  br <- sniff_trajectory_braid(cp)
  expect_s3_class(br, "birddog_flow")
  expect_no_error(validate_flow(br))
  expect_equal(sum(br$trajectories$type == "central"), 1L)   # the chain reaches 2012
})

test_that("coherence warns on a coupling-routed flow, but not an overlap one", {
  dpg <- tibble::tibble(
    group_id      = rep(c("y2010c1g1", "y2011c1g1", "y2012c1g1"), each = 2),
    document_id   = paste0("d", 1:6),
    network_until = rep(c(2010, 2011, 2012), each = 2),
    group         = rep("c1g1", 6))
  refs <- tibble::tibble(document_id = rep(paste0("d", 1:6), each = 2),
                         feature = rep(c("R1", "R2"), times = 6))

  br_cp <- sniff_trajectory_braid(
    sniff_trajectory_dag(dpg, min_group_size = 2, similarity = "coupling", references = refs))
  expect_identical(attr(br_cp, "similarity"), "coupling")     # provenance propagated
  expect_warning(sniff_trajectory_coherence(br_cp, list(coupling = refs)), "circular")

  br_ov <- sniff_trajectory_braid(sniff_trajectory_dag(dpg, min_group_size = 2))
  expect_identical(attr(br_ov, "similarity"), "overlap")      # guard stays silent
})
