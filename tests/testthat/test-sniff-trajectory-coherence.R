test_that(".node_docs splits documents by node", {
  nd <- .node_docs(make_two_central_dpg())
  expect_setequal(nd[["y2000c1g1"]], c("p1", "p2"))
  expect_setequal(nd[["y2002c1g1"]], paste0("p", 1:6))
})

test_that("incremental profiles partition the corpus (no shared documents)", {
  nd  <- .node_docs(make_two_central_dpg())
  inc <- .node_docs_incremental(nd)
  # first year keeps all its docs
  expect_setequal(inc[["y2000c1g1"]], c("p1", "p2"))
  # later years keep ONLY the newly arrived docs
  expect_setequal(inc[["y2001c1g1"]], c("p3", "p4"))
  expect_setequal(inc[["y2002c1g1"]], c("p5", "p6"))
  # every document is incremental in exactly one node -> disjoint partition
  all_inc <- unlist(inc, use.names = FALSE)
  expect_false(any(duplicated(all_inc)))
  expect_setequal(all_inc, unique(make_two_central_dpg()$document_id))
})

test_that(".stack_profiles converts a named list to a two-column data frame", {
  nd  <- .node_docs(make_two_central_dpg())
  inc <- .node_docs_incremental(nd)
  df  <- .stack_profiles(inc)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("node", "document_id"))
  # every incremental doc appears exactly once
  expect_equal(nrow(df), length(unlist(inc, use.names = FALSE)))
  # a node with zero incremental docs produces no rows (not an error)
  empty <- .stack_profiles(list(ghost = character(0)))
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("node", "document_id"))
})

test_that(".cosine_distance is Salton cosine on feature counts", {
  # node A: R1 x2, R2 x1 ; node B: R1 x1, R3 x1
  nf <- tibble::tibble(
    node = c("A", "A", "A", "B", "B"),
    document_id = c("d1", "d2", "d3", "d4", "d5"),
    feature = c("R1", "R1", "R2", "R1", "R3"))
  m <- .feature_count_matrix(nf)
  d <- .cosine_distance(m)
  # cos = (2*1) / (sqrt(4+1) * sqrt(1+1)) = 2/sqrt(10)
  expect_equal(unname(d["A", "B"]), 1 - 2 / sqrt(10), tolerance = 1e-8)
  expect_equal(unname(diag(d)), c(0, 0))
})

test_that("incremental profile removes the cumulative carry-over, full keeps it", {
  nd  <- .node_docs(make_carryover_dpg())
  inc <- .node_docs_incremental(nd)
  feat <- make_carryover_content()
  dist_for <- function(prof) {
    nf <- merge(.stack_profiles(prof), feat, by = "document_id")
    .cosine_distance(.feature_count_matrix(nf))
  }
  d_full <- dist_for(nd)
  d_inc  <- dist_for(inc)
  a <- "y2000c1g1"; b <- "y2001c1g1"
  expect_lt(d_full[a, b], 0.5)      # full: shares R1 via carried p1,p2 -> close
  expect_equal(unname(d_inc[a, b]), 1, tolerance = 1e-8)  # incremental: R1 vs R9 -> orthogonal
})

test_that(".silhouette matches a hand-computed two-cluster case", {
  nodes <- c("n1", "n2", "n3", "n4")
  d <- matrix(0.8, 4, 4, dimnames = list(nodes, nodes))
  d["n1", "n2"] <- d["n2", "n1"] <- 0.2   # cluster X
  d["n3", "n4"] <- d["n4", "n3"] <- 0.2   # cluster Y
  diag(d) <- 0
  lab <- c(n1 = "X", n2 = "X", n3 = "Y", n4 = "Y")
  s <- .silhouette(d, lab)
  # a = 0.2, b = 0.8 -> sil = (0.8 - 0.2) / 0.8 = 0.75 for every node
  expect_equal(s$sil, rep(0.75, 4), tolerance = 1e-8)
})

test_that(".silhouette gives 0 to singletons and to a single-cluster set", {
  nodes <- c("n1", "n2", "n3")
  d <- matrix(0.5, 3, 3, dimnames = list(nodes, nodes)); diag(d) <- 0
  s_single <- .silhouette(d, c(n1 = "X", n2 = "X", n3 = "Y"))
  expect_equal(s_single$sil[s_single$node == "n3"], 0)   # Y is a singleton
  s_one <- .silhouette(d, c(n1 = "X", n2 = "X", n3 = "X"))
  expect_equal(s_one$sil, rep(0, 3))                     # only one cluster
})

test_that("sniff_trajectory_coherence returns nodes + summary and rewards content coherence", {
  fl <- sniff_trajectory_braid(make_two_central_dpg(), min_group_size = 1)

  aligned   <- sniff_trajectory_coherence(fl, list(coupling = make_content_aligned()))
  scrambled <- sniff_trajectory_coherence(fl, list(coupling = make_content_scrambled()))

  expect_s3_class(aligned, "sniff_trajectory_coherence")
  expect_true(all(c("node", "signal", "profile", "resolution", "n_docs",
                    "a", "b", "sil", "year", "traj_id", "final_group")
                  %in% names(aligned$nodes)))
  expect_true(all(c("signal", "profile", "resolution", "mean_sil", "n_nodes",
                    "n_singletons", "n_excluded", "coverage")
                  %in% names(aligned$summary)))

  final_group_mean <- function(x) {
    s <- x$summary
    s$mean_sil[s$resolution == "final_group"]
  }
  expect_gt(final_group_mean(aligned), final_group_mean(scrambled))
  expect_equal(final_group_mean(aligned), 1, tolerance = 1e-8)
})

test_that("sniff_trajectory_coherence runs both profiles and reports coverage", {
  fl <- sniff_trajectory_braid(make_two_central_dpg(), min_group_size = 1)
  ap <- sniff_trajectory_coherence(fl, list(coupling = make_content_aligned()),
                      profile = c("incremental", "full"))
  expect_setequal(unique(ap$summary$profile), c("incremental", "full"))
  # every paper carries a feature -> full coverage
  expect_equal(unique(ap$summary$coverage), 1)
})

test_that("sniff_trajectory_coherence excludes nodes whose documents carry no feature", {
  fl <- sniff_trajectory_braid(make_two_central_dpg(), min_group_size = 1)
  # only p-papers get features; the c1g2 final_group is unprofilable
  partial <- make_content_aligned()
  partial <- partial[grepl("^p", partial$document_id), ]
  ap <- sniff_trajectory_coherence(fl, list(coupling = partial))
  expect_true(all(ap$nodes$final_group == "c1g1"))
  expect_gt(ap$summary$n_excluded[ap$summary$resolution == "final_group"], 0)
  expect_lt(unique(ap$summary$coverage), 1)
})

test_that("sniff_trajectory_coherence honours the signals subset and errors on none", {
  fl <- sniff_trajectory_braid(make_two_central_dpg(), min_group_size = 1)
  content <- list(coupling = make_content_aligned(),
                  keywords = make_content_scrambled())
  ap <- sniff_trajectory_coherence(fl, content, signals = "coupling")
  expect_setequal(unique(ap$summary$signal), "coupling")
  expect_setequal(unique(ap$nodes$signal), "coupling")
  expect_error(
    sniff_trajectory_coherence(fl, content, signals = "nonexistent"),
    "no requested signal")
})
