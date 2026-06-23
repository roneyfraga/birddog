test_that("sniff_trajectory_entropy gives a per-year Pielou series as a list-column", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  keywords <- tibble::tribble(
    ~document_id, ~keyword,
    "w1", "kA", "w1", "kB", "w2", "kA", "w3", "kB",
    "w4", "kC", "w5", "kC", "w5", "kD")
  out <- sniff_trajectory_entropy(fl, keywords)
  expect_true(all(c("traj_id", "type", "group", "keyword_entropy") %in% names(out)))
  expect_true(is.list(out$keyword_entropy))
  e <- out$keyword_entropy[[which(out$traj_id == "tr::c1g1")]]
  expect_true(all(c("year", "keyword_entropy") %in% names(e)))
  expect_equal(sort(as.numeric(e$year)), c(2000, 2001, 2002))   # one value per node-year
  expect_equal(e$keyword_entropy[e$year == 2000], 1.0)          # {w1,w2,w3}: kA:2,kB:2 -> J'=1
  expect_true(all(e$keyword_entropy >= 0 & e$keyword_entropy <= 1, na.rm = TRUE))
})

test_that("sniff_trajectory_entropy validates its inputs", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  expect_error(sniff_trajectory_entropy(list(), tibble::tibble()), "sniff_trajectory_braid")
  expect_error(sniff_trajectory_entropy(fl, tibble::tibble(x = 1)), "document_id")
})
