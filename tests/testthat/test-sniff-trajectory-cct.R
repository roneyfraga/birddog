# make_flow_simple_dpg() lives in helper-trajectory-braid.R: central c1g1 spans
# 2000-2002; tributary c1g2 is born and absorbed in 2000.

test_that("sniff_trajectory_cct gives a per-year CCT series from document references", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  refs <- tibble::tribble(
    ~document_id, ~ref_age,
    "w1", 2, "w1", 4, "w2", 6, "w3", 8, "w4", 1, "w4", 3, "w5", 5)
  out <- sniff_trajectory_cct(fl, refs)
  expect_true(all(c("traj_id", "type", "group", "cct") %in% names(out)))
  expect_true(is.list(out$cct))
  c1 <- out$cct[[which(out$traj_id == "tr::c1g1")]]
  expect_true(all(c("year", "cct") %in% names(c1)))
  # per-doc median ages: w1=3, w2=6, w3=8, w4=2
  expect_equal(c1$cct[c1$year == 2000], 6)     # node {w1,w2,w3}: median(3,6,8)
  expect_equal(c1$cct[c1$year == 2001], 4.5)   # node {w1,w2,w3,w4}: median(2,3,6,8)
})

test_that("sniff_trajectory_cct returns NA for a node whose documents lack references", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  refs <- tibble::tibble(document_id = "zzz", ref_age = 5)  # matches no node document
  out <- sniff_trajectory_cct(fl, refs)
  c1 <- out$cct[[which(out$traj_id == "tr::c1g1")]]
  expect_true(all(is.na(c1$cct)))
})

test_that("sniff_trajectory_cct validates its inputs", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  expect_error(sniff_trajectory_cct(list(), tibble::tibble()), "sniff_trajectory_braid")
  expect_error(sniff_trajectory_cct(fl, tibble::tibble(a = 1)), "document_id")
})
