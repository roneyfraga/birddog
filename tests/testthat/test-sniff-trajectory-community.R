test_that("sniff_trajectory_community counts distinct authors per trajectory", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  authors <- tibble::tibble(
    document_id = c("w1", "w1", "w2", "w3", "w4", "w4", "w5"),
    author      = c("A1", "A2", "A1", "A3", "A2", "A4", "A5"))
  comm <- sniff_trajectory_community(fl, authors)
  expect_true(all(c("traj_id", "type", "group", "n_docs", "n_authors",
                    "authors_per_doc") %in% names(comm)))
  c1 <- comm[comm$traj_id == "tr::c1g1", ]
  expect_equal(c1$n_authors, 4L)          # w1..w4 -> A1,A2,A3,A4
  expect_equal(c1$authors_per_doc, 1.0)   # 4 authors / 4 docs
  ab <- comm[comm$type == "absorbed", ]
  expect_equal(ab$n_authors, 3L)          # w4,w5 -> A2,A4,A5
  expect_equal(ab$authors_per_doc, 1.5)
})

test_that("sniff_trajectory_community rejects bad inputs", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  expect_error(sniff_trajectory_community(list(), tibble::tibble()),
               "sniff_trajectory_braid")
  expect_error(sniff_trajectory_community(fl, tibble::tibble(x = 1)),
               "document_id")
})
