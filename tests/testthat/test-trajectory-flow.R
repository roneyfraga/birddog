test_that(".heaviest_predecessor keeps one strongest predecessor per node", {
  e <- tibble::tibble(
    from = c("y2000c1g1", "y2000c1g2"), to = c("y2001c1g1", "y2001c1g1"),
    weight = c(0.7, 0.3), documents = c(7L, 3L)
  )
  s <- .heaviest_predecessor(e)
  expect_equal(unname(s["y2001c1g1"]), "y2000c1g1")
})

# Simple: one central spine + one tributary absorbed at the merge.
make_flow_simple_dpg <- function() {
  tibble::tibble(
    group_id = c(rep("y2000c1g1", 3), rep("y2001c1g1", 4), rep("y2002c1g1", 4),
                 rep("y2000c1g2", 2)),
    document_id = c("w1", "w2", "w3",  "w1", "w2", "w3", "w4",
                    "w1", "w2", "w3", "w4",  "w4", "w5"),
    network_until = c(rep(2000L, 3), rep(2001L, 4), rep(2002L, 4), rep(2000L, 2)),
    group = c(rep("c1g1", 3), rep("c1g1", 4), rep("c1g1", 4), rep("c1g2", 2))
  )
}

test_that("sniff_trajectory_flow splits one central and one absorbed", {
  fl <- sniff_trajectory_flow(make_flow_simple_dpg(), min_group_size = 1)
  tr <- fl$trajectories
  expect_equal(sum(tr$type == "central"), 1L)
  expect_equal(sum(tr$type == "absorbed"), 1L)
  ce <- tr[tr$type == "central", ]
  expect_equal(ce$traj_id, "tr::c1g1")
  expect_equal(ce$end, 2002L)
  ab <- tr[tr$type == "absorbed", ]
  expect_equal(ab$traj_id, "tr1")
  expect_equal(ab$absorbed_into, "tr::c1g1")
  expect_equal(ab$absorption_year, 2001L)
  expect_equal(ab$group, "c1g1")          # ultimate destination
  expect_equal(ab$depth, 1L)
  expect_equal(nrow(fl$tree), 1L)
})

# Transitive: A absorbed by B (non-central), B absorbed by C (central).
make_flow_tree_dpg <- function() {
  tibble::tibble(
    group_id = c(
      rep("y2000c1g1", 2),                 # birth A {p1,p2}
      rep("y2000c1g2", 3),                 # birth B {p3,p4,p5}
      rep("y2001c1g2", 4),                 # B absorbs A: {p3,p4,p5,p1}
      rep("y2000c1g3", 4),                 # birth C {q1..q4}
      rep("y2001c1g3", 4),                 # C continues
      rep("y2002c1g3", 6),                 # C absorbs B-line: {q1..q4,p3,p4}
      rep("y2003c1g3", 6)),                # final
    document_id = c(
      "p1", "p2",
      "p3", "p4", "p5",
      "p3", "p4", "p5", "p1",
      "q1", "q2", "q3", "q4",
      "q1", "q2", "q3", "q4",
      "q1", "q2", "q3", "q4", "p3", "p4",
      "q1", "q2", "q3", "q4", "p3", "p4"),
    network_until = c(2000L, 2000L,  2000L, 2000L, 2000L,  rep(2001L, 4),
                      rep(2000L, 4), rep(2001L, 4), rep(2002L, 6), rep(2003L, 6)),
    group = c("c1g1", "c1g1",  "c1g2", "c1g2", "c1g2",  rep("c1g2", 4),
              rep("c1g3", 4), rep("c1g3", 4), rep("c1g3", 6), rep("c1g3", 6))
  )
}

test_that("sniff_trajectory_flow captures transitive absorption (the tree)", {
  fl <- sniff_trajectory_flow(make_flow_tree_dpg(), min_group_size = 1)
  tr <- fl$trajectories
  expect_equal(sum(tr$type == "central"), 1L)         # tr::c1g3
  expect_equal(tr$traj_id[tr$type == "central"], "tr::c1g3")
  expect_equal(sum(tr$type == "absorbed"), 2L)
  # the B-line (size 4: p1,p3,p4,p5) is tr1, absorbed into the central at 2002
  b <- tr[tr$absorption_year == 2002L & tr$type == "absorbed", ]
  expect_equal(b$absorbed_into, "tr::c1g3")
  expect_equal(b$depth, 1L)
  # the A-line is absorbed into the B-line (non-central) at 2001
  a <- tr[tr$absorption_year == 2001L & tr$type == "absorbed", ]
  expect_equal(a$absorbed_into, b$traj_id)            # absorbed by another absorbed
  expect_equal(a$depth, 2L)
  expect_equal(a$group, "c1g3")                       # ultimate destination
})
