# Shared docs_per_group fixtures for the trajectory-flow / confluence tests.

# Simple: one central spine (c1g1) + one tributary (c1g2) absorbed at the merge.
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

# Transitive: A absorbed by B (non-central), B absorbed by C (central c1g3).
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

# Three disjoint centrals all reaching the final year (2003), with deliberately
# different growth / recruitment so cohort-relative (robust-z) metrics have a
# real population: c1g1 slow (recruitment 3), c1g2 fast/large (12), c1g3 born
# late / young (1). No shared documents, so no absorption -> 3 centrals, 0
# absorbed.
make_flow_three_centrals_dpg <- function() {
  node <- function(year, group, docs)
    tibble::tibble(group_id = paste0("y", year, group),
                   document_id = docs, network_until = year, group = group)
  dplyr::bind_rows(
    node(2000, "c1g1", paste0("a", 1:2)),
    node(2001, "c1g1", paste0("a", 1:3)),
    node(2002, "c1g1", paste0("a", 1:4)),
    node(2003, "c1g1", paste0("a", 1:5)),     # sizes 2,3,4,5 ; recruitment 3
    node(2000, "c1g2", paste0("b", 1:4)),
    node(2001, "c1g2", paste0("b", 1:8)),
    node(2002, "c1g2", paste0("b", 1:12)),
    node(2003, "c1g2", paste0("b", 1:16)),    # sizes 4,8,12,16 ; recruitment 12
    node(2002, "c1g3", paste0("c", 1:6)),
    node(2003, "c1g3", paste0("c", 1:7))      # sizes 6,7 ; recruitment 1 (young)
  )
}
