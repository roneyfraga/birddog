# Two document-disjoint lineages, each 2000-2002, both reaching the final year
# -> two central trajectories (c1g1, c1g2), 3 nodes each. Cumulative: a node
# holds every earlier paper plus that year's new ones.
make_two_central_dpg <- function() {
  tibble::tibble(
    group_id = c(
      rep("y2000c1g1", 2), rep("y2001c1g1", 4), rep("y2002c1g1", 6),
      rep("y2000c1g2", 2), rep("y2001c1g2", 4), rep("y2002c1g2", 6)),
    document_id = c(
      paste0("p", 1:2), paste0("p", 1:4), paste0("p", 1:6),
      paste0("q", 1:2), paste0("q", 1:4), paste0("q", 1:6)),
    network_until = c(rep(2000L, 2), rep(2001L, 4), rep(2002L, 6),
                      rep(2000L, 2), rep(2001L, 4), rep(2002L, 6)),
    group = c(rep("c1g1", 12), rep("c1g2", 12)))
}

# Fixtures below feed later tasks (feature matrix, silhouette, sniff_trajectory_coherence) -- no direct tests yet.
# Aligned content: every p-paper cites {R1,R2}; every q-paper cites {R3,R4}.
# -> each basin internally identical, basins orthogonal -> silhouette ~ 1.
make_content_aligned <- function() {
  docs_p <- paste0("p", 1:6); docs_q <- paste0("q", 1:6)
  tibble::tibble(
    document_id = c(rep(docs_p, each = 2), rep(docs_q, each = 2)),
    feature = c(rep(c("R1", "R2"), times = 6), rep(c("R3", "R4"), times = 6)))
}

# Scrambled content: references cut ACROSS basins by year-cohort, not by basin.
make_content_scrambled <- function() {
  tibble::tibble(
    document_id = c("p1", "p2", "q1", "q2", "p3", "p4", "q3", "q4",
                    "p5", "p6", "q5", "q6"),
    feature = c(rep("R1", 4), rep("R2", 4), rep("R3", 4)))
}

# One lineage, two years. Full profile of y2001 is inflated by the carried-over
# p1,p2 (which share R1 with y2000); the incremental profile of y2001 is only
# the new paper p3 (cites R9), so incremental separates the nodes, full does not.
make_carryover_dpg <- function() {
  tibble::tibble(
    group_id = c(rep("y2000c1g1", 2), rep("y2001c1g1", 3)),
    document_id = c("p1", "p2", "p1", "p2", "p3"),
    network_until = c(2000L, 2000L, 2001L, 2001L, 2001L),
    group = rep("c1g1", 5))
}

make_carryover_content <- function() {
  tibble::tibble(document_id = c("p1", "p2", "p3"),
                 feature = c("R1", "R1", "R9"))
}
