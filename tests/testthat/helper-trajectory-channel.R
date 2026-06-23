# Hand-built docs_per_group fixtures for sniff_trajectory_channel().
# Weights below are consecutive-year Jaccard of the document sets.

# Reroute: flow (local-greedy) and channel (global-optimal) put the central
# backbone on DIFFERENT lineages. A-line has the stronger FINAL edge but a weak
# birth edge; B-line has a weaker final edge but a strong birth edge.
#   w(a1->F)=4/10=0.4  w(b1->F)=2/12=0.167   -> flow central via A-line
#   prod_A=J(a0,a1)*J(a1,F)=0.25*0.4 =0.10
#   prod_B=J(b0,b1)*J(b1,F)=0.875*0.167=0.146 -> channel central via B-line
make_channel_reroute_dpg <- function() {
  tibble::tibble(
    group_id = c(
      rep("y2000c1g1", 2),                       # a0 {a1,a2}
      rep("y2001c1g1", 8),                       # a1 {a1..a8}
      rep("y2002c1g1", 6),                       # F  {a1..a4,b1,b2}  (final)
      rep("y2000c1g2", 7),                       # b0 {b1..b7}
      rep("y2001c1g2", 8)),                      # b1 {b1..b8}
    document_id = c(
      paste0("a", 1:2),
      paste0("a", 1:8),
      c(paste0("a", 1:4), paste0("b", 1:2)),
      paste0("b", 1:7),
      paste0("b", 1:8)),
    network_until = c(rep(2000L, 2), rep(2001L, 8), rep(2002L, 6),
                      rep(2000L, 7), rep(2001L, 8)),
    group = c(rep("c1g1", 2), rep("c1g1", 8), rep("c1g1", 6),
              rep("c1g2", 7), rep("c1g2", 8))
  )
}

# Late-departure: an old c1g1 trunk (birth 2000, final 2004) plus a tributary
# BORN in 2003 that feeds the final at 2004. ld(final)=2003 > central start 2000.
make_channel_latedep_dpg <- function() {
  tibble::tibble(
    group_id = c(
      rep("y2000c1g1", 2), rep("y2001c1g1", 3), rep("y2002c1g1", 4),
      rep("y2003c1g1", 5), rep("y2004c1g1", 7),               # final
      rep("y2003c1g2", 3)),                                   # late birth
    document_id = c(
      paste0("w", 1:2), paste0("w", 1:3), paste0("w", 1:4),
      paste0("w", 1:5), c(paste0("w", 1:6), "x1"),
      paste0("x", 1:3)),
    network_until = c(2000L, 2000L, rep(2001L, 3), rep(2002L, 4),
                      rep(2003L, 5), rep(2004L, 7), rep(2003L, 3)),
    group = c(rep("c1g1", 2 + 3 + 4 + 5 + 7), rep("c1g2", 3))
  )
}

# Isolated final: a continuous c1g1 trunk plus c1g2 appearing ONLY at the final
# year as a birth (no predecessor) -> a singleton central, start == end.
make_channel_isolated_dpg <- function() {
  tibble::tibble(
    group_id = c(rep("y2000c1g1", 2), rep("y2001c1g1", 3), rep("y2002c1g1", 4),
                 rep("y2002c1g2", 3)),
    document_id = c(paste0("w", 1:2), paste0("w", 1:3), paste0("w", 1:4),
                    paste0("z", 1:3)),
    network_until = c(2000L, 2000L, rep(2001L, 3), rep(2002L, 4), rep(2002L, 3)),
    group = c(rep("c1g1", 9), rep("c1g2", 3))
  )
}

# Extinct: c1g2 lives 2000-2001 then dies (no 2002 node, document-disjoint from
# c1g1) -> an absorbed trajectory with group = NA and absorbed_into = NA.
make_channel_extinct_dpg <- function() {
  tibble::tibble(
    group_id = c(rep("y2000c1g1", 2), rep("y2001c1g1", 3), rep("y2002c1g1", 4),
                 rep("y2000c1g2", 2), rep("y2001c1g2", 3)),
    document_id = c(paste0("w", 1:2), paste0("w", 1:3), paste0("w", 1:4),
                    paste0("z", 1:2), paste0("z", 1:3)),
    network_until = c(2000L, 2000L, rep(2001L, 3), rep(2002L, 4),
                      2000L, 2000L, rep(2001L, 3)),
    group = c(rep("c1g1", 9), rep("c1g2", 5))
  )
}
