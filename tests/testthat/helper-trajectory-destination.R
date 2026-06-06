# Fixture: a trajectory "tr3" whose terminal (dying) node is y2018c1g16,
# tracked forward to the final year 2020. The 4 cohort papers split:
# d1,d2 stay in c1g16; d3 moves to c1g10; d4 drifts to c1g1 then drops out.
make_destination_fixture <- function() {
  docs_per_group <- tibble::tribble(
    ~group_id,     ~document_id, ~network_until, ~group,
    "y2018c1g16",  "d1",         2018,           "c1g16",
    "y2018c1g16",  "d2",         2018,           "c1g16",
    "y2018c1g16",  "d3",         2018,           "c1g16",
    "y2018c1g16",  "d4",         2018,           "c1g16",
    "y2019c1g16",  "d1",         2019,           "c1g16",
    "y2019c1g16",  "d2",         2019,           "c1g16",
    "y2019c1g10",  "d3",         2019,           "c1g10",
    "y2019c1g1",   "d4",         2019,           "c1g1",
    "y2020c1g16",  "d1",         2020,           "c1g16",
    "y2020c1g16",  "d2",         2020,           "c1g16",
    "y2020c1g10",  "d3",         2020,           "c1g10"
    # d4 absent in 2020 -> dropped below min_group_size
  )

  detected <- list(
    trajectories = tibble::tibble(
      traj_id = c("tr1", "tr3"),
      nodes = list(
        c("y2018c1g2", "y2019c1g2", "y2020c1g2"),
        c("y2017c1g3", "y2018c1g16")
      )
    )
  )

  # All groups' detected trajectories, for trajectory-level destination.
  # c1g16::tr1 carries d1,d2 post-2018; c1g10::tr1 carries d3; d4 has no carrier.
  all_detected <- list(
    c1g16 = list(trajectories = tibble::tibble(
      traj_id = c("tr1", "tr2"),
      start = c(2018L, 2001L),
      end = c(2020L, 2002L),
      nodes = list(
        c("y2018c1g16", "y2019c1g16", "y2020c1g16"),
        c("y2001c1g5", "y2002c1g5")
      )
    )),
    c1g10 = list(trajectories = tibble::tibble(
      traj_id = c("tr1", "tr3"),
      start = c(2019L, 2017L),
      end = c(2020L, 2018L),
      nodes = list(
        c("y2019c1g10", "y2020c1g10"),
        c("y2017c1g3", "y2018c1g16")
      )
    ))
  )

  list(detected = detected, docs_per_group = docs_per_group, all_detected = all_detected)
}

# A c1g1 chain that dries up at 2002 while an unconnected 2003 cluster pushes
# last_year to 2003, so the chain has end (2002) < last_year (2003): living = FALSE.
make_dead_line_dpg <- function() {
  tibble::tibble(
    group_id = c(rep("y2000c1g1", 3), rep("y2001c1g1", 3), rep("y2002c1g1", 3),
                 rep("y2003c1g2", 3)),
    document_id = c(rep(c("w1", "w2", "w3"), 3), "w7", "w8", "w9"),
    network_until = c(rep(2000L, 3), rep(2001L, 3), rep(2002L, 3), rep(2003L, 3)),
    group = c(rep("c1g1", 9), rep("c1g2", 3))
  )
}

# Simple fixture: one central spine (c1g1) + one tributary (c1g2) absorbed at
# the merge. Used in flow-destination and flow-self-sufficiency tests.
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

# Transitive absorption fixture: A absorbed by B (non-central), B absorbed by C
# (central). Used in flow-formation tests.
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
