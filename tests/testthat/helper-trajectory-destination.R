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
