test_that("sniff_trajectory_group_contribution builds the bipartite object and indicators", {
  g <- make_contribution_fixture()
  cg <- sniff_trajectory_group_contribution(g)

  expect_named(cg, c("long", "incidence", "graph", "groups", "trajectories", "last_year"))

  # incidence matrix (rows = bare trajectory ids, cols = final groups)
  expect_equal(cg$incidence["tr1", "c1g1"], 3L)
  expect_equal(cg$incidence["tr2", "c1g2"], 2L)
  expect_equal(cg$incidence["tr3", "c1g1"], 1L)
  expect_equal(cg$incidence["tr3", "c1g2"], 1L)
  expect_equal(cg$incidence["tr1", "c1g2"], 0L)

  # long: tr3's share of its papers reaching c1g1 is 1/3
  r <- cg$long[cg$long$traj_id == "tr3" & cg$long$group_final == "c1g1", ]
  expect_equal(r$prop_of_group, 1 / 3)
  expect_equal(r$prop_of_traj, 1 / 3)

  # bipartite graph: 3 trajectories + 2 groups
  expect_true(igraph::is_bipartite(cg$graph))
  expect_equal(igraph::vcount(cg$graph), 5L)

  # per-group (formation) indicators
  c1 <- cg$groups[cg$groups$group_final == "c1g1", ]
  expect_equal(c1$n_sources, 2L)
  expect_equal(c1$dominant_traj, "tr1")
  expect_gt(c1$source_entropy, 0)             # fed by two trajectories

  # per-trajectory (reach) indicators
  t3 <- cg$trajectories[cg$trajectories$traj_id == "tr3", ]
  expect_equal(t3$n_groups, 2L)               # tr3 spans two final groups
  t1 <- cg$trajectories[cg$trajectories$traj_id == "tr1", ]
  expect_equal(t1$n_groups, 1L)
})

test_that("sniff_trajectory_group_contribution rejects a legacy (non-unique id) object", {
  fx <- make_destination_fixture()
  expect_error(
    sniff_trajectory_group_contribution(fx$all_detected, fx$docs_per_group),
    "globally-unique"
  )
})

test_that("sniff_trajectory_group_contribution returns cleanly when no papers survive", {
  # tr1's papers (d1) never appear in the final year (2021) -> empty long.
  g <- list(
    graph = NULL,
    trajectories = tibble::tibble(
      traj_id = "tr1", terminal_group = "c1g2",
      nodes = list(c("y2018c1g2", "y2019c1g2"))
    ),
    docs_per_group = tibble::tribble(
      ~group_id,    ~document_id, ~network_until, ~group,
      "y2018c1g2",  "d1",         2018,           "c1g2",
      "y2019c1g2",  "d1",         2019,           "c1g2",
      "y2021c1g1",  "d9",         2021,           "c1g1"
    )
  )
  cg <- sniff_trajectory_group_contribution(g)
  expect_equal(nrow(cg$long), 0L)
  expect_equal(nrow(cg$groups), 0L)
  expect_equal(nrow(cg$trajectories), 0L)
  expect_true(igraph::is_bipartite(cg$graph))   # all-zero incidence, no edges
})
