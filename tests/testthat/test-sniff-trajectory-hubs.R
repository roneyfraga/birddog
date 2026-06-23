# make_flow_simple_dpg(): central c1g1 holds documents {w1,w2,w3,w4} across its
# nodes; tributary c1g2 holds {w4,w5}.

test_that("sniff_trajectory_hubs aggregates hub roles over a trajectory's documents", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  # sniff_groups_hubs() returns the document id in column 'name'.
  hubs <- tibble::tibble(
    name = c("w1", "w2", "w3", "w4", "w5"),
    Zi = c(3, 1, 0, 2, 0),
    Pi = c(0.10, 0.50, 0.20, 0.80, 0.00),
    zone = c("R5", "R6", "noHub", "R7", "noHub"))
  out <- sniff_trajectory_hubs(fl, hubs)

  c1 <- out[out$traj_id == "tr::c1g1", ]        # central, docs w1,w2,w3,w4
  expect_equal(c1$n_docs, 4L)
  expect_equal(c1$hub_share, 0.75)              # 3 of 4 are hubs (R5,R6,R7)
  expect_equal(c1$connector_share, 0.5)         # w2 (R6) + w4 (R7)
  expect_equal(c1$provincial_share, 0.25)       # w1 (R5)
  expect_equal(c1$mean_Pi, mean(c(0.10, 0.50, 0.20, 0.80)))
})

test_that("sniff_trajectory_hubs marks documents absent from hubs as noHub", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  hubs <- tibble::tibble(name = "w1", Zi = 3, Pi = 0.1, zone = "R5")
  out <- sniff_trajectory_hubs(fl, hubs)
  c1 <- out[out$traj_id == "tr::c1g1", ]
  expect_equal(c1$hub_share, 0.25)              # only w1 of {w1,w2,w3,w4} is a hub
})

test_that("sniff_trajectory_hubs also accepts the legacy 'SR' id column", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  hubs <- tibble::tibble(SR = "w1", Zi = 3, Pi = 0.1, zone = "R5")
  out <- sniff_trajectory_hubs(fl, hubs)
  expect_equal(out$hub_share[out$traj_id == "tr::c1g1"], 0.25)
})

test_that("sniff_trajectory_hubs validates its inputs", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  expect_error(sniff_trajectory_hubs(list(), tibble::tibble()), "sniff_trajectory_braid")
  expect_error(sniff_trajectory_hubs(fl, tibble::tibble(a = 1)), "sniff_groups_hubs")
})
