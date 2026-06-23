test_that("fixed_state_thresholds carries the two-axis knobs", {
  th <- fixed_state_thresholds()
  expect_true(all(c("emergence_growth", "emergence_novelty", "decline_growth",
                    "convergence_entropy", "dormancy_share") %in% names(th)))
  expect_true(th$decline_growth < th$emergence_growth)
})

test_that(".classify_phase covers the living life-cycle states", {
  th <- fixed_state_thresholds()
  # emergence needs growth AND novelty; decline -> dormancy; otherwise maturity.
  expect_equal(.classify_phase(0.30, 0.50, th), "emergence")
  expect_equal(.classify_phase(0.30, 0.10, th), "maturity")    # grows but not novel
  expect_equal(.classify_phase(0.05, 0.50, th), "maturity")    # novel but flat
  expect_equal(.classify_phase(-0.20, 0.50, th), "dormancy")   # losing momentum
})

test_that(".classify_fate covers the declining terminal states", {
  th <- fixed_state_thresholds()
  expect_equal(.classify_fate("c1g1", 0.20, 0.10, th), "convergence")
  expect_equal(.classify_fate("c1g1", 0.90, 0.10, th), "divergence")
  expect_equal(.classify_fate("c1g1", 0.20, 0.80, th), "dormancy")   # cohort drops out
  expect_equal(.classify_fate(NA, NA, NA, th), "dormancy")           # extinct
})

test_that("sniff_trajectory_dynamics splits phase (central) from fate (absorbed)", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  expect_true(all(c("growth_rate", "doubling_time", "novelty", "recruitment",
                    "attraction_inflow", "dest_entropy", "dormant_share",
                    "phase", "fate", "state", "emergence_index") %in% names(dyn)))
  cen <- dyn[dyn$type == "central", ]
  abs_ <- dyn[dyn$type == "absorbed", ]
  expect_true(all(!is.na(cen$phase)) && all(is.na(cen$fate)))
  expect_true(all(!is.na(abs_$fate)) && all(is.na(abs_$phase)))
  expect_true(all(cen$state %in% c("emergence", "maturity", "dormancy")))
  expect_true(all(abs_$state %in% c("convergence", "divergence", "dormancy")))
})

test_that("emergence_index is computed for living trajectories only", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  expect_true(all(!is.na(dyn$emergence_index[dyn$type == "central"])))
  expect_true(all(is.na(dyn$emergence_index[dyn$type == "absorbed"])))
})

test_that("sniff_trajectory_dynamics joins optional cct / entropy / hubs columns", {
  fl <- sniff_trajectory_braid(make_flow_simple_dpg(), min_group_size = 1)
  cct <- sniff_trajectory_cct(fl, tibble::tibble(
    document_id = c("w1", "w2", "w3", "w4"), ref_age = c(3, 6, 8, 2)))
  ent <- sniff_trajectory_entropy(fl, tibble::tibble(
    document_id = c("w1", "w2", "w3", "w4"), keyword = c("kA", "kB", "kA", "kC")))
  hubs <- sniff_trajectory_hubs(fl, tibble::tibble(
    name = c("w1", "w2", "w3", "w4"), Zi = c(3, 1, 0, 2), Pi = c(0.1, 0.5, 0.2, 0.8),
    zone = c("R5", "R6", "noHub", "R7")))
  dyn <- sniff_trajectory_dynamics(fl, cct = cct, entropy = ent, hubs = hubs)
  expect_true(all(c("cct", "keyword_entropy", "mean_Pi", "connector_share") %in% names(dyn)))
  expect_true(is.list(dyn$cct) && is.list(dyn$keyword_entropy))  # per-year list-columns
  expect_false(any(c("group.x", "group.y") %in% names(dyn)))     # no join-suffix clash
})

test_that("emergence_index_intensive drops the extensive recruitment term", {
  fl <- sniff_trajectory_braid(make_flow_three_centrals_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl, winsorize = 3)
  expect_true("emergence_index_intensive" %in% names(dyn))
  cen <- dyn$type == "central"
  expect_true(all(!is.na(dyn$emergence_index_intensive[cen])))
  # intensive = full index minus the winsorized robust-z of recruitment
  wz <- function(x) pmax(-3, pmin(3, .robust_z(x)))
  expect_equal(dyn$emergence_index_intensive[cen],
               dyn$emergence_index[cen] - wz(dyn$recruitment[cen]))
  # non-constant recruitment -> the rate-only index genuinely differs from the full one
  expect_false(isTRUE(all.equal(dyn$emergence_index_intensive[cen],
                                dyn$emergence_index[cen])))
})

test_that("emergence_index_intensive is computed for living trajectories only", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  expect_true(all(is.na(dyn$emergence_index_intensive[dyn$type == "absorbed"])))
})

test_that("emergence_density is the share of year-steps the lineage keeps growing", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl, thresholds = fixed_state_thresholds())
  expect_true("emergence_density" %in% names(dyn))
  # central c1g3 has cumulative sizes 4,4,6,6 -> local growths 0, .5, 0 ; only the
  # middle step clears emergence_growth (0.15), so density = 1/3.
  expect_equal(dyn$emergence_density[dyn$traj_id == "tr::c1g3"], 1 / 3)
  # a single-node lineage has no year-step -> NA
  expect_true(is.na(dyn$emergence_density[dyn$type == "absorbed" & dyn$age == 1]))
  # density is bounded in [0, 1]
  d <- dyn$emergence_density[!is.na(dyn$emergence_density)]
  expect_true(all(d >= 0 & d <= 1))
})

test_that("reach_ratio is the share of the future field a lineage's origin seeds", {
  fl <- sniff_trajectory_braid(make_flow_three_centrals_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  expect_true("reach_ratio" %in% names(dyn))
  # c1g1 born 2000 reaches its 3 later nodes; 8 nodes are born after 2000 -> 3/8
  expect_equal(dyn$reach_ratio[dyn$traj_id == "tr::c1g1"], 3 / 8)
  # c1g3 born 2002 reaches its 1 later node; 3 nodes are born after 2002 -> 1/3
  expect_equal(dyn$reach_ratio[dyn$traj_id == "tr::c1g3"], 1 / 3)
  expect_true(all(dyn$reach_ratio >= 0 & dyn$reach_ratio <= 1, na.rm = TRUE))
})

test_that("winsorize caps each pillar's robust z in the emergence index", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  free <- sniff_trajectory_dynamics(fl, winsorize = NULL)
  capped <- sniff_trajectory_dynamics(fl, winsorize = 1)
  ei_free <- free$emergence_index[free$type == "central"]
  ei_cap <- capped$emergence_index[capped$type == "central"]
  expect_true(all(abs(ei_cap) <= 3 + 1e-9))            # 3 pillars, each capped at 1
  expect_true(all(abs(ei_cap) <= abs(ei_free) + 1e-9)) # capping never increases magnitude
  expect_error(sniff_trajectory_dynamics(fl, winsorize = -1), "positive number")
})

test_that("sniff_trajectory_dynamics rejects a non-flow object", {
  expect_error(sniff_trajectory_dynamics(list()), "sniff_trajectory_braid")
})
