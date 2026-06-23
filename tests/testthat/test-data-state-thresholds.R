test_that("data_state_thresholds returns a default-shaped list", {
  fl <- sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1)
  dyn <- sniff_trajectory_dynamics(fl)
  th <- data_state_thresholds(dyn)
  expect_setequal(names(th), names(fixed_state_thresholds()))
  expect_true(th$decline_growth <= th$emergence_growth)
  expect_equal(th$dormancy_share, 0.5)         # kept at the majority rule
})

test_that("data_state_thresholds places the growth cuts k robust deviations apart", {
  # hand-built dyn with a known growth distribution incl. one outlier; the robust
  # cuts must follow median +/- k*MAD and ignore the outlier.
  g <- c(0.0, 0.05, 0.10, 0.15, 0.20, 3.0)     # 3.0 is the outlier
  dyn <- tibble::tibble(
    type = "central", growth_rate = g, novelty = 0.5,
    dest_entropy = NA_real_, dormant_share = NA_real_)
  th <- data_state_thresholds(dyn, k = 1)
  m <- stats::median(g); s <- stats::mad(g)
  expect_equal(th$emergence_growth, m + s)
  expect_equal(th$decline_growth, m - s)
  expect_lt(th$emergence_growth, 0.5)          # outlier (3.0) does not inflate it
})

test_that("data_state_thresholds derives the bounded cuts from medians", {
  cen <- tibble::tibble(type = "central", growth_rate = c(0.1, 0.2),
    novelty = c(0.2, 0.8), dest_entropy = NA_real_, dormant_share = NA_real_)
  ab <- tibble::tibble(type = "absorbed", growth_rate = NA_real_, novelty = NA_real_,
    dest_entropy = c(0.4, 0.8), dormant_share = c(0, 0))
  th <- data_state_thresholds(dplyr::bind_rows(cen, ab))
  expect_equal(th$emergence_novelty, 0.5)      # median(0.2, 0.8)
  expect_equal(th$convergence_entropy, 0.6)    # median(0.4, 0.8)
})

test_that("data_state_thresholds validates its input", {
  expect_error(data_state_thresholds(list()), "sniff_trajectory_dynamics")
})

test_that(".robust_z resists an outlier where the plain z-score would not", {
  x <- c(1:9, 100)                              # tight cluster + one extreme outlier
  rz <- .robust_z(x)
  z_plain <- (x - mean(x)) / stats::sd(x)
  expect_gt(rz[10], 10)                         # outlier sits far out on the robust scale
  expect_gt(rz[10], z_plain[10])                # the plain z is deflated by its own outlier
  expect_lt(abs(rz[5]), 2)                      # a typical point stays near the centre
})
