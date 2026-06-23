# Expected indices below are the values worked out by hand in
# todo/birddog-groups-influence.qmd. `toy_groups()` is in
# helper-groups-influence.R.

test_that("cross-citation matrix matches the worked example", {
  infl <- sniff_groups_influence(toy_groups())
  expect_true(is_influence(infl))
  expect_identical(rownames(infl$matrix), c("G1", "G2", "G3"))
  expected <- matrix(c(3L,2L,1L, 4L,3L,2L, 2L,4L,4L), nrow = 3, byrow = TRUE,
                     dimnames = list(c("G1","G2","G3"), c("G1","G2","G3")))
  expect_equal(infl$matrix, expected)
  expect_equal(sum(infl$matrix), 25L)
})

test_that("balances give the source / broker / sink roles and sum to zero", {
  g <- sniff_groups_influence(toy_groups())$groups
  bal <- stats::setNames(g$balance, g$group)
  expect_equal(bal[["G1"]], 3L)
  expect_equal(bal[["G2"]], 0L)
  expect_equal(bal[["G3"]], -3L)
  expect_equal(sum(g$balance), 0L)
  role <- stats::setNames(g$role, g$group)
  expect_equal(unname(role[c("G1","G2","G3")]), c("source","broker","sink"))
})

test_that("normalized indices match the hand-computed channels", {
  flow <- sniff_groups_influence(toy_groups())$flow
  chan <- function(citing, cited) {
    flow[flow$recipient == citing & flow$influencer == cited, , drop = FALSE]
  }
  g2g1 <- chan("G2", "G1")   # G2 cites G1
  expect_equal(round(g2g1$debt, 2), 0.44)
  expect_equal(round(g2g1$surprise, 2), 1.23)
  g3g2 <- chan("G3", "G2")   # G3 cites G2
  expect_equal(round(g3g2$debt, 2), 0.40)
  expect_equal(round(g3g2$surprise, 2), 1.11)
  # the only over-represented channels are the two upward ones
  off <- flow[flow$influencer != flow$recipient, ]
  over <- off[off$surprise > 1, ]
  expect_setequal(paste(over$recipient, over$influencer), c("G2 G1", "G3 G2"))
})

test_that("net flow reproduces the spine G1=>G2=>G3 with the G1=>G3 shortcut", {
  net <- sniff_groups_influence(toy_groups())$net
  key <- stats::setNames(net$net, paste(net$from, net$to))
  expect_equal(key[["G1 G2"]], 2L)
  expect_equal(key[["G2 G3"]], 2L)
  expect_equal(key[["G1 G3"]], 1L)
  expect_equal(nrow(net), 3L)        # exactly one direction per connected pair
})

test_that("self = FALSE drops the diagonal but keeps the balances", {
  infl <- sniff_groups_influence(toy_groups(), self = FALSE)
  expect_equal(sum(diag(infl$matrix)), 0L)
  bal <- stats::setNames(infl$groups$balance, infl$groups$group)
  expect_equal(unname(bal[c("G1","G2","G3")]), c(3L, 0L, -3L))
})

test_that("null_reps is reproducible with a seed and adds p_value", {
  a <- sniff_groups_influence(toy_groups(), null_reps = 200, seed = 1)
  b <- sniff_groups_influence(toy_groups(), null_reps = 200, seed = 1)
  expect_true("p_value" %in% names(a$flow))
  expect_equal(a$flow$p_value, b$flow$p_value)
  expect_true(all(a$flow$p_value > 0 & a$flow$p_value <= 1))
})

test_that("input validation rejects non-group objects and undirected networks", {
  expect_error(sniff_groups_influence(list()), "sniff_groups")
  und <- igraph::make_ring(4)
  expect_error(sniff_groups_influence(list(network = und)), "directed")
})
