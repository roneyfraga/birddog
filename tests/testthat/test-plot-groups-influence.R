# Smoke tests for the group-influence plots. `toy_groups()` is in
# helper-groups-influence.R.

test_that("plot_groups_influence_matrix returns a ggplot for every fill", {
  infl <- sniff_groups_influence(toy_groups())
  for (f in c("surprise", "raw", "debt", "audience", "salton")) {
    expect_s3_class(plot_groups_influence_matrix(infl, fill = f), "ggplot")
  }
  for (d in c("mute", "show", "drop")) {
    expect_s3_class(plot_groups_influence_matrix(infl, diagonal = d), "ggplot")
  }
  expect_s3_class(plot_groups_influence_matrix(infl, order_by = "balance"), "ggplot")
  expect_s3_class(plot_groups_influence_matrix(infl, x_angle = 90), "ggplot")
})

test_that("plot_groups_influence_network returns a ggplot for every weight", {
  infl <- sniff_groups_influence(toy_groups())
  for (w in c("net", "gross", "surprise")) {
    expect_s3_class(plot_groups_influence_network(infl, weight = w), "ggplot")
  }
  for (s in c("equal", "io", "balance")) {
    expect_s3_class(plot_groups_influence_network(infl, node_size = s), "ggplot")
  }
  expect_s3_class(plot_groups_influence_network(infl, colour_role = FALSE), "ggplot")
})

test_that("influence plots reject non-influence input", {
  expect_error(plot_groups_influence_matrix(list()), "sniff_groups_influence")
  expect_error(plot_groups_influence_network(list()), "sniff_groups_influence")
})
