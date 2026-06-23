# Synthetic forest: central C with three direct tributaries X, Y, Z and one
# nested tributary W feeding X. All hand-built so expected values are exact.
synth_conf <- function() {
  rivers <- tibble::tibble(
    traj_id = c("C", "X", "Y", "Z", "W"),
    type    = c("central", "absorbed", "absorbed", "absorbed", "absorbed"),
    central = c("c1g1", "c1g1", "c1g1", "c1g1", "c1g1"),
    size    = c(50L, 10L, 10L, 20L, 4L),
    start   = c(2000L, 2001L, 2002L, 2001L, 2000L),
    handoff_year = c(2010L, 2006L, 2005L, 2007L, 2004L),
    depth   = c(0L, 1L, 1L, 1L, 2L),
    parent  = c(NA, "C", "C", "C", "X"),
    size_curve = list(
      tibble::tibble(year = c(2000L, 2010L), size = c(5L, 50L)),
      tibble::tibble(year = c(2001L, 2006L), size = c(2L, 10L)),
      tibble::tibble(year = c(2002L, 2005L), size = c(2L, 10L)),
      tibble::tibble(year = c(2001L, 2007L), size = c(4L, 20L)),
      tibble::tibble(year = c(2000L, 2004L), size = c(1L, 4L)))
  )
  confluences <- tibble::tibble(
    child  = c("X", "Y", "Z", "W"),
    parent = c("C", "C", "C", "X"),
    n           = c(10L, 3L, 8L, 4L),
    cohort_size = c(10L, 10L, 20L, 4L),
    first_feed_year = c(2003L, 2003L, 2002L, 2001L),
    handoff_year    = c(2006L, 2005L, 2007L, 2004L),
    inflow_curve = list(
      tibble::tibble(year = c(2003L, 2006L), size = c(5L, 10L)),
      tibble::tibble(year = c(2003L, 2005L), size = c(1L, 3L)),
      tibble::tibble(year = c(2002L, 2007L), size = c(2L, 8L)),
      tibble::tibble(year = c(2001L, 2004L), size = c(2L, 4L)))
  )
  list(rivers = rivers, confluences = confluences,
       centrals = "c1g1", last_year = 2010L)
}

test_that(".confluence_prune drops below-threshold tributaries and their subtrees", {
  cf <- synth_conf()
  # min_n = 5: X(10) and Z(8) survive; Y(3) drops; W(4) drops -> X keeps no kids
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = NULL,
                          min_n = 5, min_prop = 0)
  expect_setequal(pr$rivers$traj_id, c("C", "X", "Z"))
  expect_setequal(paste(pr$confluences$child, pr$confluences$parent),
                  c("X C", "Z C"))
  expect_setequal(pr$dropped$child, c("Y", "W"))
})

test_that(".confluence_prune applies the min_prop share gate", {
  cf <- synth_conf()
  # min_prop = 0.5: X 10/10=1.0 keep; Z 8/20=0.4 drop; Y 3/10=0.3 drop; W 4/4 keep
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = NULL,
                          min_n = 0, min_prop = 0.5)
  expect_setequal(pr$rivers$traj_id, c("C", "X", "W"))
  expect_setequal(paste(pr$confluences$child, pr$confluences$parent),
                  c("X C", "W X"))
})

test_that(".confluence_prune target keeps only that central's subtree", {
  cf <- synth_conf()
  # add a second central D so target filtering is observable
  cf$rivers <- rbind(cf$rivers, tibble::tibble(
    traj_id = "D", type = "central", central = "c1g2", size = 30L,
    start = 2000L, handoff_year = 2010L, depth = 0L, parent = NA,
    size_curve = list(tibble::tibble(year = c(2000L, 2010L), size = c(3L, 30L)))))
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0)
  expect_false("D" %in% pr$rivers$traj_id)
  expect_true(all(c("C", "X", "Y", "Z", "W") %in% pr$rivers$traj_id))
})

test_that(".confluence_prune errors on an unknown target", {
  cf <- synth_conf()
  expect_error(
    .confluence_prune(cf$rivers, cf$confluences, target = "NOPE"),
    "'target' is not a trajectory")
})

test_that(".confluence_lanes centers a parent among its children", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = "C")
  expect_setequal(ln$traj_id, c("C", "X", "Y", "Z", "W"))
  expect_equal(length(unique(ln$lane)), nrow(ln))          # distinct lanes
  lane <- stats::setNames(ln$lane, ln$traj_id)
  # C has direct kids on both sides -> its lane is strictly interior
  direct <- c("X", "Y", "Z")
  expect_gt(lane["C"], min(lane[direct]))
  expect_lt(lane["C"], max(lane[direct]))
  # the larger tributary (Z, size 20) sits nearer the spine than a smaller one (Y)
  expect_lt(abs(lane["Z"] - lane["C"]), abs(lane["Y"] - lane["C"]) + 1e-9)
})

test_that(".confluence_lanes places a sub-tributary adjacent to its own parent", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = "C")
  lane <- stats::setNames(ln$lane, ln$traj_id)
  # W feeds X: no other trajectory's lane lies strictly between W and X
  others <- lane[setdiff(names(lane), c("W", "X"))]
  lo <- min(lane["W"], lane["X"]); hi <- max(lane["W"], lane["X"])
  expect_false(any(others > lo & others < hi))
})

test_that(".confluence_lanes stacks multiple centrals without lane collisions", {
  cf <- synth_conf()
  cf$rivers <- rbind(cf$rivers, tibble::tibble(
    traj_id = "D", type = "central", central = "c1g2", size = 30L,
    start = 2000L, handoff_year = 2010L, depth = 0L, parent = NA,
    size_curve = list(tibble::tibble(year = c(2000L, 2010L), size = c(3L, 30L)))))
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = NULL,
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = NULL)
  expect_equal(length(unique(ln$lane)), nrow(ln))   # every trajectory its own lane
  expect_true(all(c("C", "D") %in% ln$traj_id))
  lane <- stats::setNames(ln$lane, ln$traj_id)
  expect_gt(lane[["C"]], lane[["D"]])              # larger central stacked higher
})

test_that(".confluence_lanes handles a childless central", {
  rivers <- tibble::tibble(
    traj_id = "C", type = "central", central = "c1g1", size = 10L,
    start = 2000L, handoff_year = 2005L, depth = 0L, parent = NA,
    size_curve = list(tibble::tibble(year = c(2000L, 2005L), size = c(2L, 10L))))
  confluences <- tibble::tibble(
    child = character(), parent = character(), n = integer(),
    cohort_size = integer(), first_feed_year = integer(),
    handoff_year = integer(), inflow_curve = list())
  ln <- .confluence_lanes(rivers, confluences, target = NULL)
  expect_equal(ln$traj_id, "C")
  expect_equal(ln$lane, 0)
})

test_that(".confluence_polygons spine grows to the cumulative inflow", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = "C")
  g <- .confluence_polygons(pr$rivers, pr$confluences, ln,
                            last_year = cf$last_year, smooth = FALSE)
  expect_true(all(c("spines", "ribbons", "merges", "scale") %in% names(g)))
  # C's direct kept kids X(10)+Y(3)+Z(8) = 21 papers; spine half-height peaks at
  # scale * 21 (smooth = FALSE so no smoothing tolerance needed)
  sp <- g$spines[g$spines$traj_id == "C", ]
  peak_half <- max(sp$hi - (sp$lo + sp$hi) / 2)
  expect_equal(peak_half, g$scale * 21, tolerance = 1e-6)
})

test_that(".confluence_polygons splits a tributary into grey then colour", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = "C")
  g <- .confluence_polygons(pr$rivers, pr$confluences, ln,
                            last_year = cf$last_year, smooth = FALSE)
  rb <- g$ribbons[g$ribbons$traj_id == "Z", ]      # Z first_feed_year = 2002
  grey <- rb[rb$seg == "pre", ]
  col  <- rb[rb$seg == "feed", ]
  expect_true(max(grey$x) <= 2002L)
  expect_true(min(col$x)  >= 2002L)
})

test_that(".confluence_polygons merge band carries n, not cohort", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = "C")
  g <- .confluence_polygons(pr$rivers, pr$confluences, ln,
                            last_year = cf$last_year, smooth = FALSE)
  mz <- g$merges[g$merges$child == "Z", ]          # Z: n = 8, cohort = 20
  start_half <- max((mz$hi - mz$lo)[mz$x == min(mz$x)]) / 2
  expect_equal(start_half, g$scale * 8, tolerance = 1e-6)
})

test_that(".confluence_polygons gives each central its own spine and a tributary-only scale", {
  rivers <- tibble::tibble(
    traj_id = c("C", "D", "cx", "dx"),
    type = c("central", "central", "absorbed", "absorbed"),
    central = c("c1g1", "c1g2", "c1g1", "c1g2"),
    size = c(40L, 30L, 12L, 7L),
    start = c(2000L, 2000L, 2001L, 2001L),
    handoff_year = c(2010L, 2010L, 2005L, 2006L),
    depth = c(0L, 0L, 1L, 1L),
    parent = c(NA, NA, "C", "D"),
    size_curve = list(
      tibble::tibble(year = c(2000L, 2010L), size = c(4L, 40L)),
      tibble::tibble(year = c(2000L, 2010L), size = c(3L, 30L)),
      tibble::tibble(year = c(2001L, 2005L), size = c(2L, 12L)),
      tibble::tibble(year = c(2001L, 2006L), size = c(1L, 7L))))
  confluences <- tibble::tibble(
    child = c("cx", "dx"), parent = c("C", "D"),
    n = c(12L, 5L), cohort_size = c(12L, 7L),
    first_feed_year = c(2002L, 2002L), handoff_year = c(2005L, 2006L),
    inflow_curve = list(
      tibble::tibble(year = c(2002L, 2005L), size = c(6L, 12L)),
      tibble::tibble(year = c(2002L, 2006L), size = c(2L, 5L))))
  ln <- .confluence_lanes(rivers, confluences, target = NULL)
  g <- .confluence_polygons(rivers, confluences, ln, last_year = 2010L, smooth = FALSE)
  half <- function(id) {
    sp <- g$spines[g$spines$traj_id == id, ]
    max(sp$hi - (sp$lo + sp$hi) / 2)
  }
  # each central's spine uses ITS OWN kids (closure not stale): C->cx n=12, D->dx n=5
  expect_equal(half("C"), g$scale * 12, tolerance = 1e-6)
  expect_equal(half("D"), g$scale * 5,  tolerance = 1e-6)
  # scale denominator excludes central size_curves: peak = max tributary/inflow = 12
  expect_equal(g$scale, 0.42 / 12, tolerance = 1e-9)
})

# --- plot_trajectory_confluence tests ---

conf_real <- function() sniff_trajectory_confluence(
  sniff_trajectory_braid(make_flow_tree_dpg(), min_group_size = 1))

test_that("plot_trajectory_confluence returns a ggplot for the full confluence", {
  p <- plot_trajectory_confluence(conf_real(), min_n = 0)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot_trajectory_confluence renders a single central's subtree", {
  p <- plot_trajectory_confluence(conf_real(), target = "tr::c1g3", min_n = 0)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("a high min_n drops tributaries and is reported in the caption", {
  conf <- conf_real()
  p <- plot_trajectory_confluence(conf, min_n = 1000)
  lab <- p$labels$caption
  expect_true(is.character(lab) && grepl("threshold", lab))
})

test_that("plot validates its input", {
  expect_error(plot_trajectory_confluence(list(a = 1)),
               "sniff_trajectory_confluence")
})

test_that(".confluence_prune max_depth keeps only direct feeders", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0, max_depth = 1)
  expect_setequal(pr$rivers$traj_id, c("C", "X", "Y", "Z"))   # W (nested) excluded
  expect_false("W" %in% pr$confluences$child)
})

test_that(".confluence_lanes separates watersheds by root_gap", {
  rivers <- tibble::tibble(
    traj_id = c("C", "D"), type = c("central", "central"),
    central = c("c1g1", "c1g2"), size = c(40L, 30L),
    start = c(2000L, 2000L), handoff_year = c(2010L, 2010L),
    depth = c(0L, 0L), parent = c(NA, NA),
    size_curve = list(tibble::tibble(year = c(2000L, 2010L), size = c(4L, 40L)),
                      tibble::tibble(year = c(2000L, 2010L), size = c(3L, 30L))))
  confluences <- tibble::tibble(
    child = character(), parent = character(), n = integer(),
    cohort_size = integer(), first_feed_year = integer(),
    handoff_year = integer(), inflow_curve = list())
  ln <- .confluence_lanes(rivers, confluences, target = NULL, root_gap = 2)
  lane <- stats::setNames(ln$lane, ln$traj_id)
  expect_equal(abs(lane[["C"]] - lane[["D"]]), 3)   # 1 (adjacency) + 2 (gap)
})

# A small two-central confluence object for the multi-target / colour tests.
synth_conf_2central <- function() {
  rivers <- tibble::tibble(
    traj_id = c("tr::c1g1", "tr::c1g2", "tr::c1g3", "t1", "t2"),
    type = c("central", "central", "central", "absorbed", "absorbed"),
    central = c("c1g1", "c1g2", "c1g3", "c1g1", "c1g2"),
    size = c(40L, 30L, 20L, 10L, 8L),
    start = c(2000L, 2000L, 2000L, 2001L, 2001L),
    handoff_year = c(2010L, 2010L, 2010L, 2005L, 2006L),
    depth = c(0L, 0L, 0L, 1L, 1L),
    parent = c(NA, NA, NA, "tr::c1g1", "tr::c1g2"),
    size_curve = list(
      tibble::tibble(year = c(2000L, 2010L), size = c(4L, 40L)),
      tibble::tibble(year = c(2000L, 2010L), size = c(3L, 30L)),
      tibble::tibble(year = c(2000L, 2010L), size = c(2L, 20L)),
      tibble::tibble(year = c(2001L, 2005L), size = c(2L, 10L)),
      tibble::tibble(year = c(2001L, 2006L), size = c(1L, 8L))))
  confluences <- tibble::tibble(
    child = c("t1", "t2"), parent = c("tr::c1g1", "tr::c1g2"),
    n = c(10L, 8L), cohort_size = c(10L, 8L),
    first_feed_year = c(2002L, 2002L), handoff_year = c(2005L, 2006L),
    inflow_curve = list(tibble::tibble(year = c(2002L, 2005L), size = c(5L, 10L)),
                        tibble::tibble(year = c(2002L, 2006L), size = c(4L, 8L))))
  list(rivers = rivers, confluences = confluences,
       centrals = c("c1g1", "c1g2", "c1g3"), last_year = 2010L)
}

test_that("plot accepts a vector target (selected centrals only)", {
  conf <- synth_conf_2central()
  p <- plot_trajectory_confluence(conf, target = c("tr::c1g1", "tr::c1g2"),
                                     min_n = 0)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_match(p$labels$title, "c1g1, c1g2")
  drawn <- unlist(lapply(p$layers, function(L)
    if ("traj_id" %in% names(L$data)) as.character(L$data$traj_id) else NULL))
  expect_false("tr::c1g3" %in% drawn)               # unselected central excluded
})

test_that("label_intermediary = 'id' drops the counts from tributary labels", {
  conf <- conf_real()
  labs <- function(p) unlist(lapply(p$layers, function(L)
    if ("lab" %in% names(L$data)) as.character(L$data$lab) else NULL))
  trib <- function(x) x[grepl("^tr[0-9]", x)]   # tributary labels only (not tr::cNgN)
  # default single-target labels carry a parenthesised document count
  expect_true(any(grepl("\\([0-9]+\\)", trib(labs(
    plot_trajectory_confluence(conf, target = "tr::c1g3", min_n = 0))))))
  # label_intermediary = "id" shows bare ids, no count
  expect_false(any(grepl("\\(", trib(labs(
    plot_trajectory_confluence(conf, target = "tr::c1g3", min_n = 0,
                                  label_intermediary = "id"))))))
})

test_that("central labels use the tr::cNgN trajectory id, controllable separately", {
  conf <- conf_real()                         # central tr::c1g3
  labs <- function(p) unlist(lapply(p$layers, function(L)
    if ("lab" %in% names(L$data)) as.character(L$data$lab) else NULL))
  expect_true(any(grepl("tr::c1g3", labs(
    plot_trajectory_confluence(conf, min_n = 0)), fixed = TRUE)))
  # label_central = "none" hides the central label but keeps tributary labels
  l_none <- labs(plot_trajectory_confluence(conf, min_n = 0,
                                               label_terminal = NULL))
  expect_false(any(grepl("c1g3", l_none, fixed = TRUE)))
  expect_true(length(l_none) > 0)             # tributary labels still present
})

test_that(".confluence_prune min_total_size drops small trajectories", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0, min_total_size = 5)
  expect_false("W" %in% pr$rivers$traj_id)    # W (size 4) dropped
  expect_true(all(c("X", "Y", "Z") %in% pr$rivers$traj_id))
})

test_that(".confluence_prune min_duration_years drops short-lived trajectories", {
  cf <- synth_conf()
  # year spans from size_curve: X = 6, Y = 4, Z = 7, W = 5; min 5 drops Y
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0, min_duration_years = 5)
  expect_false("Y" %in% pr$rivers$traj_id)
  expect_true(all(c("X", "Z", "W") %in% pr$rivers$traj_id))
})

test_that("width_range scales the river widths and floors thin streams", {
  cf <- synth_conf()
  pr <- .confluence_prune(cf$rivers, cf$confluences, target = "C",
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = "C")
  g1 <- .confluence_polygons(pr$rivers, pr$confluences, ln, last_year = cf$last_year,
                             smooth = FALSE, width_range = c(0, 0.42))
  g2 <- .confluence_polygons(pr$rivers, pr$confluences, ln, last_year = cf$last_year,
                             smooth = FALSE, width_range = c(0, 0.84))
  expect_equal(g2$scale, 2 * g1$scale, tolerance = 1e-9)   # max scales the slope
  # a floor lifts every band: the thinnest merge half-width is >= the floor
  gf <- .confluence_polygons(pr$rivers, pr$confluences, ln, last_year = cf$last_year,
                             smooth = FALSE, width_range = c(0.1, 0.5))
  half <- (gf$merges$hi - gf$merges$lo) / 2
  expect_true(all(half >= 0.1 - 1e-9))
})

test_that("labels_text adds a bottom legend keyed by central", {
  conf <- conf_real()
  lab <- data.frame(id = "c1g3", text = "Topic Z", stringsAsFactors = FALSE)
  p <- plot_trajectory_confluence(conf, labels_text = lab, min_n = 0)
  expect_equal(p$theme$legend.position, "bottom")
  has_colour_scale <- any(vapply(p$scales$scales,
    function(s) any(s$aesthetics == "colour"), logical(1)))
  expect_true(has_colour_scale)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("axis_text_size sets the x-axis tick font size", {
  conf <- conf_real()
  p <- plot_trajectory_confluence(conf, target = "tr::c1g3", min_n = 0,
                                     axis_text_size = 20)
  expect_equal(p$theme$axis.text.x$size, 20)
})

test_that("target keywords filter layers: finals=spines, intermediary=ribbons", {
  conf <- conf_real()
  spine_present <- function(p) any(vapply(p$layers, function(L)
    all(c("lo", "hi", "traj_id") %in% names(L$data)) &&
      !("seg" %in% names(L$data)) && !("child" %in% names(L$data)), logical(1)))
  ribbon_present <- function(p) any(vapply(p$layers,
    function(L) "seg" %in% names(L$data), logical(1)))

  pa <- plot_trajectory_confluence(conf, target = "all", min_n = 0)
  expect_true(spine_present(pa))
  expect_true(ribbon_present(pa))

  pf <- plot_trajectory_confluence(conf, target = "finals", min_n = 0)
  expect_true(spine_present(pf))
  expect_false(ribbon_present(pf))           # tributaries hidden
  expect_match(pf$labels$title, "Final")

  pii <- plot_trajectory_confluence(conf, target = "intermediary", min_n = 0)
  expect_false(spine_present(pii))           # central spines hidden
  expect_true(ribbon_present(pii))
  expect_match(pii$labels$title, "Intermediate")
})

test_that("show_secondary draws a faint link to a tributary's other final", {
  conf <- synth_conf_2central()
  conf$destinations <- tibble::tibble(   # t1 (primary c1g1) also fed c1g2
    traj_id = c("t1", "t1", "t2"), g_final = c("c1g1", "c1g2", "c1g2"),
    n = c(10L, 8L, 8L))
  has_sec <- function(p) Filter(function(L) "grp" %in% names(L$data) &&
    any(grepl(" tr::", as.character(L$data$grp), fixed = TRUE)), p$layers)
  p <- plot_trajectory_confluence(conf, target = "all", min_n = 0,
                                     secondary_min_prop = 0.2)
  sec <- has_sec(p)
  expect_length(sec, 1)
  expect_true(any(grepl("t1 tr::c1g2", as.character(sec[[1]]$data$grp), fixed = TRUE)))
  # toggled off, no secondary layer
  p0 <- plot_trajectory_confluence(conf, target = "all", min_n = 0,
                                      show_secondary = FALSE)
  expect_length(has_sec(p0), 0)
})

test_that("self-sufficient centrals (no tributaries) are still drawn as a spine", {
  conf <- synth_conf_2central()   # tr::c1g3 has no confluences feeding it
  pr <- .confluence_prune(conf$rivers, conf$confluences, target = NULL,
                          min_n = 0, min_prop = 0)
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = NULL)
  g <- .confluence_polygons(pr$rivers, pr$confluences, ln,
                            last_year = conf$last_year, smooth = FALSE)
  expect_true("tr::c1g3" %in% g$spines$traj_id)   # childless central still drawn
  # and the whole plot includes its label
  p <- plot_trajectory_confluence(conf, target = "all", min_n = 0)
  labs <- unlist(lapply(p$layers, function(L)
    if ("lab" %in% names(L$data)) as.character(L$data$lab) else NULL))
  expect_true(any(grepl("tr::c1g3", labs, fixed = TRUE)))
})

test_that("a one-year self-sufficient central renders a visible segment", {
  rivers <- tibble::tibble(
    traj_id = "tr::c1g9", type = "central", central = "c1g9", size = 50L,
    start = 2025L, handoff_year = 2025L, depth = 0L, parent = NA,
    size_curve = list(tibble::tibble(year = 2025L, size = 50L)))   # single point
  confluences <- tibble::tibble(
    child = character(), parent = character(), n = integer(),
    cohort_size = integer(), first_feed_year = integer(),
    handoff_year = integer(), inflow_curve = list())
  ln <- .confluence_lanes(rivers, confluences, target = NULL)
  g <- .confluence_polygons(rivers, confluences, ln, last_year = 2025L,
                            smooth = FALSE)
  sp <- g$spines[g$spines$traj_id == "tr::c1g9", ]
  expect_gte(nrow(sp), 2L)                  # padded into a drawable ribbon
  expect_gt(diff(range(sp$x)), 0)           # has horizontal extent
})

test_that("a single intermediate target gives a focus view (feeders + destinations)", {
  conf <- conf_real()
  tr_id <- conf$rivers$traj_id[conf$rivers$type == "absorbed"][1]
  p <- plot_trajectory_confluence(conf, target = tr_id, min_n = 0)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_match(p$labels$title, paste0("Focus: ", tr_id), fixed = TRUE)
})

test_that("focus view errors clearly when conf lacks destinations (old object)", {
  conf <- conf_real()
  tr_id <- conf$rivers$traj_id[conf$rivers$type == "absorbed"][1]
  conf$destinations <- NULL
  expect_error(plot_trajectory_confluence(conf, target = tr_id),
               "destinations")
})

# --- plot_trajectory_confluence_interactive tests ---

test_that("plot_trajectory_confluence_interactive returns a built-able plotly", {
  conf <- conf_real()
  p <- plot_trajectory_confluence_interactive(conf, min_n = 0)
  expect_s3_class(p, "plotly")
  b <- plotly::plotly_build(p)
  expect_gt(length(b$x$data), 0)
  expect_match(b$x$layout$title$text, "confluence")
})

test_that("plot_trajectory_confluence_interactive honours keyword and id targets", {
  conf <- conf_real()
  cen <- paste0("tr::", conf$centrals[1])
  for (tg in list("finals", "intermediary", cen)) {
    expect_s3_class(
      plot_trajectory_confluence_interactive(conf, target = tg, min_n = 0), "plotly")
  }
  bf <- plotly::plotly_build(
    plot_trajectory_confluence_interactive(conf, target = "finals", min_n = 0))
  expect_match(bf$x$layout$title$text, "Final")
})

test_that("plot_trajectory_confluence_interactive validates its input", {
  expect_error(plot_trajectory_confluence_interactive(list(a = 1)),
               "sniff_trajectory_confluence")
})

# --- zero-destination intermediates are excluded --------------------------------
# In make_flow_tree_dpg: tr1 absorbed by central c1g3 delivers 2 docs to c1g3's
# final; tr2 (absorbed by tr1) delivers 0 (its terminal cohort lands outside the
# c1g3 final community). tr2 must never be drawn, even though it passes min_n = 0.

test_that(".confluence_drop_zero_dest cuts a 0-contribution tributary and its subtree", {
  conf <- conf_real()
  out <- .confluence_drop_zero_dest(conf$rivers, conf$confluences, conf,
                                    target = NULL, single = FALSE)
  expect_true("tr1" %in% out$rivers$traj_id)        # delivers 2 -> kept
  expect_false("tr2" %in% out$rivers$traj_id)       # delivers 0 -> dropped
  expect_false("tr2" %in% out$confluences$child)    # its inbound edge is gone too
})

drawn_traj_ids <- function(p) {
  unique(unlist(lapply(p$layers, function(L)
    if (is.data.frame(L$data) && "traj_id" %in% names(L$data))
      as.character(L$data$traj_id) else NULL)))
}

test_that("plot_trajectory_confluence drops a 0-contribution intermediate", {
  conf <- conf_real()
  ids <- drawn_traj_ids(plot_trajectory_confluence(conf, min_n = 0))
  expect_true("tr1" %in% ids)
  expect_false("tr2" %in% ids)
})

test_that("plot_trajectory_confluence_interactive drops a 0-contribution intermediate", {
  conf <- conf_real()
  b <- plotly::plotly_build(plot_trajectory_confluence_interactive(conf, min_n = 0))
  txt <- unlist(lapply(b$x$data, function(d) d$text))
  expect_true(any(grepl("tr1", txt, fixed = TRUE)))
  expect_false(any(grepl("tr2", txt, fixed = TRUE)))
})

test_that("year_range fixes the x time-axis window without dropping data", {
  conf <- conf_real()
  p0 <- plot_trajectory_confluence(conf, min_n = 0)
  wide <- c(1990, 2035)
  p1 <- plot_trajectory_confluence(conf, min_n = 0, year_range = wide)
  xr0 <- ggplot2::ggplot_build(p0)$layout$panel_params[[1]]$x.range
  xr1 <- ggplot2::ggplot_build(p1)$layout$panel_params[[1]]$x.range
  # the requested window sits fully inside the rendered panel
  expect_lte(xr1[1], wide[1])
  expect_gte(xr1[2], wide[2])
  # a wider window extends the axis beyond the default data fit
  expect_lt(xr1[1], xr0[1])
  expect_gt(xr1[2], xr0[2])
  # and no trajectory is dropped: year_range never filters the data
  expect_setequal(drawn_traj_ids(p1), drawn_traj_ids(p0))
})
