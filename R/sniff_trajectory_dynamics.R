#' Fixed-constant thresholds for dynamic-state classification
#'
#' The fixed knobs for [sniff_trajectory_dynamics()]. These are **not** the applied
#' default: by default the cut points are derived from the data via
#' [data_state_thresholds()], and these constants serve as the explicit
#' fixed-constant option (pass this list as `thresholds`) and as the fallback for
#' any quantity that cannot be derived. The classification follows two axes (see
#' [sniff_trajectory_dynamics()]): a life-cycle **phase** for living (central)
#' trajectories and a terminal **fate** for declining (absorbed) ones.
#'
#' @return A list:
#'   `emergence_growth` (min annualized `growth_rate` for a central to count as
#'   emergence; default 0.15), `emergence_novelty` (min `novelty` for emergence;
#'   default 0.30), `decline_growth` (a central at or below this `growth_rate` is
#'   dormant by stall; default -0.05), `convergence_entropy` (an absorbed
#'   trajectory whose normalized destination entropy is below this converged, else
#'   diverged; default 0.5), `dormancy_share` (an absorbed trajectory whose
#'   terminal cohort drops at least this share is dormant by death; default 0.5).
#' @export
fixed_state_thresholds <- function() {
  list(
    emergence_growth = 0.15,
    emergence_novelty = 0.30,
    decline_growth = -0.05,
    convergence_entropy = 0.5,
    dormancy_share = 0.5
  )
}

#' Data-driven, outlier-robust dynamic-state thresholds
#'
#' Derives a [fixed_state_thresholds()]-shaped list from an observed
#' [sniff_trajectory_dynamics()] table, so the cut points adapt to the dataset
#' instead of fixed constants. The growth cuts are placed `k` robust deviations
#' (median absolute deviation) from the median, which resists outliers such as a
#' single fast-growing young core; the bounded metrics use their median as the
#' neutral split, and `dormancy_share` keeps the absolute majority rule (its
#' distribution is usually too concentrated at zero to standardize).
#'
#' @param dyn A [sniff_trajectory_dynamics()] tibble; only its metric columns are
#'   read, so the classification it currently carries is irrelevant.
#' @param k Number of robust deviations (scaled MAD) above/below the median for the
#'   growth cuts (default 1): `emergence_growth = median + k*MAD` and
#'   `decline_growth = median - k*MAD` over the living cores' `growth_rate`. Larger
#'   `k` widens the maturity band (fewer emergence / dormancy calls).
#'
#' @return A list shaped like [fixed_state_thresholds()]:
#'   `emergence_growth`, `decline_growth` (robust, from the centrals' growth),
#'   `emergence_novelty` (median novelty of the centrals), `convergence_entropy`
#'   (median destination entropy of the absorbed), and `dormancy_share` (the
#'   default majority rule). Any quantity that cannot be derived (too few points,
#'   zero spread) falls back to its [fixed_state_thresholds()] value.
#'
#' @details
#' Use it as a drop-in for the `thresholds` argument; the metrics are
#' threshold-independent, so one pass is enough to derive and re-classify:
#' `dyn <- sniff_trajectory_dynamics(flow)`,
#' `th <- data_state_thresholds(dyn)`,
#' `dyn <- sniff_trajectory_dynamics(flow, thresholds = th)`.
#'
#' @seealso [fixed_state_thresholds()], [sniff_trajectory_dynamics()],
#'   [plot_trajectory_dynamics()]
#' @export
data_state_thresholds <- function(dyn, k = 1) {
  need <- c("type", "growth_rate", "novelty", "dest_entropy", "dormant_share")
  if (!is.data.frame(dyn) || !all(need %in% names(dyn))) {
    stop("'dyn' must be the output of sniff_trajectory_dynamics()", call. = FALSE)
  }
  th <- fixed_state_thresholds()
  cen <- dyn[dyn$type == "central", , drop = FALSE]
  ab  <- dyn[dyn$type == "absorbed", , drop = FALSE]

  g <- .robust_center_scale(cen$growth_rate)
  if (!any(is.na(g)) && g[2] > 0) {
    th$emergence_growth <- g[1] + k * g[2]
    th$decline_growth   <- g[1] - k * g[2]
  }
  nv <- stats::median(cen$novelty, na.rm = TRUE)
  if (!is.na(nv)) th$emergence_novelty <- nv
  en <- stats::median(ab$dest_entropy, na.rm = TRUE)
  if (!is.na(en)) th$convergence_entropy <- en
  th
}

#' Robust centre and scale (median, scaled MAD) with zero-spread fallbacks
#'
#' The scale is the scaled MAD (consistent with a standard deviation under
#' normality), falling back to `IQR / 1.349` then `sd` when the MAD is zero, so a
#' single outlier cannot inflate it. Returns `c(centre, scale)`.
#' @keywords internal
.robust_center_scale <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 1) return(c(NA_real_, NA_real_))
  m <- stats::median(x)
  s <- stats::mad(x)
  if (is.na(s) || s == 0) s <- stats::IQR(x) / 1.349
  if (is.na(s) || s == 0) s <- stats::sd(x)
  c(m, s)
}

#' Outlier-robust z-score (modified z): (x - median) / MAD, NA -> 0
#' @keywords internal
.robust_z <- function(x) {
  cs <- .robust_center_scale(x)
  if (is.na(cs[2]) || cs[2] == 0) return(rep(0, length(x)))
  z <- (x - cs[1]) / cs[2]
  z[is.na(z)] <- 0
  z
}

#' Normalized Shannon entropy of a terminal cohort's destination distribution
#' @keywords internal
.dest_entropy_norm <- function(destination) {
  d <- destination[destination$g_final != "(dropped)" & destination$n > 0, , drop = FALSE]
  if (nrow(d) <= 1) return(0)
  p <- d$n / sum(d$n)
  -sum(p * log(p)) / log(nrow(d))
}

#' Exponential growth of a trajectory's recent size curve (pillar: growth)
#'
#' Annualized growth rate from a log-linear fit of size on year over the last
#' `window` + 1 curve points (the trajectory's recent momentum, in the spirit of
#' the field-level exponential model y(t) = b0 e^(b1 (t - t0))). Returns
#' `growth_rate` = exp(b1) - 1 and `doubling_time` = ln(2) / b1 (NA unless growing).
#' @keywords internal
.traj_growth <- function(gs, window = 3) {
  n <- nrow(gs)
  if (n < 2) return(list(growth_rate = NA_real_, doubling_time = NA_real_))
  idx <- seq.int(max(1L, n - window), n)
  sub <- gs[idx, , drop = FALSE]
  ok <- sub$size > 0
  yr <- sub$year[ok]; ly <- log(sub$size[ok])
  if (length(unique(yr)) < 2) return(list(growth_rate = NA_real_, doubling_time = NA_real_))
  b1 <- unname(stats::coef(stats::lm(ly ~ yr))[2])
  list(growth_rate = exp(b1) - 1,
       doubling_time = if (is.finite(b1) && b1 > 0) log(2) / b1 else NA_real_)
}

#' Novelty of a trajectory: share of its documents that arrived recently
#'
#' Each document's arrival year is the first year it appears in one of the
#' trajectory's own nodes; novelty is the share arriving within the last `window`
#' years of the timeline (pillar: novelty -- "recent documents tend to be
#' emergent").
#' @keywords internal
.traj_novelty <- function(nodes, docs_per_group, last_year, window = 5) {
  sub <- docs_per_group[docs_per_group$group_id %in% nodes,
                        c("document_id", "network_until"), drop = FALSE]
  if (nrow(sub) == 0) return(NA_real_)
  arrival <- tapply(sub$network_until, sub$document_id, min)
  mean(arrival >= last_year - window + 1)
}

#' Share of a trajectory's year-steps that keep growing at the emergence rate
#'
#' Staying power in the spirit of Carley et al. (2017): the fraction of a
#' lineage's consecutive-year steps whose local cumulative growth clears
#' `growth_bar` (the `emergence_growth` cut). High = the lineage stayed in the
#' emergent regime year after year; low = it grew once and plateaued. `NA` for a
#' single-node lineage (no step to score). Unlike `age`, it is not a span: a long
#' lineage that grew briefly scores low, a short one that kept growing scores high.
#' @keywords internal
.traj_emergence_density <- function(gs, growth_bar) {
  sz <- gs$size[gs$size > 0]
  if (length(sz) < 2) return(NA_real_)
  mean((exp(diff(log(sz))) - 1) >= growth_bar)
}

#' Life-cycle phase of a living (central) trajectory (S-curve axis)
#'
#' emergence (novelty + growth), dormancy (sustained loss of momentum), or
#' maturity (saturation, the default in between).
#' @keywords internal
.classify_phase <- function(growth_rate, novelty, th) {
  if (!is.na(growth_rate) && !is.na(novelty) &&
      growth_rate >= th$emergence_growth && novelty >= th$emergence_novelty) {
    "emergence"
  } else if (!is.na(growth_rate) && growth_rate <= th$decline_growth) {
    "dormancy"
  } else {
    "maturity"
  }
}

#' Terminal fate of a declining (absorbed) trajectory (destination axis)
#'
#' Resolves where the terminal cohort goes: dormancy (extinct, or it mostly drops
#' out of every final group -- true death), convergence (it concentrates in a
#' single living trajectory: low destination entropy), or divergence (it scatters
#' across several: high entropy).
#' @keywords internal
.classify_fate <- function(group, dest_entropy, dormant_share, th) {
  if (is.na(group)) {
    "dormancy"
  } else if (!is.na(dormant_share) && dormant_share >= th$dormancy_share) {
    "dormancy"
  } else if (is.na(dest_entropy) || dest_entropy < th$convergence_entropy) {
    "convergence"
  } else {
    "divergence"
  }
}

#' Dynamic-state indicators and classification for flow trajectories
#'
#' Characterizes each [sniff_trajectory_braid()] trajectory along the evolutionary
#' states of the method (emergence, convergence, divergence, dormancy), grounding
#' the four emergence pillars (novelty, growth, community, persistence) in two
#' complementary lenses tied to the flow model.
#'
#' **Forward lens (all trajectories).** `growth_rate` and `doubling_time` (an
#' exponential fit of the recent size curve), `novelty` (share of documents that
#' arrived recently), `recruitment` (net documents gained over the lifespan, the
#' power of attraction), `emergence_density` (staying power: the share of the
#' lineage's year-steps still growing at the emergence rate, after Carley et al.
#' 2017), and `age`/`size` (persistence and community).
#'
#' **Backward lens (absorbed trajectories only).** Where the terminal cohort goes,
#' via [sniff_trajectory_destination()]: `dest_entropy` (normalized Shannon
#' entropy of the destination split) and `dormant_share` (share dropping out).
#'
#' **Inflow lens (central trajectories only).** `attraction_inflow`, the documents
#' of the final community delivered by absorbed tributaries (the consolidating side
#' of convergence), from [sniff_trajectory_self_sufficiency()].
#'
#' **Reach lens (all trajectories).** `reach_ratio`, the share of the future field
#' a lineage's origin seeds -- its birth node's forward temporal reach in the DAG
#' over all nodes born later (after the temporal-reachability of Marino & Silva
#' 2023). Structural downstream influence, distinct from size and, via the ratio,
#' from age.
#'
#' Classification uses two axes: a life-cycle **`phase`** for living (central)
#' trajectories -- emergence / maturity / dormancy(stall) by growth and novelty --
#' and a terminal **`fate`** for declining (absorbed) ones -- convergence /
#' divergence / dormancy(death) by destination. `state` is `phase` for centrals and
#' `fate` for absorbed; `phase` and `fate` are kept separate so the two senses of
#' dormancy (a stalled living core vs a dead cohort) stay distinguishable. The
#' `emergence_index` (a within-central combination of growth, novelty and
#' recruitment) is computed for living trajectories only, so an absorbed lineage
#' that grew before dying is never ranked as emerging. Because `recruitment` is an
#' extensive (size-carrying) pillar, the full index partly tracks community size;
#' `emergence_index_intensive` drops it (growth and novelty only) for a rate-based
#' ranking that does not reward a large core for its volume alone.
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param thresholds A [fixed_state_thresholds()]-shaped list. `NULL` (default)
#'   **derives the cut points from the data** via [data_state_thresholds()] with
#'   the `k` below, so the classification adapts to the dataset; pass
#'   `fixed_state_thresholds()` to force the fixed constants instead. The
#'   thresholds actually used are attached to the result as
#'   `attr(result, "state_thresholds")`, which the plots read back.
#' @param k Robust deviations for the data-driven thresholds when `thresholds`
#'   is `NULL` (default 1); see [data_state_thresholds()]. Ignored when an explicit
#'   `thresholds` list is supplied.
#' @param growth_window Curve points over which `growth_rate` is fitted (default 3).
#' @param novelty_window Years counted as recent for `novelty` (default 5).
#' @param winsorize Cap each pillar's robust z-score at `c(-winsorize, +winsorize)`
#'   before summing into `emergence_index`, so no single extreme pillar (e.g. a
#'   young core 18 MADs out on growth) dominates the ranking. Default `3` (near the
#'   Iglewicz--Hoaglin modified-z outlier boundary of 3.5); `NULL` leaves the robust
#'   z-scores uncapped.
#' @param cct Optional [sniff_trajectory_cct()] output (one row per `traj_id`),
#'   left-joined to add the `cct` list-column (per-year renewal-pace series); `NULL`
#'   (default) omits it.
#' @param entropy Optional [sniff_trajectory_entropy()] output (one row per
#'   `traj_id`), left-joined to add the `keyword_entropy` list-column (per-year
#'   keyword-diversity series); `NULL` (default) omits it.
#' @param hubs Optional [sniff_trajectory_hubs()] output (one row per
#'   `traj_id`), left-joined to add the hub-role columns; `NULL` (default) omits
#'   them.
#'
#' @return A tibble, one row per trajectory, sorted by descending
#'   `emergence_index` (absorbed last): `traj_id`, `type`, `group`, `start`, `end`,
#'   `age`, `size`, `growth_rate`, `doubling_time`, `novelty`, `recruitment`,
#'   `emergence_density`, `attraction_inflow`, `dest_entropy`, `dormant_share`,
#'   `phase`, `fate`,
#'   `state`, `emergence_index`, `emergence_index_intensive` (the same index
#'   without the extensive `recruitment` pillar), `reach_ratio`, plus any
#'   `cct` / `keyword_entropy` (per-year list-columns) and `hubs` columns.
#'
#' @seealso [sniff_trajectory_braid()], [sniff_trajectory_destination()],
#'   [sniff_trajectory_self_sufficiency()], [sniff_trajectory_cct()],
#'   [sniff_trajectory_entropy()], [sniff_trajectory_hubs()],
#'   [fixed_state_thresholds()]
#' @family trajectory analysis
#' @export
#' @importFrom dplyr bind_rows arrange desc left_join any_of select
#' @importFrom tibble tibble
#' @importFrom rlang .data
sniff_trajectory_dynamics <- function(flow, thresholds = NULL, k = 1,
                                      growth_window = 3, novelty_window = 5,
                                      winsorize = 3, cct = NULL, entropy = NULL,
                                      hubs = NULL) {
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  if (!is.null(winsorize) && (!is.numeric(winsorize) || length(winsorize) != 1 ||
                              is.na(winsorize) || winsorize <= 0)) {
    stop("'winsorize' must be a single positive number or NULL", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 0) {
    stop("'k' must be a single non-negative number", call. = FALSE)
  }
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  last_year <- as.integer(flow$last_year)
  node_size <- .node_size_lookup(dpg)

  # central inflow (consolidation power of attraction): the documents of each
  # central's final community delivered by its tributaries, on destination
  # semantics -- reuse the self-sufficiency index rather than recompute.
  ss <- .self_sufficiency_from_flow(flow, min_size = 1L)
  ss_inflow <- stats::setNames(ss$inflow, ss$group)

  rows <- vector("list", nrow(tr))
  for (i in seq_len(nrow(tr))) {
    gs <- .feeder_growth_series(tr$nodes[[i]], node_size)
    n <- nrow(gs)
    size <- if (n > 0) gs$size[n] else 0L
    recruitment <- if (n > 0) gs$size[n] - gs$size[1] else 0L
    gr <- .traj_growth(gs, window = growth_window)
    novelty <- .traj_novelty(tr$nodes[[i]], dpg, last_year, window = novelty_window)

    dest_entropy <- NA_real_
    dormant_share <- NA_real_
    if (identical(tr$type[i], "absorbed") && !is.na(tr$group[i])) {
      dd <- sniff_trajectory_destination(flow, tr$traj_id[i])
      dest_entropy <- .dest_entropy_norm(dd$destination)
      dormant_share <- dd$dormant_share
    }
    inflow <- if (identical(tr$type[i], "central") && tr$group[i] %in% names(ss_inflow)) {
      as.integer(ss_inflow[[tr$group[i]]])
    } else {
      NA_integer_
    }
    rows[[i]] <- tibble::tibble(
      traj_id = tr$traj_id[i], type = tr$type[i], group = tr$group[i],
      start = tr$start[i], end = tr$end[i],
      age = tr$end[i] - tr$start[i] + 1L, size = size,
      growth_rate = gr$growth_rate, doubling_time = gr$doubling_time,
      novelty = novelty, recruitment = recruitment, attraction_inflow = inflow,
      dest_entropy = dest_entropy, dormant_share = dormant_share
    )
  }
  dyn <- dplyr::bind_rows(rows)

  # by default the cut points are DERIVED FROM THE DATA (robust median/MAD), so the
  # classification adapts to the dataset; an explicit `thresholds` list (e.g.
  # fixed_state_thresholds()) overrides this with fixed constants. The metrics
  # above are threshold-independent, so deriving here is exact.
  if (is.null(thresholds)) thresholds <- data_state_thresholds(dyn, k = k)

  # staying power: share of each lineage's year-steps still growing at the
  # emergence rate (uses the same emergence_growth cut as the phase axis).
  gbar <- thresholds$emergence_growth
  dyn$emergence_density <- vapply(seq_len(nrow(tr)), function(i)
    .traj_emergence_density(.feeder_growth_series(tr$nodes[[i]], node_size), gbar),
    numeric(1))

  # downstream reach (propagation potential): share of the future field a
  # lineage's origin seeds -- its birth node's forward reach in the DAG over all
  # nodes born later. The ratio strips the trivial scaling of raw reach with age,
  # leaving the structural influence (distinct from size and from hubs).
  g <- flow$graph
  vyear <- stats::setNames(.extract_year(igraph::V(g)$name), igraph::V(g)$name)
  dyn$reach_ratio <- vapply(seq_len(nrow(tr)), function(i) {
    ns <- tr$nodes[[i]]
    birth <- ns[which.min(.extract_year(ns))]
    if (!birth %in% names(vyear)) return(NA_real_)
    den <- sum(vyear > vyear[[birth]])
    if (den <= 0) return(NA_real_)
    (length(igraph::subcomponent(g, birth, mode = "out")) - 1L) / den
  }, numeric(1))

  cen <- dyn$type == "central"
  dyn$phase <- NA_character_
  dyn$fate <- NA_character_
  th <- thresholds
  if (any(cen)) dyn$phase[cen] <- vapply(which(cen), function(i)
    .classify_phase(dyn$growth_rate[i], dyn$novelty[i], th), character(1))
  if (any(!cen)) dyn$fate[!cen] <- vapply(which(!cen), function(i)
    .classify_fate(dyn$group[i], dyn$dest_entropy[i], dyn$dormant_share[i], th), character(1))
  dyn$state <- ifelse(cen, dyn$phase, dyn$fate)

  # emergence index: living population only, so a tributary that grew before being
  # absorbed is never ranked as emerging. Within-central robust z (median/MAD) of
  # the emergence pillars, so a single fast-growing outlier does not compress the
  # ranking of every other core.
  dyn$emergence_index <- NA_real_
  dyn$emergence_index_intensive <- NA_real_
  if (any(cen)) {
    wz <- function(x) {
      z <- .robust_z(x)
      if (!is.null(winsorize)) z <- pmax(-winsorize, pmin(winsorize, z))
      z
    }
    zg <- wz(dyn$growth_rate[cen]); zn <- wz(dyn$novelty[cen])
    dyn$emergence_index[cen]           <- zg + zn + wz(dyn$recruitment[cen])
    dyn$emergence_index_intensive[cen] <- zg + zn
  }

  if (!is.null(cct)) {
    dyn <- dplyr::left_join(dyn, dplyr::select(cct, -dplyr::any_of(c("type", "group"))),
                            by = "traj_id")
  }
  if (!is.null(entropy)) {
    dyn <- dplyr::left_join(dyn, dplyr::select(entropy, -dplyr::any_of(c("type", "group"))),
                            by = "traj_id")
  }
  if (!is.null(hubs)) {
    dyn <- dplyr::left_join(dyn, dplyr::select(hubs, -dplyr::any_of(c("type", "group"))),
                            by = "traj_id")
  }

  out <- dplyr::arrange(dyn, dplyr::desc(.data$emergence_index), dplyr::desc(.data$size))
  attr(out, "state_thresholds") <- thresholds   # so the plots reuse the same cuts
  out
}
