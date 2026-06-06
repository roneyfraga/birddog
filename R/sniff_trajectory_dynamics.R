#' Default, overridable thresholds for dynamic-state classification
#'
#' Knobs for [sniff_trajectory_dynamics()] and [sniff_group_dynamics()]. Override
#' by passing a modified list.
#'
#' @return A list: `emergence_growth` (default 0.15), `dormancy_growth` (0.02),
#'   `convergence_entropy` (a stopped/absorbed trajectory whose normalized
#'   destination entropy is below this converged, else diverged; default 0.5),
#'   `dormancy_share` (an absorbed trajectory whose terminal cohort drops at least
#'   this share is dormant; default 0.5), `formation_entropy` (0.7; used by the
#'   soon-removed group dynamics).
#' @export
default_state_thresholds <- function() {
  list(
    emergence_growth = 0.15,
    dormancy_growth = 0.02,
    convergence_entropy = 0.5,
    dormancy_share = 0.5,
    formation_entropy = 0.7
  )
}

#' Z-score with NA and zero-variance guards
#' @keywords internal
.zscore <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  z <- (x - m) / s
  z[is.na(z)] <- 0
  z
}

#' Emergence / maturity / dormancy from a recent-growth value
#'
#' @param recent_growth Numeric (fractional growth over the recent window); `NA`
#'   when the curve is too short to measure (treated as maturity).
#' @param emergence_growth,dormancy_growth Thresholds from [default_state_thresholds()].
#' @return Character vector of `"emergence"`, `"maturity"`, or `"dormancy"`.
#' @keywords internal
.classify_growth_phase <- function(recent_growth, emergence_growth, dormancy_growth) {
  ifelse(
    is.na(recent_growth), "maturity",
    ifelse(recent_growth >= emergence_growth, "emergence",
           ifelse(recent_growth <= dormancy_growth, "dormancy", "maturity"))
  )
}

#' Normalized Shannon entropy of a terminal cohort's destination distribution
#' @keywords internal
.dest_entropy_norm <- function(destination) {
  d <- destination[destination$g_final != "(dropped)" & destination$n > 0, , drop = FALSE]
  if (nrow(d) <= 1) return(0)
  p <- d$n / sum(d$n)
  -sum(p * log(p)) / log(nrow(d))
}

#' Five-state classifier for flow trajectories
#'
#' Central trajectories split into emergence/maturity/dormancy by growth; absorbed
#' ones into convergence/divergence by their terminal cohort's destination entropy,
#' or dormancy when the cohort mostly vanishes or the lineage is extinct.
#' @keywords internal
.classify_flow_state <- function(dyn, thresholds) {
  th <- thresholds
  vapply(seq_len(nrow(dyn)), function(i) {
    if (identical(dyn$type[i], "central")) {
      rg <- dyn$recent_growth[i]
      if (!is.na(rg) && rg >= th$emergence_growth) "emergence"
      else if (is.na(rg) || rg <= th$dormancy_growth) "dormancy"
      else "maturity"
    } else {
      if (is.na(dyn$group[i])) {
        "dormancy"                                   # extinct: no destination
      } else if (!is.na(dyn$dormant_share[i]) && dyn$dormant_share[i] >= th$dormancy_share) {
        "dormancy"
      } else if (is.na(dyn$dest_entropy[i]) || dyn$dest_entropy[i] < th$convergence_entropy) {
        "convergence"
      } else {
        "divergence"
      }
    }
  }, character(1))
}

#' Dynamic-state indicators and 5-state classification for flow trajectories
#'
#' For each [sniff_trajectory_flow()] trajectory: a forward lens (size-curve growth
#' and age) and, for absorbed trajectories, a backward lens (where its terminal
#' cohort goes, via [sniff_trajectory_destination()]). Central trajectories are
#' emergence/maturity/dormancy by growth; absorbed are convergence/divergence by
#' destination entropy (or dormancy if their cohort vanishes / they are extinct).
#'
#' @param flow A [sniff_trajectory_flow()] object.
#' @param thresholds A [default_state_thresholds()]-shaped list (default).
#' @param growth_window Curve points over which `recent_growth` is measured (default 3).
#'
#' @return A tibble, one row per trajectory, sorted by descending `emergence_score`:
#'   `traj_id`, `type`, `group`, `start`, `end`, `age`, `size`, `recent_growth`,
#'   `dest_entropy`, `dormant_share`, `emergence_score`, `state`.
#'
#' @seealso [sniff_trajectory_flow()], [sniff_trajectory_destination()],
#'   [default_state_thresholds()]
#' @export
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
sniff_trajectory_dynamics <- function(flow, thresholds = NULL, growth_window = 3) {
  if (!is.list(flow) || is.null(flow$trajectories) || !is.data.frame(flow$trajectories) ||
      !"absorbed_into" %in% names(flow$trajectories)) {
    stop("'flow' must be a sniff_trajectory_flow() object", call. = FALSE)
  }
  if (is.null(thresholds)) thresholds <- default_state_thresholds()
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  node_size <- .node_size_lookup(dpg)

  rows <- vector("list", nrow(tr))
  for (i in seq_len(nrow(tr))) {
    gs <- .feeder_growth_series(tr$nodes[[i]], node_size)
    n <- nrow(gs)
    size <- if (n > 0) gs$size[n] else 0L
    ref <- max(1L, n - growth_window)
    s_ref <- if (n > 0) gs$size[ref] else 0
    s_end <- if (n > 0) gs$size[n] else 0
    recent_growth <- if (s_ref > 0) (s_end - s_ref) / s_ref else NA_real_

    dest_entropy <- NA_real_
    dormant_share <- NA_real_
    if (identical(tr$type[i], "absorbed") && !is.na(tr$group[i])) {
      dd <- sniff_trajectory_destination(flow, tr$traj_id[i])
      dest_entropy <- .dest_entropy_norm(dd$destination)
      dormant_share <- dd$dormant_share
    }
    rows[[i]] <- tibble::tibble(
      traj_id = tr$traj_id[i], type = tr$type[i], group = tr$group[i],
      start = tr$start[i], end = tr$end[i],
      age = tr$end[i] - tr$start[i] + 1L, size = size,
      recent_growth = recent_growth, dest_entropy = dest_entropy,
      dormant_share = dormant_share
    )
  }
  dyn <- dplyr::bind_rows(rows)
  dyn$emergence_score <- .zscore(-dyn$age) + .zscore(.na_to_zero(dyn$recent_growth))
  dyn$state <- .classify_flow_state(dyn, thresholds)
  dplyr::arrange(dyn, dplyr::desc(.data$emergence_score))
}

