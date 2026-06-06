#' Default, overridable thresholds for v2 dynamic-state classification
#'
#' Knobs for [sniff_trajectory_dynamics()] and [sniff_group_dynamics()]. Override
#' by passing a modified list.
#'
#' @return A list: `emergence_growth` (recent growth at or above this is emerging;
#'   default 0.15), `dormancy_growth` (recent growth at or below this is dormant,
#'   i.e. alive but stalled; default 0.02), `formation_entropy` (a final group
#'   whose normalized feeder `source_entropy` is at or above this has a divergent
#'   formation; default 0.7).
#' @export
default_state_thresholds <- function() {
  list(
    emergence_growth = 0.15,
    dormancy_growth = 0.02,
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

#' Growth and convergence dynamics of v2 soft trajectories
#'
#' For each [detect_soft_trajectories()] trajectory, measures its size-curve
#' growth and age (the growth axis) and whether it merged into a shared backbone
#' (the convergence axis). Continuous columns are always returned; `growth_phase`
#' is the tunable overlay. Since every v2 trajectory is living, there is no
#' dormancy-by-death: `dormancy` here means alive but stalled (recent growth at or
#' below `dormancy_growth`).
#'
#' @param detected A [detect_soft_trajectories()] object.
#' @param docs_per_group Per-year membership tibble. Defaults to
#'   `detected$docs_per_group`.
#' @param thresholds A [default_state_thresholds()]-shaped list (default).
#' @param growth_window Number of curve points (present years) over which
#'   `recent_growth` is measured (default 3); a shorter curve is measured whole.
#' @param last_year Final year (default: max `network_until`).
#'
#' @return A tibble, one row per trajectory, sorted by descending
#'   `emergence_score`: `traj_id`, `terminal_group`, `birth`, `start`, `end`,
#'   `age` (years since birth), `size`, `recent_growth`, `growth_phase`,
#'   `converges` (merged into a shared backbone), `merge_year`, `emergence_score`
#'   (`zscore(-age) + zscore(recent_growth)`: young and growing).
#'
#' @seealso [sniff_group_dynamics()], [default_state_thresholds()]
#' @export
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
sniff_trajectory_dynamics <- function(detected, docs_per_group = NULL,
                                      thresholds = NULL, growth_window = 3,
                                      last_year = NULL) {
  if (is.null(docs_per_group)) docs_per_group <- detected$docs_per_group
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!is.data.frame(docs_per_group) || !all(required_cols %in% names(docs_per_group))) {
    stop("'docs_per_group' must contain columns: ",
         paste(required_cols, collapse = ", "), call. = FALSE)
  }
  if (is.null(thresholds)) thresholds <- default_state_thresholds()
  tr <- detected$trajectories
  if (is.null(tr) || nrow(tr) == 0) {
    stop("'detected' contains no trajectories", call. = FALSE)
  }
  if (is.null(last_year)) last_year <- max(docs_per_group$network_until, na.rm = TRUE)

  node_size <- .node_size_lookup(docs_per_group)
  rows <- vector("list", nrow(tr))
  for (i in seq_len(nrow(tr))) {
    gs <- .feeder_growth_series(tr$nodes[[i]], node_size)
    n <- nrow(gs)
    size <- if (n > 0) gs$size[n] else 0L
    ref <- max(1L, n - growth_window)
    s_ref <- if (n > 0) gs$size[ref] else 0
    s_end <- if (n > 0) gs$size[n] else 0
    recent_growth <- if (s_ref > 0) (s_end - s_ref) / s_ref else NA_real_
    rows[[i]] <- tibble::tibble(
      traj_id = tr$traj_id[i], terminal_group = tr$terminal_group[i],
      birth = tr$birth[i], start = tr$start[i], end = tr$end[i],
      age = last_year - tr$start[i] + 1L, size = size,
      recent_growth = recent_growth,
      growth_phase = .classify_growth_phase(
        recent_growth, thresholds$emergence_growth, thresholds$dormancy_growth),
      converges = !is.na(tr$merge_year[i]), merge_year = tr$merge_year[i]
    )
  }
  dyn <- dplyr::bind_rows(rows)
  dyn$emergence_score <- .zscore(-dyn$age) + .zscore(.na_to_zero(dyn$recent_growth))
  dplyr::arrange(dyn, dplyr::desc(.data$emergence_score))
}

