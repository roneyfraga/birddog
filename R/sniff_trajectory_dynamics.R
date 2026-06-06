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

