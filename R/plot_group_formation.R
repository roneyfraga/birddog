#' Plot a group's formation (2D confluence river)
#'
#' Thin wrapper over [plot_trajectory_formation_2d()] for a
#' [sniff_group_formation()] object: the final group is the spine, its tributary
#' trajectories converge into it over time.
#'
#' @param formation A [sniff_group_formation()] object.
#' @param ... Passed to [plot_trajectory_formation_2d()] (e.g. `feeder_curve`,
#'   `max_feeders`, `label_size`).
#' @return A `ggplot` object.
#' @seealso [sniff_group_formation()], [plot_trajectory_formation_2d()]
#' @export
plot_group_formation_2d <- function(formation, ...) {
  plot_trajectory_formation_2d(formation, ...)
}

#' Plot a group's formation (3D confluence)
#'
#' Thin wrapper over [plot_trajectory_formation_3d()] for a
#' [sniff_group_formation()] object.
#'
#' @param formation A [sniff_group_formation()] object.
#' @param ... Passed to [plot_trajectory_formation_3d()].
#' @return A plotly object.
#' @seealso [sniff_group_formation()], [plot_trajectory_formation_3d()]
#' @export
plot_group_formation_3d <- function(formation, ...) {
  plot_trajectory_formation_3d(formation, ...)
}
