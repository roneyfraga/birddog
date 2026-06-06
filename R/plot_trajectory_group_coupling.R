#' Plot the trajectory x group coupling as a heatmap
#'
#' A compact heatmap of the trajectory x group bipartite from
#' [sniff_trajectory_group_contribution()]: rows are trajectories (grouped by
#' their terminal group), columns are final groups, fill is the coupling strength.
#' The cell where a trajectory's terminal group meets its column is outlined, so
#' the one-to-one terminal coupling (where each lineage ends) reads on top of the
#' many-to-many contribution coupling (where its papers spread). Unlike
#' [plot_trajectory_group_bipartite()] (a two-column graph) this stays legible at
#' full density and marks the terminal destination.
#'
#' @param contribution Output of [sniff_trajectory_group_contribution()].
#' @param fill Tile-colour measure: `"n_shared"` (default, log colour),
#'   `"prop_of_group"`, or `"prop_of_traj"` (linear colour).
#' @param mark_terminal Outline each trajectory's terminal-group cell (default TRUE).
#' @return A ggplot object.
#' @seealso [sniff_trajectory_group_contribution()], [plot_trajectory_group_bipartite()]
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs
#' @importFrom ggplot2 theme_minimal theme element_text element_blank
#' @importFrom rlang .data
plot_trajectory_group_coupling <- function(contribution,
                                           fill = c("n_shared", "prop_of_group", "prop_of_traj"),
                                           mark_terminal = TRUE) {
  fill <- match.arg(fill)
  if (!is.list(contribution) || is.null(contribution$long) ||
      is.null(contribution$trajectories) || is.null(contribution$incidence)) {
    stop("'contribution' must be the output of sniff_trajectory_group_contribution()",
         call. = FALSE)
  }
  long <- contribution$long
  if (nrow(long) == 0) {
    stop("'contribution' has no trajectory-group coupling to plot", call. = FALSE)
  }
  tr_tbl <- contribution$trajectories
  tr_order <- tr_tbl$traj_id[order(tr_tbl$terminal_group, -tr_tbl$reach)]
  grp_order <- mixed_sort(colnames(contribution$incidence))
  long$traj_id <- factor(long$traj_id, levels = rev(tr_order))
  long$group_final <- factor(long$group_final, levels = grp_order)
  long$is_terminal <- long$terminal_group == as.character(long$group_final)

  p <- ggplot2::ggplot(long, ggplot2::aes(.data$group_final, .data$traj_id,
                                          fill = .data[[fill]])) +
    ggplot2::geom_tile(colour = "grey92")
  if (mark_terminal) {
    p <- p + ggplot2::geom_tile(
      data = long[long$is_terminal, , drop = FALSE],
      fill = NA, colour = "black", linewidth = 0.8
    )
  }
  p <- p +
    (if (fill == "n_shared") {
      ggplot2::scale_fill_viridis_c(transform = "log10", option = "C")
    } else {
      ggplot2::scale_fill_viridis_c(option = "C")
    }) +
    ggplot2::labs(x = "final group", y = "trajectory (grouped by terminal group)",
                  fill = fill, title = "Trajectory-group coupling") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
  p
}
