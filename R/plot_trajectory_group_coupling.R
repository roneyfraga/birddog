#' Trajectory x final-group paper-overlap incidence for a flow object
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
.coupling_incidence_flow <- function(flow) {
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  last_year <- flow$last_year
  node_docs <- split(dpg$document_id, dpg$group_id)

  fin <- dpg[dpg$network_until == last_year, c("document_id", "group"), drop = FALSE]
  fin <- fin[!duplicated(fin$document_id), , drop = FALSE]
  final_groups <- mixed_sort(unique(fin$group))
  final_size <- table(fin$group)
  doc_final <- stats::setNames(fin$group, fin$document_id)

  long_list <- list()
  for (i in seq_len(nrow(tr))) {
    docs <- unique(unlist(node_docs[tr$nodes[[i]]], use.names = FALSE))
    n_traj <- length(docs)
    if (n_traj == 0) next
    fg <- doc_final[docs]
    fg <- fg[!is.na(fg)]
    if (length(fg) == 0) next
    tab <- table(fg)
    long_list[[length(long_list) + 1]] <- tibble::tibble(
      traj_id = tr$traj_id[i],
      terminal_group = if (is.na(tr$group[i])) "(extinct)" else tr$group[i],
      group_final = names(tab),
      n_shared = as.integer(tab),
      prop_of_group = as.integer(tab) / as.integer(final_size[names(tab)]),
      prop_of_traj = as.integer(tab) / n_traj
    )
  }
  long <- if (length(long_list)) dplyr::bind_rows(long_list) else tibble::tibble(
    traj_id = character(), terminal_group = character(), group_final = character(),
    n_shared = integer(), prop_of_group = double(), prop_of_traj = double())

  incidence <- matrix(0L, nrow = length(unique(long$traj_id)),
                      ncol = length(final_groups),
                      dimnames = list(unique(long$traj_id), final_groups))
  if (nrow(long) > 0) incidence[cbind(long$traj_id, long$group_final)] <- long$n_shared
  reach <- stats::setNames(rowSums(incidence), rownames(incidence))
  list(long = long, incidence = incidence, reach = reach)
}

#' Plot the trajectory x group coupling as a heatmap
#'
#' A compact heatmap of how each flow trajectory's papers spread across the final
#' groups. Rows are trajectories (grouped by destination group), columns are final
#' groups, fill is the coupling strength; the cell at each trajectory's destination
#' group is outlined. Computes the incidence internally from a
#' [sniff_trajectory_flow()] object.
#'
#' @param flow A [sniff_trajectory_flow()] object.
#' @param fill `"n_shared"` (default, log), `"prop_of_group"`, or `"prop_of_traj"`.
#' @param mark_terminal Outline each trajectory's destination-group cell (default TRUE).
#' @return A ggplot object.
#' @seealso [sniff_trajectory_flow()]
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs
#' @importFrom ggplot2 theme_minimal theme element_text element_blank
#' @importFrom rlang .data
plot_trajectory_group_coupling <- function(flow,
                                           fill = c("n_shared", "prop_of_group", "prop_of_traj"),
                                           mark_terminal = TRUE) {
  fill <- match.arg(fill)
  if (!is.list(flow) || is.null(flow$trajectories) || !is.data.frame(flow$trajectories) ||
      !"absorbed_into" %in% names(flow$trajectories)) {
    stop("'flow' must be a sniff_trajectory_flow() object", call. = FALSE)
  }
  ci <- .coupling_incidence_flow(flow)
  long <- ci$long
  if (nrow(long) == 0) {
    stop("no trajectory-group coupling to plot", call. = FALSE)
  }
  tr_order <- names(sort(ci$reach, decreasing = TRUE))
  tg <- long$terminal_group[match(tr_order, long$traj_id)]
  tr_order <- tr_order[order(tg, -ci$reach[tr_order])]
  grp_order <- mixed_sort(colnames(ci$incidence))
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
    ggplot2::labs(x = "final group", y = "trajectory (grouped by destination group)",
                  fill = fill, title = "Trajectory-group coupling") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
  p
}
