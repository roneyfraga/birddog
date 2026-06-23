#' Self-sufficiency of each central trajectory
#'
#' Returns each central trajectory's endogenous fraction: the share of its
#' **final-year community** that was NOT delivered by any absorbed tributary
#' (1 = fully endogenous, ~0 = a confluence of tributaries). Inflow follows the
#' same destination semantics as [plot_trajectory_confluence()] and
#' [sniff_trajectory_destination()]: a document counts as imported when it sits
#' in the central's final community **and** in the terminal cohort of some
#' absorbed tributary, counted once even if several tributaries carried it, and
#' **regardless of which central the tributary is dominantly assigned to** -- so
#' secondary inflow (tributaries owned by other centrals) is included, not only
#' the central's own feeders.
#'
#' @param x A [sniff_trajectory_braid()] object.
#' @param ... `min_size` (default 30, the minimum final-year community size).
#' @return A tibble (`central`, `group`, `final_size`, `inflow`,
#'   `self_sufficiency`), sorted by descending `self_sufficiency`.
#' @seealso [sniff_trajectory_braid()], [sniff_trajectory_destination()]
#' @family trajectory analysis
#' @export
sniff_trajectory_self_sufficiency <- function(x, ...) {
  if (!is_flow(x)) {
    stop("`x` must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  .self_sufficiency_from_flow(x, ...)
}

#' @keywords internal
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
.self_sufficiency_from_flow <- function(flow, min_size = 30) {
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  last_year <- as.integer(flow$last_year)
  node_docs <- split(dpg$document_id, dpg$group_id)

  # final-year community of each group = the destination basis used across the
  # confluence / destination plots: the documents still in the group at last_year.
  fy <- dpg[dpg$network_until == last_year, , drop = FALSE]
  final_docs <- split(fy$document_id, as.character(fy$group))

  # each absorbed tributary's terminal cohort: the documents at its last node, just
  # before it hands off. Inflow is measured against these, not against the whole
  # tributary lineage, so shared early history does not count as importation.
  absorbed <- tr[tr$type == "absorbed", , drop = FALSE]
  term_cohort <- lapply(absorbed$nodes, function(nd) {
    if (length(nd) == 0) return(character(0))
    unique(node_docs[[nd[which.max(.extract_year(nd))]]])
  })

  centrals <- tr[tr$type == "central", , drop = FALSE]
  rows <- vector("list", nrow(centrals))
  for (i in seq_len(nrow(centrals))) {
    g <- centrals$group[i]
    fdocs <- unique(final_docs[[g]])
    fsize <- length(fdocs)
    # documents of the final community delivered by ANY tributary (primary or
    # secondary): union of each terminal cohort intersected with the final
    # community, deduped so a doc carried by several tributaries counts once.
    delivered <- unique(unlist(lapply(term_cohort, function(tc) intersect(tc, fdocs)),
                               use.names = FALSE))
    inflow <- length(delivered)
    rows[[i]] <- tibble::tibble(
      central = centrals$traj_id[i], group = g, final_size = fsize,
      inflow = inflow,
      self_sufficiency = if (fsize > 0) 1 - inflow / fsize else NA_real_
    )
  }
  ss <- dplyr::bind_rows(rows)
  ss <- ss[ss$final_size >= min_size, , drop = FALSE]
  dplyr::arrange(ss, dplyr::desc(.data$self_sufficiency))
}
