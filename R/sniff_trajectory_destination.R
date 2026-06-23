#' Where a trajectory's terminal cohort goes
#'
#' Returns the destination of `source`'s terminal cohort: its per-final-group
#' split and dormant share, plus its immediate absorber and ultimate
#' destination.
#'
#' @param x A [sniff_trajectory_braid()] object.
#' @param ... `source`, the trajectory whose terminal cohort to follow.
#' @return A list (`target`, `target_info`, `destination`, `cohort_size`,
#'   `dormant_share`, `continuation_traj`, `last_year`).
#' @seealso [sniff_trajectory_braid()], [sniff_trajectory_self_sufficiency()]
#' @family trajectory analysis
#' @export
sniff_trajectory_destination <- function(x, ...) {
  if (!is_flow(x)) {
    stop("`x` must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  .destination_from_flow(x, ...)
}

#' @keywords internal
#' @importFrom dplyr arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
.destination_from_flow <- function(flow, source) {
  tr <- flow$trajectories
  if (!is.character(source) || length(source) != 1 || !(source %in% tr$traj_id)) {
    stop("'source' must be a single trajectory id present in the flow object",
         call. = FALSE)
  }
  trow <- tr[tr$traj_id == source, ]
  dpg <- flow$docs_per_group
  last_year <- flow$last_year
  node_docs <- split(dpg$document_id, dpg$group_id)

  fin <- dpg[dpg$network_until == last_year, c("document_id", "group"), drop = FALSE]
  fin <- fin[!duplicated(fin$document_id), , drop = FALSE]
  doc_final <- stats::setNames(fin$group, fin$document_id)

  nodes <- trow$nodes[[1]]
  terminal_node <- nodes[which.max(.extract_year(nodes))]
  cohort <- unique(node_docs[[terminal_node]])
  fg <- unname(doc_final[cohort])
  dropped <- sum(is.na(fg))
  tab <- table(fg[!is.na(fg)])
  g_final <- names(tab); n <- as.integer(tab)
  if (dropped > 0) { g_final <- c(g_final, "(dropped)"); n <- c(n, dropped) }
  destination <- tibble::tibble(g_final = g_final, n = n,
                                prop = n / length(cohort))
  destination <- dplyr::arrange(destination, dplyr::desc(.data$n))

  source_info <- list(group = NA_character_, traj_id = source,
                      start = trow$start, end = trow$end,
                      cohort_size = length(cohort))
  continuation_info <- NULL
  if (!is.na(trow$absorbed_into)) {
    arow <- tr[tr$traj_id == trow$absorbed_into, ]
    a_docs <- unique(unlist(node_docs[arow$nodes[[1]]], use.names = FALSE))
    continuation_info <- list(group = NA_character_, traj_id = arow$traj_id,
                              start = arow$start, end = arow$end,
                              n_papers = length(intersect(cohort, a_docs)))
  }

  list(
    target = source,
    target_info = list(traj_id = source, type = trow$type, group = trow$group,
                       start = trow$start, end = trow$end, size = trow$size,
                       absorbed_into = trow$absorbed_into,
                       absorption_year = trow$absorption_year),
    destination = destination,
    cohort_size = length(cohort),
    dormant_share = dropped / length(cohort),
    continuation_traj = trow$absorbed_into,
    last_year = last_year,
    source_info = source_info,
    continuation_info = continuation_info
  )
}
