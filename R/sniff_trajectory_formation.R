#' Trajectories that fed into a target (confluence)
#'
#' Returns the target trajectory's direct tributaries: its children in the
#' confluence tree, the trajectories whose terminal cohort was absorbed into
#' `target`. The result is the `formation` shape that
#' [plot_trajectory_formation()] renders.
#'
#' @param x A [sniff_trajectory_braid()] object.
#' @param ... `target` (a `traj_id`), then `min_papers` (default 5) and
#'   `min_prop` (default 0.05), the thresholds behind the feeders' `kept` flag.
#' @return A `formation` list (`target`, `target_info`, `feeders`, `total_inflow`,
#'   `last_year`). The `feeders` carry `n_dest` (the feeder's documents that end
#'   in the target's final community) and `target_info` carries `final_size` (the
#'   target's final-year size); [plot_trajectory_formation()] uses these
#'   for its labels.
#' @seealso [sniff_trajectory_braid()], [plot_trajectory_formation()]
#' @family trajectory analysis
#' @export
sniff_trajectory_formation <- function(x, ...) {
  if (!is_flow(x)) {
    stop("`x` must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  .formation_from_flow(x, ...)
}

#' Formation of a trajectory from the flow tree (its direct tributaries)
#' @keywords internal
#' @importFrom dplyr bind_rows mutate arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
.formation_from_flow <- function(flow, target, min_papers = 5, min_prop = 0.05) {
  tr <- flow$trajectories
  if (!is.character(target) || length(target) != 1 || !(target %in% tr$traj_id)) {
    stop("'target' must be a single trajectory id present in the flow object",
         call. = FALSE)
  }
  trow <- tr[tr$traj_id == target, ]
  dpg <- flow$docs_per_group
  last_year <- flow$last_year
  node_size <- .node_size_lookup(dpg)
  node_docs <- split(dpg$document_id, dpg$group_id)

  # the target's final-year community = its last node's documents. Used for the
  # final-year size and for each feeder's documents that END here (n_dest), so the
  # labels match plot_trajectory_lines_3d() / plot_trajectory_confluence().
  tgt_nodes <- trow$nodes[[1]]
  tgt_final_node <- tgt_nodes[which.max(.extract_year(tgt_nodes))]
  tgt_final_docs <- node_docs[[tgt_final_node]]
  target_final_size <- length(tgt_final_docs)

  kids <- tr[!is.na(tr$absorbed_into) & tr$absorbed_into == target, , drop = FALSE]
  kids <- kids[order(kids$absorption_year, -kids$size, kids$traj_id), , drop = FALSE]

  empty_feeders <- tibble::tibble(
    source_key = character(), source_group = character(),
    start_year = integer(), handoff_year = integer(),
    cohort_size = integer(), n = integer(), n_dest = integer(),
    prop_of_source = numeric(), prop_of_inflow = numeric(), kept = logical(),
    size_curve = list(), inflow_curve = list()
  )

  claimed <- character(0)
  rows <- list()
  for (i in seq_len(nrow(kids))) {
    f_nodes <- kids$nodes[[i]]
    f_docs <- unique(unlist(node_docs[f_nodes], use.names = FALSE))
    contrib <- setdiff(f_docs, claimed)
    if (length(contrib) == 0) next
    claimed <- c(claimed, contrib)
    # documents of the feeder's terminal cohort that end in the target's final
    # community (the destination count, matching the other plots).
    f_term <- f_nodes[which.max(.extract_year(f_nodes))]
    n_dest <- length(intersect(node_docs[[f_term]], tgt_final_docs))
    handoff <- as.integer(kids$absorption_year[i])
    rows[[length(rows) + 1]] <- tibble::tibble(
      source_key = kids$traj_id[i], source_group = trow$group,
      start_year = kids$start[i], handoff_year = handoff,
      cohort_size = kids$size[i], n = length(contrib), n_dest = n_dest,
      size_curve = list(.feeder_growth_series(f_nodes, node_size)),
      inflow_curve = list(.contributed_arrival_series(contrib, f_nodes, dpg, handoff))
    )
  }

  if (length(rows) == 0) {
    feeders <- empty_feeders
    total_inflow <- 0L
  } else {
    feeders <- dplyr::bind_rows(rows)
    total_inflow <- sum(feeders$n)
    feeders <- feeders |>
      dplyr::mutate(
        prop_of_source = .data$n / .data$cohort_size,
        prop_of_inflow = .data$n / total_inflow,
        kept = .data$n >= min_papers & .data$prop_of_source >= min_prop
      ) |>
      dplyr::arrange(dplyr::desc(.data$n), .data$source_key)
  }

  target_info <- list(group = NA_character_, traj_id = target,
                      start = trow$start, size = trow$size,
                      final_size = target_final_size)
  list(target = target, target_info = target_info, feeders = feeders,
       total_inflow = total_inflow, last_year = last_year)
}
