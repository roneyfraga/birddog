#' Build the global temporal edge table from docs_per_group
#'
#' Consecutive-year Jaccard between group-year nodes, reconstructed entirely from
#' `docs_per_group`. Replicates the edge filters of [build_temporal_dag()] without
#' an anchor: keep `weight >= jaccard_min`, consecutive years only, top `k_out`
#' outgoing edges per source node.
#'
#' @param docs_per_group Tibble with `group_id` and `document_id`.
#' @param min_group_size Minimum distinct-document count for a node (default 10).
#' @param jaccard_min Minimum Jaccard weight to keep an edge (default 0.05).
#' @param k_out Max outgoing edges kept per source node (default 2).
#' @return Tibble with `from`, `to`, `weight`, `documents`.
#' @keywords internal
#' @importFrom rlang .data
.build_global_edges <- function(docs_per_group, min_group_size = 10,
                                jaccard_min = 0.05, k_out = 2) {
  dpg <- docs_per_group[!duplicated(docs_per_group[c("group_id", "document_id")]), ,
                        drop = FALSE]
  docs_by_node <- split(dpg$document_id, dpg$group_id)
  docs_by_node <- docs_by_node[lengths(docs_by_node) >= min_group_size]

  empty <- tibble::tibble(from = character(), to = character(),
                          weight = numeric(), documents = integer())
  if (length(docs_by_node) == 0) return(empty)

  node_year <- stats::setNames(.extract_year(names(docs_by_node)), names(docs_by_node))
  years <- sort(unique(node_year))

  rows <- list()
  for (t in years) {
    from_nodes <- names(node_year)[node_year == t]
    to_nodes <- names(node_year)[node_year == t + 1L]
    if (length(to_nodes) == 0) next
    for (a in from_nodes) {
      da <- docs_by_node[[a]]
      for (b in to_nodes) {
        db <- docs_by_node[[b]]
        inter <- length(intersect(da, db))
        if (inter == 0) next
        w <- inter / (length(da) + length(db) - inter)
        if (w >= jaccard_min) {
          rows[[length(rows) + 1]] <- tibble::tibble(
            from = a, to = b, weight = w, documents = as.integer(inter))
        }
      }
    }
  }
  if (length(rows) == 0) return(empty)

  dplyr::bind_rows(rows) |>
    dplyr::arrange(dplyr::desc(.data$weight), dplyr::desc(.data$documents)) |>
    dplyr::group_by(.data$from) |>
    dplyr::slice_head(n = k_out) |>
    dplyr::ungroup()
}

#' Terminal (last-year) group label of a trajectory's node set
#'
#' @param nodes Character vector of node names (e.g. `"y2018c1g16"`).
#' @return The group label of the max-year node (e.g. `"c1g16"`).
#' @keywords internal
.terminal_group <- function(nodes) {
  tn <- nodes[which.max(.extract_year(nodes))]
  sub("^y[0-9]{4}", "", tn)
}
