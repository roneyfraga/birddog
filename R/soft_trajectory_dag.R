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

#' Heaviest single successor per node
#'
#' @param edges Tibble from [.build_global_edges()] (`from`, `to`, `weight`,
#'   `documents`), already top-`k_out` per source.
#' @return Named character vector mapping each `from` node to its single heaviest
#'   successor (max `weight`, tie-break max `documents`).
#' @keywords internal
.heaviest_successor <- function(edges) {
  if (nrow(edges) == 0) return(stats::setNames(character(0), character(0)))
  e <- edges[order(edges$from, -edges$weight, -edges$documents), , drop = FALSE]
  e <- e[!duplicated(e$from), , drop = FALSE]
  stats::setNames(e$to, e$from)
}

#' Terminal group label reached by following heaviest successors forward
#'
#' @param nodes Character vector of node names (e.g. `"y2000c1g1"`).
#' @param succ Named vector from [.heaviest_successor()].
#' @return Character vector of terminal group labels, one per input node.
#' @keywords internal
.forward_terminal_group <- function(nodes, succ) {
  vapply(nodes, function(n) {
    cur <- n
    seen <- character(0)
    repeat {
      seen <- c(seen, cur)
      nxt <- unname(succ[cur])
      if (is.na(nxt) || nxt %in% seen) break   # sink or cycle guard
      cur <- nxt
    }
    sub("^y[0-9]{4}", "", cur)
  }, character(1), USE.NAMES = FALSE)
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

#' Build the soft cumulative-clustering trajectory DAG
#'
#' Reconstructs, from `docs_per_group` alone, the full temporal DAG of
#' cluster-year nodes: vertices are every cluster whose distinct-document count is
#' at least `min_group_size` (isolated nodes included), edges are consecutive-year
#' Jaccard links (top `k_out` per source, `weight >= jaccard_min`). Each node is
#' labelled with the final group its heaviest-successor walk reaches
#' (`terminal_group`) and whether it is a birth (no strong predecessor). This DAG
#' is the first-class object the soft trajectories and group-formation views are
#' read from.
#'
#' @param docs_per_group The membership tibble (`group_id`, `document_id`,
#'   `network_until`, `group`) or a [sniff_groups_trajectories()] object (its
#'   `$docs_per_group` is used).
#' @param min_group_size Minimum distinct-document count for a node (default 10).
#' @param jaccard_min Minimum Jaccard weight to keep an edge (default 0.05).
#' @param k_out Max outgoing edges kept per source node (default 2).
#'
#' @return A list: `graph` (scored igraph DAG with vertex attrs `year`, `size`,
#'   `group`, `is_birth`, `terminal_group`, plus `doc_ids`), `nodes` (tidy tibble
#'   `name`, `year`, `group`, `size`, `is_birth`, `terminal_group`), `edges`
#'   (`from`, `to`, `weight`, `documents`), `births` (character vector of birth
#'   node names), `last_year`, and the de-duplicated `docs_per_group`.
#'
#' @seealso [detect_soft_trajectories()], [plot_trajectory_dag()]
#' @export
#' @importFrom tibble tibble
#' @importFrom igraph graph_from_data_frame
sniff_trajectory_dag <- function(docs_per_group, min_group_size = 10,
                                 jaccard_min = 0.05, k_out = 2) {
  if (is.list(docs_per_group) && !is.data.frame(docs_per_group) &&
      "docs_per_group" %in% names(docs_per_group)) {
    docs_per_group <- docs_per_group$docs_per_group
  }
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!is.data.frame(docs_per_group) || !all(required_cols %in% names(docs_per_group))) {
    stop("'docs_per_group' must contain columns: ",
         paste(required_cols, collapse = ", "), call. = FALSE)
  }

  dpg <- docs_per_group[!duplicated(docs_per_group[c("group_id", "document_id")]), ,
                        drop = FALSE]
  size <- table(dpg$group_id)
  keep <- names(size)[size >= min_group_size]
  if (length(keep) == 0) stop("no node reaches min_group_size = ", min_group_size, call. = FALSE)

  edges <- .build_global_edges(dpg, min_group_size = min_group_size,
                               jaccard_min = jaccard_min, k_out = k_out)
  succ <- .heaviest_successor(edges)
  births <- setdiff(keep, unique(edges$to))

  nodes <- tibble::tibble(
    name = keep,
    year = .extract_year(keep),
    group = sub("^y[0-9]{4}", "", keep),
    size = as.integer(size[keep]),
    is_birth = keep %in% births,
    terminal_group = .forward_terminal_group(keep, succ)
  )
  nodes <- nodes[order(nodes$year, nodes$name), , drop = FALSE]

  vertices <- data.frame(
    name = nodes$name,
    quantity_papers = nodes$size,
    prop_tracked_intra_group = 1,
    PY.sd = NA_real_,
    year = nodes$year,
    size = nodes$size,
    group = nodes$group,
    is_birth = nodes$is_birth,
    terminal_group = nodes$terminal_group,
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    if (nrow(edges)) edges else edges[, c("from", "to"), drop = FALSE],
    directed = TRUE, vertices = vertices
  )
  g <- attach_docs_to_vertices(g, dpg[, c("group_id", "document_id")])
  g <- score_nodes_edges(g)

  list(graph = g, nodes = nodes, edges = edges, births = births,
       last_year = max(dpg$network_until, na.rm = TRUE), docs_per_group = dpg)
}
