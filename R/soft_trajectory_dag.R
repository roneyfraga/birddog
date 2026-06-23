#' Build the global temporal edge table from docs_per_group
#'
#' Consecutive-year Jaccard between group-year nodes, reconstructed entirely from
#' `docs_per_group`. Applies the edge filters globally, without an anchor group:
#' keep `weight >= jaccard_min`, consecutive years only, top `k_out` outgoing
#' edges per source node.
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

#' Build the global temporal edge table by bibliographic coupling
#'
#' The coupling twin of [.build_global_edges()]: consecutive-year edges weighted
#' by the Jaccard of two nodes' cited-reference sets (the references their
#' documents cite) instead of the Jaccard of their shared documents. Same filters:
#' `weight >= jaccard_min`, consecutive years only, top `k_out` outgoing edges per
#' source. Two nodes can couple without sharing a single document.
#'
#' @param docs_per_group Tibble with `group_id` and `document_id`.
#' @param references Data frame with `document_id` and `feature` (one cited
#'   reference per row).
#' @param min_group_size Minimum distinct-document count for a node (default 10).
#' @param jaccard_min Minimum Jaccard weight to keep an edge (default 0.05).
#' @param k_out Max outgoing edges kept per source node (default 2).
#' @return Tibble with `from`, `to`, `weight`, `documents` (here `documents` is
#'   the count of shared references, the tie-break analogue of the overlap count).
#' @keywords internal
#' @importFrom rlang .data
.build_coupling_edges <- function(docs_per_group, references, min_group_size = 10,
                                  jaccard_min = 0.05, k_out = 2) {
  if (!is.data.frame(references) ||
      !all(c("document_id", "feature") %in% names(references))) {
    stop("`references` must be a data frame with columns `document_id` and `feature`",
         call. = FALSE)
  }
  dpg <- docs_per_group[!duplicated(docs_per_group[c("group_id", "document_id")]), ,
                        drop = FALSE]
  size <- table(dpg$group_id)
  keep <- names(size)[size >= min_group_size]

  empty <- tibble::tibble(from = character(), to = character(),
                          weight = numeric(), documents = integer())
  if (length(keep) == 0) return(empty)

  ref <- references[!is.na(references$feature) & nzchar(references$feature),
                    c("document_id", "feature"), drop = FALSE]
  dn <- merge(dpg[dpg$group_id %in% keep, c("group_id", "document_id")], ref,
              by = "document_id")
  if (nrow(dn) == 0) return(empty)
  dn <- dn[!duplicated(dn[c("group_id", "feature")]), , drop = FALSE]
  dn$fi <- match(dn$feature, unique(dn$feature))      # integer-encode references
  refs_by_node <- split(dn$fi, dn$group_id)

  node_year <- stats::setNames(.extract_year(keep), keep)
  years <- sort(unique(node_year))

  rows <- list()
  for (t in years) {
    from_nodes <- names(node_year)[node_year == t]
    to_nodes <- names(node_year)[node_year == t + 1L]
    if (length(to_nodes) == 0) next
    for (a in from_nodes) {
      ra <- refs_by_node[[a]]
      if (is.null(ra)) next
      for (b in to_nodes) {
        rb <- refs_by_node[[b]]
        if (is.null(rb)) next
        inter <- length(intersect(ra, rb))
        if (inter == 0) next
        w <- inter / (length(ra) + length(rb) - inter)
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

#' Heaviest single predecessor per node
#'
#' @param edges Tibble from [.build_global_edges()] (`from`, `to`, `weight`,
#'   `documents`).
#' @return Named character vector mapping each `to` node to its single heaviest
#'   predecessor (max `weight`, tie-break max `documents`).
#' @keywords internal
.heaviest_predecessor <- function(edges) {
  if (nrow(edges) == 0) return(stats::setNames(character(0), character(0)))
  e <- edges[order(edges$to, -edges$weight, -edges$documents), , drop = FALSE]
  e <- e[!duplicated(e$to), , drop = FALSE]
  stats::setNames(e$from, e$to)
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
#' @param x The membership tibble `docs_per_group` (`group_id`, `document_id`,
#'   `network_until`, `group`) or a [sniff_groups_lineage()] object (its
#'   `$docs_per_group` is used).
#' @param min_group_size Minimum distinct-document count for a node (default 10).
#' @param jaccard_min Minimum Jaccard weight to keep an edge (default 0.05).
#' @param k_out Max outgoing edges kept per source node (default 2).
#' @param similarity How to weight consecutive-year edges: `"overlap"` (default)
#'   uses the Jaccard of the nodes' shared documents (the cumulative carry-over);
#'   `"coupling"` uses the Jaccard of their shared cited references (bibliographic
#'   coupling) and requires `references`. Both detectors ([sniff_trajectory_braid()],
#'   [sniff_trajectory_channel()]) route either DAG unchanged.
#' @param references When `similarity = "coupling"`, a data frame with columns
#'   `document_id` and `feature` (one cited reference per row), the same shape as a
#'   `content` element of [sniff_trajectory_coherence()]. Ignored for `"overlap"`.
#'
#' @return A `birddog_dag` object: a classed list with `graph` (scored igraph DAG
#'   with vertex attrs `year`, `size`, `group`, `is_birth`, `terminal_group`,
#'   plus `doc_ids`), `nodes` (tidy tibble `name`, `year`, `group`, `size`,
#'   `is_birth`, `terminal_group`), `edges` (`from`, `to`, `weight`,
#'   `documents`), `births` (character vector of birth node names), `last_year`,
#'   and the de-duplicated `docs_per_group`.
#'
#' @seealso [plot_trajectory_dag()]
#' @family trajectory detection
#' @export
#' @importFrom tibble tibble
#' @importFrom igraph graph_from_data_frame
sniff_trajectory_dag <- function(x, min_group_size = 10,
                                 jaccard_min = 0.05, k_out = 2,
                                 similarity = c("overlap", "coupling"),
                                 references = NULL) {
  similarity <- match.arg(similarity)
  if (is.list(x) && !is.data.frame(x) && "docs_per_group" %in% names(x)) {
    x <- x$docs_per_group
  }
  docs_per_group <- x
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

  edges <- if (similarity == "coupling") {
    if (is.null(references)) {
      stop("similarity = \"coupling\" requires `references`: a data frame of ",
           "(document_id, feature) cited references", call. = FALSE)
    }
    .build_coupling_edges(dpg, references, min_group_size = min_group_size,
                          jaccard_min = jaccard_min, k_out = k_out)
  } else {
    .build_global_edges(dpg, min_group_size = min_group_size,
                        jaccard_min = jaccard_min, k_out = k_out)
  }
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

  structure(
    list(graph = g, nodes = nodes, edges = edges, births = births,
         last_year = max(dpg$network_until, na.rm = TRUE), docs_per_group = dpg),
    class = c("birddog_dag", "list"),
    similarity = similarity
  )
}

#' Print a soft trajectory DAG
#'
#' @param x A `birddog_dag` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.birddog_dag <- function(x, ...) {
  yrs <- range(x$nodes$year)
  cat("<birddog_dag> ", nrow(x$nodes), " group-year nodes, ",
      nrow(x$edges), " edges, ", yrs[1], "-", yrs[2], "\n", sep = "")
  invisible(x)
}

#' Coerce trajectory-layer input to a soft DAG
#'
#' Resolves the flexible entry contract: a dag passes through (classed, or a 1.x
#' list with the dag shape), a [sniff_groups_lineage()] object contributes its
#' `docs_per_group`, and a `docs_per_group` tibble is built into a dag.
#'
#' @param x A [sniff_trajectory_dag()] object, a [sniff_groups_lineage()] list,
#'   or a `docs_per_group` tibble.
#' @inheritParams sniff_trajectory_dag
#' @return A `birddog_dag` object.
#' @keywords internal
as_trajectory_dag <- function(x, min_group_size = 10, jaccard_min = 0.05,
                              k_out = 2) {
  if (inherits(x, "birddog_dag")) return(x)
  if (is.list(x) && !is.data.frame(x) &&
      all(c("graph", "nodes", "edges", "births") %in% names(x))) {
    class(x) <- c("birddog_dag", "list")       # 1.x dag saved without the class
    return(x)
  }
  if (is.list(x) && !is.data.frame(x) && "docs_per_group" %in% names(x)) {
    x <- x$docs_per_group                      # gct / lineage object
  }
  if (!is.data.frame(x) || !all(c("group_id", "document_id") %in% names(x))) {
    stop("`x` must be a sniff_trajectory_dag() object, a sniff_groups_lineage() ",
         "object, or a `docs_per_group` tibble", call. = FALSE)
  }
  sniff_trajectory_dag(x, min_group_size = min_group_size,
                       jaccard_min = jaccard_min, k_out = k_out)
}
