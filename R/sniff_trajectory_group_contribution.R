#' Shannon entropy of a non-negative weight vector
#'
#' @param counts Non-negative numeric vector.
#' @return Entropy in nats (0 when concentrated on one element).
#' @keywords internal
.shannon_entropy <- function(counts) {
  total <- sum(counts)
  if (total <= 0) return(0)
  p <- counts / total
  p <- p[p > 0]
  -sum(p * log(p))
}

#' Trajectory-to-group contribution as a bipartite network
#'
#' For a [detect_global_trajectories()] object, computes how much each global
#' trajectory contributes to each final-year group. The result is the
#' trajectory x group incidence matrix `B` (weight = shared papers), the bipartite
#' igraph, the tidy long table, and Layer-1 node indicators. Nestedness and
#' bipartite modularity are intentionally left to the user's `bipartite` package
#' tooling, which runs directly on `B` / the graph.
#'
#' For trajectory `tr` (papers = the union of documents across its nodes) and
#' final group `g` (documents in `g`'s last-year cluster):
#' `n_shared = |papers(tr) ∩ final(g)|`,
#' `prop_of_group = n_shared / |final(g)|` (how much of `g` came from `tr`),
#' `prop_of_traj = n_shared / |papers(tr)|` (where `tr`'s papers ended up).
#' Because trajectories share papers, the columns of `B` do not sum to one; these
#' are overlapping contributions, not a partition.
#'
#' @param detected A [detect_global_trajectories()] object (its `trajectories`
#'   tibble must carry a `terminal_group` column and globally-unique `traj_id`).
#' @param docs_per_group Per-year membership tibble (`group_id`, `document_id`,
#'   `network_until`, `group`). Defaults to `detected$docs_per_group`.
#'
#' @return A list:
#' \describe{
#'   \item{long}{Tidy tibble `traj_id`, `terminal_group`, `group_final`,
#'     `n_shared`, `prop_of_group`, `prop_of_traj`.}
#'   \item{incidence}{Integer matrix `B` (trajectories x final groups), weight =
#'     `n_shared`; ready for `igraph::graph_from_biadjacency_matrix()` or the
#'     `bipartite` package.}
#'   \item{graph}{Weighted bipartite igraph (rows = trajectories `type = FALSE`,
#'     columns = groups `type = TRUE`).}
#'   \item{groups}{Per final group: `n_sources`, `source_entropy` (low =
#'     convergent formation, high = divergent), `dominant_traj`,
#'     `dominant_traj_share`.}
#'   \item{trajectories}{Per trajectory: `n_groups`, `reach` (weighted degree =
#'     papers placed in final groups), `dominant_group`, `dominant_group_share`.}
#'   \item{last_year}{The final year used.}
#' }
#'
#' @seealso [detect_global_trajectories()], [sniff_trajectory_projection()],
#'   [plot_trajectory_group_bipartite()]
#'
#' @export
#' @importFrom dplyr group_by summarise arrange desc n
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom igraph graph_from_biadjacency_matrix
sniff_trajectory_group_contribution <- function(detected, docs_per_group = NULL) {
  if (is.null(docs_per_group)) docs_per_group <- detected$docs_per_group
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!is.data.frame(docs_per_group) || !all(required_cols %in% names(docs_per_group))) {
    stop("'docs_per_group' must contain columns: ",
         paste(required_cols, collapse = ", "), call. = FALSE)
  }

  pool <- .flatten_trajectories(detected)
  if (nrow(pool) == 0) {
    stop("'detected' contains no trajectories", call. = FALSE)
  }
  if (anyDuplicated(pool$traj_id)) {
    stop("sniff_trajectory_group_contribution() expects a detect_global_trajectories() ",
         "object with globally-unique trajectory ids", call. = FALSE)
  }

  last_year <- max(docs_per_group$network_until, na.rm = TRUE)
  fin <- docs_per_group[docs_per_group$network_until == last_year,
                        c("document_id", "group"), drop = FALSE]
  fin <- fin[!duplicated(fin$document_id), , drop = FALSE]
  final_groups <- mixed_sort(unique(fin$group))
  final_size <- table(fin$group)
  doc_final <- stats::setNames(fin$group, fin$document_id)

  node_docs <- split(docs_per_group$document_id, docs_per_group$group_id)

  long_list <- list()
  for (i in seq_len(nrow(pool))) {
    docs <- unique(unlist(node_docs[pool$nodes[[i]]], use.names = FALSE))
    n_traj <- length(docs)
    if (n_traj == 0) next
    fg <- doc_final[docs]
    fg <- fg[!is.na(fg)]
    if (length(fg) == 0) next
    tab <- table(fg)
    long_list[[length(long_list) + 1]] <- tibble::tibble(
      traj_id = pool$traj_id[i],
      terminal_group = pool$group[i],
      group_final = names(tab),
      n_shared = as.integer(tab),
      prop_of_group = as.integer(tab) / as.integer(final_size[names(tab)]),
      prop_of_traj = as.integer(tab) / n_traj
    )
  }
  long <- if (length(long_list)) {
    dplyr::bind_rows(long_list)
  } else {
    tibble::tibble(
      traj_id = character(), terminal_group = character(),
      group_final = character(), n_shared = integer(),
      prop_of_group = double(), prop_of_traj = double()
    )
  }

  incidence <- matrix(
    0L, nrow = nrow(pool), ncol = length(final_groups),
    dimnames = list(pool$traj_id, final_groups)
  )
  if (nrow(long) > 0) {
    incidence[cbind(long$traj_id, long$group_final)] <- long$n_shared
  }
  graph <- igraph::graph_from_biadjacency_matrix(incidence, weighted = TRUE)

  groups_tbl <- long |>
    dplyr::group_by(.data$group_final) |>
    dplyr::summarise(
      n_sources = dplyr::n(),
      source_entropy = .shannon_entropy(.data$n_shared),
      dominant_traj = .data$traj_id[which.max(.data$n_shared)],
      dominant_traj_share = max(.data$prop_of_group),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_sources), .data$group_final)

  traj_tbl <- long |>
    dplyr::group_by(.data$traj_id, .data$terminal_group) |>
    dplyr::summarise(
      n_groups = dplyr::n(),
      reach = sum(.data$n_shared),
      dominant_group = .data$group_final[which.max(.data$n_shared)],
      dominant_group_share = max(.data$prop_of_traj),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$reach), .data$traj_id)

  list(
    long = long, incidence = incidence, graph = graph,
    groups = groups_tbl, trajectories = traj_tbl, last_year = last_year
  )
}
