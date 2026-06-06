#' Detect the flow trajectories (central + absorbed) of the soft DAG
#'
#' Decomposes the cumulative-clustering DAG into trajectories via **trunk edges**
#' (mutual dominance: `v -> u` where `u` is `v`'s heaviest successor and `v` is
#' `u`'s heaviest predecessor). Trunk edges form disjoint chains. A chain ending at
#' a final-year group node is a **central** trajectory (`tr::<group>`, one per final
#' group, reaches the final year); every other chain is **absorbed** (`tr1 … trN`),
#' merging into an absorber (central or another absorbed) where its head flows out
#' via a non-trunk edge. Absorption is transitive, forming a confluence forest.
#'
#' @param x A [sniff_trajectory_dag()] object, or a `docs_per_group` tibble /
#'   [sniff_groups_trajectories()] object (the DAG is built internally).
#' @param min_group_size,jaccard_min,k_out Passed to [sniff_trajectory_dag()] when
#'   `x` is not already a DAG object.
#'
#' @return A list: `trajectories` (tibble: `traj_id`, `type` (`"central"`/
#'   `"absorbed"`), `group` (destination group; NA if extinct), `start`, `end`,
#'   `size` (distinct papers), `depth` (0 = central), `absorbed_into` (absorber
#'   `traj_id`, NA for central), `absorption_year` (NA for central), `nodes`
#'   list-col), `tree` (edges `child`, `parent`, `absorption_year`), `graph`,
#'   `docs_per_group`, `last_year`.
#'
#' @seealso [sniff_trajectory_dag()], [sniff_trajectory_dynamics()]
#' @export
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
sniff_trajectory_flow <- function(x, min_group_size = 10, jaccard_min = 0.05,
                                  k_out = 2) {
  dag <- if (is.list(x) && !is.data.frame(x) &&
             all(c("graph", "nodes", "edges", "births") %in% names(x))) {
    x
  } else {
    sniff_trajectory_dag(x, min_group_size = min_group_size,
                         jaccard_min = jaccard_min, k_out = k_out)
  }
  edges <- dag$edges
  dpg <- dag$docs_per_group
  last_year <- dag$last_year
  all_nodes <- dag$nodes$name
  node_docs <- split(dpg$document_id, dpg$group_id)

  wsucc <- .heaviest_successor(edges)
  wpred <- .heaviest_predecessor(edges)

  # Trunk edges (mutual dominance) -> disjoint chains.
  trunk_succ <- stats::setNames(rep(NA_character_, length(all_nodes)), all_nodes)
  for (v in all_nodes) {
    u <- unname(wsucc[v])
    if (!is.na(u) && identical(unname(wpred[u]), v)) trunk_succ[[v]] <- u
  }
  trunk_pred <- stats::setNames(rep(NA_character_, length(all_nodes)), all_nodes)
  for (v in all_nodes) {
    u <- trunk_succ[[v]]
    if (!is.na(u)) trunk_pred[[u]] <- v
  }

  heads <- all_nodes[is.na(trunk_succ[all_nodes])]
  owner <- stats::setNames(rep(NA_character_, length(all_nodes)), all_nodes)
  chains <- vector("list", length(heads))
  names(chains) <- heads
  for (h in heads) {
    path <- h; cur <- h
    repeat {
      p <- trunk_pred[[cur]]
      if (is.na(p)) break
      path <- c(p, path); cur <- p
    }
    chains[[h]] <- path
    owner[path] <- h
  }

  final_nodes <- dag$nodes$name[dag$nodes$year == last_year]
  rows <- vector("list", length(heads))
  for (i in seq_along(heads)) {
    h <- heads[i]; ch <- chains[[h]]
    yrs <- .extract_year(ch)
    size <- length(unique(unlist(node_docs[ch], use.names = FALSE)))
    if (h %in% final_nodes) {
      grp <- sub("^y[0-9]{4}", "", h)
      rows[[i]] <- tibble::tibble(
        head = h, type = "central", group = grp, traj_id = paste0("tr::", grp),
        start = min(yrs), end = max(yrs), size = size, nodes = list(ch),
        absorber_head = NA_character_, absorption_year = NA_integer_)
    } else {
      a <- unname(wsucc[h])
      rows[[i]] <- tibble::tibble(
        head = h, type = "absorbed", group = NA_character_, traj_id = NA_character_,
        start = min(yrs), end = max(yrs), size = size, nodes = list(ch),
        absorber_head = if (is.na(a)) NA_character_ else unname(owner[a]),
        absorption_year = if (is.na(a)) NA_integer_ else .extract_year(a))
    }
  }
  trajectories <- dplyr::bind_rows(rows)

  abs_idx <- which(trajectories$type == "absorbed")
  if (length(abs_idx) > 0) {
    abs_ord <- abs_idx[order(-trajectories$size[abs_idx], trajectories$start[abs_idx],
                             trajectories$head[abs_idx])]
    trajectories$traj_id[abs_ord] <- paste0("tr", seq_along(abs_ord))
  }
  head2id <- stats::setNames(trajectories$traj_id, trajectories$head)
  trajectories$absorbed_into <- ifelse(
    is.na(trajectories$absorber_head), NA_character_,
    unname(head2id[trajectories$absorber_head]))

  # Destination group + depth: follow absorbed_into to a central.
  follow <- function(tid, want) {
    seen <- character(0); cur <- tid; d <- 0L
    repeat {
      row <- trajectories[trajectories$traj_id == cur, ]
      if (row$type == "central") return(if (want == "group") row$group else d)
      nxt <- row$absorbed_into
      if (is.na(nxt) || nxt %in% seen) return(if (want == "group") NA_character_ else d)
      seen <- c(seen, cur); cur <- nxt; d <- d + 1L
    }
  }
  for (j in seq_len(nrow(trajectories))) {
    if (trajectories$type[j] == "absorbed") {
      trajectories$group[j] <- follow(trajectories$traj_id[j], "group")
    }
  }
  trajectories$depth <- vapply(trajectories$traj_id,
                               function(t) follow(t, "depth"), integer(1),
                               USE.NAMES = FALSE)

  trajectories <- trajectories[, c("traj_id", "type", "group", "start", "end",
                                   "size", "depth", "absorbed_into",
                                   "absorption_year", "nodes")]
  trajectories <- dplyr::arrange(trajectories, dplyr::desc(.data$type),
                                 dplyr::desc(.data$size))

  tree <- trajectories[trajectories$type == "absorbed" &
                         !is.na(trajectories$absorbed_into),
                       c("traj_id", "absorbed_into", "absorption_year")]
  names(tree) <- c("child", "parent", "absorption_year")

  list(trajectories = trajectories, tree = tree, graph = dag$graph,
       docs_per_group = dpg, last_year = last_year)
}
