#' Detect channel trajectories by global optimal-path routing
#'
#' An alternative to [sniff_trajectory_braid()]. Instead of flow's local
#' mutual-dominance trunk edges, each node is routed to the successor on its
#' globally cheapest path to a final-year node, where edge cost is
#' `-log(weight)` (the Jaccard). The minimum-cost path is the maximum-product
#' Jaccard path (highest geometric-mean coherence). Final-year nodes root one
#' in-tree each (the watershed of a final community); the cheapest birth -> final
#' chain is the **central** trajectory (`tr::<group>`), the rest are **absorbed**
#' tributaries (`tr1 … trN`), mirroring the flow object exactly. "Flow" names the
#' object kind, not the algorithm, so the result is a `birddog_flow` and passes
#' [validate_flow()].
#'
#' Adds one column to `trajectories`: `latest_departure`, the year of the most
#' recent birth that can reach the trajectory's tail by any time-respecting path
#' (Wu et al. 2014 latest-departure). For a central this is the freshest input
#' anywhere in the final community's ancestry; combined with `start` it separates
#' old-but-freshly-fed communities from genuinely young ones.
#'
#' @param x A [sniff_trajectory_dag()] object, a [sniff_groups_lineage()] object,
#'   or a `docs_per_group` tibble (the DAG is built internally).
#' @param min_group_size,jaccard_min,k_out Passed to [sniff_trajectory_dag()] when
#'   `x` is not already a DAG object.
#' @return A `birddog_flow` object (see [sniff_trajectory_braid()]) with the extra
#'   `latest_departure` column.
#' @seealso [sniff_trajectory_braid()], [validate_flow()], [sniff_trajectory_dag()]
#' @family trajectory detection
#' @references Wu H, Cheng J, Huang S, Ke Y, Lu Y, Xu Y (2014). Path Problems in
#'   Temporal Graphs. Proc. VLDB Endowment 7(9).
#' @export
#' @importFrom dplyr arrange desc bind_rows
#' @importFrom tibble tibble
#' @importFrom rlang .data
sniff_trajectory_channel <- function(x, min_group_size = 10, jaccard_min = 0.05,
                                     k_out = 2) {
  dag <- as_trajectory_dag(x, min_group_size = min_group_size,
                           jaccard_min = jaccard_min, k_out = k_out)
  edges <- dag$edges
  dpg <- dag$docs_per_group
  last_year <- dag$last_year
  all_nodes <- dag$nodes$name
  node_year <- stats::setNames(dag$nodes$year, dag$nodes$name)
  node_docs <- split(dpg$document_id, dpg$group_id)
  births <- dag$births
  final_nodes <- dag$nodes$name[dag$nodes$year == last_year]
  wsucc <- .heaviest_successor(edges)

  ekey <- paste(edges$from, edges$to, sep = "\x1f")
  ekey_idx <- stats::setNames(seq_len(nrow(edges)), ekey)
  cost <- -log(edges$weight)
  out_idx <- split(seq_len(nrow(edges)), edges$from)
  in_idx  <- split(seq_len(nrow(edges)), edges$to)

  # 1. forward potential phi + best_succ (reverse-year DP)
  phi <- stats::setNames(rep(Inf, length(all_nodes)), all_nodes)
  best_succ <- stats::setNames(rep(NA_character_, length(all_nodes)), all_nodes)
  phi[final_nodes] <- 0
  for (v in all_nodes[order(-node_year[all_nodes], all_nodes)]) {
    if (v %in% final_nodes) next
    oi <- out_idx[[v]]
    if (is.null(oi)) next
    u <- edges$to[oi]; cand <- cost[oi] + phi[u]
    keep <- is.finite(cand)
    if (!any(keep)) next
    oi <- oi[keep]; u <- u[keep]; cand <- cand[keep]
    b <- order(cand, -edges$weight[oi], -edges$documents[oi], u)[1]
    phi[v] <- cand[b]; best_succ[v] <- u[b]
  }

  # 2. unified forward pointer: optimal where reachable, myopic fallback if extinct
  fwd <- ifelse(all_nodes %in% final_nodes, NA_character_,
                ifelse(is.finite(phi[all_nodes]), best_succ[all_nodes],
                       unname(wsucc[all_nodes])))
  names(fwd) <- all_nodes

  # 3. chain-decomposition DP: D (cheapest birth->node) + chain_child (asc-year)
  kids <- split(names(fwd)[!is.na(fwd)], fwd[!is.na(fwd)])  # parent -> children
  D <- stats::setNames(rep(0, length(all_nodes)), all_nodes)
  chain_child <- stats::setNames(rep(NA_character_, length(all_nodes)), all_nodes)
  for (v in all_nodes[order(node_year[all_nodes], all_nodes)]) {
    ch <- kids[[v]]
    if (is.null(ch) || !length(ch)) next
    ei <- ekey_idx[paste(ch, v, sep = "\x1f")]
    val <- D[ch] + cost[ei]
    b <- order(val, -edges$weight[ei], -edges$documents[ei], ch)[1]
    D[v] <- val[b]; chain_child[v] <- ch[b]
  }

  # 4. enumerate chains by tail (a node that does NOT continue its fwd's chain)
  is_tail <- function(t) {
    f <- fwd[[t]]
    is.na(f) || !identical(chain_child[[f]], t)
  }
  tails <- all_nodes[vapply(all_nodes, is_tail, logical(1))]
  owner <- stats::setNames(rep(NA_character_, length(all_nodes)), all_nodes)
  chains <- vector("list", length(tails)); names(chains) <- tails
  for (t in tails) {
    path <- t; cur <- t
    repeat { p <- chain_child[[cur]]; if (is.na(p)) break; path <- c(p, path); cur <- p }
    chains[[t]] <- path; owner[path] <- t
  }

  # 5. ancestor DP for latest-departure (most recent birth reaching each node)
  ld <- stats::setNames(rep(NA_integer_, length(all_nodes)), all_nodes)
  for (v in all_nodes[order(node_year[all_nodes], all_nodes)]) {
    vals <- integer(0)
    if (v %in% births) vals <- c(vals, node_year[[v]])
    ii <- in_idx[[v]]
    if (!is.null(ii)) vals <- c(vals, ld[edges$from[ii]])
    ld[v] <- if (length(vals)) max(vals) else node_year[[v]]
  }

  # 6. one row per chain
  rows <- vector("list", length(tails))
  for (i in seq_along(tails)) {
    t <- tails[i]; ch <- chains[[t]]; yrs <- node_year[ch]
    size <- length(unique(unlist(node_docs[ch], use.names = FALSE)))
    if (t %in% final_nodes) {
      grp <- sub("^y[0-9]{4}", "", t)
      rows[[i]] <- tibble::tibble(
        tail = t, type = "central", group = grp, traj_id = paste0("tr::", grp),
        start = min(yrs), end = max(yrs), size = size,
        latest_departure = unname(ld[t]), nodes = list(ch),
        absorber_tail = NA_character_, absorption_year = NA_integer_)
    } else {
      f <- fwd[[t]]
      rows[[i]] <- tibble::tibble(
        tail = t, type = "absorbed", group = NA_character_, traj_id = NA_character_,
        start = min(yrs), end = max(yrs), size = size,
        latest_departure = unname(ld[t]), nodes = list(ch),
        absorber_tail = if (is.na(f)) NA_character_ else unname(owner[[f]]),
        absorption_year = if (is.na(f)) NA_integer_ else unname(node_year[[f]]))
    }
  }
  trajectories <- dplyr::bind_rows(rows)

  # 7. number absorbed (desc size), resolve absorbed_into, destination group, depth
  abs_idx <- which(trajectories$type == "absorbed")
  if (length(abs_idx)) {
    ord <- abs_idx[order(-trajectories$size[abs_idx], trajectories$start[abs_idx],
                         trajectories$tail[abs_idx])]
    trajectories$traj_id[ord] <- paste0("tr", seq_along(ord))
  }
  tail2id <- stats::setNames(trajectories$traj_id, trajectories$tail)
  trajectories$absorbed_into <- ifelse(
    is.na(trajectories$absorber_tail), NA_character_,
    unname(tail2id[trajectories$absorber_tail]))

  follow <- function(tid, want) {
    seen <- character(0); cur <- tid; d <- 0L
    repeat {
      row <- trajectories[trajectories$traj_id == cur, ]
      if (row$type == "central") return(if (want == "group") row$group else d)
      nxt <- row$absorbed_into
      if (is.na(nxt) || nxt %in% seen) return(if (want == "group") NA_character_ else NA_integer_)
      seen <- c(seen, cur); cur <- nxt; d <- d + 1L
    }
  }
  for (j in which(trajectories$type == "absorbed")) {
    trajectories$group[j] <- follow(trajectories$traj_id[j], "group")
  }
  trajectories$depth <- vapply(trajectories$traj_id, function(t) follow(t, "depth"),
                               integer(1), USE.NAMES = FALSE)

  trajectories <- trajectories[, c("traj_id", "type", "group", "start", "end",
                                   "size", "depth", "absorbed_into",
                                   "absorption_year", "latest_departure", "nodes")]
  trajectories <- dplyr::arrange(trajectories, dplyr::desc(.data$type),
                                 dplyr::desc(.data$size))

  tree <- trajectories[trajectories$type == "absorbed" &
                         !is.na(trajectories$absorbed_into),
                       c("traj_id", "absorbed_into", "absorption_year")]
  names(tree) <- c("child", "parent", "absorption_year")

  out <- list(trajectories = trajectories, tree = tree, graph = dag$graph,
              docs_per_group = dpg, last_year = last_year)
  class(out) <- c("birddog_flow", "list")
  attr(out, "similarity") <- attr(dag, "similarity")   # routing provenance
  out
}
