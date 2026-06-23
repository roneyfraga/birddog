#' Is this a trajectory flow object?
#'
#' "Flow" names the object kind, not the algorithm (stock vs flow: groups are
#' stock, trajectories are flow). Every trajectory detector returns a flow --
#' the temporal decomposition of the DAG into trajectories plus the confluence
#' tree -- so this predicate accepts the result of [sniff_trajectory_braid()]
#' and of any future `sniff_trajectory_<algo>()` detector. `is_flow()` is the
#' cheap predicate; [validate_flow()] is the contract's authoritative checker.
#'
#' @param x Any object.
#' @return A length-one logical.
#' @seealso [sniff_trajectory_braid()], [validate_flow()]
#' @family trajectory detection
#' @export
is_flow <- function(x) {
  if (inherits(x, "birddog_flow")) return(TRUE)
  # structural fallback: flow objects saved by birddog 1.x carry no class
  is.list(x) && !is.data.frame(x) && !is.null(x$trajectories) &&
    is.data.frame(x$trajectories) && "absorbed_into" %in% names(x$trajectories)
}

#' Validate the trajectory flow contract
#'
#' The authoritative checker of the flow contract (`trajectories`, `tree`,
#' `graph`, `docs_per_group`, `last_year`). "Flow" names the object kind, not
#' the algorithm: any detector -- [sniff_trajectory_braid()] today, alternative
#' `sniff_trajectory_<algo>()` detectors in the future -- must return an object
#' that passes this validator and inherits the `birddog_flow` class.
#'
#' Beyond the column schema, the checked invariants are the ones that otherwise
#' fail silently: every trajectory node label must be a `docs_per_group$group_id`
#' key and a `graph` vertex (the triple key documents and analyses share), the
#' `graph` must carry the `quantity_papers` / `prop_tracked_intra_group` vertex
#' attributes the line plots read, `absorbed_into` must reference existing
#' trajectories without forming a cycle, each final group must have exactly one
#' `central` present at `last_year`, central ids must use the `tr::` convention,
#' and `start`/`end` must match the node years.
#'
#' @param x The object to validate, typically a [sniff_trajectory_braid()] object
#'   or the output of an alternative detector shaped like one.
#' @return `x`, invisibly, when valid; otherwise an error listing every violation.
#' @seealso [sniff_trajectory_braid()], [is_flow()]
#' @family trajectory detection
#' @export
#' @importFrom igraph is_igraph V vertex_attr_names
validate_flow <- function(x) {
  # ---- structural: must hold before the semantic checks can run ----
  if (!is.list(x) || is.data.frame(x)) {
    stop("not a flow object: expected a list, got ", class(x)[1], call. = FALSE)
  }
  miss_top <- setdiff(c("trajectories", "tree", "graph", "docs_per_group", "last_year"),
                      names(x))
  if (length(miss_top)) {
    stop("flow object missing element(s): ", paste(miss_top, collapse = ", "),
         call. = FALSE)
  }
  tr <- x$trajectories
  dpg <- x$docs_per_group
  if (!is.data.frame(tr)) stop("flow$trajectories must be a data frame", call. = FALSE)
  miss_tr <- setdiff(c("traj_id", "type", "group", "start", "end", "size", "depth",
                       "absorbed_into", "absorption_year", "nodes"), names(tr))
  if (length(miss_tr)) {
    stop("flow$trajectories missing column(s): ", paste(miss_tr, collapse = ", "),
         call. = FALSE)
  }
  if (!is.data.frame(dpg) ||
      !all(c("group_id", "document_id", "network_until", "group") %in% names(dpg))) {
    stop("flow$docs_per_group must have columns group_id, document_id, ",
         "network_until, group", call. = FALSE)
  }
  if (!is.data.frame(x$tree) ||
      !all(c("child", "parent", "absorption_year") %in% names(x$tree))) {
    stop("flow$tree must have columns child, parent, absorption_year",
         call. = FALSE)
  }
  if (!igraph::is_igraph(x$graph)) {
    stop("flow$graph must be an igraph object", call. = FALSE)
  }

  # ---- semantic: collect every violation, then report together ----
  p <- character(0)

  bad_type <- setdiff(unique(tr$type), c("central", "absorbed"))
  if (length(bad_type)) {
    p <- c(p, paste0("type must be central/absorbed; found: ",
                     paste(bad_type, collapse = ", ")))
  }

  # node labels: format + the triple key (docs_per_group ids and graph vertices)
  nodes <- unique(unlist(tr$nodes, use.names = FALSE))
  bad_fmt <- nodes[!grepl("^y[0-9]{4}", nodes)]
  if (length(bad_fmt)) {
    p <- c(p, paste0(length(bad_fmt), " node label(s) not in y<YYYY><group> form, e.g. ",
                     bad_fmt[1]))
  }
  miss_dpg <- setdiff(nodes, unique(dpg$group_id))
  if (length(miss_dpg)) {
    p <- c(p, paste0(length(miss_dpg),
                     " trajectory node(s) absent from docs_per_group$group_id, e.g. ",
                     miss_dpg[1], " (empty cohorts -> self_sufficiency = 1, dormant NaN)"))
  }
  miss_v <- setdiff(nodes, igraph::V(x$graph)$name)
  if (length(miss_v)) {
    p <- c(p, paste0(length(miss_v),
                     " trajectory node(s) absent from flow$graph vertices, e.g. ",
                     miss_v[1], " (breaks the line plots' induced_subgraph)"))
  }

  miss_va <- setdiff(c("quantity_papers", "prop_tracked_intra_group"),
                     igraph::vertex_attr_names(x$graph))
  if (length(miss_va)) {
    p <- c(p, paste0("flow$graph missing vertex attribute(s): ",
                     paste(miss_va, collapse = ", ")))
  }

  # absorbed_into referential integrity, acyclicity, central expectations
  ai <- tr$absorbed_into
  bad_ref <- setdiff(ai[!is.na(ai)], tr$traj_id)
  if (length(bad_ref)) {
    p <- c(p, paste0("absorbed_into points to unknown traj_id(s): ",
                     paste(unique(bad_ref), collapse = ", ")))
  }
  if (.flow_has_cycle(tr)) {
    p <- c(p, "absorbed_into forms a cycle (absorption must be acyclic)")
  }
  # tree must mirror the absorbed edges of trajectories (same multiset of triples);
  # \x1f is the unit separator, safe against any legal traj_id or year string
  ab <- tr[tr$type == "absorbed" & !is.na(tr$absorbed_into), , drop = FALSE]
  want_e <- paste(ab$traj_id, ab$absorbed_into, ab$absorption_year, sep = "\x1f")
  have_e <- paste(x$tree$child, x$tree$parent, x$tree$absorption_year, sep = "\x1f")
  if (!identical(sort(want_e), sort(have_e))) {
    p <- c(p, paste0("flow$tree disagrees with trajectories$absorbed_into: ",
                     sum(!want_e %in% have_e), " edge(s) missing, ",
                     sum(!have_e %in% want_e), " extra"))
  }
  cen <- tr[tr$type == "central", , drop = FALSE]
  if (any(!is.na(cen$absorbed_into))) {
    p <- c(p, "central trajectories must have absorbed_into = NA")
  }
  bad_id <- cen$traj_id[!grepl("^tr::", cen$traj_id)]
  if (length(bad_id)) {
    p <- c(p, paste0("central traj_id must start with 'tr::', e.g. ", bad_id[1]))
  }
  dup_grp <- unique(cen$group[duplicated(cen$group) & !is.na(cen$group)])
  if (length(dup_grp)) {
    p <- c(p, paste0("more than one central for group(s): ",
                     paste(dup_grp, collapse = ", ")))
  }
  ly <- suppressWarnings(as.integer(x$last_year))
  if (length(ly) != 1 || is.na(ly)) {
    p <- c(p, "last_year is not a single integer")
  } else {
    miss_fg <- setdiff(cen$group[!is.na(cen$group)],
                       unique(dpg$group[dpg$network_until == ly]))
    if (length(miss_fg)) {
      p <- c(p, paste0("central group(s) absent from docs_per_group at last_year (",
                       ly, "): ", paste(miss_fg, collapse = ", ")))
    }
  }

  # start/end consistent with the node years
  yr_bad <- vapply(seq_len(nrow(tr)), function(i) {
    nd <- tr$nodes[[i]]
    if (length(nd) == 0) return(FALSE)
    yrs <- suppressWarnings(.extract_year(nd))
    !isTRUE(tr$start[i] == min(yrs)) || !isTRUE(tr$end[i] == max(yrs))
  }, logical(1))
  if (any(yr_bad)) {
    p <- c(p, paste0(sum(yr_bad),
                     " trajectory row(s) whose start/end disagree with their node years"))
  }

  if (length(p)) {
    stop("invalid flow object:\n", paste0("  - ", p, collapse = "\n"), call. = FALSE)
  }
  invisible(x)
}

#' Does the absorption forest contain a cycle?
#'
#' @param tr A flow `trajectories` data frame (`traj_id`, `absorbed_into`).
#' @return `TRUE` if following `absorbed_into` from any trajectory revisits a node.
#' @keywords internal
.flow_has_cycle <- function(tr) {
  nxt <- stats::setNames(tr$absorbed_into, tr$traj_id)
  for (start in tr$traj_id) {
    seen <- character(0)
    cur <- start
    repeat {
      if (is.na(cur)) break
      if (cur %in% seen) return(TRUE)
      seen <- c(seen, cur)
      cur <- if (cur %in% names(nxt)) unname(nxt[[cur]]) else NA_character_
    }
  }
  FALSE
}
