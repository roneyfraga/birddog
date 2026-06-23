#' Subset a trajectory flow, preserving the contract
#'
#' Filters the trajectories of a flow object while keeping it a valid flow:
#' `$tree` and `absorbed_into` are reattached by transitive bypass (a kept
#' child points to its nearest kept ancestor; `NA` if none survives) and
#' `$docs_per_group` follows the kept nodes. The result is a view, not a
#' census: the call is recorded in `attr(, "pruned")` and shown by `print()`.
#' Dispatches for any detector whose result inherits from `birddog_flow`
#' (the contract class), so future detection methods are subsettable for free.
#'
#' @param x A `birddog_flow` object (or a subclass from a future detector).
#' @param subset Logical expression on the columns of `x$trajectories`,
#'   e.g. `size >= 50` or `type == "central"`.
#' @param target Optional character vector of `traj_id`s: keep each target and
#'   its whole tributary subtree (the watershed). Intersects with `subset`.
#' @param ... Ignored (base generic compatibility).
#' @return An object of the same class as `x`, passing [validate_flow()].
#' @seealso [sniff_trajectory_braid()], [validate_flow()]
#' @family trajectory detection
#' @export
subset.birddog_flow <- function(x, subset, target = NULL, ...) {
  tt <- x$trajectories
  keep <- rep(TRUE, nrow(tt))
  if (!missing(subset)) {
    keep <- keep & eval(substitute(subset), tt, parent.frame())
    keep[is.na(keep)] <- FALSE
  }
  if (!is.null(target)) {
    if (!all(target %in% tt$traj_id)) {
      stop("unknown traj_id in `target`: ",
           paste(setdiff(target, tt$traj_id), collapse = ", "), call. = FALSE)
    }
    ids <- target
    repeat {                              # the watershed: target + subtree
      kids <- x$tree$child[x$tree$parent %in% ids & !x$tree$child %in% ids]
      if (length(kids) == 0) break
      ids <- c(ids, kids)
    }
    keep <- keep & tt$traj_id %in% ids
  }
  kept_ids <- tt$traj_id[keep]
  if (length(kept_ids) == 0) {
    stop("the subset keeps no trajectories", call. = FALSE)
  }

  # transitive bypass: nearest kept ancestor (NA if none survives)
  parent_of <- stats::setNames(x$tree$parent, x$tree$child)
  bypass <- function(id) {
    p <- unname(parent_of[id])
    while (!is.na(p) && !p %in% kept_ids) p <- unname(parent_of[p])
    if (length(p) == 0 || is.na(p)) NA_character_ else p
  }

  out <- x
  out$trajectories <- tt[keep, , drop = FALSE]
  reattached <- vapply(out$trajectories$traj_id, bypass, character(1))
  is_absorbed <- out$trajectories$type == "absorbed"
  out$trajectories$absorbed_into[is_absorbed] <- reattached[is_absorbed]
  out$tree <- x$tree[x$tree$child %in% kept_ids, , drop = FALSE]
  out$tree$parent <- vapply(out$tree$child, bypass, character(1))
  out$tree <- out$tree[!is.na(out$tree$parent), , drop = FALSE]
  kept_nodes <- unique(unlist(out$trajectories$nodes))
  out$docs_per_group <-
    x$docs_per_group[x$docs_per_group$group_id %in% kept_nodes, , drop = FALSE]

  # group/depth follow the new chain (stale after bypass); analogous to follow()
  # in sniff_trajectory_braid.R, but lookup is via named vectors (the frame is
  # already final) rather than a row-scan
  kt <- out$trajectories
  nxt <- stats::setNames(kt$absorbed_into, kt$traj_id)
  typ <- stats::setNames(kt$type, kt$traj_id)
  grp <- stats::setNames(kt$group, kt$traj_id)
  follow <- function(tid) {
    seen <- character(0); cur <- tid; d <- 0L
    repeat {
      if (typ[[cur]] == "central") return(list(group = grp[[cur]], depth = d))
      nx <- unname(nxt[[cur]])
      if (is.na(nx) || nx %in% seen) {
        return(list(group = NA_character_, depth = NA_integer_))
      }
      seen <- c(seen, cur); cur <- nx; d <- d + 1L
    }
  }
  walked <- lapply(kt$traj_id, follow)
  out$trajectories$group <- vapply(walked, `[[`, character(1), "group")
  out$trajectories$depth <- vapply(walked, `[[`, integer(1), "depth")

  attr(out, "pruned") <- deparse1(sys.call())
  out
}
