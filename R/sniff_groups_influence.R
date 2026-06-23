#' Measure directed citation influence between research groups
#'
#' *Experimental.* Lifts the internal citations of a [sniff_groups()] network to
#' the group level and measures how much each group's output flows into every
#' other group's. Influence is **directional**: if group \eqn{B}'s papers cite
#' group \eqn{A}'s papers, knowledge flows \eqn{A \to B}. The function returns
#' the cross-citation matrix, four size-corrected indices per ordered pair, the
#' net flow between pairs, and a source / broker / sink role per group.
#'
#' @details
#' Writing \eqn{C_{ij}} for the number of citations from group \eqn{i} to group
#' \eqn{j} (rows cite columns), out-strength \eqn{o_i=\sum_k C_{ik}}, in-strength
#' \eqn{\iota_j=\sum_k C_{kj}}, and total \eqn{m=\sum_{ij}C_{ij}}, each ordered
#' pair carries four normalizations:
#' \itemize{
#'   \item **debt** \eqn{C_{ij}/o_i} -- the share of \eqn{i}'s citations owed to
#'     \eqn{j} (how much \eqn{i} leans on \eqn{j});
#'   \item **audience** \eqn{C_{ij}/\iota_j} -- the share of \eqn{j}'s citations
#'     coming from \eqn{i};
#'   \item **salton** \eqn{C_{ij}/\sqrt{o_i\,\iota_j}} -- a size-free channel
#'     strength, symmetric in the pair;
#'   \item **surprise** \eqn{C_{ij}/(o_i\,\iota_j/m)} -- the flow against the
#'     configuration-model expectation; \eqn{>1} over-represented, \eqn{<1}
#'     under-represented.
#' }
#' The **net influence** \eqn{\nu_{ij}=C_{ij}-C_{ji}} removes reciprocal flow,
#' and the group **balance** \eqn{\beta_i=\iota_i-o_i} (received minus made)
#' classifies each group as a *source* (\eqn{\beta>0}, foundational), a *sink*
#' (\eqn{\beta<0}, frontier consumer), or a *broker* (\eqn{\beta\approx 0}). The
#' balances of a closed system sum to zero. The diagonal \eqn{C_{ii}} is
#' intra-group cohesion; `self = FALSE` drops it before any normalization (the
#' balances and net are unchanged, since the diagonal cancels).
#'
#' @param groups A [sniff_groups()] object: a list with a `network` component, a
#'   directed `tidygraph`/`igraph` whose nodes carry a `group` attribute and
#'   whose edges are the internal citations (citing -> cited).
#' @param self Keep the diagonal (intra-group citations) in the matrix and in
#'   the out/in strengths. Default `TRUE`. `FALSE` studies only between-group
#'   flow.
#' @param null_reps Number of group-label permutations for a per-channel
#'   p-value: shuffle the group labels over documents, recompute the
#'   cross-citation matrix, and record how often the random flow meets or beats
#'   the observed count. Default `0` (no p-values). When `> 0`, `flow` gains a
#'   `p_value` column.
#' @param seed Optional integer seed for the permutation null, for
#'   reproducibility. Default `NULL`.
#'
#' @return An object of class `birddog_influence`, a list with:
#'   \itemize{
#'     \item `matrix`: the \eqn{G \times G} cross-citation matrix \eqn{C} (rows
#'       cite columns), groups in `mixed_sort()` order.
#'     \item `flow`: a tibble, one row per observed ordered pair, sorted by
#'       descending `surprise` -- `influencer` (cited group), `recipient`
#'       (citing group), `citations`, `debt`, `audience`, `salton`, `surprise`
#'       (and `p_value` when `null_reps > 0`).
#'     \item `groups`: per-group `received`, `made`, `balance`, `role`
#'       (source / broker / sink), sorted by descending `balance`.
#'     \item `net`: the net-influence edge list -- `from` (source), `to`
#'       (recipient), `net` (\eqn{\nu_{ij} > 0}).
#'     \item `params`: the call settings.
#'   }
#'
#' @seealso [sniff_groups()], [sniff_groups_hubs()],
#'   [plot_groups_influence_matrix()], [plot_groups_influence_network()]
#'
#' @examples
#' \dontrun{
#' groups <- sniff_groups(net)
#' infl <- sniff_groups_influence(groups)
#' infl
#' infl$groups          # source / broker / sink per group
#' infl$net             # who, on balance, leads whom
#' plot_groups_influence_matrix(infl)
#' }
#'
#' @family groups (stock)
#' @export
#' @importFrom igraph as_data_frame is_directed
#' @importFrom tibble tibble
#' @importFrom stats setNames
sniff_groups_influence <- function(groups, self = TRUE, null_reps = 0, seed = NULL) {

  # ---- validate input (mirrors sniff_groups_hubs) -------------------------
  if (!is.list(groups) || is.null(groups$network)) {
    stop("'groups' must be a sniff_groups() object (a list with a 'network' component).",
         call. = FALSE)
  }
  if (!is.numeric(null_reps) || length(null_reps) != 1 || null_reps < 0) {
    stop("'null_reps' must be a single non-negative number.", call. = FALSE)
  }
  net <- groups$network
  if (!igraph::is_directed(net)) {
    stop("sniff_groups_influence() needs a directed citation network; ",
         "'groups$network' is undirected.", call. = FALSE)
  }
  nodes <- igraph::as_data_frame(net, what = "vertices")
  edges <- igraph::as_data_frame(net, what = "edges")
  if (!"group" %in% names(nodes)) {
    stop("nodes of 'groups$network' must carry a 'group' attribute.", call. = FALSE)
  }
  if (!all(c("from", "to") %in% names(edges)) || nrow(edges) == 0) {
    stop("'groups$network' has no internal citation edges.", call. = FALSE)
  }

  node_name <- if ("name" %in% names(nodes)) nodes$name else rownames(nodes)
  gmap <- stats::setNames(as.character(nodes$group), node_name)

  # ---- lift edges onto (citing group i, cited group j) --------------------
  gi <- gmap[edges$from]
  gj <- gmap[edges$to]
  keep <- !is.na(gi) & !is.na(gj)
  if (!isTRUE(self)) keep <- keep & (gi != gj)
  gi <- gi[keep]; gj <- gj[keep]
  if (length(gi) == 0) {
    stop("no internal citations remain after mapping documents to groups.", call. = FALSE)
  }

  glev <- mixed_sort(unique(c(gi, gj)))
  C <- .influence_matrix(gi, gj, glev)
  out <- .influence_core(C, glev)

  # ---- permutation null (per-channel p-value) -----------------------------
  if (null_reps > 0) {
    if (!is.null(seed)) set.seed(seed)
    pmat <- .influence_null(C, gmap, edges, self = isTRUE(self),
                            glev = glev, null_reps = null_reps)
    out$flow$p_value <- pmat[cbind(out$flow$recipient, out$flow$influencer)]
  }

  out$params <- list(self = isTRUE(self), null_reps = null_reps, seed = seed,
                     n_groups = length(glev), m = sum(C))
  structure(out, class = c("birddog_influence", "list"))
}

#' Cross-citation matrix from citing/cited group vectors
#' @keywords internal
.influence_matrix <- function(gi, gj, glev) {
  C <- table(factor(gi, levels = glev), factor(gj, levels = glev))
  C <- unclass(C)
  storage.mode(C) <- "integer"
  dimnames(C) <- list(glev, glev)
  C
}

#' Indices, balance and net flow from a cross-citation matrix
#' @keywords internal
#' @importFrom tibble tibble
.influence_core <- function(C, glev) {
  o <- rowSums(C); iota <- colSums(C); m <- sum(C)
  ov <- as.numeric(o); iov <- as.numeric(iota)

  # long table over observed channels (cells with at least one citation)
  rr <- row(C); cc <- col(C); hit <- C > 0
  ri <- rr[hit]; ci <- cc[hit]; cnt <- as.integer(C[hit])
  flow <- tibble::tibble(
    influencer = glev[ci],
    recipient  = glev[ri],
    citations  = cnt,
    debt     = cnt / ov[ri],
    audience = cnt / iov[ci],
    salton   = cnt / sqrt(ov[ri] * iov[ci]),
    surprise = cnt / (ov[ri] * iov[ci] / m)
  )
  flow <- flow[order(-flow$surprise, -flow$citations), ]

  balance <- as.integer(iota - o)
  groups_tbl <- tibble::tibble(
    group    = glev,
    received = as.integer(iota),
    made     = as.integer(o),
    balance  = balance,
    role     = ifelse(balance > 0, "source", ifelse(balance < 0, "sink", "broker"))
  )
  groups_tbl <- groups_tbl[order(-groups_tbl$balance, groups_tbl$group), ]

  # net flow: nu[i, j] = C[i, j] - C[j, i]; nu > 0 means j is a net source for i
  nu <- C - t(C)
  ij <- which(nu > 0, arr.ind = TRUE)
  net <- tibble::tibble(
    from = glev[ij[, "col"]],   # source = cited group j
    to   = glev[ij[, "row"]],   # recipient = citing group i
    net  = as.integer(nu[ij])
  )
  net <- net[order(-net$net), ]

  list(matrix = C, flow = flow, groups = groups_tbl, net = net)
}

#' Per-channel permutation p-value matrix
#' @keywords internal
.influence_null <- function(C, gmap, edges, self, glev, null_reps) {
  from_idx <- match(edges$from, names(gmap))
  to_idx   <- match(edges$to, names(gmap))
  grp_vals <- unname(gmap)
  hits <- matrix(0L, nrow(C), ncol(C), dimnames = dimnames(C))
  for (r in seq_len(null_reps)) {
    perm <- sample(grp_vals)
    fi <- perm[from_idx]; fj <- perm[to_idx]
    keep <- !is.na(fi) & !is.na(fj)
    if (!self) keep <- keep & (fi != fj)
    Cn <- .influence_matrix(fi[keep], fj[keep], glev)
    hits <- hits + (Cn >= C)
  }
  (1L + hits) / (null_reps + 1)
}

#' @rdname sniff_groups_influence
#' @param x A `birddog_influence` object.
#' @export
is_influence <- function(x) inherits(x, "birddog_influence")

#' @rdname sniff_groups_influence
#' @param ... Ignored.
#' @export
print.birddog_influence <- function(x, ...) {
  p <- x$params
  cat(sprintf("<birddog_influence>  %d groups, %d internal citations%s\n",
              p$n_groups, p$m, if (!p$self) "  (between-group only)" else ""))
  g <- x$groups
  if (nrow(g)) {
    cat(sprintf("  roles: %d source / %d broker / %d sink\n",
                sum(g$role == "source"), sum(g$role == "broker"),
                sum(g$role == "sink")))
    src <- g[g$role == "source", , drop = FALSE]
    snk <- g[g$role == "sink", , drop = FALSE]
    if (nrow(src)) cat(sprintf("  top source: %s (balance %+d)\n",
                               src$group[1], src$balance[1]))
    if (nrow(snk)) {
      k <- which.min(snk$balance)
      cat(sprintf("  top sink:   %s (balance %+d)\n", snk$group[k], snk$balance[k]))
    }
  }
  off <- x$flow[x$flow$influencer != x$flow$recipient, , drop = FALSE]
  if (nrow(off)) {
    top <- off[which.max(off$surprise), ]
    cat(sprintf("  strongest channel: %s cites %s (surprise %.2f)\n",
                top$recipient, top$influencer, top$surprise))
  }
  cat("  components: $matrix  $flow  $groups  $net\n")
  invisible(x)
}
