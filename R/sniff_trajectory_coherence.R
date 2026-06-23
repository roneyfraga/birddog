# Internal helpers for sniff_trajectory_coherence() ---------------------------------------

#' Node -> distinct document ids
#'
#' @param docs_per_group Membership tibble with `group_id` and `document_id`.
#' @return Named list mapping each node (`group_id`) to its distinct
#'   `document_id` vector.
#' @keywords internal
.node_docs <- function(docs_per_group) {
  dpg <- docs_per_group[!duplicated(docs_per_group[c("group_id", "document_id")]), ,
                        drop = FALSE]
  split(dpg$document_id, dpg$group_id)
}

#' Node -> incremental document ids (docs new to the node's year)
#'
#' `P_inc(n) = docs(n) \ union{ docs(m) : year(m) == year(n) - 1 }`. First-year
#' nodes keep all their documents. Strips the cumulative carry-over so the
#' incremental sets partition the corpus.
#' @keywords internal
.node_docs_incremental <- function(node_docs) {
  nodes <- names(node_docs)
  yr <- .extract_year(nodes)
  by_year <- split(nodes, yr)
  union_by_year <- lapply(by_year, function(ns)
    unique(unlist(node_docs[ns], use.names = FALSE)))
  out <- stats::setNames(vector("list", length(nodes)), nodes)
  for (i in seq_along(nodes)) {
    prev <- union_by_year[[as.character(yr[i] - 1L)]]
    out[[nodes[i]]] <- if (is.null(prev)) node_docs[[nodes[i]]]
                       else setdiff(node_docs[[nodes[i]]], prev)
  }
  out
}

#' Stack a node -> docs list into a (node, document_id) data frame
#'
#' @param prof_list Named list mapping each node to a character vector of
#'   document ids (e.g. from [.node_docs()]).
#' @return A `data.frame` with columns `node` and `document_id`, one row per
#'   (node, document); zero rows (not zero columns) when every element is empty.
#' @keywords internal
.stack_profiles <- function(prof_list) {
  data.frame(
    node = rep(names(prof_list), lengths(prof_list)),
    document_id = unlist(prof_list, use.names = FALSE) %||% character(0),
    stringsAsFactors = FALSE)
}

#' Sparse node x feature count matrix from a (node, document_id, feature) frame
#'
#' Cell (n, f) = number of documents in node n carrying feature f.
#' @keywords internal
#' @importFrom rlang .data
.feature_count_matrix <- function(nf) {
  cc <- dplyr::count(nf, .data$node, .data$feature, name = "w")
  nodes_u <- sort(unique(cc$node))
  feats_u <- sort(unique(cc$feature))
  Matrix::sparseMatrix(
    i = match(cc$node, nodes_u), j = match(cc$feature, feats_u),
    x = as.double(cc$w), dims = c(length(nodes_u), length(feats_u)),
    dimnames = list(nodes_u, NULL))
}

#' 1 - Salton cosine between every pair of node profile vectors
#' @keywords internal
.cosine_distance <- function(m) {
  norms <- sqrt(Matrix::rowSums(m * m))
  g <- as.matrix(Matrix::tcrossprod(m))
  denom <- outer(norms, norms)
  cosv <- g / denom
  cosv[denom == 0] <- 0
  d <- 1 - cosv
  d[d < 0] <- 0
  dimnames(d) <- list(rownames(m), rownames(m))
  diag(d) <- 0
  d
}

#' Per-node silhouette over a distance matrix and a cluster labeling
#'
#' `sil = (b - a) / max(a, b)`. Singletons and single-cluster sets get 0.
#'
#' @param dmat A square numeric distance matrix with node names on both axes.
#' @param label Named vector of cluster labels; names must cover
#'   `rownames(dmat)`. Indexed by name, then used positionally.
#' @return A tibble with columns `node`, `a`, `b`, `sil`.
#' @keywords internal
.silhouette <- function(dmat, label) {
  nodes <- rownames(dmat)
  label <- as.character(label[nodes])
  clusters <- unique(label)
  n <- length(nodes)
  a <- numeric(n)
  b <- rep(NA_real_, n)
  for (k in seq_len(n)) {
    own <- label[k]
    same <- which(label == own & seq_len(n) != k)
    a[k] <- if (length(same)) mean(dmat[k, same]) else 0
    others <- setdiff(clusters, own)
    if (length(others)) {
      b[k] <- min(vapply(others,
        function(cl) mean(dmat[k, which(label == cl)]), numeric(1)))
    }
  }
  sil <- (b - a) / pmax(a, b)
  singleton <- vapply(seq_len(n), function(k) sum(label == label[k]) == 1, logical(1))
  sil[singleton] <- 0
  sil[!is.finite(sil)] <- 0
  tibble::tibble(node = nodes, a = a, b = b, sil = sil)
}

#' Node -> (traj_id, final_group) map from a flow's trajectories
#' @keywords internal
.node_partition_map <- function(flow) {
  tr <- flow$trajectories
  do.call(rbind, lapply(seq_len(nrow(tr)), function(i) {
    ns <- tr$nodes[[i]]
    if (!length(ns)) return(NULL)
    data.frame(node = ns, traj_id = tr$traj_id[i], final_group = tr$group[i],
               stringsAsFactors = FALSE)
  }))
}

#' Appraise the content coherence of a trajectory flow
#'
#' Scores how content-coherent a [sniff_trajectory_braid()] / [sniff_trajectory_channel()]
#' partition is, using a silhouette over an independent content signal (shared
#' references or keywords) the detectors never optimized. Each group-year node is
#' profiled from its documents' features; node-to-node distance is `1 - Salton
#' cosine`; the silhouette is computed at two cluster resolutions: `trajectory`
#' (each backbone/tributary chain) and `final_group` (each central + its tributaries).
#'
#' @param flow A `birddog_flow` (from [sniff_trajectory_braid()] or
#'   [sniff_trajectory_channel()]).
#' @param content Named list; each element a data frame with columns
#'   `document_id` and `feature` (long, one row per document-feature pair). The
#'   element name labels the signal in the output (e.g. `coupling`, `keywords`).
#' @param profile `"incremental"` (default: profile a node from the papers new to
#'   its year, stripping cumulative carry-over) and/or `"full"` (all of a node's
#'   papers).
#' @param signals Which `names(content)` to score (default all).
#' @return An `sniff_trajectory_coherence` object: a list with `nodes` (one row per
#'   node x signal x profile x resolution) and `summary` (one row per
#'   signal x profile x resolution, with `mean_sil`, `n_nodes`, `n_singletons`,
#'   `n_excluded`, `coverage`).
#' @seealso [sniff_trajectory_comparison()], [sniff_trajectory_braid()], [sniff_trajectory_channel()],
#'   [validate_flow()]
#' @family flow utilities
#' @export
sniff_trajectory_coherence <- function(flow, content,
                          profile = "incremental",
                          signals = names(content)) {
  if (!is_flow(flow)) stop("`flow` must be a birddog_flow object", call. = FALSE)
  if (identical(attr(flow, "similarity"), "coupling")) {
    warning("`flow` was routed by reference coupling (sniff_trajectory_dag(",
            "similarity = \"coupling\")); scoring it on those same references is ",
            "circular -- it re-grades what the detector optimised. Read the ",
            "result on an independent signal such as keywords.", call. = FALSE)
  }
  if (!is.list(content) || is.null(names(content)) || !length(content)) {
    stop("`content` must be a named list of (document_id, feature) data frames",
         call. = FALSE)
  }
  profile <- match.arg(profile, choices = c("incremental", "full"), several.ok = TRUE)
  signals <- intersect(signals, names(content))
  if (!length(signals)) stop("no requested signal present in `content`", call. = FALSE)

  pmap <- .node_partition_map(flow)
  universe <- pmap$node
  dpg <- flow$docs_per_group
  dpg <- dpg[dpg$group_id %in% universe, c("group_id", "document_id"), drop = FALSE]
  node_docs_full <- .node_docs(dpg)
  node_docs_inc  <- .node_docs_incremental(node_docs_full)
  corpus_n <- length(unique(dpg$document_id))

  node_rows <- list(); summ_rows <- list()
  for (pf in profile) {
    prof_list <- if (pf == "incremental") node_docs_inc else node_docs_full
    sizes <- lengths(prof_list)
    for (sg in signals) {
      feat <- content[[sg]][, c("document_id", "feature")]
      feat <- feat[!is.na(feat$feature) & feat$feature != "", , drop = FALSE]
      feat <- feat[!duplicated(feat), , drop = FALSE]
      nf <- merge(.stack_profiles(prof_list), feat, by = "document_id")
      coverage <- if (corpus_n) length(unique(nf$document_id)) / corpus_n else 0
      if (!nrow(nf)) next
      m <- .feature_count_matrix(nf)
      if (nrow(m) < 2) next
      cdist <- .cosine_distance(m)
      for (res in c("trajectory", "final_group")) {
        lab_col <- if (res == "trajectory") "traj_id" else "final_group"
        lab <- stats::setNames(pmap[[lab_col]], pmap$node)
        keep <- rownames(cdist)[!is.na(lab[rownames(cdist)])]
        if (length(keep) < 2) next
        sil <- .silhouette(cdist[keep, keep, drop = FALSE], lab[keep])
        w <- as.integer(sizes[sil$node])
        sil$signal <- sg; sil$profile <- pf; sil$resolution <- res
        sil$n_docs <- w
        sil$year <- .extract_year(sil$node)
        sil$traj_id <- pmap$traj_id[match(sil$node, pmap$node)]
        sil$final_group <- pmap$final_group[match(sil$node, pmap$node)]
        node_rows[[length(node_rows) + 1L]] <- sil
        lab_keep <- lab[keep]
        n_single <- sum(vapply(sil$node,
          function(nd) sum(lab_keep == lab_keep[[nd]]) == 1L, logical(1)))
        summ_rows[[length(summ_rows) + 1L]] <- tibble::tibble(
          signal = sg, profile = pf, resolution = res,
          mean_sil = sum(sil$sil * w) / sum(w),
          n_nodes = nrow(sil), n_singletons = as.integer(n_single),
          n_excluded = as.integer(length(unique(universe)) - nrow(sil)),
          coverage = coverage)
      }
    }
  }
  out <- list(nodes = dplyr::bind_rows(node_rows),
              summary = dplyr::bind_rows(summ_rows))
  class(out) <- c("sniff_trajectory_coherence", "list")
  out
}

#' @keywords internal
#' @export
print.sniff_trajectory_coherence <- function(x, ...) {
  cat("<sniff_trajectory_coherence>\n")
  print(x$summary)
  invisible(x)
}
