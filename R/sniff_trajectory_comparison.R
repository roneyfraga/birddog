#' Compare trajectory detectors by content coherence
#'
#' Runs [sniff_trajectory_coherence()] on each named flow, across every content signal and the
#' chosen `profile`(s), and reports which detector produces the more
#' content-coherent partition, whether the signals agree, and the contested nodes
#' (those a pair of detectors place in different final groups).
#'
#' @param flows Named list of `birddog_flow` objects, e.g.
#'   `list(flow = ..., channel = ...)`.
#' @param content Named list of `(document_id, feature)` data frames (see
#'   [sniff_trajectory_coherence()]).
#' @param profile Passed to [sniff_trajectory_coherence()] (default `"incremental"`).
#' @param signals Which `names(content)` to score (default all).
#' @return A `sniff_trajectory_comparison` object: a list with `summary` (per
#'   method x signal x profile x resolution), `verdict` (`best_method`, `delta`
#'   per signal x profile x resolution), `agreement` (do all signals pick the same
#'   `best_method`, per profile x resolution), and `contested` (nodes the
#'   detectors place in different final groups, with each method's `final_group`
#'   and silhouette, plus the `closer_method`;
#'   `NULL` unless exactly two flows are given).
#' @seealso [sniff_trajectory_coherence()]
#' @family flow utilities
#' @export
#' @importFrom rlang .data
sniff_trajectory_comparison <- function(flows, content, profile = "incremental",
                          signals = names(content)) {
  if (!is.list(flows) || is.null(names(flows)) || !length(flows)) {
    stop("`flows` must be a named list of birddog_flow objects", call. = FALSE)
  }
  apps <- lapply(flows, sniff_trajectory_coherence, content = content,
                 profile = profile, signals = signals)
  summary <- dplyr::bind_rows(lapply(names(apps), function(m) {
    s <- apps[[m]]$summary; s$method <- m; s
  }))
  summary <- summary[, c("method", setdiff(names(summary), "method")), drop = FALSE]

  if (length(flows) < 2L) {
    message("`flows` has a single element; nothing to compare. ",
            "Returning the per-flow summary only.")
    out <- list(summary = tibble::as_tibble(summary),
                verdict = tibble::tibble(), agreement = tibble::tibble(),
                contested = NULL)
    class(out) <- c("sniff_trajectory_comparison", "list")
    return(out)
  }

  verdict <- summary |>
    dplyr::group_by(.data$signal, .data$profile, .data$resolution) |>
    dplyr::summarise(
      best_method = .data$method[which.max(.data$mean_sil)],
      delta = {
        v <- sort(.data$mean_sil, decreasing = TRUE)
        if (length(v) >= 2) v[1] - v[2] else NA_real_
      }, .groups = "drop")

  agreement <- verdict |>
    dplyr::group_by(.data$profile, .data$resolution) |>
    dplyr::summarise(agree = dplyr::n_distinct(.data$best_method) == 1L,
                     .groups = "drop")

  contested <- NULL
  if (length(flows) == 2L) {
    m1 <- names(flows)[1]; m2 <- names(flows)[2]
    p1 <- .node_partition_map(flows[[m1]])[, c("node", "final_group")]
    p2 <- .node_partition_map(flows[[m2]])[, c("node", "final_group")]
    base <- merge(p1, p2, by = "node", suffixes = c("1", "2"))
    diff_final_group <- function(x, y) ifelse(is.na(x) != is.na(y), TRUE,
                                 ifelse(is.na(x) & is.na(y), FALSE, x != y))
    base <- base[diff_final_group(base$final_group1, base$final_group2), , drop = FALSE]
    if (nrow(base)) {
      # per-method final_group-resolution silhouette, keyed by (node, signal, profile)
      sil_tab <- function(m) {
        nn <- apps[[m]]$nodes
        nn <- nn[nn$resolution == "final_group",
                 c("node", "signal", "profile", "sil")]
        stats::setNames(nn,
          c("node", "signal", "profile", paste0("sil_", m)))
      }
      # full join: keep a divergent node even if only one method scored it
      node_sil <- merge(sil_tab(m1), sil_tab(m2),
                        by = c("node", "signal", "profile"), all = TRUE)
      # left join onto the divergent set: never silently drop a divergent node
      contested <- merge(base, node_sil, by = "node", all.x = TRUE)
      contested$closer_method <- ifelse(
        contested[[paste0("sil_", m1)]] >= contested[[paste0("sil_", m2)]], m1, m2)
      contested <- tibble::as_tibble(contested)
    } else {
      contested <- tibble::tibble()
    }
  }

  out <- list(summary = tibble::as_tibble(summary), verdict = verdict,
              agreement = agreement, contested = contested)
  class(out) <- c("sniff_trajectory_comparison", "list")
  out
}

#' @keywords internal
#' @export
print.sniff_trajectory_comparison <- function(x, ...) {
  cat("<sniff_trajectory_comparison>\n")
  print(x$verdict)
  print(x$agreement)
  invisible(x)
}
