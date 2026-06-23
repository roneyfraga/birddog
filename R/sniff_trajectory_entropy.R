#' Per-year keyword diversity (Pielou's J') along each flow trajectory
#'
#' Recomputes, from the documents in each [sniff_trajectory_braid()] trajectory's
#' own group-year nodes, the normalized Shannon entropy (Pielou's evenness J') of
#' their keyword distribution, one value per year of the trajectory's life. A
#' falling series marks thematic convergence, a rising one diversification. Unlike
#' [sniff_entropy()], which scores a static `sniff_groups()` partition by
#' group-year, this follows the trajectory's actual membership, so an absorbed
#' tributary is scored on its own documents rather than its destination group's.
#'
#' This is **keyword** entropy (thematic diversity), distinct from the dynamics
#' `dest_entropy` (the dispersion of an absorbed cohort across destination groups);
#' the dynamics column is named `keyword_entropy` to keep the two apart.
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param keywords A long data frame with columns `document_id` and `keyword`, one
#'   row per document-keyword pair (e.g. the corpus `DE` field split on `;` and
#'   keyed by the membership id).
#'
#' @return A tibble, one row per trajectory: `traj_id`, `type`, `group`, and
#'   `keyword_entropy`, a list-column whose each cell is a tibble with columns
#'   `year` and `keyword_entropy` (Pielou's J' over that node-year's documents, in
#'   `[0, 1]`; `0` for a single keyword, `NA` when the year's documents carry none).
#'
#' @seealso [sniff_entropy()], [sniff_trajectory_cct()], [sniff_trajectory_dynamics()],
#'   [sniff_trajectory_braid()]
#' @family trajectory analysis
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
sniff_trajectory_entropy <- function(flow, keywords) {
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  if (!is.data.frame(keywords) || !all(c("document_id", "keyword") %in% names(keywords))) {
    stop("'keywords' must be a data frame with columns 'document_id' and 'keyword'",
         call. = FALSE)
  }
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  node_docs <- split(dpg$document_id, dpg$group_id)
  kw <- keywords[!is.na(keywords$keyword) & keywords$keyword != "" &
                   !is.na(keywords$document_id), c("document_id", "keyword"), drop = FALSE]
  by_doc <- split(kw$keyword, kw$document_id)

  rows <- lapply(seq_len(nrow(tr)), function(i) {
    ns <- tr$nodes[[i]]; yrs <- .extract_year(ns); o <- order(yrs)
    ser <- dplyr::bind_rows(lapply(o, function(k) {
      docs <- unique(node_docs[[ns[k]]])
      tibble::tibble(year = yrs[k],
                     keyword_entropy = .pielou_evenness(unlist(by_doc[docs], use.names = FALSE)))
    }))
    tibble::tibble(traj_id = tr$traj_id[i], type = tr$type[i], group = tr$group[i],
                   keyword_entropy = list(ser))
  })
  dplyr::bind_rows(rows)
}

#' Pielou's evenness (normalized Shannon entropy) of a vector of categories
#'
#' `J' = H / log2(n)` over the category frequencies; `0` for a single category,
#' `NA` for none. Matches the normalization of [sniff_entropy()].
#' @keywords internal
.pielou_evenness <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_real_)
  f <- table(x); n <- length(f)
  if (n <= 1) return(0)
  p <- as.numeric(f) / sum(f)
  -sum(p * log2(p)) / log2(n)
}
