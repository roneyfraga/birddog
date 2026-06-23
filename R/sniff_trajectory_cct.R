#' Per-year citation cycle time (renewal pace) along each flow trajectory
#'
#' Recomputes the Citation Cycle Time (Kayal, 1999) from the documents in each
#' [sniff_trajectory_braid()] trajectory's own group-year nodes, one value per
#' year of the trajectory's life: per node-year, the median over its documents of
#' each document's median cited-reference age. A low or falling series marks a
#' fast-renewing knowledge stock, a diagnostic signal of emergence.
#'
#' This is the faithful, per-document counterpart of [sniff_cct()]
#' (which scores a static `sniff_groups()` partition by group-year): it follows the
#' trajectory's actual membership, so an absorbed tributary is scored on its own
#' documents rather than its destination group's.
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param references A long data frame with columns `document_id` and `ref_age`,
#'   one row per document-cited-reference, `ref_age` the reference's age in years
#'   (`citing_year - cited_year`). Build it from the corpus `CR` field joined to the
#'   cited works' publication years (e.g. the `tracked_cr_py` of
#'   [sniff_cct()]).
#'
#' @return A tibble, one row per trajectory: `traj_id`, `type`, `group`, and `cct`,
#'   a list-column whose each cell is a tibble with columns `year` and `cct` (the
#'   median citation age over that node-year's documents; `NA` when none of them
#'   carry references).
#'
#' @references Kayal AA, Waters RC (1999). An empirical evaluation of the technology
#'   cycle time indicator. IEEE Trans. Eng. Manag. 46(2):127-31.
#' @seealso [sniff_cct()], [sniff_trajectory_entropy()],
#'   [sniff_trajectory_dynamics()], [sniff_trajectory_braid()]
#' @family trajectory analysis
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
sniff_trajectory_cct <- function(flow, references) {
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  if (!is.data.frame(references) ||
      !all(c("document_id", "ref_age") %in% names(references))) {
    stop("'references' must be a data frame with columns 'document_id' and 'ref_age'",
         call. = FALSE)
  }
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  node_docs <- split(dpg$document_id, dpg$group_id)
  rf <- references[!is.na(references$ref_age) & !is.na(references$document_id),
                   c("document_id", "ref_age"), drop = FALSE]
  # per-document median reference age (so reference-rich papers do not dominate)
  paper_cct <- vapply(split(rf$ref_age, rf$document_id), stats::median, numeric(1))

  rows <- lapply(seq_len(nrow(tr)), function(i) {
    ns <- tr$nodes[[i]]; yrs <- .extract_year(ns); o <- order(yrs)
    ser <- dplyr::bind_rows(lapply(o, function(k) {
      docs <- unique(node_docs[[ns[k]]])
      v <- paper_cct[intersect(docs, names(paper_cct))]
      tibble::tibble(year = yrs[k], cct = if (length(v)) stats::median(v) else NA_real_)
    }))
    tibble::tibble(traj_id = tr$traj_id[i], type = tr$type[i], group = tr$group[i],
                   cct = list(ser))
  })
  dplyr::bind_rows(rows)
}
