#' Community breadth (distinct authors) of each flow trajectory
#'
#' Measures the FUSE *community* pillar (Carley et al., 2018) for every
#' [sniff_trajectory_braid()] trajectory: how many distinct authors stand behind
#' its documents, and how broad that authorship is per document. A raw author
#' count tracks trajectory size; `authors_per_doc` is the size-independent breadth
#' --- a high value marks a lineage carried by many distinct contributors (a wide
#' community), a low value a lineage concentrated in a tight group.
#'
#' This is the document-cluster analogue of the term-level community measure in
#' the emergence-indicator literature, where a term counts as emergent only when
#' used by more than one independent author. It complements
#' [sniff_trajectory_dynamics()], which names the community pillar but scores it
#' only through document counts (`recruitment`, `size`).
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param authors A long data frame with columns `document_id` and `author`, one
#'   row per document-author pair (e.g. the corpus `AU` field split on `;` and
#'   joined to the membership key). Documents absent from `authors` contribute no
#'   authors but still count in `n_docs`, so keep author coverage high.
#'
#' @return A tibble, one row per trajectory: `traj_id`, `type`, `group`,
#'   `n_docs` (distinct documents of the trajectory), `n_authors` (distinct
#'   authors across them), `authors_per_doc` (`n_authors / n_docs`, the breadth;
#'   `NA` for an empty trajectory).
#'
#' @seealso [sniff_trajectory_dynamics()], [sniff_trajectory_hubs()],
#'   [sniff_trajectory_braid()]
#' @family trajectory analysis
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
sniff_trajectory_community <- function(flow, authors) {
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  if (!is.data.frame(authors) || !all(c("document_id", "author") %in% names(authors))) {
    stop("'authors' must be a data frame with columns 'document_id' and 'author'",
         call. = FALSE)
  }
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  node_docs <- split(dpg$document_id, dpg$group_id)

  au <- authors[!is.na(authors$author) & authors$author != "" &
                  !is.na(authors$document_id), c("document_id", "author"), drop = FALSE]
  au <- au[!duplicated(au), , drop = FALSE]
  by_doc <- split(au$author, au$document_id)

  rows <- lapply(seq_len(nrow(tr)), function(i) {
    docs <- unique(unlist(node_docs[tr$nodes[[i]]], use.names = FALSE))
    a <- unique(unlist(by_doc[docs], use.names = FALSE))
    tibble::tibble(
      traj_id = tr$traj_id[i], type = tr$type[i], group = tr$group[i],
      n_docs = length(docs), n_authors = length(a),
      authors_per_doc = if (length(docs)) length(a) / length(docs) else NA_real_)
  })
  dplyr::bind_rows(rows)
}
