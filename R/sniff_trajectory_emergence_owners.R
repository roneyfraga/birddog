#' Roll trajectory emergence up to the authors who own it
#'
#' The actor-level companion to [sniff_trajectory_dynamics()], after the player
#' scoring of Garner et al. (2017): it credits each author with the emergence of
#' the living trajectories their documents belong to. An author who publishes many
#' documents inside high-`emergence_index` centrals is an owner of the field's
#' emergence; one whose work sits in mature or absorbed lineages is not.
#'
#' For every central trajectory with a defined `emergence_index` (the living
#' population), each author is credited `emergence_index(t)` per distinct document
#' they hold in `t`. Absorbed lineages carry no `emergence_index` and are skipped,
#' so a contributor to a dead tributary is never scored as owning emergence.
#'
#' \deqn{Total(a) = \sum_t emergence\_index(t)\, n_{a,t}, \qquad
#'       Norm(a) = Total(a) / \sqrt{\sum_t n_{a,t}}}
#'
#' where \eqn{n_{a,t}} is author \eqn{a}'s distinct documents in trajectory
#' \eqn{t}. `Total` favours prolific owners; `Norm` (per square-root of output,
#' Garner's normalization) surfaces high-intensity authors with fewer papers.
#'
#' With `by_trajectory = TRUE` the per-trajectory term \eqn{n_{a,t}} is returned
#' instead of being summed away: one row per (trajectory, author), so you can read
#' which authors *dominate each living trajectory* rather than the field as a
#' whole. `top_n` then trims to the leading authors per trajectory.
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param dynamics A [sniff_trajectory_dynamics()] tibble (supplies `traj_id` and
#'   `emergence_index`).
#' @param authors A long data frame with columns `document_id` and `author`, one
#'   row per document-author pair.
#' @param min_docs Drop authors with fewer than this many credited documents:
#'   summed across trajectories when aggregated, or within the trajectory when
#'   `by_trajectory = TRUE`. Default 1 keeps all. Use a higher cut to read
#'   `norm`/`share` without one-paper noise.
#' @param by_trajectory If `TRUE`, return the per-trajectory breakdown (one row
#'   per trajectory-author) instead of the field-level aggregate (one row per
#'   author). Default `FALSE`.
#' @param top_n Keep only the highest-scoring authors -- overall (by `total`) when
#'   aggregated, or per trajectory (by `contribution`) when
#'   `by_trajectory = TRUE`. Default `NULL` keeps all.
#'
#' @return A tibble.
#'
#'   When `by_trajectory = FALSE` (default): one row per author, sorted by
#'   descending `total` -- `author`, `total` (emergence ownership), `ndocs`
#'   (credited document-trajectory incidences), `norm` (`total / sqrt(ndocs)`).
#'
#'   When `by_trajectory = TRUE`: one row per trajectory-author, sorted by
#'   trajectory then descending `contribution` -- `traj_id`, `group`, `author`,
#'   `ndocs` (the author's distinct documents in the trajectory, \eqn{n_{a,t}}),
#'   `emergence_index` (the trajectory's weight), `contribution`
#'   (`emergence_index * ndocs`, the author's share of that trajectory's
#'   emergence), and `share` (`ndocs` over the trajectory's distinct documents --
#'   the author's coverage of its papers).
#'
#' @seealso [sniff_trajectory_dynamics()], [sniff_trajectory_community()],
#'   [sniff_trajectory_contribution()]
#' @family trajectory analysis
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows group_by summarise mutate arrange desc transmute slice_head ungroup
#' @importFrom rlang .data
sniff_trajectory_emergence_owners <- function(flow, dynamics, authors, min_docs = 1,
                                              by_trajectory = FALSE, top_n = NULL) {
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  if (!is.data.frame(dynamics) ||
      !all(c("traj_id", "emergence_index") %in% names(dynamics))) {
    stop("'dynamics' must be a sniff_trajectory_dynamics() tibble", call. = FALSE)
  }
  if (!is.data.frame(authors) || !all(c("document_id", "author") %in% names(authors))) {
    stop("'authors' must be a data frame with columns 'document_id' and 'author'",
         call. = FALSE)
  }
  if (!is.logical(by_trajectory) || length(by_trajectory) != 1L || is.na(by_trajectory)) {
    stop("'by_trajectory' must be a single TRUE/FALSE", call. = FALSE)
  }
  if (!is.null(top_n) && (!is.numeric(top_n) || length(top_n) != 1L || top_n < 1)) {
    stop("'top_n' must be NULL or a single positive number", call. = FALSE)
  }
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  node_docs <- split(dpg$document_id, dpg$group_id)
  ei <- stats::setNames(dynamics$emergence_index, dynamics$traj_id)

  au <- authors[!is.na(authors$author) & authors$author != "" &
                  !is.na(authors$document_id), c("document_id", "author"), drop = FALSE]
  au <- au[!duplicated(au), , drop = FALSE]
  by_doc <- split(au$author, au$document_id)

  rows <- list()
  for (i in seq_len(nrow(tr))) {
    w <- ei[[tr$traj_id[i]]]
    if (is.null(w) || is.na(w)) next                   # living population only
    docs <- unique(unlist(node_docs[tr$nodes[[i]]], use.names = FALSE))
    a <- unlist(by_doc[docs], use.names = FALSE)        # one entry per (author, doc)
    a <- a[!is.na(a)]
    if (!length(a)) next
    tb <- table(a)
    rows[[length(rows) + 1L]] <- tibble::tibble(
      traj_id = tr$traj_id[i], group = tr$group[i], emergence_index = w,
      n_traj_docs = length(docs),
      author = names(tb), contrib = w * as.integer(tb), nd = as.integer(tb))
  }

  if (!length(rows)) {
    if (by_trajectory) {
      return(tibble::tibble(traj_id = character(), group = character(),
                            author = character(), ndocs = integer(),
                            emergence_index = numeric(), contribution = numeric(),
                            share = numeric()))
    }
    return(tibble::tibble(author = character(), total = numeric(),
                          ndocs = integer(), norm = numeric()))
  }

  scored <- dplyr::bind_rows(rows)

  if (by_trajectory) {
    out <- dplyr::transmute(scored,
                            .data$traj_id, .data$group, .data$author,
                            ndocs = .data$nd,
                            .data$emergence_index,
                            contribution = .data$contrib,
                            share = .data$nd / .data$n_traj_docs)
    out <- out[out$ndocs >= min_docs, , drop = FALSE]
    out <- dplyr::arrange(out, .data$traj_id, dplyr::desc(.data$contribution))
    if (!is.null(top_n)) {
      out <- dplyr::ungroup(dplyr::slice_head(
        dplyr::group_by(out, .data$traj_id), n = top_n))
    }
    return(out)
  }

  out <- dplyr::summarise(dplyr::group_by(scored, .data$author),
                          total = sum(.data$contrib), ndocs = sum(.data$nd),
                          .groups = "drop")
  out <- dplyr::mutate(out, norm = .data$total / sqrt(.data$ndocs))
  out <- out[out$ndocs >= min_docs, , drop = FALSE]
  out <- dplyr::arrange(out, dplyr::desc(.data$total))
  if (!is.null(top_n)) out <- utils::head(out, top_n)
  out
}
