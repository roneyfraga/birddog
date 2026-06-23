#' Documents an intermediate trajectory contributes to a target in a given year
#'
#' Selects a `source` trajectory's node in a specific `year` and returns its
#' documents, flagging which of them belong to the `target` trajectory's
#' final-year community. This is the fine-grained, per-document view of the
#' inter-trajectory contribution that [sniff_trajectory_destination()] aggregates:
#' where `destination` splits a terminal cohort across every final group,
#' `contribution` answers "which papers of `source`, as they stood in `year`,
#' consolidate into this one `target`".
#'
#' The result is document ids only (plus the contribution flag), so it composes
#' with any downstream join: keywords (via the corpus `DE` field or
#' [sniff_groups_keywords()]), authors, citations, or STM topics. Filter on
#' `in_target` for the contributed set.
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param source Trajectory id whose year-node is read, typically an intermediate
#'   `trN` (e.g. `"tr14"`). Any trajectory id present in `flow` is accepted; each
#'   trajectory has at most one node per year.
#' @param year Integer publication year. `source` must have a node in this year.
#' @param target Trajectory id whose final-year community is the contribution
#'   basis, typically a central `tr::cNgN`. Its destination `group` must be known.
#' @return A tibble with one row per document in `source`'s `year` node:
#'   `source`, `target`, `year`, `document_id`, and `in_target` (logical: the
#'   document is in `target`'s final-year community). The `source`/`target`/`year`
#'   columns are constant, so results for several (source, year, target) triples
#'   row-bind cleanly. Summarise with one line, e.g.
#'   `dplyr::summarise(x, n = dplyr::n(), contributed = sum(in_target))`.
#' @seealso [sniff_trajectory_destination()], [sniff_trajectory_self_sufficiency()],
#'   [sniff_groups_keywords()]
#' @family trajectory analysis
#' @export
#' @importFrom tibble tibble
sniff_trajectory_contribution <- function(flow, source, year, target) {
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  tr <- flow$trajectories
  for (nm in c("source", "target")) {
    val <- get(nm)
    if (!is.character(val) || length(val) != 1 || !(val %in% tr$traj_id)) {
      stop("'", nm, "' must be a single trajectory id present in the flow object",
           call. = FALSE)
    }
  }
  if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
    stop("'year' must be a single year", call. = FALSE)
  }
  year <- as.integer(year)

  tgroup <- tr$group[tr$traj_id == target]
  if (is.na(tgroup)) {
    stop("'target' (", target, ") has no destination group (extinct trajectory); ",
         "its final-year community is undefined", call. = FALSE)
  }

  nodes <- tr$nodes[[which(tr$traj_id == source)]]
  node <- nodes[.extract_year(nodes) == year]
  if (length(node) == 0) {
    stop("'source' (", source, ") has no node in year ", year, call. = FALSE)
  }

  dpg <- flow$docs_per_group
  cohort <- unique(unlist(split(dpg$document_id, dpg$group_id)[node], use.names = FALSE))

  last_year <- as.integer(flow$last_year)
  final_comm <- unique(dpg$document_id[dpg$network_until == last_year &
                                         dpg$group == tgroup])

  tibble::tibble(
    source = source, target = target, year = year,
    document_id = cohort, in_target = cohort %in% final_comm
  )
}
