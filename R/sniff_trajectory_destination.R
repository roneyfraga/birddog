#' Where a trajectory's terminal cohort goes
#'
#' Dispatches on `x`: a [sniff_trajectory_flow()] object returns the destination of
#' `traj_id`'s terminal cohort (its per-final-group split and dormant share) plus its
#' immediate absorber and ultimate destination; a legacy detected object keeps the
#' per-group behavior.
#'
#' @param x A [sniff_trajectory_flow()] object, or a legacy detected object.
#' @param ... For the flow object: `traj_id`. For legacy: `traj_id, docs_per_group, ...`.
#' @return A list (`target`, `target_info`, `destination`, `cohort_size`,
#'   `dormant_share`, `continuation_traj`, `last_year`).
#' @seealso [sniff_trajectory_flow()], [sniff_trajectory_self_sufficiency()]
#' @export
sniff_trajectory_destination <- function(x, ...) {
  if (is.list(x) && !is.data.frame(x) && !is.null(x$trajectories) &&
      is.data.frame(x$trajectories) && "absorbed_into" %in% names(x$trajectories)) {
    .destination_from_flow(x, ...)
  } else {
    .destination_legacy(x, ...)
  }
}

#' @keywords internal
#' @importFrom dplyr arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
.destination_from_flow <- function(flow, traj_id) {
  tr <- flow$trajectories
  if (!is.character(traj_id) || length(traj_id) != 1 || !(traj_id %in% tr$traj_id)) {
    stop("'traj_id' must be a single trajectory id present in the flow object",
         call. = FALSE)
  }
  trow <- tr[tr$traj_id == traj_id, ]
  dpg <- flow$docs_per_group
  last_year <- flow$last_year
  node_docs <- split(dpg$document_id, dpg$group_id)

  fin <- dpg[dpg$network_until == last_year, c("document_id", "group"), drop = FALSE]
  fin <- fin[!duplicated(fin$document_id), , drop = FALSE]
  doc_final <- stats::setNames(fin$group, fin$document_id)

  nodes <- trow$nodes[[1]]
  terminal_node <- nodes[which.max(.extract_year(nodes))]
  cohort <- unique(node_docs[[terminal_node]])
  fg <- unname(doc_final[cohort])
  dropped <- sum(is.na(fg))
  tab <- table(fg[!is.na(fg)])
  g_final <- names(tab); n <- as.integer(tab)
  if (dropped > 0) { g_final <- c(g_final, "(dropped)"); n <- c(n, dropped) }
  destination <- tibble::tibble(g_final = g_final, n = n,
                                prop = n / length(cohort))
  destination <- dplyr::arrange(destination, dplyr::desc(.data$n))

  source_info <- list(group = NA_character_, traj_id = traj_id,
                      start = trow$start, end = trow$end,
                      cohort_size = length(cohort))
  continuation_info <- NULL
  if (!is.na(trow$absorbed_into)) {
    arow <- tr[tr$traj_id == trow$absorbed_into, ]
    a_docs <- unique(unlist(node_docs[arow$nodes[[1]]], use.names = FALSE))
    continuation_info <- list(group = NA_character_, traj_id = arow$traj_id,
                              start = arow$start, end = arow$end,
                              n_papers = length(intersect(cohort, a_docs)))
  }

  list(
    target = traj_id,
    target_info = list(traj_id = traj_id, type = trow$type, group = trow$group,
                       start = trow$start, end = trow$end, size = trow$size,
                       absorbed_into = trow$absorbed_into,
                       absorption_year = trow$absorption_year),
    destination = destination,
    cohort_size = length(cohort),
    dormant_share = dropped / length(cohort),
    continuation_traj = trow$absorbed_into,
    last_year = last_year,
    source_info = source_info,
    continuation_info = continuation_info
  )
}

#' Track where an incomplete trajectory's papers end up (legacy)
#'
#' In cumulative clustering a paper is never removed from the network, so a
#' trajectory that stops before the last year (a "dying" trajectory detected by
#' [detect_main_trajectories()]) does not mean its papers died. They are
#' re-clustered and, by the final year, land in some group. This function takes
#' the documents at a trajectory's terminal (last) node and tracks them forward
#' to the final year, reporting where they went.
#'
#' Group labels (`c1g1`, `c1g2`, ...) are size ranks recomputed independently
#' every cumulative year, so the same community drifts across labels over time.
#' Destinations are therefore resolved by document identity, never by label.
#'
#' @param detected The output of [detect_main_trajectories()] for one group.
#'   Only its `trajectories` tibble (`traj_id` and `nodes` columns) is used.
#' @param traj_id Character identifier of the trajectory to track (e.g.
#'   `"tr3"`).
#' @param docs_per_group A tibble mapping documents to group-year nodes, as in
#'   `groups_cumulative_trajectories$docs_per_group` (columns `group_id`,
#'   `document_id`, `network_until`, `group`). Holds the full per-year
#'   membership, which is required to follow papers that leave the anchor group.
#' @param last_year Final year to resolve destinations against (default: the
#'   maximum `network_until` in `docs_per_group`).
#' @param all_detected Optional named list of [detect_main_trajectories()]
#'   outputs, one per group (i.e. the object you build for every group, keyed by
#'   group id). When supplied, the cohort is also resolved at the *trajectory*
#'   level: each paper is attributed to the trajectory (in any group) that
#'   carries it across the most years after the death year. This identifies
#'   which living trajectory absorbed the stagnant one.
#' @param group Optional source group id (e.g. `"c1g10"`), used only to label the
#'   source trajectory in `source_info`.
#'
#' @return A list with:
#' \describe{
#'   \item{terminal_node}{The trajectory's last node (e.g. `"y2018c1g16"`).}
#'   \item{cohort_size}{Number of documents in the terminal node.}
#'   \item{destination}{Tibble `g_final`, `n`, `prop` of where the cohort lands
#'     in `last_year`. Papers below the minimum group size in the final year
#'     (absent from `docs_per_group`) are grouped as `"(dropped)"`.}
#'   \item{dormant_share}{Fraction of the cohort that ends up `"(dropped)"`.}
#'   \item{continuation}{Dominant non-dropped destination group, i.e. the group
#'     whose own trajectory continues the cohort. `NA` if all dropped.}
#'   \item{flow}{Long tibble of consecutive-year transitions
#'     (`from_id`, `to_id`, `from_year`, `to_year`, `from_group`, `to_group`,
#'     `final_group`, `n`, plus `dest_traj` when `all_detected` is given), one
#'     row per (transition x eventual destination). Ready for an alluvial /
#'     Sankey via [plot_trajectory_destination()].}
#'   \item{destination_traj}{(only with `all_detected`) Tibble `traj_key`,
#'     `group`, `traj_id`, `n`, `prop`: the cohort split across the trajectories
#'     that absorbed it. Papers with no post-death carrier are `"(none)"`.}
#'   \item{continuation_traj}{(only with `all_detected`) Dominant absorbing
#'     trajectory key, e.g. `"c1g16::tr1"`.}
#'   \item{continuation_info, source_info}{(only with `all_detected`) Small
#'     lists describing the absorbing and source trajectories (group, traj_id,
#'     start, end), used by [plot_trajectory_handoff()].}
#' }
#'
#' @keywords internal
#'
#' @importFrom dplyr filter mutate select left_join count arrange desc group_by
#' @importFrom dplyr ungroup lead slice_max pull coalesce distinct n
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid
.destination_legacy <- function(detected,
                                traj_id,
                                docs_per_group,
                                last_year = NULL,
                                all_detected = NULL,
                                group = NULL) {
  # Input validation
  if (!is.list(detected) || is.null(detected$trajectories)) {
    stop("'detected' must be the output of detect_main_trajectories()", call. = FALSE)
  }
  if (!is.character(traj_id) || length(traj_id) != 1) {
    stop("'traj_id' must be a single character value", call. = FALSE)
  }
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!all(required_cols %in% names(docs_per_group))) {
    stop(
      "'docs_per_group' must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Resolve the requested trajectory and its terminal (latest-year) node
  traj_row <- detected$trajectories |> dplyr::filter(.data$traj_id == !!traj_id)
  if (nrow(traj_row) == 0) {
    stop("Trajectory '", traj_id, "' not found in detected$trajectories", call. = FALSE)
  }
  nodes <- traj_row$nodes[[1]]
  terminal_node <- nodes[which.max(.extract_year(nodes))]
  terminal_year <- .extract_year(terminal_node)

  if (is.null(last_year)) {
    last_year <- max(docs_per_group$network_until, na.rm = TRUE)
  }

  # Cohort: full membership of the terminal node
  cohort <- docs_per_group |>
    dplyr::filter(.data$group_id == terminal_node) |>
    dplyr::pull(.data$document_id) |>
    unique()
  cohort_size <- length(cohort)

  # Where the cohort lands in the final year (label-independent, by document)
  final_membership <- docs_per_group |>
    dplyr::filter(.data$network_until == last_year) |>
    dplyr::distinct(.data$document_id, g_final = .data$group)

  cohort_final <- tibble::tibble(document_id = cohort) |>
    dplyr::left_join(final_membership, by = "document_id") |>
    dplyr::mutate(g_final = dplyr::coalesce(.data$g_final, "(dropped)"))

  destination <- cohort_final |>
    dplyr::count(.data$g_final, name = "n") |>
    dplyr::mutate(prop = .data$n / sum(.data$n)) |>
    dplyr::arrange(dplyr::desc(.data$n), .data$g_final == "(dropped)", .data$g_final)

  dormant_share <- sum(cohort_final$g_final == "(dropped)") / cohort_size

  continuation <- destination |>
    dplyr::filter(.data$g_final != "(dropped)")
  continuation <- if (nrow(continuation) == 0) {
    NA_character_
  } else {
    continuation |>
      dplyr::slice_max(.data$n, n = 1, with_ties = FALSE) |>
      dplyr::pull(.data$g_final)
  }

  # Per-year flow: complete grid (every cohort doc x every year) so that papers
  # falling below the minimum group size are routed to a "(dropped)" node
  # instead of vanishing from the diagram.
  years <- terminal_year:last_year

  memb <- docs_per_group |>
    dplyr::filter(
      .data$document_id %in% cohort,
      .data$network_until >= terminal_year,
      .data$network_until <= last_year
    ) |>
    dplyr::distinct(.data$document_id, year = .data$network_until, group = .data$group)

  # Trajectory-level attribution (optional)
  assign_tbl <- NULL
  if (!is.null(all_detected)) {
    assign_tbl <- .assign_destination_trajectory(
      cohort, terminal_year, all_detected, docs_per_group
    )
  }

  per_doc <- tidyr::expand_grid(document_id = cohort, year = years) |>
    dplyr::left_join(memb, by = c("document_id", "year")) |>
    dplyr::mutate(
      group = dplyr::coalesce(.data$group, "(dropped)"),
      id = paste0("y", .data$year, .data$group)
    ) |>
    dplyr::left_join(
      cohort_final |> dplyr::select(.data$document_id, final_group = .data$g_final),
      by = "document_id"
    ) |>
    dplyr::arrange(.data$document_id, .data$year) |>
    dplyr::group_by(.data$document_id) |>
    dplyr::mutate(
      to_group = dplyr::lead(.data$group),
      to_id = dplyr::lead(.data$id),
      to_year = dplyr::lead(.data$year)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$to_year))

  if (is.null(assign_tbl)) {
    flow <- per_doc |>
      dplyr::count(
        from_id = .data$id, to_id = .data$to_id,
        from_year = .data$year, to_year = .data$to_year,
        from_group = .data$group, to_group = .data$to_group,
        final_group = .data$final_group, name = "n"
      )
  } else {
    flow <- per_doc |>
      dplyr::left_join(
        dplyr::select(assign_tbl, .data$document_id, dest_traj = .data$dest_traj_key),
        by = "document_id"
      ) |>
      dplyr::count(
        from_id = .data$id, to_id = .data$to_id,
        from_year = .data$year, to_year = .data$to_year,
        from_group = .data$group, to_group = .data$to_group,
        final_group = .data$final_group, dest_traj = .data$dest_traj, name = "n"
      )
  }

  # Source trajectory descriptor (always available, for labelling)
  source_info <- list(
    group = group, traj_id = traj_id,
    start = min(.extract_year(nodes)), end = terminal_year,
    cohort_size = cohort_size
  )

  # Trajectory-level destination summary
  destination_traj <- NULL
  continuation_traj <- NA_character_
  continuation_info <- NULL
  if (!is.null(assign_tbl)) {
    destination_traj <- assign_tbl |>
      dplyr::count(
        traj_key = .data$dest_traj_key, group = .data$dest_group,
        traj_id = .data$dest_traj_id, name = "n"
      ) |>
      dplyr::mutate(prop = .data$n / sum(.data$n)) |>
      dplyr::arrange(
        dplyr::desc(.data$n), .data$traj_key == "(none)", .data$traj_key
      )

    real_traj <- dplyr::filter(destination_traj, .data$traj_key != "(none)")
    if (nrow(real_traj) > 0) {
      top <- dplyr::slice_max(real_traj, .data$n, n = 1, with_ties = FALSE)
      continuation_traj <- top$traj_key
      drow <- all_detected[[top$group]]$trajectories
      drow <- drow[drow$traj_id == top$traj_id, ]
      continuation_info <- list(
        group = top$group, traj_id = top$traj_id,
        start = if (!is.null(drow$start)) drow$start[1] else min(.extract_year(drow$nodes[[1]])),
        end = if (!is.null(drow$end)) drow$end[1] else max(.extract_year(drow$nodes[[1]])),
        n_papers = top$n
      )
    }
  }

  list(
    terminal_node = terminal_node,
    cohort_size = cohort_size,
    destination = destination,
    dormant_share = dormant_share,
    continuation = continuation,
    flow = flow,
    destination_traj = destination_traj,
    continuation_traj = continuation_traj,
    continuation_info = continuation_info,
    source_info = source_info
  )
}

#' Attribute cohort papers to the trajectory that carries them forward
#'
#' For each cohort paper, finds the trajectory (across all groups) whose nodes
#' contain it in the most years after the death year. The source trajectory,
#' whose nodes all fall on or before the death year, is excluded automatically.
#'
#' @param cohort Character vector of document ids.
#' @param terminal_year Death year of the source trajectory.
#' @param all_detected Named list of [detect_main_trajectories()] outputs.
#' @param docs_per_group Per-year membership tibble.
#' @param exclude_key Optional trajectory key(s) (`"group::traj_id"`) to skip.
#' @return Tibble `document_id`, `dest_traj_key`, `dest_group`, `dest_traj_id`,
#'   with `"(none)"` for papers that no trajectory carries past the death year.
#' @keywords internal
#' @importFrom dplyr filter distinct inner_join group_by summarise count
#' @importFrom dplyr left_join arrange desc slice_head ungroup select bind_rows
#' @importFrom tibble tibble
.assign_destination_trajectory <- function(cohort, terminal_year, all_detected,
                                          docs_per_group, exclude_key = NULL) {
  cand_list <- list()
  for (g in names(all_detected)) {
    trs <- all_detected[[g]]$trajectories
    for (i in seq_len(nrow(trs))) {
      key <- paste0(g, "::", trs$traj_id[i])
      if (!is.null(exclude_key) && key %in% exclude_key) next
      cand_list[[length(cand_list) + 1]] <- tibble::tibble(
        traj_key = key, group = g, traj_id = trs$traj_id[i],
        node_id = trs$nodes[[i]]
      )
    }
  }
  cand <- dplyr::bind_rows(cand_list)

  memb <- docs_per_group |>
    dplyr::filter(
      .data$document_id %in% cohort,
      .data$network_until > terminal_year
    ) |>
    dplyr::distinct(.data$document_id, node_id = .data$group_id, year = .data$network_until)

  traj_end <- cand |>
    dplyr::group_by(.data$traj_key) |>
    dplyr::summarise(traj_end = max(.extract_year(.data$node_id)), .groups = "drop")

  assigned <- cand |>
    dplyr::inner_join(memb, by = "node_id", relationship = "many-to-many") |>
    dplyr::distinct(.data$document_id, .data$traj_key, .data$group, .data$traj_id, .data$year) |>
    dplyr::count(.data$document_id, .data$traj_key, .data$group, .data$traj_id, name = "years") |>
    dplyr::left_join(traj_end, by = "traj_key") |>
    dplyr::group_by(.data$document_id) |>
    dplyr::arrange(
      dplyr::desc(.data$years), dplyr::desc(.data$traj_end), .data$traj_key,
      .by_group = TRUE
    ) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(
      .data$document_id, dest_traj_key = .data$traj_key,
      dest_group = .data$group, dest_traj_id = .data$traj_id
    )

  none_docs <- setdiff(cohort, assigned$document_id)
  if (length(none_docs) > 0) {
    assigned <- dplyr::bind_rows(
      assigned,
      tibble::tibble(
        document_id = none_docs, dest_traj_key = "(none)",
        dest_group = NA_character_, dest_traj_id = NA_character_
      )
    )
  }
  assigned
}
