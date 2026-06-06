#' Self-sufficiency of each central trajectory
#'
#' Dispatches on `x`. For a [sniff_trajectory_flow()] object, returns each central
#' trajectory's endogenous fraction: the share of its spine papers NOT also carried by
#' any of its absorbed tributaries (1 = fully endogenous, ~0 = a confluence of
#' tributaries).
#'
#' @param x A [sniff_trajectory_flow()] object, or a legacy detected named list.
#' @param ... For the flow object: `min_size` (default 30). For legacy:
#'   `docs_per_group, min_size, ...`.
#' @return A tibble (`central`, `group`, `size`, `inflow`, `self_sufficiency`),
#'   sorted by descending `self_sufficiency`.
#' @seealso [sniff_trajectory_flow()], [sniff_trajectory_destination()]
#' @export
sniff_trajectory_self_sufficiency <- function(x, ...) {
  if (is.list(x) && !is.data.frame(x) && !is.null(x$trajectories) &&
      is.data.frame(x$trajectories) && "absorbed_into" %in% names(x$trajectories)) {
    .self_sufficiency_from_flow(x, ...)
  } else {
    .self_sufficiency_legacy(x, ...)
  }
}

#' @keywords internal
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
.self_sufficiency_from_flow <- function(flow, min_size = 30) {
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  node_docs <- split(dpg$document_id, dpg$group_id)
  centrals <- tr[tr$type == "central", , drop = FALSE]

  rows <- vector("list", nrow(centrals))
  for (i in seq_len(nrow(centrals))) {
    g <- centrals$group[i]
    c_docs <- unique(unlist(node_docs[centrals$nodes[[i]]], use.names = FALSE))
    trib <- tr[tr$type == "absorbed" & !is.na(tr$group) & tr$group == g, , drop = FALSE]
    trib_docs <- if (nrow(trib) > 0) {
      unique(unlist(node_docs[unlist(trib$nodes, use.names = FALSE)], use.names = FALSE))
    } else character(0)
    inflow <- length(intersect(c_docs, trib_docs))
    rows[[i]] <- tibble::tibble(
      central = centrals$traj_id[i], group = g, size = length(c_docs),
      inflow = inflow, self_sufficiency = 1 - inflow / length(c_docs)
    )
  }
  ss <- dplyr::bind_rows(rows)
  ss <- ss[ss$size >= min_size, , drop = FALSE]
  dplyr::arrange(ss, dplyr::desc(.data$self_sufficiency))
}

#' Self-sufficiency of each trajectory (endogenous growth vs absorption) (legacy)
#'
#' Ranks trajectories by how much they grew on their own versus by absorbing
#' papers handed off from trajectories in *other* groups. For every trajectory it
#' compares its size (unique papers across its nodes) with the unique papers it
#' imported from external feeders. It is the cross-trajectory aggregate of
#' [sniff_trajectory_formation()]: self-sufficiency near 1 means the community
#' formed endogenously; near 0 means it is largely a confluence of other
#' communities.
#'
#' Imported papers are counted as a unique set per trajectory (not as a sum of
#' handoff weights), so a paper absorbed via several feeders is counted once and
#' the index always lies in `[0, 1]`. Feeders from the *same* group (a group
#' consolidating its own sub-lineages) count as internal, not as external
#' dependence.
#'
#' Secondary trajectories that stop early often receive no inflow and so score a
#' trivial 1. The meaningful comparison is usually among the surviving
#' trajectories: filter the result on `living`.
#'
#' @param all_detected Named list of [detect_main_trajectories()] outputs, one
#'   per group, keyed by group id.
#' @param docs_per_group Per-year membership tibble, as in
#'   `groups_cumulative_trajectories$docs_per_group` (columns `group_id`,
#'   `document_id`, `network_until`, `group`).
#' @param last_year Final year (default: the maximum `network_until`).
#'   Trajectories reaching it are flagged `living`.
#' @param min_size Drop trajectories with fewer than this many papers (default:
#'   1), removing tiny spurs whose index is not meaningful.
#'
#' @return A tibble, one row per trajectory, sorted by descending
#'   `self_sufficiency`: `key`, `group`, `traj_id`, `size`, `inflow_internal`,
#'   `inflow_external`, `self_sufficiency` (= 1 - inflow_external / size),
#'   `living`.
#'
#' @keywords internal
#'
#' @importFrom dplyr filter mutate arrange desc bind_rows group_by summarise
#' @importFrom dplyr left_join n_distinct
#' @importFrom tibble tibble
#' @importFrom rlang .data
.self_sufficiency_legacy <- function(all_detected,
                                     docs_per_group,
                                     last_year = NULL,
                                     min_size = 1L) {
  if (!is.list(all_detected) || is.null(names(all_detected))) {
    stop("'all_detected' must be a named list of detect_main_trajectories() outputs", call. = FALSE)
  }
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!all(required_cols %in% names(docs_per_group))) {
    stop("'docs_per_group' must contain columns: ",
         paste(required_cols, collapse = ", "), call. = FALSE)
  }
  if (is.null(last_year)) {
    last_year <- max(docs_per_group$network_until, na.rm = TRUE)
  }

  # Size and living flag for every trajectory
  meta_list <- list()
  for (g in names(all_detected)) {
    trs <- all_detected[[g]]$trajectories
    for (i in seq_len(nrow(trs))) {
      nodes <- trs$nodes[[i]]
      yrs <- .extract_year(nodes)
      docs <- unique(docs_per_group$document_id[docs_per_group$group_id %in% nodes])
      meta_list[[length(meta_list) + 1]] <- tibble::tibble(
        key = paste0(g, "::", trs$traj_id[i]), group = g,
        traj_id = trs$traj_id[i], size = length(docs),
        living = max(yrs) >= last_year
      )
    }
  }
  meta <- dplyr::bind_rows(meta_list)

  # For every stopped trajectory, which papers it hands off and to which
  # trajectory (dominant carrier). Collected as (document, dest, source group).
  assign_list <- list()
  for (g in names(all_detected)) {
    trs <- all_detected[[g]]$trajectories
    for (i in seq_len(nrow(trs))) {
      nodes <- trs$nodes[[i]]
      yrs <- .extract_year(nodes)
      terminal_year <- max(yrs)
      if (terminal_year >= last_year) next
      terminal_node <- nodes[which.max(yrs)]
      cohort <- unique(docs_per_group$document_id[docs_per_group$group_id == terminal_node])
      if (length(cohort) == 0) next
      a <- .assign_destination_trajectory(cohort, terminal_year, all_detected, docs_per_group)
      a <- a[a$dest_traj_key != "(none)", , drop = FALSE]
      if (nrow(a) == 0) next
      assign_list[[length(assign_list) + 1]] <- tibble::tibble(
        document_id = a$document_id, dest_key = a$dest_traj_key,
        dest_group = a$dest_group, source_group = g
      )
    }
  }

  if (length(assign_list) == 0) {
    inflow <- tibble::tibble(
      dest_key = character(), inflow_internal = integer(), inflow_external = integer()
    )
  } else {
    inflow <- dplyr::bind_rows(assign_list) |>
      dplyr::mutate(external = .data$source_group != .data$dest_group) |>
      dplyr::group_by(.data$dest_key) |>
      dplyr::summarise(
        inflow_internal = dplyr::n_distinct(.data$document_id[!.data$external]),
        inflow_external = dplyr::n_distinct(.data$document_id[.data$external]),
        .groups = "drop"
      )
  }

  meta |>
    dplyr::left_join(inflow, by = c("key" = "dest_key")) |>
    dplyr::mutate(
      inflow_internal = ifelse(is.na(.data$inflow_internal), 0L, .data$inflow_internal),
      inflow_external = ifelse(is.na(.data$inflow_external), 0L, .data$inflow_external),
      self_sufficiency = ifelse(
        .data$size > 0, 1 - .data$inflow_external / .data$size, NA_real_
      )
    ) |>
    dplyr::filter(.data$size >= min_size) |>
    dplyr::arrange(dplyr::desc(.data$self_sufficiency), dplyr::desc(.data$size))
}
