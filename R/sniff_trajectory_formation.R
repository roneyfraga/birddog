#' Trajectories that fed into a target (confluence)
#'
#' Dispatches on `x`: a [sniff_trajectory_flow()] object returns the target
#' trajectory's direct tributaries (its children in the confluence tree); a legacy
#' `"group::traj_id"` string keeps the per-group behavior. The result has the same
#' shape in both cases, so [plot_trajectory_formation_2d()] renders it.
#'
#' @param x A [sniff_trajectory_flow()] object, or a legacy target key string.
#' @param ... For the flow object: `target` (a `traj_id`), then `min_papers`,
#'   `min_prop`. For the legacy string: `all_detected`, `docs_per_group`, ... .
#' @return A `formation` list (`target`, `target_info`, `feeders`, `total_inflow`,
#'   `last_year`).
#' @seealso [sniff_trajectory_flow()], [plot_trajectory_formation_2d()]
#' @export
sniff_trajectory_formation <- function(x, ...) {
  if (is.list(x) && !is.data.frame(x) && !is.null(x$trajectories) &&
      is.data.frame(x$trajectories) && "absorbed_into" %in% names(x$trajectories)) {
    .formation_from_flow(x, ...)
  } else {
    .formation_legacy(x, ...)
  }
}

#' Track which trajectories fed into a given trajectory (legacy per-group version)
#'
#' The inverse of [sniff_trajectory_destination()]. Instead of following one
#' dying trajectory forward to where its papers go, this takes a target
#' trajectory and finds every other trajectory whose terminal cohort was
#' absorbed into it. In the river analogy, [sniff_trajectory_destination()]
#' follows a tributary downstream; this stands at the main river and lists the
#' tributaries that formed it.
#'
#' A source trajectory is a *direct feeder* of `target` when, after the source
#' stops (its terminal year), the dominant carrier of some of its terminal
#' cohort papers is `target`. Attribution uses the same dominant-assignment
#' rule as [sniff_trajectory_destination()], so feeders are the exact inverse of
#' destinations: if `sniff_trajectory_destination()` reports source S handing
#' off to `target`, then S appears here as a feeder of `target`.
#'
#' Group labels (`c1g1`, ...) are size ranks recomputed every cumulative year,
#' so feeders are resolved by document identity, never by label.
#'
#' @param target Trajectory key `"group::traj_id"` to find the feeders of (e.g.
#'   `"c1g10::tr1"`).
#' @param all_detected Named list of [detect_main_trajectories()] outputs, one
#'   per group, keyed by group id (the same object passed as `all_detected` to
#'   [sniff_trajectory_destination()]).
#' @param docs_per_group A tibble mapping documents to group-year nodes, as in
#'   `groups_cumulative_trajectories$docs_per_group` (columns `group_id`,
#'   `document_id`, `network_until`, `group`).
#' @param last_year Final year of the analysis (default: the maximum
#'   `network_until` in `docs_per_group`). Trajectories reaching `last_year` are
#'   living and cannot be feeders.
#' @param min_papers Minimum papers a source must hand into `target` to be kept
#'   (default: 5). Smaller flows are still returned with `kept = FALSE`.
#' @param min_prop Minimum share of the source's own terminal cohort that must
#'   flow into `target` to be kept (default: 0.05). Guards against counting
#'   incidental overlaps as real tributaries.
#'
#' @return A list with:
#' \describe{
#'   \item{target}{The target trajectory key.}
#'   \item{target_info}{List `group`, `traj_id`, `start`, `end`, and `size` (the
#'     target trajectory's own paper count, i.e. its terminal node size).}
#'   \item{feeders}{Tibble, one row per feeder, sorted by descending papers:
#'     `source_key`, `source_group`, `source_traj`, `start_year`,
#'     `handoff_year` (the source's terminal year), `cohort_size`, `n` (papers
#'     into `target`), `prop_of_source`, `prop_of_inflow`, `kept`, and two
#'     list-columns of `year`/`size` tibbles: `size_curve` (the feeder's
#'     measured cluster size per year, ending at `cohort_size`) and
#'     `inflow_curve` (the cumulative arrival of just the `n` contributed
#'     papers as they join the feeder's own year-nodes, within the feeder's
#'     lifespan, ending at `n` in `handoff_year`).}
#'   \item{total_inflow}{Total papers absorbed into `target` across all
#'     feeders.}
#'   \item{last_year}{The resolved final year.}
#' }
#'
#' @keywords internal
#'
#' @importFrom dplyr filter pull mutate arrange desc bind_rows
#' @importFrom tibble tibble
#' @importFrom rlang .data
.formation_legacy <- function(target,
                              all_detected,
                              docs_per_group,
                              last_year = NULL,
                              min_papers = 5,
                              min_prop = 0.05) {
  # Input validation
  if (!is.character(target) || length(target) != 1 || !grepl("::", target, fixed = TRUE)) {
    stop("'target' must be a single 'group::traj_id' string (e.g. 'c1g10::tr1')",
         call. = FALSE)
  }
  if (!is.list(all_detected) || is.null(names(all_detected))) {
    stop("'all_detected' must be a named list of detect_main_trajectories() outputs",
         call. = FALSE)
  }
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!all(required_cols %in% names(docs_per_group))) {
    stop("'docs_per_group' must contain columns: ",
         paste(required_cols, collapse = ", "), call. = FALSE)
  }

  node_size <- .node_size_lookup(docs_per_group)

  parts <- strsplit(target, "::", fixed = TRUE)[[1]]
  target_group <- parts[1]
  target_traj <- parts[2]

  if (is.null(all_detected[[target_group]])) {
    stop("Target group '", target_group, "' not found in all_detected", call. = FALSE)
  }
  ttrajs <- all_detected[[target_group]]$trajectories
  trow <- ttrajs[ttrajs$traj_id == target_traj, ]
  if (nrow(trow) == 0) {
    stop("Target trajectory '", target, "' not found in all_detected", call. = FALSE)
  }
  tnodes <- trow$nodes[[1]]
  tterm <- tnodes[which.max(.extract_year(tnodes))]
  target_size <- as.integer(node_size[tterm])
  if (is.na(target_size)) target_size <- 0L
  target_info <- list(
    group = target_group, traj_id = target_traj,
    start = min(.extract_year(tnodes)), end = max(.extract_year(tnodes)),
    size = target_size
  )

  if (is.null(last_year)) {
    last_year <- max(docs_per_group$network_until, na.rm = TRUE)
  }

  # Walk every candidate source trajectory; keep those whose terminal cohort is
  # dominantly carried into the target after the source's death year.
  feeders_list <- list()
  for (g in names(all_detected)) {
    trs <- all_detected[[g]]$trajectories
    for (i in seq_len(nrow(trs))) {
      tr <- trs$traj_id[i]
      key <- paste0(g, "::", tr)
      if (key == target) next

      nodes <- trs$nodes[[i]]
      yrs <- .extract_year(nodes)
      terminal_year <- max(yrs)
      if (terminal_year >= last_year) next   # living trajectory, not a feeder
      terminal_node <- nodes[which.max(yrs)]

      cohort <- docs_per_group |>
        dplyr::filter(.data$group_id == terminal_node) |>
        dplyr::pull(.data$document_id) |>
        unique()
      if (length(cohort) == 0) next

      assign_tbl <- .assign_destination_trajectory(
        cohort, terminal_year, all_detected, docs_per_group
      )
      contrib_docs <- assign_tbl$document_id[assign_tbl$dest_traj_key == target]
      n_to_target <- length(contrib_docs)
      if (n_to_target == 0) next

      feeders_list[[length(feeders_list) + 1]] <- tibble::tibble(
        source_key = key, source_group = g, source_traj = tr,
        start_year = min(yrs), handoff_year = terminal_year,
        cohort_size = length(cohort), n = n_to_target,
        size_curve = list(.feeder_growth_series(nodes, node_size)),
        inflow_curve = list(.contributed_arrival_series(
          contrib_docs, nodes, docs_per_group, terminal_year))
      )
    }
  }

  if (length(feeders_list) == 0) {
    feeders <- tibble::tibble(
      source_key = character(), source_group = character(),
      source_traj = character(), start_year = integer(),
      handoff_year = integer(), cohort_size = integer(), n = integer(),
      prop_of_source = numeric(), prop_of_inflow = numeric(), kept = logical(),
      size_curve = list(), inflow_curve = list()
    )
    total_inflow <- 0L
  } else {
    feeders <- dplyr::bind_rows(feeders_list)
    total_inflow <- sum(feeders$n)
    feeders <- feeders |>
      dplyr::mutate(
        prop_of_source = .data$n / .data$cohort_size,
        prop_of_inflow = .data$n / total_inflow,
        kept = .data$n >= min_papers & .data$prop_of_source >= min_prop
      ) |>
      dplyr::arrange(dplyr::desc(.data$n), .data$source_key)
  }

  list(
    target = target,
    target_info = target_info,
    feeders = feeders,
    total_inflow = total_inflow,
    last_year = last_year
  )
}

#' Formation of a trajectory from the flow tree (its direct tributaries)
#' @keywords internal
#' @importFrom dplyr bind_rows mutate arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
.formation_from_flow <- function(flow, target, min_papers = 5, min_prop = 0.05) {
  tr <- flow$trajectories
  if (!is.character(target) || length(target) != 1 || !(target %in% tr$traj_id)) {
    stop("'target' must be a single trajectory id present in the flow object",
         call. = FALSE)
  }
  trow <- tr[tr$traj_id == target, ]
  dpg <- flow$docs_per_group
  last_year <- flow$last_year
  node_size <- .node_size_lookup(dpg)
  node_docs <- split(dpg$document_id, dpg$group_id)

  kids <- tr[!is.na(tr$absorbed_into) & tr$absorbed_into == target, , drop = FALSE]
  kids <- kids[order(kids$absorption_year, -kids$size, kids$traj_id), , drop = FALSE]

  empty_feeders <- tibble::tibble(
    source_key = character(), source_group = character(),
    start_year = integer(), handoff_year = integer(),
    cohort_size = integer(), n = integer(),
    prop_of_source = numeric(), prop_of_inflow = numeric(), kept = logical(),
    size_curve = list(), inflow_curve = list()
  )

  claimed <- character(0)
  rows <- list()
  for (i in seq_len(nrow(kids))) {
    f_nodes <- kids$nodes[[i]]
    f_docs <- unique(unlist(node_docs[f_nodes], use.names = FALSE))
    contrib <- setdiff(f_docs, claimed)
    if (length(contrib) == 0) next
    claimed <- c(claimed, contrib)
    handoff <- as.integer(kids$absorption_year[i])
    rows[[length(rows) + 1]] <- tibble::tibble(
      source_key = kids$traj_id[i], source_group = trow$group,
      start_year = kids$start[i], handoff_year = handoff,
      cohort_size = kids$size[i], n = length(contrib),
      size_curve = list(.feeder_growth_series(f_nodes, node_size)),
      inflow_curve = list(.contributed_arrival_series(contrib, f_nodes, dpg, handoff))
    )
  }

  if (length(rows) == 0) {
    feeders <- empty_feeders
    total_inflow <- 0L
  } else {
    feeders <- dplyr::bind_rows(rows)
    total_inflow <- sum(feeders$n)
    feeders <- feeders |>
      dplyr::mutate(
        prop_of_source = .data$n / .data$cohort_size,
        prop_of_inflow = .data$n / total_inflow,
        kept = .data$n >= min_papers & .data$prop_of_source >= min_prop
      ) |>
      dplyr::arrange(dplyr::desc(.data$n), .data$source_key)
  }

  target_info <- list(group = NA_character_, traj_id = target,
                      start = trow$start, size = trow$size)
  list(target = target, target_info = target_info, feeders = feeders,
       total_inflow = total_inflow, last_year = last_year)
}
