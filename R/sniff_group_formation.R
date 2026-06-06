#' How a final group was formed (group <- trajectories confluence)
#'
#' For one final `group`, attributes each of its final papers to the trajectory
#' that carried it *earliest* (earliest-carrier attribution). This covers all final
#' groups, including crossroads groups that no trajectory terminates in. Each paper
#' is assigned to exactly one trajectory (the earliest carrier; ties broken by
#' trajectory birth year then id), so the feeders partition the group's papers with
#' no double count. The returned object has the same shape as
#' [sniff_trajectory_formation()], so [plot_trajectory_formation_2d()] /
#' [plot_trajectory_formation_3d()] (and the [plot_group_formation_2d()] wrappers)
#' render it directly, with the group as the spine.
#'
#' @param group A single final-group label (e.g. `"c1g1"`).
#' @param x A [detect_soft_trajectories()] object, or a `docs_per_group` tibble /
#'   [sniff_groups_trajectories()] object (the DAG and trajectories are then built).
#' @param docs_per_group Per-year membership tibble. Defaults to
#'   `x$docs_per_group` when `x` is a detected object.
#' @param min_papers Minimum papers a trajectory must bring to be kept (default 5).
#' @param min_prop Minimum share of the trajectory's own cohort that must reach the
#'   group to be kept (default 0.05).
#' @param last_year Final year (default: max `network_until`).
#'
#' @return A list with `target` (the group), `target_info`
#'   (`group`, `traj_id = "all"`, `start`, `size = |final(group)|`), `feeders`
#'   (one row per contributing trajectory, sorted by descending papers:
#'   `source_key` (the `traj_id`), `source_group`, `start_year`,
#'   `handoff_year` (median intro year for attributed papers), `cohort_size`
#'   (trajectory papers present in this group across all years), `n` (attributed
#'   papers), `prop_of_source`, `prop_of_inflow`, `kept`, and the `size_curve` /
#'   `inflow_curve` list-columns), `total_inflow`, and `last_year`.
#'
#' @seealso [sniff_trajectory_group_contribution()], [plot_group_formation_2d()],
#'   [plot_trajectory_formation_2d()]
#'
#' @export
#' @importFrom dplyr bind_rows arrange desc count distinct left_join
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom stats median setNames
sniff_group_formation <- function(group, x, docs_per_group = NULL,
                                  min_papers = 5, min_prop = 0.05,
                                  last_year = NULL) {
  detected <- if (is.list(x) && !is.data.frame(x) &&
                  !is.null(x$trajectories) && is.data.frame(x$trajectories) &&
                  "terminal_group" %in% names(x$trajectories)) {
    x
  } else {
    detect_soft_trajectories(x)
  }
  if (is.null(docs_per_group)) docs_per_group <- detected$docs_per_group
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!is.data.frame(docs_per_group) || !all(required_cols %in% names(docs_per_group))) {
    stop("'docs_per_group' must contain columns: ",
         paste(required_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single final-group label (e.g. 'c1g1')", call. = FALSE)
  }
  if (is.null(last_year)) last_year <- max(docs_per_group$network_until, na.rm = TRUE)

  final_id <- paste0("y", last_year, group)
  g_final <- unique(docs_per_group$document_id[docs_per_group$group_id == final_id])
  if (length(g_final) == 0) {
    stop("group '", group, "' has no final-year (", last_year,
         ") cluster; it is not a final group", call. = FALSE)
  }

  node_size <- .node_size_lookup(docs_per_group)
  trs <- detected$trajectories
  tr_nodes <- stats::setNames(trs$nodes, trs$traj_id)
  dpg_g <- docs_per_group[docs_per_group$document_id %in% g_final, , drop = FALSE]

  empty_feeders <- tibble::tibble(
    source_key = character(), source_group = character(),
    start_year = integer(), handoff_year = integer(),
    cohort_size = integer(), n = integer(),
    prop_of_source = numeric(), prop_of_inflow = numeric(), kept = logical(),
    size_curve = list(), inflow_curve = list()
  )

  # For each trajectory, the year each of `group`'s final papers first appears in
  # that trajectory's own nodes.
  intro_list <- list()
  for (i in seq_len(nrow(trs))) {
    sub <- dpg_g[dpg_g$group_id %in% trs$nodes[[i]], c("document_id", "network_until")]
    if (nrow(sub) == 0) next
    fy <- tapply(sub$network_until, sub$document_id, min)
    intro_list[[length(intro_list) + 1]] <- tibble::tibble(
      traj_id = trs$traj_id[i], traj_start = trs$start[i],
      document_id = names(fy), intro_year = as.integer(fy)
    )
  }

  if (length(intro_list) == 0) {
    feeders <- empty_feeders
    total_inflow <- 0L
  } else {
    intro <- dplyr::bind_rows(intro_list)
    cohort <- dplyr::count(
      dplyr::distinct(intro, .data$traj_id, .data$document_id),
      .data$traj_id, name = "cohort_size")
    # earliest carrier wins (tie: earliest-born lineage, then id) -> partition
    attrib <- dplyr::arrange(intro, .data$document_id, .data$intro_year,
                             .data$traj_start, .data$traj_id)
    attrib <- attrib[!duplicated(attrib$document_id), , drop = FALSE]
    total_inflow <- nrow(attrib)

    rows <- list()
    for (k in sort(unique(attrib$traj_id))) {
      a <- attrib[attrib$traj_id == k, , drop = FALSE]
      tab <- table(sort(a$intro_year))
      inflow <- tibble::tibble(year = as.integer(names(tab)),
                               size = as.integer(cumsum(tab)))
      rows[[length(rows) + 1]] <- tibble::tibble(
        source_key = k, source_group = group,
        start_year = as.integer(a$traj_start[1]),
        handoff_year = as.integer(round(stats::median(a$intro_year))),
        cohort_size = cohort$cohort_size[cohort$traj_id == k],
        n = nrow(a),
        size_curve = list(.feeder_growth_series(tr_nodes[[k]], node_size)),
        inflow_curve = list(inflow)
      )
    }
    feeders <- dplyr::bind_rows(rows)
    feeders$prop_of_source <- feeders$n / feeders$cohort_size
    feeders$prop_of_inflow <- feeders$n / total_inflow
    feeders$kept <- feeders$n >= min_papers & feeders$prop_of_source >= min_prop
    feeders <- dplyr::arrange(feeders, dplyr::desc(.data$n), .data$source_key)
  }

  target_info <- list(
    group = group, traj_id = "all",
    start = if (nrow(feeders)) min(feeders$start_year) else last_year,
    size = length(g_final)
  )
  list(target = group, target_info = target_info, feeders = feeders,
       total_inflow = total_inflow, last_year = last_year)
}
