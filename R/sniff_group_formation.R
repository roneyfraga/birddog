#' How a final group was formed (group <- trajectories confluence)
#'
#' The inverse of [sniff_trajectory_group_contribution()] read as a river: for one
#' final `group`, the birth-lineages that terminate in it (its backbone), each
#' drawn as a *pre-merge tributary* — the independent segment of the lineage
#' (nodes before its `merge_year`) that joins the group's shared backbone at the
#' merge. Because nodes shared by two or more trajectories are post-merge for all
#' of them, the pre-merge segments are disjoint, so tributaries partition the
#' group's papers and the confluence sums to the group. The returned object has the
#' same shape as [sniff_trajectory_formation()], so [plot_trajectory_formation_2d()]
#' / [plot_trajectory_formation_3d()] (and the [plot_group_formation_2d()] wrappers)
#' render it directly, with the group as the spine.
#'
#' @param group A single final-group label (e.g. `"c1g1"`).
#' @param x A [detect_soft_trajectories()] object, or a `docs_per_group` tibble /
#'   [sniff_groups_trajectories()] object (the DAG and trajectories are then built).
#' @param docs_per_group Per-year membership tibble. Defaults to
#'   `x$docs_per_group` when `x` is a detected object.
#' @param min_papers Minimum papers a tributary must bring to be kept (default 5).
#' @param min_prop Minimum share of the tributary's own pre-merge cohort that must
#'   reach the group to be kept (default 0.05).
#' @param last_year Final year (default: max `network_until`).
#'
#' @return A list with `target` (the group), `target_info`
#'   (`group`, `traj_id = "all"`, `start`, `size = |final(group)|`), `feeders`
#'   (one row per tributary, sorted by descending papers: `source_key` (the
#'   `traj_id`), `source_group`, `start_year`, `handoff_year` (the merge year),
#'   `cohort_size` (pre-merge papers), `n` (pre-merge papers reaching the group),
#'   `prop_of_source`, `prop_of_inflow`, `kept`, and the `size_curve` /
#'   `inflow_curve` list-columns), `total_inflow`, and `last_year`.
#'
#' @seealso [sniff_trajectory_group_contribution()], [plot_group_formation_2d()],
#'   [plot_trajectory_formation_2d()]
#'
#' @export
#' @importFrom dplyr bind_rows mutate arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang .data
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
  trs <- detected$trajectories
  fset <- trs[trs$terminal_group == group, , drop = FALSE]
  if (nrow(fset) == 0) {
    stop("no trajectory terminates in group '", group,
         "'. Terminal groups present: ",
         paste(sort(unique(trs$terminal_group)), collapse = ", "), call. = FALSE)
  }
  if (is.null(last_year)) last_year <- max(docs_per_group$network_until, na.rm = TRUE)

  node_size <- .node_size_lookup(docs_per_group)
  node_docs <- split(docs_per_group$document_id, docs_per_group$group_id)
  final_id <- paste0("y", last_year, group)
  g_final <- unique(docs_per_group$document_id[docs_per_group$group_id == final_id])

  feeders_list <- list()
  for (i in seq_len(nrow(fset))) {
    nodes <- fset$nodes[[i]]
    my <- fset$merge_year[i]
    pre <- if (is.na(my)) nodes else nodes[.extract_year(nodes) < my]
    if (length(pre) == 0) pre <- nodes[which.min(.extract_year(nodes))]
    handoff <- if (is.na(my)) fset$end[i] else my
    docs <- unique(unlist(node_docs[pre], use.names = FALSE))
    contrib <- intersect(g_final, docs)
    n <- length(contrib)
    if (n == 0) next
    feeders_list[[length(feeders_list) + 1]] <- tibble::tibble(
      source_key = fset$traj_id[i],
      source_group = group,
      start_year = fset$start[i],
      handoff_year = as.integer(handoff),
      cohort_size = length(docs),
      n = n,
      size_curve = list(.feeder_growth_series(pre, node_size)),
      inflow_curve = list(.contributed_arrival_series(contrib, pre, docs_per_group, handoff))
    )
  }

  if (length(feeders_list) == 0) {
    feeders <- tibble::tibble(
      source_key = character(), source_group = character(),
      start_year = integer(), handoff_year = integer(),
      cohort_size = integer(), n = integer(),
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

  target_info <- list(
    group = group, traj_id = "all",
    start = if (nrow(feeders)) min(feeders$start_year) else last_year,
    size = length(g_final)
  )
  list(target = group, target_info = target_info, feeders = feeders,
       total_inflow = total_inflow, last_year = last_year)
}
