#' Precompute the formation of every trajectory in one pass
#'
#' Batch version of [sniff_trajectory_formation()]. A single
#' [sniff_trajectory_formation()] call already walks every trajectory that
#' stops, so collecting the feeders of *all* targets at once costs essentially
#' the same. Use it to precompute results offline (e.g. for a Shiny app) so the
#' server only loads and plots, never recomputes.
#'
#' @param all_detected Named list of [detect_main_trajectories()] outputs, one
#'   per group, keyed by group id.
#' @param docs_per_group Per-year membership tibble, as in
#'   `groups_cumulative_trajectories$docs_per_group` (columns `group_id`,
#'   `document_id`, `network_until`, `group`).
#' @param last_year Final year (default: the maximum `network_until`).
#'
#' @return A named list keyed by target trajectory (`"group::traj_id"`), one
#'   entry per target that has at least one feeder. Each entry has the same shape
#'   as [sniff_trajectory_formation()] output (`target`, `target_info`,
#'   `feeders`, `total_inflow`, `last_year`), with all feeders kept
#'   (`kept = TRUE`); re-apply thresholds downstream by overwriting `kept` before
#'   [plot_trajectory_formation_2d()].
#'
#' @examples
#' \dontrun{
#' fs <- sniff_trajectory_formations(
#'   groups_detected_trajectories,
#'   groups_cumulative_trajectories$docs_per_group
#' )
#' saveRDS(fs, "rawfiles/trajectory_formations.rds")
#' plot_trajectory_formation_2d(fs[["c1g10::tr1"]])
#' }
#'
#' @seealso [sniff_trajectory_formation()], [sniff_trajectory_self_sufficiency()]
#'
#' @importFrom dplyr count arrange desc bind_rows
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
sniff_trajectory_formations <- function(all_detected, docs_per_group,
                                        last_year = NULL) {
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

  node_size <- .node_size_lookup(docs_per_group)

  # One assignment pass over every stopped trajectory; keep all (source, dest)
  # document assignments.
  rows <- list()
  growth_map <- list()   # source_key -> size_curve (depends only on source)
  nodes_map <- list()    # source_key -> its trajectory node names
  inflow_docs <- list()  # "dest||source" -> contributed document ids
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
      sk <- paste0(g, "::", trs$traj_id[i])
      if (is.null(growth_map[[sk]])) {
        growth_map[[sk]] <- .feeder_growth_series(nodes, node_size)
        nodes_map[[sk]] <- nodes
      }
      for (dk in unique(a$dest_traj_key)) {
        inflow_docs[[paste0(dk, "||", sk)]] <- a$document_id[a$dest_traj_key == dk]
      }
      rows[[length(rows) + 1]] <- tibble::tibble(
        dest_key = a$dest_traj_key, source_key = sk,
        source_group = g, source_traj = trs$traj_id[i],
        source_start = min(yrs), handoff_year = terminal_year,
        cohort_size = length(cohort)
      )
    }
  }
  if (length(rows) == 0) return(list())

  feeders_all <- dplyr::bind_rows(rows) |>
    dplyr::count(
      .data$dest_key, .data$source_key, .data$source_group, .data$source_traj,
      .data$source_start, .data$handoff_year, .data$cohort_size, name = "n"
    )

  target_info_of <- function(key) {
    parts <- strsplit(key, "::", fixed = TRUE)[[1]]
    tr <- all_detected[[parts[1]]]$trajectories
    nodes <- tr$nodes[[which(tr$traj_id == parts[2])[1]]]
    tterm <- nodes[which.max(.extract_year(nodes))]
    tsize <- as.integer(node_size[tterm])
    if (is.na(tsize)) tsize <- 0L
    list(group = parts[1], traj_id = parts[2],
         start = min(.extract_year(nodes)), end = max(.extract_year(nodes)),
         size = tsize)
  }

  split_f <- split(feeders_all, feeders_all$dest_key)
  out <- lapply(names(split_f), function(key) {
    fd <- split_f[[key]]
    total <- sum(fd$n)
    feeders <- tibble::tibble(
      source_key = fd$source_key, source_group = fd$source_group,
      source_traj = fd$source_traj, start_year = fd$source_start,
      handoff_year = fd$handoff_year, cohort_size = fd$cohort_size, n = fd$n,
      prop_of_source = fd$n / fd$cohort_size, prop_of_inflow = fd$n / total,
      kept = TRUE,
      size_curve = growth_map[fd$source_key],
      inflow_curve = lapply(seq_len(nrow(fd)), function(r)
        .contributed_arrival_series(
          inflow_docs[[paste0(key, "||", fd$source_key[r])]],
          nodes_map[[fd$source_key[r]]], docs_per_group, fd$handoff_year[r]))
    ) |>
      dplyr::arrange(dplyr::desc(.data$n), .data$source_key)
    list(target = key, target_info = target_info_of(key), feeders = feeders,
         total_inflow = total, last_year = last_year)
  })
  names(out) <- names(split_f)
  out
}
