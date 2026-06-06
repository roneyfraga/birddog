#' Growth phase and formation of each final group
#'
#' For each final-year group, builds its production curve (cumulative count of its
#' current members by their first-appearance year) and classifies the growth
#' `phase` (emergence / maturity / dormancy). The convergent-vs-divergent character
#' of a group is its `formation`, read from the bipartite `source_entropy` when a
#' [sniff_trajectory_group_contribution()] object is supplied (low normalized
#' entropy = convergent, high = divergent).
#'
#' @param docs_per_group Per-year membership tibble (`group_id`, `document_id`,
#'   `network_until`, `group`).
#' @param contribution Optional [sniff_trajectory_group_contribution()] output;
#'   when given, `source_entropy`, `n_sources`, and `formation` are attached.
#' @param thresholds A [default_state_thresholds()]-shaped list (default).
#' @param growth_window Production-curve points over which `recent_growth` is
#'   measured (default 3).
#' @param last_year Final year (default: max `network_until`).
#'
#' @return A tibble, one row per final group, sorted by descending `recent_growth`:
#'   `group`, `size`, `start`, `end`, `span`, `median_pub_year`, `recent_growth`,
#'   `phase`, and (with `contribution`) `source_entropy`, `n_sources`, `formation`,
#'   `group_role` (`"terminus"` when at least one trajectory terminates in the group,
#'   `"crossroads"` otherwise).
#'
#' @seealso [sniff_trajectory_dynamics()], [sniff_trajectory_group_contribution()]
#' @export
#' @importFrom dplyr bind_rows arrange desc left_join
#' @importFrom tibble tibble
#' @importFrom rlang .data
sniff_group_dynamics <- function(docs_per_group, contribution = NULL,
                                 thresholds = NULL, growth_window = 3,
                                 last_year = NULL) {
  required_cols <- c("group_id", "document_id", "network_until", "group")
  if (!is.data.frame(docs_per_group) || !all(required_cols %in% names(docs_per_group))) {
    stop("'docs_per_group' must contain columns: ",
         paste(required_cols, collapse = ", "), call. = FALSE)
  }
  if (is.null(thresholds)) thresholds <- default_state_thresholds()
  if (is.null(last_year)) last_year <- max(docs_per_group$network_until, na.rm = TRUE)

  doc_first <- tapply(docs_per_group$network_until, docs_per_group$document_id, min)
  fin <- docs_per_group[docs_per_group$network_until == last_year,
                        c("document_id", "group"), drop = FALSE]
  fin <- fin[!duplicated(fin$document_id), , drop = FALSE]
  final_groups <- mixed_sort(unique(fin$group))
  members <- split(fin$document_id, fin$group)

  rows <- vector("list", length(final_groups))
  for (j in seq_along(final_groups)) {
    g <- final_groups[j]
    fy <- as.integer(doc_first[members[[g]]])
    curve <- cumsum(table(fy))
    n <- length(curve)
    ref <- max(1L, n - growth_window)
    s_ref <- as.integer(curve[ref])
    s_end <- as.integer(curve[n])
    recent_growth <- if (s_ref > 0) (s_end - s_ref) / s_ref else NA_real_
    rows[[j]] <- tibble::tibble(
      group = g, size = length(fy), start = min(fy), end = last_year,
      span = last_year - min(fy) + 1L, median_pub_year = stats::median(fy),
      recent_growth = recent_growth,
      phase = .classify_growth_phase(
        recent_growth, thresholds$emergence_growth, thresholds$dormancy_growth)
    )
  }
  gd <- dplyr::bind_rows(rows)

  if (!is.null(contribution) && !is.null(contribution$groups)) {
    src <- contribution$groups[, c("group_final", "source_entropy", "n_sources")]
    names(src)[names(src) == "group_final"] <- "group"
    gd <- dplyr::left_join(gd, src, by = "group")
    norm_ent <- ifelse(gd$n_sources > 1, gd$source_entropy / log(gd$n_sources), 0)
    gd$formation <- ifelse(
      is.na(norm_ent) | norm_ent < thresholds$formation_entropy,
      "convergent", "divergent"
    )
    terminus <- unique(contribution$trajectories$terminal_group)
    gd$group_role <- ifelse(gd$group %in% terminus, "terminus", "crossroads")
  }
  dplyr::arrange(gd, dplyr::desc(.data$recent_growth))
}
