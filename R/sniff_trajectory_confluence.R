#' Build the render-ready confluence forest of the soft DAG
#'
#' Turns a [sniff_trajectory_braid()] object into the data a trajectory confluence
#' needs: one `rivers` row per trajectory (with a per-year size curve) and one
#' `confluences` row per child to parent merge edge (papers transferred, the
#' first year a transferred paper was published, the handoff year, and the
#' cumulative-inflow curve). Every edge of the flow tree is kept; thresholds are
#' applied later by [plot_trajectory_confluence()], so a slider can re-filter
#' without recomputing.
#'
#' @param x A [sniff_trajectory_braid()] object, or a `docs_per_group` tibble /
#'   [sniff_groups_lineage()] object / [sniff_trajectory_dag()] object (the
#'   flow is built internally).
#' @param min_group_size,jaccard_min,k_out Passed to [sniff_trajectory_braid()]
#'   when `x` is not already a flow object.
#'
#' @return A list with:
#' \describe{
#'   \item{rivers}{Tibble, one row per trajectory: `traj_id`, `type`
#'     (`"central"`/`"absorbed"`), `central` (destination group `cNgN`), `size`,
#'     `start`, `handoff_year` (= `last_year` for centrals, else absorption year),
#'     `depth`, `parent` (absorber `traj_id`, `NA` for central), and `size_curve`
#'     (list-col of `tibble(year, size)`, the trajectory's community size per
#'     year).}
#'   \item{confluences}{Tibble, one row per child to parent merge: `child`,
#'     `parent`, `n` (papers transferred), `cohort_size` (the child's size),
#'     `first_feed_year` (publication year of the first transferred paper),
#'     `handoff_year`, and `inflow_curve` (list-col of `tibble(year, size)`).}
#'   \item{destinations}{Tibble, one row per (absorbed trajectory, final group) it
#'     fed: `traj_id`, `g_final`, `n` (papers of the trajectory's terminal cohort
#'     that ended up in `g_final`). A trajectory usually appears under several
#'     finals, so this is the multi-destination split the forest collapses to one.}
#'   \item{centrals}{The central group ids (`cNgN`) in `mixed_sort()` order.}
#'   \item{last_year}{Final year of the analysis.}
#' }
#'
#' @seealso [sniff_trajectory_braid()], [plot_trajectory_confluence()]
#' @family trajectory analysis
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
sniff_trajectory_confluence <- function(x, min_group_size = 10,
                                        jaccard_min = 0.05, k_out = 2) {
  flow <- if (is_flow(x)) {
    x
  } else if (is.data.frame(x) ||
             (is.list(x) && !is.null(x$graph)) ||
             (is.list(x) && !is.null(x$docs_per_group))) {
    sniff_trajectory_braid(x, min_group_size = min_group_size,
                          jaccard_min = jaccard_min, k_out = k_out)
  } else {
    stop("'x' must be a sniff_trajectory_braid() object or a docs_per_group / ",
         "sniff_trajectory_dag() object", call. = FALSE)
  }

  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  last_year <- as.integer(flow$last_year)
  node_size <- .node_size_lookup(dpg)

  rivers <- tibble::tibble(
    traj_id = tr$traj_id,
    type = tr$type,
    central = tr$group,
    size = as.integer(tr$size),
    start = as.integer(tr$start),
    handoff_year = ifelse(tr$type == "central", last_year,
                          as.integer(tr$absorption_year)),
    depth = as.integer(tr$depth),
    parent = tr$absorbed_into,
    size_curve = lapply(tr$nodes, function(nd) .feeder_growth_series(nd, node_size))
  )

  parents <- unique(flow$tree$parent)
  rows <- list()
  for (p in parents) {
    fe <- .formation_from_flow(flow, target = p, min_papers = 0,
                               min_prop = 0)$feeders
    if (nrow(fe) == 0) next
    for (i in seq_len(nrow(fe))) {
      ic <- fe$inflow_curve[[i]]
      first_feed <- if (nrow(ic) > 0) ic$year[1] else fe$handoff_year[i]
      rows[[length(rows) + 1]] <- tibble::tibble(
        child = fe$source_key[i], parent = p,
        n = as.integer(fe$n[i]), cohort_size = as.integer(fe$cohort_size[i]),
        first_feed_year = as.integer(first_feed),
        handoff_year = as.integer(fe$handoff_year[i]),
        inflow_curve = list(ic)
      )
    }
  }
  confluences <- if (length(rows)) {
    dplyr::bind_rows(rows)
  } else {
    tibble::tibble(child = character(), parent = character(), n = integer(),
                   cohort_size = integer(), first_feed_year = integer(),
                   handoff_year = integer(), inflow_curve = list())
  }

  # Per-tributary destination split across the final groups. A trajectory's
  # terminal cohort can land in several finals, but the forest keeps only the
  # dominant one (its `parent`); this records the full split so the plot can show
  # the secondary destinations.
  node_docs <- split(dpg$document_id, dpg$group_id)
  fy <- dpg[dpg$network_until == last_year, , drop = FALSE]
  paper_final <- stats::setNames(as.character(fy$group), fy$document_id)
  dest_rows <- list()
  for (i in which(tr$type == "absorbed")) {
    nd <- tr$nodes[[i]]
    term <- nd[which.max(.extract_year(nd))]
    cohort <- unique(node_docs[[term]])
    if (is.null(cohort) || !length(cohort)) next
    fg <- paper_final[cohort]; fg <- fg[!is.na(fg)]
    if (!length(fg)) next
    tab <- table(fg)
    dest_rows[[length(dest_rows) + 1]] <- tibble::tibble(
      traj_id = tr$traj_id[i], g_final = names(tab), n = as.integer(tab))
  }
  destinations <- if (length(dest_rows)) dplyr::bind_rows(dest_rows) else
    tibble::tibble(traj_id = character(), g_final = character(), n = integer())

  centrals <- mixed_sort(unique(tr$group[tr$type == "central"]))
  list(rivers = rivers, confluences = confluences, destinations = destinations,
       centrals = centrals, last_year = last_year)
}
