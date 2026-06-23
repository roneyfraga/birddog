#' Plot the global trajectories as 2D variable-width lines
#'
#' The line counterpart of [plot_trajectory_confluence()] over the **global** flow
#' DAG. Every global trajectory is drawn as a variable-width line through the
#' year-aware Sugiyama layout: the central finals `tr::cNgN` and the intermediate
#' `trN` that feed them, each line widening with the documents accumulated along
#' its path. A bottom legend reports, for every intermediate, how many papers of
#' its terminal cohort reached each final group -- a single `trN` can feed more
#' than one final (its terminal cohort splits across groups), which the legend
#' makes explicit.
#'
#' @param flow A [sniff_trajectory_braid()] object, or a `docs_per_group` tibble /
#'   [sniff_trajectory_dag()] object the flow is built from.
#' @param target What to draw. `"all"` (default) draws every central and the
#'   trajectories feeding it; a central `traj_id` (e.g. `"tr::c1g1"`) or a vector
#'   of them draws only those finals and their feeders.
#' @param conf An optional precomputed [sniff_trajectory_confluence()] object for
#'   `flow`; pass it to skip recomputing the confluence on every call (e.g. in a
#'   Shiny app that re-renders as a selector changes). `NULL` (default) computes it.
#' @param min_n,min_prop,min_total_size,min_duration_years Feeder thresholds,
#'   passed to the same pruning used by [plot_trajectory_confluence()]: an
#'   intermediate is drawn only if it transfers `>= min_n` papers and `>= min_prop`
#'   of its cohort, has total size `>= min_total_size` and lifespan
#'   `>= min_duration_years`; a pruned intermediate takes its own feeders with it.
#' @param min_target_n Minimum papers an intermediate must leave in the selected
#'   final(s) by the last year to be drawn (default `0`, no filter). Unlike `min_n`
#'   (the tree-edge transfer to its absorber), this counts the intermediate's own
#'   papers that **end up** in the target group, so a feeder that merges in but
#'   barely persists there (e.g. `tr39` with 12 of its papers in `c1g1`) is dropped
#'   by `min_target_n = 20`. Only applies to a targeted final view.
#' @param width_range Line-width range `c(min, max)` (default `c(0.8, 6)`), scaled
#'   by the cumulative tracked documents along each trajectory.
#' @param use_raw_papers Scale widths by raw `quantity_papers` (`TRUE`) or by the
#'   weighted `quantity_papers * prop_tracked_intra_group` (`FALSE`, default).
#' @param label_size Text size of the `trN` / `tr::cNgN` end labels (default `4`).
#' @param dest_min_prop In the `target = "all"` view only, the minimum share of an
#'   intermediate's terminal cohort that must reach a final for that final to
#'   appear in its legend entry (default `0.05`). A targeted view reports only the
#'   selected final(s), so this does not apply.
#' @param lowlight_alpha,lowlight_color Opacity and colour of the background
#'   (non-trajectory) edges (defaults `0.18`, neutral grey).
#' @param legend_ncol Number of columns in the feeding legend; `NULL` (default)
#'   lets ggplot choose.
#' @param legend_text_size Font size (pt) of the legend text; `NULL` (default)
#'   keeps the theme default.
#' @param axis_text_size Font size (pt) of the x-axis (year) tick labels; `NULL`
#'   (default) keeps the theme default.
#' @param year_range Optional `c(from, to)` years fixing the time axis, so a short
#'   trajectory can be shown on the same window as a long one (e.g. compare a young
#'   `tr::c1g14` against `tr::c1g1`). `NULL` (default) fits the drawn trajectories;
#'   a wider range only extends the axis (it never drops data).
#' @param palette Optional named colour vector overriding the default hues (keyed
#'   by trajectory `traj_id`).
#' @param title Plot title; `NULL` (default) derives one from `target`.
#'
#' @return A `ggplot` object.
#'
#' @details
#' - **x = publication year** (Sugiyama layers); the **y-axis has no intrinsic
#'   meaning** (a branching position).
#' - **Line width** grows with cumulative tracked documents along the trajectory.
#' - A single-year **feeder** has no edge to draw as a line and is dropped (it
#'   would otherwise show a label/legend with no visible line); a single-year
#'   **central** (the selected final, e.g. a final-year-only community like
#'   `tr::c1g17`) is kept and drawn as a point.
#' - **Colour** is per trajectory. With a single targeted final the legend is
#'   summarised to `trN: p% (n)` -- `n` of `trN`'s papers landed in that final,
#'   which is `p%` of `trN`'s size (the feeders' counts overlap, so they do not
#'   sum to the final's total). With `target = "all"` it shows the full split
#'   `trN -> c1g1:n, c1g2:n`. A central is `tr::cNgN (final, n)`, where `n` is the
#'   documents that **remained** in its final-year community (the last point of
#'   its size curve), not the cumulative documents the trajectory ever held.
#'
#' @seealso [plot_trajectory_lines_3d()], [plot_trajectory_confluence()],
#'   [sniff_trajectory_braid()], [sniff_trajectory_confluence()]
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' plot_trajectory_lines_2d(flow, min_n = 20)
#' plot_trajectory_lines_2d(flow, target = "tr::c1g1")
#' }
#'
#' @family visualization
#' @export
#' @importFrom igraph induced_subgraph vcount ecount V E
#' @importFrom ggraph ggraph geom_edge_link scale_edge_colour_manual
#'   scale_edge_width_identity
#' @importFrom ggplot2 aes labs geom_label geom_blank geom_point scale_colour_identity scale_x_continuous scale_y_continuous
#'   theme element_text element_blank element_rect expansion guides guide_legend
#' @importFrom rlang .data
#' @importFrom stats setNames
plot_trajectory_lines_2d <- function(flow, target = "all", conf = NULL,
                                     min_n = 5, min_prop = 0.05,
                                     min_total_size = 0, min_duration_years = 0,
                                     min_target_n = 0,
                                     width_range = c(0.8, 6), use_raw_papers = FALSE,
                                     label_size = 4, dest_min_prop = 0.05,
                                     lowlight_alpha = 0.18, lowlight_color = "#9AA5B1",
                                     legend_ncol = NULL, legend_text_size = NULL,
                                     axis_text_size = NULL, year_range = NULL,
                                     palette = NULL, title = NULL) {
  if (!is_flow(flow)) flow <- sniff_trajectory_braid(flow)
  if (is.null(conf)) conf <- sniff_trajectory_confluence(flow)

  tgt <- if (is.null(target) || identical(target, "all")) NULL else target
  target_groups <- if (!is.null(tgt)) sub("^tr::", "", tgt) else NULL
  pr <- .confluence_prune(conf$rivers, conf$confluences, target = tgt,
                          min_n = min_n, min_prop = min_prop,
                          min_total_size = min_total_size,
                          min_duration_years = min_duration_years)
  if (nrow(pr$rivers) == 0) {
    stop("no trajectories pass the thresholds (min_n / min_prop / min_total_size / ",
         "min_duration_years)", call. = FALSE)
  }
  kept <- pr$rivers$traj_id
  # min_target_n: keep only feeders that leave at least this many papers in the
  # selected final(s) by the last year (the targeted central itself is always kept).
  if (!is.null(target_groups) && min_target_n > 0) {
    dst <- conf$destinations
    contrib <- vapply(pr$rivers$traj_id, function(id) {
      d <- dst[dst$traj_id == id & dst$g_final %in% target_groups, , drop = FALSE]
      if (nrow(d)) sum(d$n) else 0L
    }, numeric(1))
    kept <- pr$rivers$traj_id[pr$rivers$type == "central" | contrib >= min_target_n]
  }
  # node paths of the kept trajectories. A single-year FEEDER draws no line and is
  # dropped (it would show a label/legend with no visible vertex). A single-year
  # CENTRAL (the selected target, e.g. a final-year-only community like tr::c1g17)
  # is kept and drawn as a point below, so the view still shows it.
  trj <- flow$trajectories[flow$trajectories$traj_id %in% kept, , drop = FALSE]
  trj <- trj[lengths(trj$nodes) >= 2 | trj$type == "central", , drop = FALSE]
  if (nrow(trj) == 0) {
    stop("no trajectory to draw for this target / thresholds", call. = FALSE)
  }
  keep_ids <- trj$traj_id
  riv <- pr$rivers[match(keep_ids, pr$rivers$traj_id), , drop = FALSE]
  tr_highlight <- tibble::tibble(traj_id = trj$traj_id, nodes = trj$nodes)
  nodes <- unique(unlist(tr_highlight$nodes, use.names = FALSE))
  g <- igraph::induced_subgraph(flow$graph, nodes)
  if (igraph::vcount(g) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "No trajectory nodes to draw"))
  }
  g <- assign_traj_edge_widths(g, tr_highlight, width_range, use_raw_papers)

  # colour + feeding legend text, keyed by traj_id (centrals first, then mixed_sort)
  levs <- c(mixed_sort(keep_ids[startsWith(keep_ids, "tr::")]),
            mixed_sort(keep_ids[!startsWith(keep_ids, "tr::")]))
  pal <- if (!is.null(palette)) palette else if (length(levs) <= 8) {
    stats::setNames(RColorBrewer::brewer.pal(max(3, length(levs)), "Set2")[seq_along(levs)], levs)
  } else {
    stats::setNames(scales::hue_pal()(length(levs)), levs)
  }
  # the central label reports what remained in its FINAL-year node (the last point
  # of its size curve), not the cumulative documents the trajectory ever held.
  final_size_of <- stats::setNames(vapply(riv$size_curve, function(s)
    if (nrow(s)) as.integer(s$size[which.max(s$year)]) else NA_integer_, integer(1)),
    riv$traj_id)
  type_of <- stats::setNames(riv$type, riv$traj_id)
  size_of <- stats::setNames(riv$size, riv$traj_id)
  dest <- conf$destinations
  # when a final is targeted, the legend reports only how many of each feeder's
  # papers landed in the selected final(s) -- not the feeder's other destinations.
  feed_label <- function(id) {
    if (identical(type_of[[id]], "central")) {
      return(paste0(id, " (final, ", final_size_of[[id]], ")"))
    }
    dd <- dest[dest$traj_id == id & dest$n > 0, , drop = FALSE]
    if (!is.null(target_groups)) {
      dd <- dd[dd$g_final %in% target_groups, , drop = FALSE]
      if (length(target_groups) == 1) {
        n <- if (nrow(dd)) sum(dd$n) else 0L
        pct <- if (size_of[[id]] > 0) round(100 * n / size_of[[id]]) else 0
        return(paste0(id, ": ", pct, "% (", n, ")"))
      }
      if (!nrow(dd)) return(paste0(id, ": 0"))
      dd <- dd[order(-dd$n), , drop = FALSE]
      return(paste0(id, " \u2192 ", paste0("tr::", dd$g_final, ":", dd$n, collapse = ", ")))
    }
    if (!nrow(dd)) return(id)
    tot <- sum(dd$n)
    dd <- dd[dd$n / tot >= dest_min_prop, , drop = FALSE]
    dd <- dd[order(-dd$n), , drop = FALSE]
    paste0(id, " \u2192 ", paste0("tr::", dd$g_final, ":", dd$n, collapse = ", "))
  }
  leg_labels <- vapply(levs, feed_label, character(1))

  igraph::E(g)$traj_id <- factor(igraph::E(g)$traj_id, levels = levs)

  # layout: a year-aware Sugiyama. Use the real publication year as the x-coordinate
  # (not the collapsed layer index) so trajectories of different lengths share a true
  # time axis; `year_range` widens the window for cross-trajectory comparison.
  ml <- mk_layout_and_year_scale(g); lay <- ml$lay
  lay$x <- .extract_year(lay$name)
  yr <- if (!is.null(year_range)) range(year_range) else range(lay$x, na.rm = TRUE)
  breaks <- seq(yr[1], yr[2], by = 2); labels <- breaks

  # end labels: trN / tr::cNgN at each trajectory's last node
  get_last <- function(nd) nd[order(.extract_year(nd), nd)][length(nd)]
  lnodes <- vapply(tr_highlight$nodes, get_last, character(1))
  vidx <- match(lnodes, lay$name)
  lab_df <- tibble::tibble(x = lay$x[vidx] + 0.3, y = lay$y[vidx],
                           label = tr_highlight$traj_id)
  lab_df <- lab_df[is.finite(lab_df$x) & is.finite(lab_df$y), , drop = FALSE]

  p <- ggraph::ggraph(lay) +
    ggraph::geom_edge_link(
      ggplot2::aes(filter = is.na(.data$traj_id)),
      width = 0.7, alpha = lowlight_alpha, colour = lowlight_color,
      lineend = "round", show.legend = FALSE) +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_colour = .data$traj_id, edge_width = .data$traj_width,
                   filter = !is.na(.data$traj_id)),
      lineend = "round") +
    ggraph::scale_edge_colour_manual(values = pal, limits = levs, breaks = levs,
                                     labels = leg_labels, drop = FALSE, name = NULL) +
    ggraph::scale_edge_width_identity(guide = "none")

  # point marker for a single-year trajectory that has no line to draw (a
  # final-year-only central such as tr::c1g17), placed on its single node.
  one <- lengths(tr_highlight$nodes) < 2
  if (any(one)) {
    sn <- vapply(tr_highlight$nodes[one], function(nd) nd[[1]], character(1))
    pidx <- match(sn, lay$name)
    pt_df <- tibble::tibble(x = lay$x[pidx], y = lay$y[pidx],
                            fill = unname(pal[tr_highlight$traj_id[one]]))
    pt_df <- pt_df[is.finite(pt_df$x) & is.finite(pt_df$y), , drop = FALSE]
    if (nrow(pt_df)) {
      p <- p + ggplot2::geom_point(
        data = pt_df, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$fill),
        size = label_size * 1.3, show.legend = FALSE) +
        ggplot2::scale_colour_identity()
    }
  }

  if (nrow(lab_df) > 0) {
    p <- p + ggplot2::geom_label(
      data = lab_df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      fill = "gray25", colour = "white", size = label_size, fontface = "bold",
      label.size = 0, alpha = 0.85, hjust = 0, show.legend = FALSE)
  }

  if (is.null(title)) {
    title <- if (is.null(tgt)) "Trajectory lines (global)"
      else paste0("Feeders of ", paste(tgt, collapse = ", "))
  }
  guide_args <- list(override.aes = list(edge_width = 2), title = NULL)
  if (!is.null(legend_ncol)) guide_args$ncol <- legend_ncol

  p +
    ggplot2::guides(edge_colour = do.call(ggplot2::guide_legend, guide_args)) +
    # extend the time axis to the requested window without dropping any data
    ggplot2::geom_blank(data = data.frame(x = yr, y = rep(mean(range(lay$y)), 2L)),
                        ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::scale_x_continuous(breaks = breaks, labels = labels,
                                expand = ggplot2::expansion(mult = c(0.02, 0.10))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.08)) +
    ggplot2::labs(title = title, x = "Publication year", y = NULL) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = axis_text_size),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.position = "bottom")
}
