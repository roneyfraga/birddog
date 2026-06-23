# Spread overlapping label y-positions apart by at least `min_gap`, preserving
# order and re-centring on the original midpoint. A 1D de-overlap so plotly text
# labels do not collide (plotly has no ggrepel).
.spread_y <- function(y, min_gap) {
  if (length(y) < 2 || min_gap <= 0) return(y)
  o <- order(y)
  ys <- y[o]
  for (i in 2:length(ys)) {
    if (ys[i] - ys[i - 1] < min_gap) ys[i] <- ys[i - 1] + min_gap
  }
  shift <- mean(range(y)) - mean(range(ys))
  out <- numeric(length(y))
  out[o] <- ys + shift
  out
}

#' Interactive trajectory DAG (plotly): hover a node for its trajectory
#'
#' The plotly twin of [plot_trajectory_dag()]. Nodes are laid out by the same
#' year-aware Sugiyama layout, sized by paper count, and coloured by the final
#' group their lineage reaches. Dormant lineages that never reach the final year
#' are drawn grey. There is no legend: hovering a final-year node shows its group
#' id and `labels_text` description (`c1gN: description`), and hovering any other
#' (intermediate or dormant) node shows its raw id (e.g. `y2021c1g2`). When
#' `label_terminal` is `TRUE`, the
#' final group id (`c1gN`) is also printed to the right of each final-year node.
#' Returns a `plotly` htmlwidget.
#'
#' @param dag A [sniff_trajectory_dag()] object.
#' @param labels_text Optional `data.frame` with columns `id` and `text` mapping
#'   final group ids to descriptions, shown after the group id when hovering a
#'   final-year node (`c1gN: description`); `NULL` (default) falls back to the
#'   bare group id.
#' @param marker_range Min/max marker size in pixels; paper count is scaled into
#'   this range (default `c(4, 14)`).
#' @param edge_alpha Edge opacity, `0`-`1` (default `0.3`).
#' @param label_terminal Show the final group id (`c1gN`) to the right of each
#'   final-year node (default `TRUE`).
#' @param label_size Font size (px) of the final-node labels (default `12`).
#' @param title Plot title (default `"Cumulative network groups"`); `NULL` for
#'   none.
#' @return A `plotly` htmlwidget.
#' @seealso [plot_trajectory_dag()], [plot_groups_per_year()]
#'
#' @details
#' The interactive twin of [plot_trajectory_dag()], built with plotly:
#' - The **x-axis is the publication year** (Sugiyama layout); the **y-axis has no
#'   intrinsic meaning**.
#' - **Node colour** is the trajectory basin (the final group reached); **dormant
#'   lineages** that never reach the final year are grey. **Node size** scales with
#'   paper count over `marker_range`.
#' - **There is no legend.** Hovering a **final-year node** shows `c1gN: description`
#'   (its group id plus the `labels_text` description); hovering any **other node**
#'   shows its raw id (`yYYYYcXgN`).
#' - **Edges** are a single faint layer (`edge_alpha`) and are excluded from hover.
#'   With `label_terminal = TRUE` the final group ids (`c1gN`) are printed in a
#'   de-overlapped column to the right, each joined to its node by a leader line.
#'
#' The widget renders in the RStudio Viewer, embeds in Quarto/Shiny, or saves to a
#' standalone file with `htmlwidgets::saveWidget()`.
#'
#' @examples
#' \dontrun{
#' # docs_per_group comes from sniff_groups_lineage()
#' dag <- sniff_trajectory_dag(docs_per_group)
#'
#' plot_trajectory_dag_interactive(dag)
#'
#' # hover the final nodes for descriptions; fainter edges
#' descr <- data.frame(id = c("c1g1", "c1g2"), text = c("Topic A", "Topic B"))
#' p <- plot_trajectory_dag_interactive(dag, labels_text = descr, edge_alpha = 0.2)
#' # htmlwidgets::saveWidget(p, "trajectory_dag.html", selfcontained = TRUE)
#' }
#'
#' @family visualization
#' @export
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom igraph is_igraph vcount
#' @importFrom stats setNames
plot_trajectory_dag_interactive <- function(dag,
                                            labels_text = NULL,
                                            marker_range = c(4, 14),
                                            edge_alpha = 0.3,
                                            label_terminal = TRUE,
                                            label_size = 12,
                                            title = "Cumulative network groups") {
  if (!is.list(dag) || is.null(dag$graph) || !igraph::is_igraph(dag$graph)) {
    stop("'dag' must be the output of sniff_trajectory_dag()", call. = FALSE)
  }
  if (igraph::vcount(dag$graph) == 0) {
    stop("the DAG has no nodes", call. = FALSE)
  }
  ml  <- mk_layout_and_year_scale(dag$graph)
  lay <- ml$lay
  lay$terminal_group <- as.character(lay$terminal_group)
  lay$group <- as.character(lay$group)
  lay$name  <- as.character(lay$name)

  # Final groups = those present in the last year; a lineage whose terminal_group
  # isn't one of them is dormant (never reached the final year) and drawn grey.
  final_year <- max(lay$year, na.rm = TRUE)
  final_groups <- unique(lay$group[lay$year == final_year])
  is_final <- lay$terminal_group %in% final_groups

  groups <- mixed_sort(final_groups)
  pal <- if (length(groups) <= 8) {
    RColorBrewer::brewer.pal(max(3, length(groups)), "Set2")[seq_along(groups)]
  } else {
    scales::hue_pal()(length(groups))
  }
  names(pal) <- groups

  # Hover: a final-year node shows "c1gN: description" (its group id, plus the
  # labels_text description when matched); every other (intermediate or dormant)
  # node shows its raw node id (e.g. y2021c1g2).
  desc <- .dag_label_text(lay$group, labels_text)
  final_label <- ifelse(desc == lay$group, lay$group, paste0(lay$group, ": ", desc))
  lay$hover <- ifelse(lay$year == final_year, final_label, lay$name)
  lay$px <- scales::rescale(sqrt(lay$size), to = marker_range)

  pos_x <- stats::setNames(lay$x, lay$name)
  pos_y <- stats::setNames(lay$y, lay$name)

  edges <- dag$edges
  p <- plotly::plot_ly()
  if (nrow(edges)) {
    ex <- as.numeric(t(cbind(pos_x[as.character(edges$from)],
                             pos_x[as.character(edges$to)], NA)))
    ey <- as.numeric(t(cbind(pos_y[as.character(edges$from)],
                             pos_y[as.character(edges$to)], NA)))
    p <- plotly::add_trace(
      p, x = ex, y = ey, type = "scatter", mode = "lines",
      line = list(color = "grey80", width = 1), opacity = edge_alpha,
      hoverinfo = "skip", showlegend = FALSE
    )
  }
  dor <- lay[!is_final, , drop = FALSE]
  if (nrow(dor)) {
    p <- plotly::add_trace(
      p, x = dor$x, y = dor$y, type = "scatter", mode = "markers",
      marker = list(size = dor$px, color = "grey70"),
      text = dor$hover, hoverinfo = "text", showlegend = FALSE
    )
  }
  fin <- lay[is_final, , drop = FALSE]
  p <- plotly::add_trace(
    p, x = fin$x, y = fin$y, type = "scatter", mode = "markers",
    marker = list(size = fin$px, color = unname(pal[fin$terminal_group])),
    text = fin$hover, hoverinfo = "text", showlegend = FALSE
  )

  if (label_terminal) {
    fy <- lay[lay$year == final_year, , drop = FALSE]
    gap <- if (nrow(fy) > 1) diff(range(fy$y)) / (nrow(fy) - 1) else 0
    fy$ly <- .spread_y(fy$y, gap)        # de-overlapped label y positions
    lx <- max(fy$x) + 0.7                # label column to the right of the nodes
    # leader line from each final-year vertex to its (spread) label
    seg_x <- as.numeric(t(cbind(fy$x, lx, NA)))
    seg_y <- as.numeric(t(cbind(fy$y, fy$ly, NA)))
    p <- plotly::add_trace(
      p, x = seg_x, y = seg_y, type = "scatter", mode = "lines",
      line = list(color = "grey60", width = 0.7), hoverinfo = "skip", showlegend = FALSE
    )
    p <- plotly::add_trace(
      p, x = rep(lx, nrow(fy)), y = fy$ly, type = "scatter", mode = "text",
      text = fy$group, textposition = "middle right",
      textfont = list(size = label_size, color = "black"),
      cliponaxis = FALSE, hoverinfo = "skip", showlegend = FALSE
    )
  }

  xax <- list(visible = TRUE, showgrid = FALSE, zeroline = FALSE, title = "Year",
              tickmode = "array", tickvals = ml$all_breaks, ticktext = ml$all_labels)
  if (label_terminal) {
    xr <- range(lay$x, na.rm = TRUE)
    xax$range <- c(xr[1] - 0.5, xr[2] + 3)
  }
  yax <- list(visible = FALSE, showgrid = FALSE, zeroline = FALSE)
  ttl <- if (is.null(title)) list(text = "") else list(text = title)
  plotly::layout(p, xaxis = xax, yaxis = yax, title = ttl, showlegend = FALSE)
}
