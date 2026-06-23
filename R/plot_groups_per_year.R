# Per-year column layout: rank the nodes by size within each year and place them
# at integer y positions with the largest at the baseline (y = 1), equal
# spacing. Returns x (year), y (rank), size, group, terminal_group.
.dag_columns_layout <- function(nodes) {
  ys <- integer(nrow(nodes))
  for (idx in split(seq_len(nrow(nodes)), nodes$year)) {
    o <- idx[order(-nodes$size[idx])]
    ys[o] <- seq_along(o)
  }
  data.frame(
    x = nodes$year, y = ys, size = nodes$size,
    group = nodes$group, terminal_group = nodes$terminal_group,
    stringsAsFactors = FALSE
  )
}

#' Plot groups per year: the clustering fan converging to the final groups
#'
#' The stock-side bridge view. One column per year; within a column the
#' cluster-year nodes are ranked by paper count with the largest at the baseline,
#' dot area encodes paper count, and (optionally) the number of groups that year
#' is printed atop the column. No succession edges, no birth rings -- it shows the
#' annual + cumulative clustering fan settling onto the final-year groups. It is
#' the stock-side counterpart of the flow-side [plot_trajectory_dag()], and reads
#' the `sniff_trajectory_dag()` object directly.
#'
#' @param dag A [sniff_trajectory_dag()] object.
#' @param color_terminal Colour the dots by `terminal_group` to foreshadow the
#'   braid (default `FALSE`, a single neutral grey that reads as stock). The
#'   colour legend is suppressed either way.
#' @param point_size Upper bound of the dot-size range; paper count is mapped to
#'   `[1, point_size]` (default `6`).
#' @param show_count Print the number of groups atop each year column, on a
#'   filled label for emphasis (default `TRUE`).
#' @param count_size Font size of the per-year group-count text (default `3.2`).
#' @param count_colour Font colour of the count text (default `"white"`).
#' @param count_fill Background colour of the count label (default `"grey45"`, a
#'   grey chip that makes the count stand out); use `"white"` for a plain box.
#' @param label_terminal Label the final-year nodes with their group id (`c1gN`)
#'   to the right of the last column, with a leader line (default `TRUE`).
#' @param label_size Font size of the final-node labels (default `3`).
#' @param axis_text_size Font size of the x-axis (year) tick labels; `NULL`
#'   (default) keeps the theme default.
#' @param title Plot title (default `"Groups per year"`); `NULL` for none.
#' @return A `ggplot` object.
#' @seealso [sniff_trajectory_dag()], [plot_trajectory_dag()],
#'   [plot_trajectory_dag_interactive()]
#'
#' @details
#' The visual encoding:
#' - **One column per year** along the **x-axis**; the **y-axis has no intrinsic
#'   meaning** (nodes are stacked by rank).
#' - Within a column the cluster-year nodes are **ranked by paper count** with the
#'   largest at the baseline, and **dot area encodes the paper count**.
#' - With `show_count = TRUE` the **number of groups that year** is printed atop
#'   the column on a filled label (`count_fill` / `count_colour`).
#' - There are **no succession edges and no births**: this is the stock-side view
#'   of how many groups exist each year and how large they are, settling onto the
#'   final-year groups. For the flow (edges, trajectories) use
#'   [plot_trajectory_dag()].
#' - With `label_terminal = TRUE` the final-year nodes are labelled `c1gN` to the
#'   right of the last column.
#'
#' @examples
#' \dontrun{
#' # docs_per_group comes from sniff_groups_lineage()
#' dag <- sniff_trajectory_dag(docs_per_group)
#'
#' plot_groups_per_year(dag)
#'
#' # colour the dots by destination, bigger count chips and year labels
#' plot_groups_per_year(dag, color_terminal = TRUE, count_size = 5, axis_text_size = 14)
#' }
#'
#' @family visualization
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_label geom_text scale_size scale_x_continuous labs theme_minimal theme element_blank element_text expansion waiver
#' @importFrom rlang .data
plot_groups_per_year <- function(dag,
                                 color_terminal = FALSE,
                                 point_size = 6,
                                 show_count = TRUE,
                                 count_size = 3.2,
                                 count_colour = "white",
                                 count_fill = "grey45",
                                 label_terminal = TRUE,
                                 label_size = 3,
                                 axis_text_size = NULL,
                                 title = "Groups per year") {
  if (!is.list(dag) || is.null(dag$nodes) || !is.data.frame(dag$nodes)) {
    stop("'dag' must be the output of sniff_trajectory_dag()", call. = FALSE)
  }
  if (nrow(dag$nodes) == 0) {
    stop("the DAG has no nodes", call. = FALSE)
  }
  lay <- .dag_columns_layout(dag$nodes)
  x_expand <- if (label_terminal) ggplot2::expansion(mult = c(0.02, 0.1)) else ggplot2::waiver()

  p <- ggplot2::ggplot()
  if (color_terminal) {
    p <- p + ggplot2::geom_point(
      data = lay,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$size,
                   colour = .data$terminal_group),
      show.legend = FALSE
    )
  } else {
    p <- p + ggplot2::geom_point(
      data = lay,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$size),
      colour = "grey40"
    )
  }
  p <- p + ggplot2::scale_size(range = c(1, point_size), guide = "none")

  if (show_count) {
    counts <- do.call(rbind, lapply(split(lay, lay$x), function(d) {
      data.frame(x = d$x[1], y = max(d$y) + 1, n = nrow(d))
    }))
    p <- p + ggplot2::geom_label(
      data = counts,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$n),
      size = count_size, colour = count_colour, fill = count_fill, linewidth = 0
    )
  }

  if (label_terminal) {
    fin <- lay[lay$x == max(lay$x), , drop = FALSE]
    fin$label_text <- as.character(fin$group)
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        data = fin,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label_text),
        direction = "y", hjust = 0, nudge_x = 0.7, size = label_size,
        segment.size = 0.3, segment.colour = "grey50",
        min.segment.length = 0, point.padding = 0.3, max.overlaps = Inf
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = fin,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label_text),
        hjust = 0, nudge_x = 0.7, size = label_size
      )
    }
  }

  breaks <- sort(unique(lay$x))
  p <- p +
    ggplot2::scale_x_continuous(breaks = breaks, labels = breaks, expand = x_expand) +
    ggplot2::labs(x = "Year", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = axis_text_size),
      axis.text.y = ggplot2::element_blank()
    )
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  p
}
