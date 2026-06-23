# Resolve group ids to label strings via an optional labels_text (id, text)
# data.frame; fall back to the bare id where unmatched or when labels_text is
# NULL. Warns once when a non-NULL labels_text shares no id with `ids`.
.dag_label_text <- function(ids, labels_text) {
  ids <- as.character(ids)
  if (is.null(labels_text)) return(ids)
  if (!is.data.frame(labels_text) || !all(c("id", "text") %in% names(labels_text))) {
    stop("'labels_text' must be a data.frame with columns 'id' and 'text'", call. = FALSE)
  }
  if (!any(ids %in% labels_text$id)) {
    warning("none of labels_text$id match the group ids; using the group ids as labels",
            call. = FALSE)
  }
  mapped <- labels_text$text[match(ids, labels_text$id)]
  ifelse(is.na(mapped), ids, mapped)
}

#' Plot the soft trajectory DAG (all intermediate nodes)
#'
#' The braided-river panorama: every cluster-year node laid out by year (x), with
#' succession edges, node colour = `terminal_group` (or `group`), node size =
#' paper count, and births ringed. Uses the year-aware Sugiyama layout from
#' [sniff_trajectory_dag()]'s graph. Nodes from dormant lineages that never reach
#' the final year (a colour value outside the final groups) are drawn grey and
#' kept out of the colour legend.
#'
#' @param dag A [sniff_trajectory_dag()] object.
#' @param color_by How to colour the nodes (default `"terminal_group"`).
#'   `"terminal_group"` uses the final group each node's lineage flows into (its
#'   heaviest-successor destination), so a whole trajectory keeps one colour from
#'   source to mouth and the colour legend has one entry per final group.
#'   `"group"` uses the node's own per-year id (`c1gN`), which is a within-year
#'   size rank rather than a stable identity, so the same trajectory changes
#'   colour across years. Pick `"terminal_group"` to read how the final groups
#'   formed, `"group"` to inspect the raw annual clustering labels.
#' @param edge_alpha Base edge transparency, scaled by Jaccard weight (default 0.25).
#' @param point_size Base node size (default 2.5).
#' @param show_legend Show the colour legend (default `FALSE`; many final groups
#'   make a busy legend).
#' @param label_terminal Label the final-year nodes with their bare group id
#'   (`c1gN`), to the right of each vertex, with a leader line back to the vertex
#'   (default `TRUE`).
#' @param labels_text Optional `data.frame` with columns `id` and `text`. When
#'   supplied, the node-colour legend moves to the bottom and each group is
#'   relabelled `"c1gN: text"` (groups without a match keep the bare id); `NULL`
#'   (default) leaves the legend governed by `show_legend`. The final-node labels
#'   (`label_terminal`) are always the bare group id, independent of this.
#' @param label_size Font size of the final-node labels (default `3`).
#' @param title Plot title (default `"Cumulative network groups"`); `NULL` for
#'   no title.
#' @param legend_text_size Font size of the colour-legend text; `NULL` (default)
#'   keeps the theme default.
#' @param legend_ncol Number of columns in the colour legend; `NULL` (default)
#'   lets ggplot choose.
#' @param axis_text_size Font size of the x-axis (year) tick labels; `NULL`
#'   (default) keeps the theme default.
#' @return A `ggplot` object.
#' @seealso [sniff_trajectory_dag()], [plot_groups_per_year()],
#'   [plot_trajectory_dag_interactive()]
#'
#' @details
#' The visual encoding:
#' - The **x-axis is the publication year** (a Sugiyama layered layout); the
#'   **y-axis has no intrinsic meaning**, it only spreads nodes to reduce edge
#'   crossings.
#' - **Node colour** is the trajectory basin (see `color_by`), **node size** is
#'   the paper count, and **succession edges** fade with their Jaccard weight.
#' - **Births** (nodes with no strong predecessor) are ringed in black.
#' - **Dormant lineages** that never reach the final year are drawn grey and kept
#'   out of the colour legend, so the colours stand for the final groups only.
#' - With `label_terminal = TRUE` the final-year nodes are labelled `c1gN` to the
#'   right (with a leader line); with `labels_text` the colour legend moves to the
#'   bottom as `"c1gN: description"` entries.
#'
#' @examples
#' \dontrun{
#' # docs_per_group comes from sniff_groups_lineage()
#' dag <- sniff_trajectory_dag(docs_per_group)
#'
#' # the whole braid, coloured by destination group
#' plot_trajectory_dag(dag)
#'
#' # human-readable bottom legend, two columns, larger year labels
#' descr <- data.frame(id = c("c1g1", "c1g2"), text = c("Topic A", "Topic B"))
#' plot_trajectory_dag(dag, labels_text = descr, legend_ncol = 2, axis_text_size = 14)
#' }
#'
#' @family visualization
#' @export
#' @importFrom ggraph ggraph geom_edge_link geom_node_point scale_edge_alpha
#' @importFrom ggplot2 aes scale_size scale_x_continuous labs theme_minimal theme
#' @importFrom ggplot2 element_blank element_text expansion waiver geom_text
#' @importFrom ggplot2 scale_colour_discrete guides guide_legend
#' @importFrom igraph is_igraph vcount V
#' @importFrom rlang .data
plot_trajectory_dag <- function(dag, color_by = c("terminal_group", "group"),
                                edge_alpha = 0.25, point_size = 2.5,
                                show_legend = FALSE,
                                label_terminal = TRUE, labels_text = NULL,
                                label_size = 3,
                                title = "Cumulative network groups",
                                legend_text_size = NULL, legend_ncol = NULL,
                                axis_text_size = NULL) {
  color_by <- match.arg(color_by)
  if (!is.list(dag) || is.null(dag$graph) || !igraph::is_igraph(dag$graph)) {
    stop("'dag' must be the output of sniff_trajectory_dag()", call. = FALSE)
  }
  if (igraph::vcount(dag$graph) == 0) {
    stop("the DAG has no nodes", call. = FALSE)
  }
  ml <- mk_layout_and_year_scale(dag$graph)
  lay <- ml$lay

  # The final groups are those present in the last year (c1g1..c1gN). A node
  # whose colour value isn't one of them belongs to a dormant lineage that never
  # reached the final year; draw it grey and keep it out of the colour legend.
  final_year <- max(lay$year, na.rm = TRUE)
  final_groups <- unique(as.character(lay$group[lay$year == final_year]))

  x_expand <- if (label_terminal) ggplot2::expansion(mult = c(0.02, 0.25)) else ggplot2::waiver()
  show_col_legend <- show_legend || !is.null(labels_text)

  p <- ggraph::ggraph(lay) +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_alpha = .data$weight),
      edge_colour = "grey55", show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      data = function(d) d[!(as.character(d[[color_by]]) %in% final_groups), , drop = FALSE],
      ggplot2::aes(size = .data$size),
      colour = "grey75", show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      data = function(d) d[as.character(d[[color_by]]) %in% final_groups, , drop = FALSE],
      ggplot2::aes(colour = .data[[color_by]], size = .data$size),
      show.legend = show_col_legend
    ) +
    ggraph::geom_node_point(
      data = function(d) d[d$is_birth %in% TRUE, , drop = FALSE],
      shape = 21, colour = "black", fill = NA,
      size = point_size + 1.6, stroke = 0.5, show.legend = FALSE
    ) +
    ggraph::scale_edge_alpha(range = c(0.08, 0.6), guide = "none") +
    ggplot2::scale_size(range = c(1, 6), guide = "none") +
    ggplot2::scale_x_continuous(breaks = ml$all_breaks, labels = ml$all_labels,
                                expand = x_expand) +
    ggplot2::labs(x = "Year", y = NULL, title = title,
                  colour = color_by) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = axis_text_size),
      axis.text.y = ggplot2::element_blank()
    )

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

  if (show_col_legend) {
    guide_args <- list(override.aes = list(size = 3))
    if (!is.null(legend_ncol)) guide_args$ncol <- legend_ncol
    p <- p + ggplot2::guides(colour = do.call(ggplot2::guide_legend, guide_args))
    if (!is.null(legend_text_size)) {
      p <- p + ggplot2::theme(legend.text = ggplot2::element_text(size = legend_text_size))
    }
  }
  if (!is.null(labels_text)) {
    lev <- mixed_sort(final_groups)
    desc <- .dag_label_text(lev, labels_text)
    leg <- ifelse(desc == lev, lev, paste0(lev, ": ", desc))
    p <- p +
      ggplot2::scale_colour_discrete(breaks = lev, labels = leg) +
      ggplot2::labs(colour = NULL) +
      ggplot2::theme(legend.position = "bottom")
  }
  p
}
