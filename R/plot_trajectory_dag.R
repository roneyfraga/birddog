#' Plot the soft trajectory DAG (all intermediate nodes)
#'
#' The braided-river panorama: every cluster-year node laid out by year (x), with
#' succession edges, node colour = `terminal_group` (or `group`), node size =
#' paper count, and births ringed. Uses the year-aware Sugiyama layout from
#' [sniff_trajectory_dag()]'s graph.
#'
#' @param dag A [sniff_trajectory_dag()] object.
#' @param color_by `"terminal_group"` (default) or `"group"` — the node colour.
#' @param edge_alpha Base edge transparency, scaled by Jaccard weight (default 0.25).
#' @param point_size Base node size (default 2.5).
#' @param show_legend Show the colour legend (default `FALSE`; 18 final groups
#'   make a busy legend).
#' @return A `ggplot` object.
#' @seealso [sniff_trajectory_dag()]
#' @export
#' @importFrom ggraph ggraph geom_edge_link geom_node_point scale_edge_alpha
#' @importFrom ggplot2 aes scale_size scale_x_continuous labs theme_minimal theme
#' @importFrom ggplot2 element_blank element_text
#' @importFrom igraph is_igraph vcount V
#' @importFrom rlang .data
plot_trajectory_dag <- function(dag, color_by = c("terminal_group", "group"),
                                edge_alpha = 0.25, point_size = 2.5,
                                show_legend = FALSE) {
  color_by <- match.arg(color_by)
  if (!is.list(dag) || is.null(dag$graph) || !igraph::is_igraph(dag$graph)) {
    stop("'dag' must be the output of sniff_trajectory_dag()", call. = FALSE)
  }
  if (igraph::vcount(dag$graph) == 0) {
    stop("the DAG has no nodes", call. = FALSE)
  }
  ml <- mk_layout_and_year_scale(dag$graph)
  lay <- ml$lay

  ggraph::ggraph(lay) +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_alpha = .data$weight),
      edge_colour = "grey55", show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(colour = .data[[color_by]], size = .data$size),
      show.legend = show_legend
    ) +
    ggraph::geom_node_point(
      data = function(d) d[d$is_birth %in% TRUE, , drop = FALSE],
      shape = 21, colour = "black", fill = NA,
      size = point_size + 1.6, stroke = 0.5, show.legend = FALSE
    ) +
    ggraph::scale_edge_alpha(range = c(0.08, 0.6), guide = "none") +
    ggplot2::scale_size(range = c(1, 6), guide = "none") +
    ggplot2::scale_x_continuous(breaks = ml$all_breaks, labels = ml$all_labels) +
    ggplot2::labs(x = "Year", y = NULL, title = "Soft trajectory DAG",
                  colour = color_by) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = ggplot2::element_blank()
    )
}
