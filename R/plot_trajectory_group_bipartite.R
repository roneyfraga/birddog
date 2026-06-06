#' Plot the trajectory x group bipartite network
#'
#' A two-column bipartite layout: trajectories on the left, final groups on the
#' right, with diagonal edges whose width is the number of shared papers. A thin
#' convenience view; for matrix-style (`bipartite::visweb`) or nestedness plots,
#' run the `bipartite` package on `contribution$incidence` directly.
#'
#' @param contribution Output of [sniff_trajectory_group_contribution()].
#' @param max_edges Optional integer. If set and the graph has more edges, keep
#'   only the `max_edges` heaviest edges (by shared papers) before drawing, so a
#'   dense matrix stays readable. All vertices are kept. Default `NULL` (no cap).
#' @param label_size Node label text size (default 3).
#' @return A ggplot object.
#'
#' @seealso [sniff_trajectory_group_contribution()]
#'
#' @export
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph create_layout geom_edge_diagonal geom_node_point
#' @importFrom ggraph geom_node_text scale_edge_width
#' @importFrom ggplot2 aes theme_void
#' @importFrom igraph V E ecount subgraph_from_edges
plot_trajectory_group_bipartite <- function(contribution, max_edges = NULL, label_size = 3) {
  if (!is.list(contribution) || is.null(contribution$graph) ||
      !igraph::is_igraph(contribution$graph)) {
    stop("'contribution' must be the output of sniff_trajectory_group_contribution()",
         call. = FALSE)
  }
  g <- contribution$graph
  if (!is.null(max_edges) && igraph::ecount(g) > max_edges) {
    keep <- order(igraph::E(g)$weight, decreasing = TRUE)[seq_len(max_edges)]
    g <- igraph::subgraph_from_edges(g, igraph::E(g)[keep], delete.vertices = FALSE)
  }
  tg <- tidygraph::as_tbl_graph(g)
  types <- igraph::V(g)$type                     # FALSE = trajectory, TRUE = group
  x <- ifelse(types, 1, 0)
  y <- stats::ave(seq_along(types), types, FUN = seq_along)
  lay <- ggraph::create_layout(tg, layout = "manual", x = x, y = y)

  ggraph::ggraph(lay) +
    ggraph::geom_edge_diagonal(ggplot2::aes(width = weight), alpha = 0.4) +
    ggraph::geom_node_point(ggplot2::aes(color = type), size = 3, show.legend = FALSE) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name),
      hjust = ifelse(types, 0, 1), size = label_size
    ) +
    ggraph::scale_edge_width(range = c(0.3, 3), guide = "none") +
    ggplot2::theme_void()
}
