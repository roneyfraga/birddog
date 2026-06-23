#' Plot the group-influence network (the net-influence spine)
#'
#' *Experimental.* Draws the directed influence backbone from a
#' [sniff_groups_influence()] object as a node-link graph: an arrow runs from the
#' **source** (the cited, more foundational group) to the **recipient** (the
#' citing group). By default the edges are the net flow `$net` (\eqn{\nu_{ij} =
#' C_{ij} - C_{ji}}), the "who, on balance, leads whom" spine; nodes are coloured
#' by role (source / broker / sink) and sized by citation activity.
#'
#' @param influence A [sniff_groups_influence()] object.
#' @param weight Which edges and weights to draw: `"net"` (default, the net-flow
#'   spine `$net`, one arrow per connected pair), `"gross"` (every directed
#'   channel, weighted by citation count), or `"surprise"` (every directed
#'   channel, weighted by the size-null surprise).
#' @param min_weight Drop edges whose weight is below this (default `0`).
#' @param node_size How to size the group nodes: `"equal"` (default), `"io"`
#'   (citations made plus received) or `"balance"` (the absolute balance
#'   \eqn{|\beta|}).
#' @param colour_role Colour nodes by their source / broker / sink role
#'   (default `TRUE`).
#' @param edge_labels Print the weight on each edge (default `TRUE`).
#' @param edge_digits Decimal places for the edge weight labels; `NULL`
#'   (default) shows integers for `"net"`/`"gross"` and two decimals for the
#'   continuous `"surprise"` weight.
#' @param labels Print the group label on each node (default `TRUE`).
#' @param label_size Font size of the node labels; `NULL` (default) auto-sizes
#'   the nodes and the text so even the smallest node holds its group id.
#' @param layout A `ggraph`/`igraph` layout name (default `"sugiyama"`, the
#'   layered DAG layout).
#' @param title Plot title; `NULL` (default) removes it.
#'
#' @return A `ggplot` object.
#'
#' @seealso [sniff_groups_influence()], [plot_groups_influence_matrix()]
#'
#' @examples
#' \dontrun{
#' infl <- sniff_groups_influence(groups)
#' plot_groups_influence_network(infl)
#' plot_groups_influence_network(infl, weight = "gross", min_weight = 50)
#' }
#'
#' @family visualization
#' @export
#' @importFrom igraph graph_from_data_frame
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph geom_edge_arc geom_node_point geom_node_text circle
#' @importFrom ggraph scale_edge_width_continuous
#' @importFrom ggplot2 aes arrow unit scale_colour_manual scale_size_continuous
#' @importFrom ggplot2 labs theme_void guides guide_legend
#' @importFrom rlang .data
plot_groups_influence_network <- function(influence,
                                          weight = c("net", "gross", "surprise"),
                                          min_weight = 0,
                                          node_size = c("equal", "io", "balance"),
                                          colour_role = TRUE,
                                          edge_labels = TRUE, edge_digits = NULL,
                                          labels = TRUE, label_size = NULL,
                                          layout = "sugiyama", title = NULL) {
  weight <- match.arg(weight)
  node_size <- match.arg(node_size)
  if (!is_influence(influence)) {
    stop("'influence' must be a sniff_groups_influence() object.", call. = FALSE)
  }

  # edges: from = source (cited), to = recipient (citing)
  if (weight == "net") {
    e <- data.frame(from = influence$net$from, to = influence$net$to,
                    weight = influence$net$net, stringsAsFactors = FALSE)
  } else {
    fl <- influence$flow[influence$flow$influencer != influence$flow$recipient, ,
                         drop = FALSE]
    w <- if (weight == "gross") fl$citations else fl$surprise
    e <- data.frame(from = fl$influencer, to = fl$recipient, weight = w,
                    stringsAsFactors = FALSE)
  }
  e <- e[is.finite(e$weight) & e$weight >= min_weight, , drop = FALSE]
  if (nrow(e) == 0) {
    stop("no influence edges to draw at this 'weight'/'min_weight'.", call. = FALSE)
  }
  edge_digits <- if (is.null(edge_digits)) {
    if (weight == "surprise") 2L else 0L
  } else edge_digits
  e$elabel <- formatC(e$weight, format = "f", digits = edge_digits)

  g_tbl <- influence$groups
  verts <- data.frame(
    name = g_tbl$group,
    role = factor(g_tbl$role, levels = c("source", "broker", "sink")),
    balance = g_tbl$balance,
    nsize = switch(node_size,
                   io      = g_tbl$received + g_tbl$made,
                   balance = abs(g_tbl$balance),
                   equal   = 1),
    stringsAsFactors = FALSE
  )

  gg <- igraph::graph_from_data_frame(e, vertices = verts, directed = TRUE) |>
    tidygraph::as_tbl_graph()

  # size the nodes to hold the longest group id, and the text to fit that node,
  # so even the smallest vertex contains its own label
  lmax <- max(nchar(verts$name))
  fit  <- max(14, 4.4 * lmax)
  lab  <- if (is.null(label_size)) min(4.2, 0.73 * fit / lmax) else label_size
  cap  <- ggraph::circle(fit * 0.42, "mm")

  role_cols <- c(source = "#d7191c", broker = "grey60", sink = "#2c7bb6")

  p <- ggraph::ggraph(gg, layout = layout) +
    ggraph::geom_edge_arc(
      ggplot2::aes(width = .data$weight,
                   label = if (edge_labels) .data$elabel else NULL),
      strength = 0.12, colour = "grey55",
      arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
      end_cap = cap, start_cap = cap,
      angle_calc = "along", label_dodge = ggplot2::unit(2.5, "mm")) +
    ggraph::scale_edge_width_continuous(range = c(0.5, 2.6), guide = "none")

  size_aes <- node_size != "equal"
  np <- if (size_aes && isTRUE(colour_role)) {
    ggraph::geom_node_point(ggplot2::aes(size = .data$nsize, colour = .data$role))
  } else if (size_aes) {
    ggraph::geom_node_point(ggplot2::aes(size = .data$nsize), colour = "steelblue")
  } else if (isTRUE(colour_role)) {
    ggraph::geom_node_point(ggplot2::aes(colour = .data$role), size = fit)
  } else {
    ggraph::geom_node_point(size = fit, colour = "steelblue")
  }
  p <- p + np
  if (isTRUE(colour_role)) {
    p <- p + ggplot2::scale_colour_manual(values = role_cols, drop = FALSE, name = "role")
  }
  # the smallest variable node is `fit`, so its label still fits
  if (size_aes) p <- p + ggplot2::scale_size_continuous(range = c(fit, fit + 9), guide = "none")

  if (isTRUE(labels)) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = .data$name),
                                    colour = "white", fontface = "bold", size = lab)
  }

  p +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(size = 6))) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 12)
}
