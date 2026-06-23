#' Visualize 2D Technological Trajectories from Group Evolution
#'
#' Creates a 2D visualization of technological trajectories based on group similarity metrics,
#' showing the evolution of research groups over time with node size representing group importance
#' and color representing publication-year deviation.
#'
#' A descriptive stock view of one group's lineage network. A group's lineage
#' can share year communities with other groups (soft membership), so this view
#' makes no trajectory claims -- for detection see [sniff_trajectory_braid()].
#'
#' @param groups_lineage A list with components
#'   `groups_similarity` and `groups_attributes`, typically produced by
#'   \code{sniff_groups_lineage()}. The `groups_similarity` element must be
#'   a named list of edge tables (one per group) with at least \code{from}, \code{to}, and \code{weight};
#'   the `groups_attributes` element must be a named list of node tables containing, among others,
#'   \code{network_until}, \code{quantity_papers}, \code{prop_tracked_intra_group}, \code{tracked_documents}, and \code{PY.sd}.
#' @param group The specific group to visualize (default: "c1g1").
#' @param jaccard_similarity Minimum Jaccard similarity threshold for connections (default: 0.1).
#' @param prop_tracked_intra_group_treshold Minimum proportion of tracked intra-group documents
#'   for nodes to be included (default: 0.2).
#' @param label_type Type of labels to display on nodes ("size" for weighted size or "id" for group IDs).
#' @param label_vertical_position Kept for backward compatibility; no longer
#'   applied, as labels are auto-positioned (repelled) with a white halo so they
#'   stay readable over dark nodes and do not overlap.
#' @param label_horizontal_position Kept for backward compatibility; not applied
#'   (see \code{label_vertical_position}).
#' @param label_angle Kept for backward compatibility; not applied (labels are
#'   auto-positioned).
#' @param time_span Optional vector of years to display; if \code{NA}, shows all (default: \code{NA}).
#' @param show_legend Logical indicating whether to show the color legend (default: \code{TRUE}).
#' @param color_by What the node color encodes: \code{"py_deviation"} (default)
#'   colors by the average publication-year deviation (\code{PY.sd});
#'   \code{"final_group_share"} colors each node by the share of its papers that
#'   belong to \code{group}'s last-year cluster (a plasma palette, 0-100\%),
#'   showing how much of each earlier cluster funnels into the final group. The
#'   latter requires \code{docs_per_group} in \code{groups_lineage}.
#'
#' @return A \code{ggplot2} object visualizing the technological trajectories.
#'
#' @examples
#' \dontrun{
#' # Compute trajectories first
#' traj_data <- sniff_groups_lineage(groups_cumulative)
#'
#' # Visualize a specific group (pass the whole object; the function extracts what it needs internally)
#' plot_groups_lineage_2d(
#'   groups_lineage = traj_data,
#'   group = "c1g5",
#'   jaccard_similarity = 0.3
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes theme element_text element_rect scale_x_continuous
#' @importFrom ggplot2 scale_y_reverse coord_flip guides guide_colourbar scale_size labs
#' @importFrom ggraph ggraph geom_edge_fan geom_node_point geom_node_text create_layout
#' @importFrom igraph graph_from_data_frame degree V as_data_frame
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom dplyr filter mutate relocate left_join arrange desc join_by
#' @importFrom tibble as_tibble
#' @importFrom grid unit
#' @family visualization
#' @export
plot_groups_lineage_2d <- function(
  groups_lineage,
  group = "c1g1",
  jaccard_similarity = 0.01,
  prop_tracked_intra_group_treshold = 0.2,
  label_type = "size",
  label_vertical_position = 0,
  label_horizontal_position = 0,
  label_angle = 0,
  time_span = NA,
  show_legend = TRUE,
  color_by = c("py_deviation", "final_group_share")) {

  color_by <- match.arg(color_by)

  groups_similarity <- groups_lineage[["groups_similarity"]]
  groups_attributes <- groups_lineage[["groups_attributes"]]
  docs_per_group <- groups_lineage[["docs_per_group"]]

  # Validate inputs
  if (missing(groups_similarity) || missing(groups_attributes)) {
    stop("Both groups_similarity and groups_attributes must be provided")
  }
  
  if (!is.list(groups_similarity) || !is.list(groups_attributes)) {
    stop("groups_similarity and groups_attributes must be lists")
  }
  
  if (!group %in% names(groups_similarity)) {
    stop("Specified group not found in groups_similarity")
  }
  
  if (!group %in% names(groups_attributes)) {
    stop("Specified group not found in groups_attributes")
  }
  
  if (!label_type %in% c("size", "id")) {
    stop("label_type must be either 'size' or 'id'")
  }
  
  if (jaccard_similarity < 0 || jaccard_similarity > 1) {
    stop("jaccard_similarity must be between 0 and 1")
  }
  
  if (prop_tracked_intra_group_treshold < 0 || prop_tracked_intra_group_treshold > 1) {
    stop("prop_tracked_intra_group_treshold must be between 0 and 1")
  }

  share_lookup <- NULL
  if (color_by == "final_group_share") {
    if (is.null(docs_per_group)) {
      stop("color_by = 'final_group_share' requires 'docs_per_group' in groups_lineage",
           call. = FALSE)
    }
    share_lookup <- .final_group_share_lookup(docs_per_group, group)
  }

  tryCatch({
    # Create network based on Jaccard similarity
    groups_similarity[[group]] |>
      dplyr::filter(weight >= jaccard_similarity) |>
      igraph::graph_from_data_frame() |>
      tidygraph::as_tbl_graph() ->
      net

    # Prepare node attributes
    groups_attributes[[group]] |>
      dplyr::mutate(name = paste0("y", network_until, group)) |>
      dplyr::relocate(name) ->
      dados3

    # Join attributes with network and filter
    net |>
      tidygraph::activate(nodes) |>
      dplyr::left_join(dados3, by = dplyr::join_by(name)) |>
      dplyr::mutate(
        size = quantity_papers * prop_tracked_intra_group,
        color_value = if (color_by == "final_group_share") {
          unname(share_lookup[name])
        } else {
          round(PY.sd, 1)
        }
      ) |>
      dplyr::filter(prop_tracked_intra_group >= prop_tracked_intra_group_treshold, tracked_documents > 1) |>
      dplyr::arrange(dplyr::desc(network_until)) ->
      net2

    # Prepare layout data
    igraph::as_data_frame(net2, what = "vertices") |>
      tibble::as_tibble() ->
      dt

    # Handle time span
    if (any(is.na(time_span))) {
      time_span <- as.character(seq(max(dt$network_until), min(dt$network_until)))
    } else {
      time_span <- as.character(time_span)
    }

    # Create Sugiyama layout
    net2 |>
      tidygraph::activate(nodes) |>
      dplyr::mutate(deg = igraph::degree(net2)) |>
      dplyr::filter(deg != 0) |>
      dplyr::filter(network_until %in% as.numeric(time_span)) |>
      {
        \(x) ggraph::create_layout(x, layout = "sugiyama", layers = V(x)$network_until)
      }() ->
      sugi

    # Color palette, scale and legend depend on color_by
    if (color_by == "final_group_share") {
      # plasma reversed: 100% = dark (core lineage stands out), few = light yellow
      pal_colors <- viridis::viridis(10, option = "C", direction = -1)
      legend_title <- paste0("Share of node papers in final ", group)
      color_scale <- ggplot2::scale_colour_gradientn(
        colors = pal_colors, limits = c(0, 1), labels = scales::label_percent()
      )
    } else {
      pal_colors <- viridis::viridis(10, option = "D", direction = -1)
      legend_title <- "Average Publication Year Deviation"
      color_scale <- ggplot2::scale_colour_gradientn(colors = pal_colors)
    }

    # Base plot
    p <- ggraph::ggraph(sugi) +
      ggraph::geom_edge_fan(ggplot2::aes(alpha = ggplot2::after_stat(index)), show.legend = FALSE) +
      ggraph::geom_node_point(
        ggplot2::aes(
          size = quantity_papers * prop_tracked_intra_group,
          color = color_value
        ),
        show.legend = show_legend,
        stroke = 4
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1,
          size = 18
        ),
        axis.text.y = ggplot2::element_text(
          angle = 0,
          vjust = 0.5,
          hjust = 1,
          size = 18
        ),
        panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
        legend.text = ggplot2::element_text(size = 18),
        legend.title = ggplot2::element_text(size = 18),
        plot.title = ggplot2::element_text(size = 24)
      ) +
      ggplot2::scale_y_reverse(breaks = seq_along(time_span)) +
      ggplot2::scale_x_continuous(
        breaks = seq(
          from = floor(min(sugi$x)) - 1,
          to = ceiling(max(sugi$x)),
          by = 0.5
        )
      ) +
      ggplot2::coord_flip() +
      color_scale +
      ggplot2::guides(
        size = "none",
        edge_alpha = "none",
        colour = ggplot2::guide_colourbar(
          position = "bottom",
          direction = "horizontal",
          title = legend_title,
          title.position = "top",
          barwidth = grid::unit(12, "cm"),
          barheight = grid::unit(0.5, "cm")
        )
      ) +
      ggplot2::scale_size(range = c(0, 10))

    # Add appropriate labels
    label_aes <- if (label_type == "id") {
      ggplot2::aes(label = name)
    } else {
      ggplot2::aes(label = round(quantity_papers * prop_tracked_intra_group))
    }

    # White halo (ggrepel) keeps labels readable over dark nodes in either
    # colour mode; labels stay near their node and de-overlap each other.
    # NB: nudge_x/nudge_y are NOT passed here -- with coord_flip they make
    # ggrepel fling every label into a corner.
    p <- p +
      ggraph::geom_node_text(
        label_aes,
        repel = TRUE,
        bg.color = "white",
        bg.r = 0.18,
        point.size = NA,
        min.segment.length = grid::unit(Inf, "lines"),
        max.overlaps = Inf
      )

    # Final adjustments
    p +
      ggplot2::scale_x_continuous(
        limits = c(min(sugi$x) - 0.1, max(sugi$x) + 0.5)
      ) +
      ggplot2::scale_y_reverse(
        limits = c(length(time_span), 1),
        breaks = seq_along(time_span),
        labels = sort(time_span, decreasing = TRUE)
      ) +
      ggplot2::theme(
        axis.text.y = NULL,
        axis.ticks.length = ggplot2::unit(.001, "cm")
      ) +
      ggplot2::labs(title = group)
    
  }, error = function(e) {
    stop("Error in plot_groups_lineage_2d: ", e$message)
  })
}
