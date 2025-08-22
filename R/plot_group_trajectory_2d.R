#' Visualize 2D Technological Trajectories from Group Evolution
#'
#' Creates a 2D visualization of technological trajectories based on group similarity metrics,
#' showing the evolution of research groups over time with node size representing group importance
#' and color representing publication-year deviation.
#'
#' @param groups_cumulative_trajectories A list with components
#'   `groups_similarity` and `groups_attributes`, typically produced by
#'   \code{plot_groups_trajectories()}. The `groups_similarity` element must be
#'   a named list of edge tables (one per group) with at least \code{from}, \code{to}, and \code{weight};
#'   the `groups_attributes` element must be a named list of node tables containing, among others,
#'   \code{network_until}, \code{quantity_papers}, \code{prop_tracked_intra_group}, \code{tracked_documents}, and \code{PY.sd}.
#' @param group The specific group to visualize (default: "component1_g01").
#' @param jaccard_similarity Minimum Jaccard similarity threshold for connections (default: 0.1).
#' @param prop_tracked_intra_group_treshold Minimum proportion of tracked intra-group documents
#'   for nodes to be included (default: 0.2).
#' @param label_type Type of labels to display on nodes ("size" for weighted size or "id" for group IDs).
#' @param label_vertical_position Vertical adjustment for node labels (default: 0).
#' @param label_horizontal_position Horizontal adjustment for node labels (default: 0).
#' @param label_angle Angle for node labels (default: 0).
#' @param time_span Optional vector of years to display; if \code{NA}, shows all (default: \code{NA}).
#' @param show_legend Logical indicating whether to show the color legend (default: \code{TRUE}).
#'
#' @return A \code{ggplot2} object visualizing the technological trajectories.
#'
#' @examples
#' \dontrun{
#' # Compute trajectories first
#' traj_data <- plot_groups_trajectories(groups_cumulative)
#'
#' # Visualize a specific group (pass the whole object; the function extracts what it needs internally)
#' plot_group_trajectories_2d(
#'   groups_cumulative_trajectories = traj_data,
#'   group = "component1_g05",
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
#' @export
plot_group_trajectories_2d <- function(
  groups_cumulative_trajectories,
  group = "component1_g01",
  jaccard_similarity = 0.1,
  prop_tracked_intra_group_treshold = 0.2,
  label_type = "size",
  label_vertical_position = 0,
  label_horizontal_position = 0,
  label_angle = 0,
  time_span = NA,
  show_legend = TRUE) {

  groups_similarity <- groups_cumulative_trajectories[["groups_similarity"]]
  groups_attributes <- groups_cumulative_trajectories[["groups_attributes"]]

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

  tryCatch({
    # Create network based on Jaccard similarity
    groups_similarity[[group]] |>
      dplyr::filter(weight >= jaccard_similarity) |>
      igraph::graph_from_data_frame() |>
      tidygraph::as_tbl_graph() ->
      net

    # Prepare node attributes
    groups_attributes[[group]] |>
      dplyr::mutate(name = paste0("y", network_until, gsub("^.*_", "", group))) |>
      dplyr::relocate(name) ->
      dados3

    # Join attributes with network and filter
    net |>
      tidygraph::activate(nodes) |>
      dplyr::left_join(dados3, by = dplyr::join_by(name)) |>
      dplyr::mutate(size = quantity_papers * prop_tracked_intra_group) |>
      dplyr::filter(
        prop_tracked_intra_group >= prop_tracked_intra_group_treshold,
        tracked_documents > 1
      ) |>
      dplyr::arrange(dplyr::desc(network_until)) ->
      net2

    # Prepare layout data
    dt <- igraph::as_data_frame(net2, what = "vertices") |>
      tibble::as_tibble()

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

    # Set up color palette
    viridis_colors <- viridis::viridis(10, option = "D", direction = -1)

    # Base plot
    p <- ggraph::ggraph(sugi) +
      ggraph::geom_edge_fan(ggplot2::aes(alpha = ggplot2::after_stat(index)), show.legend = FALSE) +
      ggraph::geom_node_point(
        ggplot2::aes(
          size = quantity_papers * prop_tracked_intra_group,
          color = round(PY.sd, 1)
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
      ggplot2::scale_colour_gradientn(colors = viridis_colors) +
      ggplot2::guides(
        size = "none",
        edge_alpha = "none",
        colour = ggplot2::guide_colourbar(
          position = "bottom",
          direction = "horizontal",
          title = "Average Publication Year Deviation"
        )
      ) +
      ggplot2::scale_size(range = c(0, 10))

    # Add appropriate labels
    if (label_type == "id") {
      p <- p +
        ggraph::geom_node_text(
          ggplot2::aes(label = name),
          vjust = label_vertical_position,
          hjust = label_horizontal_position,
          angle = label_angle,
          check_overlap = TRUE
        )
    } else {
      p <- p +
        ggraph::geom_node_text(
          ggplot2::aes(label = round(quantity_papers * prop_tracked_intra_group)),
          vjust = label_vertical_position,
          hjust = label_horizontal_position,
          angle = label_angle,
          check_overlap = TRUE
        )
    }

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
      ggplot2::labs(title = gsub("component01_", "", group))
    
  }, error = function(e) {
    stop("Error in plot_group_trajectories_2d: ", e$message)
  })
}
