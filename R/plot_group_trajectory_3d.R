#' Visualize 3D Technological Trajectories from Group Evolution
#'
#' Creates an interactive 3D visualization of technological trajectories showing the evolution
#' of research groups over time with node size representing group importance and color
#' representing publication year deviation.
#'
#' @param groups_cumulative_trajectories A list containing two components:
#'        \itemize{
#'          \item groups_similarity: Similarity data between groups
#'          \item groups_attributes: Attribute data for each group
#'        }
#' @param group The specific group to visualize (default: "component1_g01")
#' @param jaccard_similarity Minimum Jaccard similarity threshold for connections (default: 0.1)
#' @param prop_tracked_intra_group_treshold Minimum proportion of tracked intra-group documents
#'        for nodes to be included (default: 0.2)
#' @param label_type Type of labels to display on nodes ("size" for weighted size or "id" for group IDs)
#' @param label_vertical_position Vertical adjustment for node labels (default: 0)
#' @param label_horizontal_position Horizontal adjustment for node labels (default: 0)
#' @param label_angle Angle for node labels (default: 0)
#' @param time_span Optional vector specifying the time span to display (default: NA shows all years)
#' @param show_legend Logical indicating whether to show the color legend (default: TRUE)
#' @param last_year_keywords Optional keywords description for the last year (default: NULL)
#'
#' @return A plotly 3D visualization object
#'
#' @examples
#' \dontrun{
#' # First get trajectory data
#' traj_data <- sniff_groups_trajectories(groups_cumulative)
#'
#' # Visualize a specific group in 3D
#' plot_group_trajectory_3d(
#'   groups_cumulative_trajectories = traj_data,
#'   group = "component1_g05",
#'   jaccard_similarity = 0.2
#' )
#' }
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom igraph graph_from_data_frame V as_edgelist
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom dplyr filter mutate relocate left_join arrange desc select
#' @importFrom tibble as_tibble
#' @importFrom ggraph create_layout
#' @export
plot_group_trajectories_3d <- function(
  groups_cumulative_trajectories,
  group = "component1_g01",
  jaccard_similarity = 0.1,
  prop_tracked_intra_group_treshold = 0.2,
  label_type = "size",
  label_vertical_position = 0,
  label_horizontal_position = 0,
  label_angle = 0,
  time_span = NA,
  show_legend = TRUE,
  last_year_keywords = NULL) {

  # Extract components from input
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

  tryCatch(
    {
      # Create network based on Jaccard similarity
      net <- groups_similarity[[group]] |>
        dplyr::filter(weight >= jaccard_similarity) |>
        igraph::graph_from_data_frame() |>
        tidygraph::as_tbl_graph()

      # Prepare node attributes
      dados3 <- groups_attributes[[group]] |>
        dplyr::mutate(name = paste0("y", network_until, gsub("^.*_", "", group))) |>
        dplyr::relocate(name)

      # Join attributes with network and filter
      net2 <- net |>
        tidygraph::activate(nodes) |>
        dplyr::left_join(dados3, by = dplyr::join_by(name)) |>
        dplyr::mutate(size = quantity_papers * prop_tracked_intra_group) |>
        dplyr::filter(
          prop_tracked_intra_group >= prop_tracked_intra_group_treshold,
          tracked_documents > 1
        ) |>
        dplyr::arrange(dplyr::desc(network_until))

      # Create Sugiyama layout for positioning
      sugi <- net2 |>
        tidygraph::activate(nodes) |>
        {
          \(x) ggraph::create_layout(x, layout = "sugiyama", layers = V(x)$network_until)
        }()

      # Prepare layout data
      sugi2 <- sugi |>
        dplyr::select(x, y, name, year = network_until, size, orig_index = .ggraph.orig_index) |>
        dplyr::arrange(year)

      # Add layout positions to network
      net3 <- net2 |>
        tidygraph::as_tbl_graph() |>
        tidygraph::activate(nodes) |>
        dplyr::mutate(sugiyama = sugi$x) |>
        dplyr::arrange(network_until)

      # Prepare node data for visualization
      g <- net3 |>
        tidygraph::activate(nodes) |>
        dplyr::mutate(average_age_sd = network_until - average_age_group) |>
        dplyr::select(
          id = name,
          z = size,
          y = sugiyama,
          x = network_until,
          keywords = keywords,
          py_sd = PY.sd,
          py = network_until,
          publications = quantity_papers
        ) |>
        dplyr::mutate(keywords = gsub("^.*?;", "", keywords))

      # Extract node and edge data
      nodes <- g |>
        tidygraph::activate(nodes) |>
        tibble::as_tibble()

      edges <- as.data.frame(igraph::as_edgelist(g))

      # Build edge lines for 3D visualization
      edge_x <- edge_y <- edge_z <- numeric(0)

      for (i in seq_len(nrow(edges))) {
        node1 <- nodes[edges[i, 1], ]
        node2 <- nodes[edges[i, 2], ]

        edge_x <- c(edge_x, node1$x, node2$x, NA) # NA creates breaks
        edge_y <- c(edge_y, node1$y, node2$y, NA)
        edge_z <- c(edge_z, node1$z, node2$z, NA)
      }

      # Define camera settings for initial view
      camera_settings <- list(
        eye = list(
          x = -1.5,
          y = -2,
          z = 1
        )
      )

      # Set up color palette
      viridis_colors <- viridis::viridis(10, option = "D", direction = -1)

      # Prepare hover text
      g_df <- g |>
        tidygraph::activate(nodes) |>
        tibble::as_tibble() |>
        dplyr::mutate(text = paste(
          "ID = ", id, "<br>",
          "Publications = ", publications, "<br>",
          "Deviation from Current Year = ", round(py_sd, digits = 1), "<br>",
          "Keywords = ", gsub(";", ", ", keywords),
          sep = ""
        ))

      # Update keywords for last year if provided
      if (!is.null(last_year_keywords)) {
        g_df <- g_df |>
          dplyr::mutate(text = gsub("Keywords = NA", paste0("Description = ", last_year_keywords), text))
      }

      # Create the 3D plot
      plot_ly() |>
        # Add edges
        add_trace(
          x = edge_x, y = edge_y, z = edge_z,
          type = "scatter3d", mode = "lines",
          line = list(color = "gray", width = 3),
          hoverinfo = "none"
        ) |>
        # Add nodes
        add_trace(
          x = nodes$x, y = nodes$y, z = nodes$z,
          type = "scatter3d",
          mode = "markers",
          text = g_df$text,
          marker = list(
            size = scales::rescale(nodes$z, to = c(5, 20)),
            opacity = 0.9,
            color = igraph::V(g)$py_sd,
            colorscale = list(
              list(0, viridis_colors[1]),
              list(1, viridis_colors[10])
            ),
            showscale = show_legend,
            colorbar = list(
              title = "Pub Year Deviation",
              tickfont = list(size = 12),
              titlefont = list(size = 14)
            )
          ),
          hoverinfo = "text",
          hoverlabel = list(font = list(size = 20))
        ) |>
        # Layout settings
        layout(
          showlegend = FALSE,
          title = list(
            text =  group,
            font = list(size = 24),
            x = 0.05,
            xanchor = "left",
            y = 0.95
          ),
          scene = list(
            xaxis = list(
              title = "Time",
              titlefont = list(size = 22),
              tickfont = list(size = 13)
            ),
            yaxis = list(
              title = "Route",
              titlefont = list(size = 22),
              tickfont = list(size = 13)
            ),
            zaxis = list(
              title = "Size",
              titlefont = list(size = 22),
              tickfont = list(size = 13)
            ),
            camera = camera_settings
          ),
          margin = list(l = 0, r = 0, b = 0, t = 50)
        )
    },
    error = function(e) {
      stop("Error in plot_group_trajectory_3d: ", e$message)
    }
  )
}
