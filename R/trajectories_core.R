#' Build temporal directed acyclic graph from trajectory data
#'
#' Constructs a DAG from group trajectory data by filtering edges based on
#' Jaccard similarity and node attributes, then keeping only the strongest
#' outgoing connections per node.
#'
#' @param groups_cumulative_trajectories List with `groups_similarity` and
#'   `groups_attributes` components
#' @param group Character ID of the group to process (e.g., "component1_g01")
#' @param jaccard_min Minimum Jaccard similarity for edges (default: 0.05)
#' @param intra_min Minimum proportion of tracked documents within group for
#'   nodes (default: 0.10)
#' @param k_out Maximum number of outgoing edges to keep per node (default: 2)
#'
#' @return An igraph object representing the temporal DAG
#' @keywords internal
#' @importFrom igraph graph_from_data_frame make_empty_graph
#' @importFrom dplyr filter mutate relocate arrange desc group_by slice_head ungroup
build_temporal_dag <- function(
  groups_cumulative_trajectories,
  group,
  jaccard_min = 0.05,
  intra_min = 0.10,
  k_out = 2) {
  # Input validation
  if (!group %in% names(groups_cumulative_trajectories$groups_similarity)) {
    stop("Group '", group, "' not found in groups_similarity", call. = FALSE)
  }
  if (!group %in% names(groups_cumulative_trajectories$groups_attributes)) {
    stop("Group '", group, "' not found in groups_attributes", call. = FALSE)
  }

  # Extract and filter similarity data
  sim_data <- groups_cumulative_trajectories$groups_similarity[[group]] %>%
    dplyr::filter(.data$weight >= jaccard_min)

  # Process node attributes
  node_attrs <- groups_cumulative_trajectories$groups_attributes[[group]] %>%
    dplyr::mutate(name = paste0(
      "y", .data$network_until,
      gsub("^.*_", "", .data$group)
    )) %>%
    dplyr::relocate(.data$name) %>%
    dplyr::filter(
      .data$prop_tracked_intra_group >= intra_min,
      .data$tracked_documents > 1
    )

  # Filter edges to consecutive years and keep top k_out per node
  edge_data <- sim_data %>%
    dplyr::mutate(
      y_from = .extract_year(.data$from),
      y_to = .extract_year(.data$to)
    ) %>%
    dplyr::filter(.data$y_to == .data$y_from + 1) %>%
    dplyr::arrange(dplyr::desc(.data$weight), dplyr::desc(.data$documents)) %>%
    dplyr::group_by(.data$from) %>%
    dplyr::slice_head(n = k_out) %>%
    dplyr::ungroup()

  # Filter edges to only include valid nodes
  valid_nodes <- node_attrs$name
  final_edges <- edge_data %>%
    dplyr::filter(.data$from %in% valid_nodes, .data$to %in% valid_nodes)

  # Return empty graph if no valid edges or nodes
  if (nrow(final_edges) == 0 || nrow(node_attrs) == 0) {
    return(igraph::make_empty_graph(n = 0, directed = TRUE))
  }

  igraph::graph_from_data_frame(final_edges, directed = TRUE, vertices = node_attrs)
}

#' @keywords internal
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Replace NA values with zero
#'
#' @param x Numeric vector
#' @return Vector with NA values replaced by 0
#' @keywords internal
.na_to_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

#' Extract year from node name
#'
#' @param x Character vector of node names (e.g., "y2005g01")
#' @return Integer vector of years
#' @keywords internal
.extract_year <- function(x) {
  as.integer(sub("^y(\\d{4}).*$", "\\1", x))
}

#' @keywords internal
#' @importFrom igraph vcount ecount V E
.extract_year <- function(x) {
  as.integer(sub("^y(\\d{4}).*$", "\\1", x))
}

#' @keywords internal
.na_to_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

#' Score nodes and edges for trajectory detection
#'
#' Computes node scores based on paper quantity and proportion tracked,
#' and edge scores based on similarity and document overlap.
#'
#' @param g igraph object
#' @param alpha Weight for edge strength in scoring (default: 1)
#' @param beta Per-step persistence bonus (default: 0.1)
#'
#' @return Modified igraph with node_score and edge_score attributes
#' @keywords internal
#' @importFrom igraph vcount ecount V E
score_nodes_edges <- function(g, alpha = 1, beta = 0.1) {
  if (igraph::vcount(g) == 0) {
    return(g)
  }

  # Node scoring: log(1 + quantity * proportion)
  quantity <- .na_to_zero(igraph::V(g)$quantity_papers %||% NA_real_)
  proportion <- .na_to_zero(igraph::V(g)$prop_tracked_intra_group %||% NA_real_)
  igraph::V(g)$node_score <- log1p(quantity * proportion)

  # Edge scoring: weight * log(1 + documents)
  if (igraph::ecount(g) > 0) {
    weight <- .na_to_zero(igraph::E(g)$weight %||% NA_real_)
    documents <- .na_to_zero(igraph::E(g)$documents %||% NA_real_)
    igraph::E(g)$edge_score <- weight * log1p(documents)
  } else {
    igraph::E(g)$edge_score <- numeric(0)
  }

  # Store scoring parameters as graph attributes
  attr(g, "alpha") <- alpha
  attr(g, "beta") <- beta

  g
}

#' Find heaviest path in directed acyclic graph
#'
#' Uses dynamic programming to find the highest-scoring path in a DAG
#' where scores combine node quality and edge strength.
#'
#' @param g igraph object with node_score and edge_score attributes
#'
#' @return List with path nodes, edges, and total score
#' @keywords internal
#' @importFrom igraph incident ends vcount V E
heaviest_path_dag <- function(g) {
  if (igraph::vcount(g) == 0) {
    return(list(
      nodes = character(0),
      edges = igraph::E(g)[0],
      total = -Inf
    ))
  }

  # Extract scoring parameters
  alpha <- attr(g, "alpha") %||% 1
  beta <- attr(g, "beta") %||% 0.1

  # Initialize scoring arrays
  node_names <- igraph::V(g)$name
  years <- .extract_year(node_names)
  order_idx <- order(years, node_names)

  score <- igraph::V(g)$node_score
  parent <- rep(NA_integer_, igraph::vcount(g))

  # Dynamic programming: find optimal paths
  for (v in order_idx) {
    incoming_edges <- igraph::incident(g, v, mode = "in")
    if (length(incoming_edges) == 0) next

    for (edge in incoming_edges) {
      edge_ends <- igraph::ends(g, edge, names = FALSE)
      u <- edge_ends[1]

      candidate_score <- score[u] + igraph::V(g)$node_score[v] +
        alpha * igraph::E(g)$edge_score[edge] + beta

      if (!is.na(candidate_score) && candidate_score > score[v]) {
        score[v] <- candidate_score
        parent[v] <- u
      }
    }
  }

  # Backtrack from highest-scoring node
  end_vertex <- which.max(score)
  if (!is.finite(score[end_vertex])) {
    return(list(
      nodes = character(0),
      edges = igraph::E(g)[0],
      total = -Inf
    ))
  }

  path_vertex_ids <- integer()
  current <- end_vertex
  while (!is.na(current)) {
    path_vertex_ids <- c(current, path_vertex_ids)
    current <- parent[current]
  }

  list(
    nodes = node_names[path_vertex_ids],
    edges = igraph::E(g, path = path_vertex_ids),
    total = score[end_vertex]
  )
}

#' Attach document IDs to graph vertices
#'
#' Adds document ID lists to each vertex in the graph based on the
#' group-document mapping.
#'
#' @param g igraph object
#' @param docs_tbl Tibble with columns `group_id` and `document_id`
#'
#' @return Modified igraph with `doc_ids` vertex attribute
#' @export
#' @importFrom dplyr group_by summarise
#' @importFrom rlang set_names
attach_docs_to_vertices <- function(g, docs_tbl) {
  if (is.null(docs_tbl) || nrow(docs_tbl) == 0 || igraph::vcount(g) == 0) {
    return(g)
  }

  # Create mapping from group_id to document IDs
  doc_mapping <- docs_tbl %>%
    dplyr::group_by(.data$group_id) %>%
    dplyr::summarise(doc_ids = list(unique(.data$document_id)), .groups = "drop") %>%
    tibble::deframe()

  # Attach document lists to vertices
  igraph::V(g)$doc_ids <- lapply(igraph::V(g)$name, function(vertex_id) {
    doc_mapping[[vertex_id]] %||% character(0)
  })

  g
}

#' Count unique documents along a path
#'
#' Calculates the number of unique documents covered by a trajectory path,
#' accounting for document overlap between connected nodes.
#'
#' @param g igraph object with document information
#' @param path_nodes Character vector of node names along the path
#' @param path_edges Edge sequence along the path
#'
#' @return Integer count of unique documents
#' @keywords internal
.count_unique_docs_on_path <- function(g, path_nodes, path_edges) {
  # Use document IDs if available
  if (!is.null(igraph::V(g)$doc_ids)) {
    doc_lists <- igraph::V(g)$doc_ids[match(path_nodes, igraph::V(g)$name)]
    doc_lists <- doc_lists[!vapply(doc_lists, is.null, FUN.VALUE = logical(1))]
    return(length(unique(unlist(doc_lists, use.names = FALSE))))
  }

  # Fallback: estimate from node counts and edge overlaps
  node_counts <- .na_to_zero(
    igraph::V(g)$tracked_documents[match(path_nodes, igraph::V(g)$name)] %||% 0
  )
  edge_overlap <- .na_to_zero(
    if (length(path_edges) > 0) igraph::E(g)[path_edges]$documents %||% 0 else 0
  )

  unique_count <- sum(node_counts, na.rm = TRUE) - sum(edge_overlap, na.rm = TRUE)
  max(0, unique_count)
}

#' Extract top trajectories from graph
#'
#' Iteratively extracts the highest-scoring disjoint trajectories from the graph.
#'
#' @param g igraph object with scoring attributes
#' @param M Maximum number of trajectories to extract (default: 5)
#' @param min_len Minimum number of distinct years for valid trajectory (default: 3)
#'
#' @return Tibble of trajectory information
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom igraph delete_vertices vcount ecount V E
extract_top_trajectories <- function(g, M = 5, min_len = 3) {
  trajectories <- list()
  current_graph <- g

  for (i in seq_len(M)) {
    if (igraph::vcount(current_graph) == 0) break

    path <- heaviest_path_dag(current_graph)
    path_years <- if (length(path$nodes) > 0) .extract_year(path$nodes) else integer(0)

    # Check trajectory validity
    if (length(unique(path_years)) < min_len || !is.finite(path$total)) break

    # Calculate trajectory metrics
    edge_scores <- if (length(path$edges) > 0) {
      igraph::E(current_graph)[path$edges]$edge_score
    } else {
        numeric(0)
      }

    node_sizes <- igraph::V(current_graph)$quantity_papers *
      igraph::V(current_graph)$prop_tracked_intra_group
    matched_sizes <- node_sizes[match(path$nodes, igraph::V(current_graph)$name)]

    unique_docs <- .count_unique_docs_on_path(current_graph, path$nodes, path$edges)

    # Store trajectory information
    trajectories[[i]] <- tibble::tibble(
      traj_id = paste0("tr", i),
      start = min(path_years),
      end = max(path_years),
      length = length(unique(path_years)),
      nodes = list(path$nodes),
      score = path$total,
      mean_w = mean(edge_scores, na.rm = TRUE),
      sum_docs = unique_docs,
      mean_size = mean(.na_to_zero(matched_sizes), na.rm = TRUE),
      mean_PYsd = mean(igraph::V(current_graph)$PY.sd[
        match(path$nodes, igraph::V(current_graph)$name)
        ], na.rm = TRUE)
    )

    # Remove used nodes for next iteration
    current_graph <- igraph::delete_vertices(current_graph, path$nodes)
  }

  dplyr::bind_rows(trajectories)
}

#' Assign trajectory-specific edge attributes
#'
#' Computes edge-level trajectory identifiers and widths based on
#' cumulative paper counts along each trajectory path.
#'
#' @param g igraph object
#' @param tr_tbl Tibble of trajectories with `traj_id` and `nodes` columns
#' @param width_range Numeric range for edge width scaling (default: c(0.8, 6.0))
#' @param use_raw_papers Whether to use raw paper counts (TRUE) or
#'   weighted counts (FALSE) for width calculation
#'
#' @return Modified igraph with `traj_id` and `traj_width` edge attributes
#' @keywords internal
#' @importFrom scales rescale
assign_traj_edge_widths <- function(
  g,
  tr_tbl,
  width_range = c(0.8, 6.0),
  use_raw_papers = FALSE) {
  if (igraph::vcount(g) == 0 || is.null(tr_tbl) || !nrow(tr_tbl)) {
    return(g)
  }

  # Initialize edge attributes
  igraph::E(g)$traj_id <- NA_character_
  igraph::E(g)$traj_width <- NA_real_

  # Calculate node measure for paper counts
  node_measure <- if (use_raw_papers) {
    igraph::V(g)$quantity_papers %||% 0
  } else {
      (igraph::V(g)$quantity_papers %||% 0) * (igraph::V(g)$prop_tracked_intra_group %||% 0)
    }
  node_names <- igraph::V(g)$name

  # Process each trajectory
  for (i in seq_len(nrow(tr_tbl))) {
    traj_id <- tr_tbl$traj_id[i]
    traj_nodes <- tr_tbl$nodes[[i]]

    if (length(traj_nodes) < 2) next

    # Order nodes chronologically
    node_order <- order(.extract_year(traj_nodes), traj_nodes)
    ordered_nodes <- traj_nodes[node_order]

    # Calculate cumulative paper counts
    node_sizes <- node_measure[match(ordered_nodes, node_names)]
    node_sizes[is.na(node_sizes)] <- 0
    cumulative_sizes <- cumsum(node_sizes)

    # Get edges along trajectory path
    path_edges <- igraph::E(g, path = ordered_nodes)
    edge_cumulative <- cumulative_sizes[-1] # Cumulative size at end of each edge

    # Scale edge widths
    if (length(unique(edge_cumulative)) <= 1) {
      scaled_widths <- rep(mean(width_range), length(path_edges))
    } else {
      scaled_widths <- scales::rescale(
        edge_cumulative,
        to = width_range,
        from = range(edge_cumulative, na.rm = TRUE)
      )
    }

    # Assign trajectory attributes to edges
    igraph::E(g)[path_edges]$traj_id <- traj_id
    igraph::E(g)[path_edges]$traj_width <- scaled_widths
  }

  g
}

#' Create temporal layout for trajectory plotting
#'
#' Generates a Sugiyama layout with nodes aligned by publication year,
#' providing mappings between layout coordinates and actual years.
#'
#' @param g igraph object with year-encoded vertex names
#'
#' @return List with layout data and year scaling information
#' @keywords internal
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph create_layout
mk_layout_and_year_scale <- function(g) {
  # Extract years from vertex names
  vertex_years <- .extract_year(igraph::V(g)$name)

  # Create Sugiyama layout with year layers
  graph_tbl <- tidygraph::as_tbl_graph(g)
  layout <- ggraph::create_layout(graph_tbl, layout = "sugiyama", layers = vertex_years)

  # Map layout layers to years
  unique_layers <- sort(unique(layout$y))

  year_by_layer <- vapply(
    unique_layers,
    function(layer) {
      median(vertex_years[abs(layout$y - layer) < 1e-9], na.rm = TRUE)
    },
    numeric(1)
  )

  # Order layers chronologically
  layer_order <- order(year_by_layer)
  layer_to_x <- setNames(seq_along(unique_layers), unique_layers[layer_order])

  # Transform layout coordinates
  layout$x_time <- unname(layer_to_x[as.character(layout$y)])
  layout$y_original <- layout$y
  layout$y <- layout$x # Use original x as vertical coordinate
  layout$x <- layout$x_time # Use time as horizontal coordinate

  # Create scaling information
  vertex_x_map <- setNames(layout$x, layout$name)
  x_to_year_map <- setNames(
    as.integer(year_by_layer[layer_order]),
    seq_along(unique_layers)
  )

  list(
    lay = layout,
    vertex_x = vertex_x_map,
    x_to_year = x_to_year_map,
    all_breaks = as.integer(names(x_to_year_map)),
    all_labels = unname(as.integer(x_to_year_map))
  )
}

#' Detect main temporal trajectories in group-year DAG
#'
#' Identifies the most significant temporal trajectories within a group's evolution
#' over time by building a directed acyclic graph (DAG) from similarity data and
#' extracting highest-scoring disjoint paths using dynamic programming.
#'
#' @param groups_cumulative_trajectories List containing three components:
#'   - `groups_similarity`: Nested list with similarity data for each group,
#'     containing edges with `from`, `to`, `weight` (Jaccard), and `documents`
#'   - `groups_attributes`: Nested list with node attributes for each group,
#'     containing `quantity_papers`, `prop_tracked_intra_group`, `tracked_documents`,
#'     `PY.sd`, and `network_until`
#'   - `docs_per_group`: Data frame mapping group IDs to document IDs for
#'     accurate unique document counting
#' @param group Character ID of the group to analyze (e.g., "component1_g01")
#' @param jaccard_min Minimum Jaccard similarity for edges (default: 0.05).
#'   Higher values create sparser graphs with stronger connections.
#' @param intra_min Minimum proportion of tracked documents within group for
#'   nodes (default: 0.10). Higher values filter out weaker nodes.
#' @param k_out Maximum number of outgoing edges to keep per node (default: 2).
#'   Controls graph sparsity - lower values create simpler backbone structures.
#' @param alpha Weight for edge strength in path scoring (default: 1).
#'   Higher values emphasize transition strength over node quality.
#' @param beta Per-step persistence bonus in path scoring (default: 0.1).
#'   Higher values encourage longer trajectories.
#' @param top_M Maximum number of disjoint trajectories to extract (default: 5)
#' @param min_len Minimum number of distinct years for valid trajectory (default: 3)
#' @param use_docs_per_group Whether to use document IDs for accurate unique
#'   document counting (default: TRUE). If FALSE, uses approximation.
#'
#' @return A list with two components:
#'   - `graph`: An igraph object representing the temporal DAG with scoring
#'     attributes and optional document IDs
#'   - `trajectories`: A tibble of detected trajectories sorted by score, with columns:
#'     * `traj_id`: Trajectory identifier ("tr1", "tr2", ...)
#'     * `start`, `end`: First and last year of the trajectory
#'     * `length`: Number of distinct years in the trajectory
#'     * `nodes`: List of node names along the path (e.g., "y2009g03")
#'     * `score`: Total path score from dynamic programming
#'     * `mean_w`: Mean edge score along the path
#'     * `sum_docs`: Count of unique documents covered by the path
#'     * `mean_size`: Mean node size (quantity_papers × proportion tracked)
#'     * `mean_PYsd`: Mean publication year standard deviation
#'
#' @details
#' This function implements a comprehensive pipeline for detecting significant
#' temporal trajectories in research group evolution:
#'
#' ## Algorithm Overview
#'
#' 1. **Build Temporal DAG**: Constructs a directed acyclic graph where:
#'    - Nodes represent group-year combinations filtered by `intra_min` quality threshold
#'    - Edges represent transitions between consecutive years filtered by `jaccard_min`
#'    - Graph is sparsified to top `k_out` edges per node
#'
#' 2. **Score Components**: Computes node and edge scores:
#'    - Node score: \eqn{s_v = \log(1 + \text{quantity\_papers}_v \times \text{prop\_tracked\_intra\_group}_v)}
#'    - Edge score: \eqn{s_e = \text{weight}_e \times \log(1 + \text{documents}_e)}
#'
#' 3. **Extract Trajectories**: Uses dynamic programming to find heaviest paths:
#'    - Path score: \eqn{\text{best}(v) = \max\left( s_v, \max_{u \to v} \left( \text{best}(u) + s_v + \alpha \cdot s_{(u,v)} + \beta \right) \right)}
#'    - Iteratively extracts top `top_M` disjoint trajectories
#'    - Trajectories must span at least `min_len` distinct years
#'
#' 4. **Count Documents**: Calculates unique document coverage:
#'    - If `use_docs_per_group = TRUE`: Exact count via set union of document IDs
#'    - Otherwise: Approximation: \eqn{\sum \text{node documents} - \sum \text{edge documents}}
#'
#' ## Parameter Tuning Guidance
#'
#' - For **smoother, longer trajectories**: Increase `beta` (persistence bonus)
#' - For **transition-focused scoring**: Increase `alpha` (edge weight)
#' - For **denser connectivity**: Lower `jaccard_min` or increase `k_out`
#' - For **higher quality nodes**: Increase `intra_min`
#' - For **exact document counts**: Ensure `use_docs_per_group = TRUE` and provide `docs_per_group` data
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' trajectories <- detect_main_trajectories(
#'   groups_cumulative_trajectories = my_data,
#'   group = "component1_g01"
#' )
#'
#' # Tuned for longer, transition-focused trajectories
#' trajectories <- detect_main_trajectories(
#'   groups_cumulative_trajectories = my_data,
#'   group = "component1_g01",
#'   jaccard_min = 0.03, # More permissive connectivity
#'   k_out = 3, # Denser backbone
#'   alpha = 1.5, # Emphasize edge strength
#'   beta = 0.2, # Encourage longer paths
#'   top_M = 8, # Extract more trajectories
#'   min_len = 4 # Require longer trajectories
#' )
#'
#' # Access results
#' graph <- trajectories$graph
#' trajectory_data <- trajectories$trajectories
#'
#' # Plot the top trajectory
#' top_trajectory <- trajectory_data[1, ]
#' }
#'
#' @seealso
#' [filter_trajectories()] for post-processing detected trajectories,
#' [plot_group_trajectories_lines_2d()] and [plot_group_trajectories_lines_3d()]
#' for visualization
#'
#' @export
#' @importFrom igraph vcount ecount V E
#' @importFrom dplyr arrange desc filter select
detect_main_trajectories <- function(
  groups_cumulative_trajectories,
  group,
  jaccard_min = 0.05,
  intra_min = 0.10,
  k_out = 2,
  alpha = 1,
  beta = 0.1,
  top_M = 5,
  min_len = 3,
  use_docs_per_group = TRUE) {
  # Input validation
  if (!is.list(groups_cumulative_trajectories)) {
    stop("groups_cumulative_trajectories must be a list", call. = FALSE)
  }

  required_components <- c("groups_similarity", "groups_attributes", "docs_per_group")
  missing_components <- setdiff(required_components, names(groups_cumulative_trajectories))
  if (length(missing_components) > 0) {
    stop("groups_cumulative_trajectories is missing components: ",
      paste(missing_components, collapse = ", "),
      call. = FALSE
    )
  }

  if (!group %in% names(groups_cumulative_trajectories$groups_similarity)) {
    stop("Group '", group, "' not found in groups_cumulative_trajectories$groups_similarity", call. = FALSE)
  }

  if (!group %in% names(groups_cumulative_trajectories$groups_attributes)) {
    stop("Group '", group, "' not found in groups_cumulative_trajectories$groups_attributes", call. = FALSE)
  }

  # Build and score temporal graph
  build_temporal_dag(
    groups_cumulative_trajectories,
    group,
    jaccard_min = jaccard_min,
    intra_min = intra_min,
    k_out = k_out
  ) ->
    trajectory_graph

  # Attach document information from docs_per_group
  if (use_docs_per_group) {
    # Filter docs_per_group for the specific group
    groups_cumulative_trajectories$docs_per_group |>
      dplyr::filter(.data$group == group) |>
      dplyr::select(.data$group_id, .data$document_id) ->
      group_docs

    trajectory_graph <- attach_docs_to_vertices(trajectory_graph, group_docs)
  }

  # Score nodes and edges
  trajectory_graph <- score_nodes_edges(trajectory_graph, alpha = alpha, beta = beta)

  # Extract top trajectories
  extract_top_trajectories(
    trajectory_graph,
    M = top_M,
    min_len = min_len
  ) |>
    dplyr::arrange(dplyr::desc(.data$score)) ->
    trajectory_data

  list(graph = trajectory_graph, trajectories = trajectory_data)
}

#' Filter and rank detected trajectories
#'
#' Applies post-processing filters and ranking to trajectory data based on score,
#' length, and other criteria. This function helps refine the output from
#' `detect_main_trajectories()` by keeping only the most relevant trajectories
#' according to user-specified constraints.
#'
#' @param tr_tbl A tibble of trajectories from `detect_main_trajectories()$trajectories`.
#'   Must contain at least `traj_id`, `score`, and `length` columns.
#' @param top_n Maximum number of trajectories to keep after filtering and sorting
#'   (default: 3). If `NULL`, keeps all trajectories that meet the filter criteria.
#' @param min_score Minimum score threshold for trajectories (default: `NULL`).
#'   Trajectories with score less than `min_score` are discarded. Useful for
#'   removing weak or noisy paths.
#' @param min_length Minimum trajectory length in distinct years (default: `NULL`).
#'   Trajectories shorter than `min_length` are discarded. Ensures only trajectories
#'   spanning a meaningful temporal horizon are kept.
#'
#' @return A filtered and sorted trajectory tibble with the same structure as input,
#'   containing only trajectories that meet all criteria, sorted by descending score.
#'   Returns an empty tibble if no trajectories meet the criteria.
#'
#' @details
#' This function provides a straightforward way to refine trajectory detection results
#' by applying quality filters and ranking. The filtering process occurs in three steps:
#'
#' 1. **Quality Filtering**: Remove trajectories that don't meet minimum quality standards
#'    - `min_score`: Filters by the dynamic programming path score (higher = better)
#'    - `min_length`: Filters by temporal span in distinct years
#'
#' 2. **Ranking**: Sort remaining trajectories by descending score to prioritize the
#'    most significant paths
#'
#' 3. **Selection**: Keep only the top `top_n` trajectories after filtering and sorting
#'
#' ## Typical Use Cases
#'
#' - **Focus on strongest signals**: Use `min_score` to remove low-confidence trajectories
#' - **Ensure temporal significance**: Use `min_length` to require multi-year evolution
#' - **Limit visualization complexity**: Use `top_n` to focus on the most important paths
#' - **Progressive refinement**: Chain multiple calls with different criteria
#'
#' @examples
#' \dontrun{
#' # Get trajectories first
#' traj_data <- detect_main_trajectories(
#'   groups_cumulative_trajectories = my_data,
#'   group = "component1_g01"
#' )
#'
#' # Basic: Keep top 3 trajectories by score
#' top_trajectories <- filter_trajectories(traj_data$trajectories)
#'
#' # Keep top 5 trajectories with minimum quality standards
#' quality_trajectories <- filter_trajectories(
#'   tr_tbl = traj_data$trajectories,
#'   top_n = 5,
#'   min_score = 10,
#'   min_length = 4
#' )
#'
#' # Keep all trajectories meeting minimum length (no top_n limit)
#' long_trajectories <- filter_trajectories(
#'   tr_tbl = traj_data$trajectories,
#'   top_n = NULL,
#'   min_length = 5
#' )
#'
#' # Very strict filtering for high-quality, long trajectories
#' strict_trajectories <- filter_trajectories(
#'   tr_tbl = traj_data$trajectories,
#'   top_n = 3,
#'   min_score = 15,
#'   min_length = 6
#' )
#'
#' # Use filtered trajectories for visualization
#' plot_group_trajectories_lines_2d(
#'   traj_data = traj_data,
#'   traj_filtered = quality_trajectories
#' )
#' }
#'
#' @seealso
#' [detect_main_trajectories()] for generating the trajectory data,
#' [plot_group_trajectories_lines_2d()] and [plot_group_trajectories_lines_3d()]
#' for visualizing filtered trajectories
#'
#' @export
#' @importFrom dplyr filter arrange desc slice_head
filter_trajectories <- function(tr_tbl, top_n = 3, min_score = NULL, min_length = NULL) {
  if (is.null(tr_tbl) || !nrow(tr_tbl)) {
    return(tr_tbl)
  }

  filtered_trajectories <- tr_tbl

  # Apply filters
  if (!is.null(min_score)) {
    filtered_trajectories <- dplyr::filter(filtered_trajectories, .data$score >= min_score)
  }

  if (!is.null(min_length)) {
    filtered_trajectories <- dplyr::filter(filtered_trajectories, .data$length >= min_length)
  }

  # Sort and select top trajectories
  sorted_trajectories <- dplyr::arrange(filtered_trajectories, dplyr::desc(.data$score))

  if (!is.null(top_n)) {
    dplyr::slice_head(sorted_trajectories, n = top_n)
  } else {
    sorted_trajectories
  }
}
