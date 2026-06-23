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

#' Document count per year-node
#'
#' @param docs_per_group Membership tibble with `group_id` and `document_id`.
#' @return Named integer vector mapping node name (e.g. "y2018c1g16") to its
#'   distinct document count.
#' @keywords internal
.node_size_lookup <- function(docs_per_group) {
  u <- docs_per_group[!duplicated(docs_per_group[c("group_id", "document_id")]),
                      "group_id", drop = TRUE]
  tab <- table(u)
  stats::setNames(as.integer(tab), names(tab))
}

#' Per-year cumulative size of a trajectory (papers)
#'
#' Counts the documents in each of a trajectory's year-nodes, one (year, size)
#' row per year. Under cumulative clustering a node already holds every paper up
#' to its year, so the node size is the trajectory's cumulative size that year;
#' the series therefore ends at the terminal cohort size.
#'
#' @param nodes Character vector of node names (e.g. "y2018c1g16").
#' @param node_size Named integer vector from [.node_size_lookup()].
#' @return A tibble with integer columns `year` and `size`, ordered by year.
#' @keywords internal
.feeder_growth_series <- function(nodes, node_size) {
  yr <- .extract_year(nodes)
  sz <- as.integer(node_size[nodes])
  sz[is.na(sz)] <- 0L
  agg <- tapply(sz, yr, max)
  tibble::tibble(year = as.integer(names(agg)), size = as.integer(agg))
}

#' Cumulative arrival of a feeder's contributed papers
#'
#' For the subset of papers that actually flow from a feeder into a target,
#' counts them cumulatively by the year each first joins one of the feeder's own
#' year-nodes. Restricting to the feeder's nodes keeps the curve within the
#' feeder's lifespan, instead of reaching back to a paper's corpus first-
#' appearance year, which under cumulative clustering can long precede the
#' trajectory. Monotone non-decreasing, ending at the number of contributed
#' papers; if their last node-year precedes the handoff, a final point at
#' `handoff_year` holds the full count.
#'
#' @param doc_ids Character vector of the contributed document ids.
#' @param nodes Character vector of the feeder trajectory's node names (e.g.
#'   "y2018c1g16"), restricting arrival to the feeder's own clusters.
#' @param docs_per_group Membership tibble with `group_id`, `document_id`,
#'   `network_until`.
#' @param handoff_year Integer year the feeder hands off into the target.
#' @return A tibble with integer columns `year` and `size`, ordered by year.
#' @keywords internal
.contributed_arrival_series <- function(doc_ids, nodes, docs_per_group, handoff_year) {
  n <- length(doc_ids)
  if (n == 0) {
    return(tibble::tibble(year = integer(), size = integer()))
  }
  sub <- docs_per_group[docs_per_group$group_id %in% nodes &
                          docs_per_group$document_id %in% doc_ids,
                        c("document_id", "network_until")]
  first_year <- tapply(sub$network_until, sub$document_id, min)
  tab <- table(as.integer(first_year))
  out <- tibble::tibble(year = as.integer(names(tab)),
                        size = as.integer(cumsum(tab)))
  if (out$year[nrow(out)] < handoff_year) {
    out <- tibble::tibble(year = c(out$year, as.integer(handoff_year)),
                          size = c(out$size, n))
  }
  out
}

#' Share of each cluster's papers that end up in a group's final cluster
#'
#' For every year-group node in `docs_per_group`, the fraction of its documents
#' that also belong to `group`'s last-year cluster. Captures how much of an
#' earlier cluster funnels into the group as it stands in the final year.
#'
#' @param docs_per_group Membership tibble with `group_id`, `document_id`,
#'   `network_until` and `group`.
#' @param group Final-year group label (e.g. "c1g1").
#' @return Named numeric vector keyed by `group_id`, each in `[0, 1]`.
#' @keywords internal
.final_group_share_lookup <- function(docs_per_group, group) {
  grp <- docs_per_group[docs_per_group$group == group, , drop = FALSE]
  if (nrow(grp) == 0) {
    stop("group '", group, "' not found in docs_per_group", call. = FALSE)
  }
  final_year <- max(grp$network_until)
  final_id <- paste0("y", final_year, group)
  final_docs <- docs_per_group$document_id[docs_per_group$group_id == final_id]
  vapply(
    split(docs_per_group$document_id, docs_per_group$group_id),
    function(d) mean(d %in% final_docs),
    numeric(1)
  )
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

#' Attach document IDs to graph vertices
#'
#' Adds document ID lists to each vertex in the graph based on the
#' group-document mapping.
#'
#' @param g igraph object
#' @param docs_tbl Tibble with columns `group_id` and `document_id`
#'
#' @return Modified igraph with `doc_ids` vertex attribute
#' @keywords internal
#' @noRd
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
