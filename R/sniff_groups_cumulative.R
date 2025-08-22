#' Analyze Cumulative Network Groups Over Time
#'
#' Performs cumulative community detection on a network over specified time spans,
#' returning group statistics and keyword analysis for each time period.
#'
#' @param groups A list created by \code{sniff_groups()} containing at least
#'   \code{network} (a \code{tbl_graph} or \code{igraph} object whose vertices
#'   have attributes \code{PY}, \code{component}, and \code{DE}), as well as
#'   auxiliary elements such as \code{pubs_by_year} and \code{aggregate}.
#' @param time_span Numeric vector of years to analyze (default: 2000:2024).
#' @param min_group_size Minimum size for a cluster to be retained (default = 10).
#' @param top_n_keywords Number of top keywords to extract per group (default = 10).
#' @param algorithm Community detection algorithm to use. One of:
#'   \code{"louvain"}, \code{"walktrap"}, \code{"edge_betweenness"},
#'   \code{"fast_greedy"} (default), or \code{"leiden"}.
#'
#' @return A named list (by year) where each element contains:
#' \describe{
#'   \item{groups}{A tibble with group statistics and top keywords}
#'   \item{documents}{A tibble mapping documents to groups}
#'   \item{network}{The cumulative network up to that year}
#' }
#'
#' @examples
#' \dontrun{
#' # Typical pipeline:
#' data  <- read_wos("savedrecs.txt")
#' net   <- sniff_network(data)
#' comps <- sniff_components(net)
#' groups <- sniff_groups(comps)
#'
#' # Cumulative analysis
#' groups_cumulative <- sniff_groups_cumulative(
#'   groups,
#'   time_span = 2010:2020,
#'   algorithm = "leiden"
#' )
#'
#' # Access results for 2015
#' groups_cumulative[["network_until_2015"]]$groups
#' }
#'
#' @importFrom purrr map map_int
#' @importFrom igraph vcount V cluster_louvain cluster_walktrap cluster_edge_betweenness
#' @importFrom igraph cluster_fast_greedy cluster_leiden as_undirected vertex_attr_names
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom dplyr filter group_by summarise arrange mutate n desc select relocate left_join tally slice_head
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_trim str_pad
#' @export
sniff_groups_cumulative <- function(groups,
                                    time_span = 2000:2024,
                                    min_group_size = 10,
                                    top_n_keywords = 10,
                                    algorithm = "fast_greedy") {
  # Input validation
  if (!is.list(groups)) {
    stop("groups must be a list containing network data")
  }

  required_components <- c("network", "pubs_by_year", "aggregate")
  if (!all(required_components %in% names(groups))) {
    stop(paste("groups must contain:", paste(required_components, collapse = ", ")))
  }

  net <- groups$network

  if (!inherits(net, c("tbl_graph", "igraph"))) {
    stop("Input must be a network object (tbl_graph or igraph)", call. = FALSE)
  }

  if (!"PY" %in% igraph::vertex_attr_names(net)) {
    stop("Network must contain 'PY' (publication year) vertex attribute", call. = FALSE)
  }

  if (!"component" %in% igraph::vertex_attr_names(net)) {
    stop("Network must contain 'component' vertex attribute", call. = FALSE)
  }

  if (!"DE" %in% igraph::vertex_attr_names(net)) {
    stop("Network must contain 'DE' (keywords) vertex attribute", call. = FALSE)
  }

  if (!is.numeric(time_span)) {
    stop("time_span must be a numeric vector of years", call. = FALSE)
  }

  valid_algorithms <- c("louvain", "walktrap", "edge_betweenness", "fast_greedy", "leiden")
  if (!algorithm %in% valid_algorithms) {
    stop("algorithm must be one of: ", paste(valid_algorithms, collapse = ", "), call. = FALSE)
  }

  # Convert publication years to numeric
  igraph::V(net)$PY <- as.numeric(igraph::V(net)$PY)

  tryCatch(
    {
      # Create cumulative networks for each time point
      purrr::map(time_span, ~ {
        net |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::filter(.data$PY <= .x)
      }) ->
        netl

      # Check for empty networks
      n_nodes <- purrr::map_int(netl, igraph::vcount)
      if (any(n_nodes == 0)) {
        empty_years <- time_span[n_nodes == 0]
        stop("No articles found in years: ", paste(empty_years, collapse = ", "), call. = FALSE)
      }

      # Process each time point
      results <- purrr::map(time_span, function(year) {
        idx <- which(time_span == year)
        current_net <- netl[[idx]]

        # Apply clustering algorithm
        eb <- switch(algorithm,
          "louvain" = igraph::cluster_louvain(igraph::as_undirected(current_net)),
          "walktrap" = igraph::cluster_walktrap(current_net),
          "edge_betweenness" = igraph::cluster_edge_betweenness(current_net),
          "fast_greedy" = igraph::cluster_fast_greedy(igraph::as_undirected(current_net)),
          "leiden" = igraph::cluster_leiden(igraph::as_undirected(current_net))
        )

        # Add group membership to vertices
        igraph::V(current_net)$group <- eb$membership
        groups_nchar <- nchar(max(eb$membership, na.rm = TRUE))
        groups_nchar <- ifelse(groups_nchar < 2, 2, groups_nchar)

        # Create group summary statistics
        group_stats <- current_net |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::as_tibble() |>
          dplyr::group_by(.data$group) |>
          dplyr::summarise(
            quantity_papers = dplyr::n(),
            average_age = mean(.data$PY, na.rm = TRUE),
            component = unique(.data$component),
            .groups = "drop"
          ) |>
          dplyr::arrange(.data$component, dplyr::desc(.data$quantity_papers)) |>
          dplyr::mutate(
            group_new = paste(.data$component, "_g",
              stringr::str_pad(1:dplyr::n(), width = groups_nchar, pad = "0"),
              sep = ""
            )
          ) |>
          dplyr::filter(.data$quantity_papers >= min_group_size) |>
          dplyr::select(group = .data$group_new, .data$quantity_papers, .data$average_age) |>
          dplyr::mutate(network_until = year)

        # Create document-group mapping
        doc_groups <- current_net |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::as_tibble() |>
          dplyr::mutate(
            group_new = paste(.data$component, "_g",
              stringr::str_pad(.data$group, width = groups_nchar, pad = "0"),
              sep = ""
            )
          ) |>
          dplyr::select(.data$name, group = .data$group_new, .data$DE) |>
          dplyr::mutate(network_until = year)

        # Extract top keywords for each group
        group_keywords <- doc_groups |>
          tidyr::separate_rows(.data$DE, sep = ";") |>
          dplyr::mutate(DE = stringr::str_trim(.data$DE)) |>
          dplyr::filter(!is.na(.data$DE) & .data$DE != "") |>
          dplyr::group_by(.data$group, .data$DE) |>
          dplyr::tally(name = "qtde") |>
          dplyr::arrange(.data$group, dplyr::desc(.data$qtde)) |>
          dplyr::group_by(.data$group) |>
          dplyr::slice_head(n = top_n_keywords) |>
          dplyr::mutate(keywords_freq = paste0(.data$DE, " (", .data$qtde, ")")) |>
          dplyr::summarise(
            keywords = paste(.data$keywords_freq, collapse = ";"),
            .groups = "drop"
          )

        # Combine group stats with keywords
        final_groups <- dplyr::left_join(group_stats, group_keywords, by = "group") |>
          dplyr::relocate(.data$keywords, .after = .data$network_until)

        list(groups = final_groups, documents = doc_groups, network = netl[[idx]])
        # list(groups = final_groups, documents = doc_groups)
      })

      # Name the results by year
      names(results) <- paste("network_until", time_span, sep = "_")
      return(results)
    },
    error = function(e) {
      stop("Error in cumulative group analysis: ", e$message, call. = FALSE)
    }
  )
}
