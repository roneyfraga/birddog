#' Calculate Citation Cycle Time (CCT) indicator
#'
#' @description
#' Calculates the Citation Cycle Time (CCT) - the average citation cycle time -
#' to measure the pace of technological or scientific progress in a publication network.
#' Based on Kayal (1999) methodology, the indicator measures the median age of cited
#' publications, where lower values indicate faster technological replacement cycles.
#'
#' @param network Required. Network object containing publication data. For `scope = "groups"`:
#'   object returned by `sniff_groups()`. For `scope = "network"`: network object
#'   (`tbl_graph` or `igraph`).
#' @param scope Analysis scope. Either `"groups"` (default) for separate group analysis
#'   or `"network"` for complete network analysis.
#' @param start_year,end_year Start and end years for temporal analysis. If not specified,
#'   uses minimum and maximum years found in the data.
#' @param tracked_cr_py Pre-processed citation year data (optional). A tibble with columns
#'   `CR` (OpenAlex work ID) and `CR_PY` (publication year). If provided, skips fetching
#'   data from OpenAlex API. Useful for avoiding repeated API calls.
#' @param batch_size For OpenAlex data: number of IDs to process per API call (default: 50).
#'   Smaller batches help avoid API rate limits, larger batches process data faster
#'   but may trigger rate limiting.
#'
#' @details
#' The Citation Cycle Time (CCT) is calculated as:
#' \enumerate{
#'   \item Extract citation IDs from the network's CR column
#'   \item Fetch publication years for cited works from OpenAlex API using `get_openalex_fields()`
#'   \item For each publication, calculate the year difference between the publication and its references
#'   \item Calculate the median of these differences for each publication
#'   \item Calculate the mean of the medians for the group/network + 0.5 (Kayal adjustment)
#' }
#'
#' Lower CCT values indicate that publications are citing more recent work, suggesting
#' a faster pace of technological/scientific replacement.
#'
#' The function automatically handles:
#' \itemize{
#'   \item Splitting semicolon-separated citation IDs
#'   \item Batch processing of OpenAlex API requests
#'   \item Filtering invalid citations (where cited work was published after citing work)
#'   \item Creating temporal plots for each group
#' }
#'
#' @return
#' A list with the following components:
#' \item{data}{Tibble with CCT data containing columns: group, year, index}
#' \item{plots}{Named list of plotly objects showing temporal evolution of CCT for each group.
#'   Each plot shows both absolute CCT values and year-over-year differences.}
#' \item{years_range}{Named vector with start_year and end_year used in the analysis}
#' \item{tracked_cr_py}{Complete citation year data fetched from OpenAlex, with columns
#'   CR (OpenAlex ID) and CR_PY (publication year). Can be saved and reused in subsequent
#'   analyses to avoid repeated API calls.}
#'
#' @references
#' Kayal AA, Waters RC. An empirical evaluation of the technology cycle time indicator
#' as a measure of the pace of technological progress in superconductor technology.
#' IEEE Transactions on Engineering Management. 1999;46(2):127–31. doi:10.1109/17.759138
#'
#' @examples
#' \dontrun{
#' # Group analysis
#' results <- sniff_citations_cycle_time(network_groups, scope = "groups")
#'
#' # Network analysis
#' results_network <- sniff_citations_cycle_time(complete_network, scope = "network")
#'
#' # Accessing results
#' cct_data <- results$data
#' plots <- results$plots
#' plots$c1g1  # View plot for specific group
#'
#' # Specifying time period and batch size
#' results_period <- sniff_citations_cycle_time(
#'   network_groups,
#'   start_year = 2010,
#'   end_year = 2020,
#'   batch_size = 50
#' )
#'
#' # Reuse citation data to avoid repeated API calls
#' saved_citations <- results$tracked_cr_py
#' results2 <- sniff_citations_cycle_time(
#'   network_groups,
#'   tracked_cr_py = saved_citations
#' )
#' }
#'
#' @seealso
#' [sniff_groups()], [get_openalex_fields()], [indexes_plots()]
#'
#' @importFrom tidygraph activate
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr select filter mutate left_join group_by group_map summarise pull
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_trim
#' @importFrom purrr map map_dfr
#' @importFrom igraph V
#' @importFrom glue glue
#' @export
sniff_citations_cycle_time <- function(
  network,
  scope = "groups",
  start_year = NULL,
  end_year = NULL,
  tracked_cr_py = NULL,
  batch_size = 50) {
  # checks
  if (is.null(network)) {
    stop("Network data not found in groups object", call. = FALSE)
  }

  required_scope <- c("network", "groups")
  if (!scope %in% required_scope) {
    stop(glue::glue("scope must be: {paste(required_scope, collapse = ' or ')}"), call. = FALSE)
  }

  if (scope == "groups") {
    # Input validation for groups scope
    list_dimensions <- c("network", "pubs_by_year", "aggregate")
    if (!all(list_dimensions %in% names(network))) {
      stop(glue::glue("network file must be generated by sniff_groups()"), call. = FALSE)
    }
    net_data <- network$network
  } else { # network
    # Input validation for network scope
    if (!inherits(network, c("tbl_graph", "igraph"))) {
      stop("Input (network) must be a network object (tbl_graph or igraph)", call. = FALSE)
    }
    network |>
      tidygraph::activate(nodes) |>
      dplyr::mutate(group = "full_network") ->
      net_data
  }

  # Determine start_year and end_year if not provided
  if (is.null(start_year) || is.null(end_year)) {
    publication_years <- tryCatch(
      {
        igraph::V(net_data)$PY
      },
      error = function(e) {
        stop("Error accessing publication years from network: ", e$message, call. = FALSE)
      }
    )

    publication_years <- publication_years[!is.na(publication_years)]

    if (length(publication_years) == 0) {
      stop("No publication years found in network data", call. = FALSE)
    }

    if (is.null(start_year)) {
      start_year <- min(publication_years, na.rm = TRUE)
      message("Using minimum publication year as start_year: ", start_year)
    }

    if (is.null(end_year)) {
      end_year <- max(publication_years, na.rm = TRUE)
      message("Using maximum publication year as end_year: ", end_year)
    }
  }

  if (start_year >= end_year) {
    stop("start_year must be less than end_year", call. = FALSE)
  }

  # Get unique groups
  group <- tryCatch(
    {
      net_data |>
        tidygraph::activate(nodes) |>
        tibble::as_tibble() |>
        dplyr::pull("group") |>
        stats::na.omit() |>
        unique() |>
        sort()
    },
    error = function(e) {
      stop("Error extracting groups from network: ", e$message, call. = FALSE)
    }
  )

  if (length(group) == 0) {
    stop("No valid groups found for analysis", call. = FALSE)
  }

  if (is.null(tracked_cr_py)) {
    # Extract citation IDs from network
    net_data |>
      tidygraph::activate(nodes) |>
      tibble::as_tibble() |>
      dplyr::select(.data$name, .data$PY, .data$CR) |>
      dplyr::filter(!is.na(.data$CR) & .data$CR != "") ->
      citation_data

    # Get publication years from OpenAlex
    citation_data |>
      tidyr::separate_rows(.data$CR, sep = ";") |>
      dplyr::mutate(CR = stringr::str_trim(.data$CR)) |>
      dplyr::filter(.data$CR != "") ->
      citation_data_split

    # Fetch years using get_openalex_fields()
    get_openalex_fields(
      citation_data_split$CR,
      variables = "publication_year",
      batch_size = batch_size
    ) ->
      openalex_years

    # Join back and filter
    citation_data_split |>
      dplyr::left_join(openalex_years, by = c("CR" = "id")) |>
      dplyr::rename(CR_PY = .data$publication_year) |>
      dplyr::filter(!is.na(.data$CR_PY)) |>
      dplyr::filter(.data$PY > .data$CR_PY) ->
      tracked_publications_year_from_references
  } else {
    tracked_cr_py -> tracked_publications_year_from_references
  }

  years_seq <- start_year:end_year

  net_data |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::select(.data$name, .data$group, .data$PY, .data$CR) |>
    tidyr::separate_rows(.data$CR, sep = ";") |>
    dplyr::mutate(CR = stringr::str_trim(.data$CR)) |>
    dplyr::filter(.data$CR != "") |>
    dplyr::left_join(tracked_publications_year_from_references, by = "CR") |>
    tibble::rownames_to_column(var = "node") |>
    dplyr::filter(.data$group %in% !!group) |>
    dplyr::group_by(.data$group) |>
    dplyr::group_map(~ {
      purrr::map_dfr(years_seq, function(current_year) {
        .x |>
          dplyr::filter(.data$PY <= current_year) |>
          dplyr::mutate(diff = abs(.data$PY - .data$CR_PY)) ->
          current_data

        if (nrow(current_data) == 0) {
          return(tibble::tibble(index = NA, year = current_year, group = .y$group))
        }

        current_data |>
          dplyr::group_by(.data$node) |>
          dplyr::summarise(median = median(.data$diff, na.rm = TRUE)) |>
          dplyr::summarise(index = mean(.data$median, na.rm = TRUE) + 0.5) |>
          dplyr::pull(.data$index) ->
          cct_value

        tibble::tibble(index = cct_value, year = current_year, group = .y$group)
      })
    }, .keep = TRUE) ->
    cct_list

  names(cct_list) <- group

  # Create plots for each group
  plots_list <- purrr::map(group, function(group_name) {
    group_data <- cct_list[[group_name]]
    if (all(is.na(group_data$index))) {
      warning("No data available for group: ", group_name, " - skipping plot")
      return(NULL)
    }
    tryCatch(
      {
        # Pass the full group_data (includes year, index, and group columns)
        indexes_plots(group_data, group_name = group_name, start_year, end_year, method = "cct")
      },
      error = function(e) {
        warning("Error creating plot for group ", group_name, ": ", e$message)
        return(NULL)
      }
    )
  })

  # Remove NULL plots and update names
  valid_plots <- !sapply(plots_list, is.null)
  plots_list <- plots_list[valid_plots]
  names(plots_list) <- group[valid_plots]

  dplyr::bind_rows(cct_list) |>
    dplyr::select("group", "year", "index") ->
    cct_data

  list(
    data = cct_data,
    plots = plots_list,
    years_range = c(start_year = start_year, end_year = end_year),
    tracked_cr_py = tracked_publications_year_from_references
  ) ->
    result

  return(result)
}
