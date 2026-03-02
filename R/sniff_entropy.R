#' Calculate Entropy Based on Keywords Over Time
#'
#' Computes the normalized Shannon entropy of keyword distributions from scientific
#' publications over a specified time range. Entropy measures the diversity and
#' evenness of keyword usage within research groups or the entire network.
#'
#' @param network A network object to analyze. For `scope = "groups"`, this should be
#'   the output of `sniff_groups()`. For `scope = "network"`, this should be a
#'   `tbl_graph` or `igraph` object from `sniff_network()`.
#' @param scope Character specifying the analysis scope: "groups" for multiple groups
#'   or "network" for the entire network (default: "groups").
#' @param start_year Starting year for entropy calculation. If NULL, uses the minimum
#'   publication year found in the network data.
#' @param end_year Ending year for entropy calculation. If NULL, uses the maximum
#'   publication year found in the network data.
#' @param mode Character specifying the temporal mode for entropy calculation:
#'   \describe{
#'     \item{"annual"}{Uses only publications from each specific year (default).}
#'     \item{"cumulative"}{Uses all publications from the start up to each year.}
#'     \item{"rolling"}{Uses a sliding window of \code{window_size} years ending at each year.}
#'   }
#' @param window_size Integer specifying the rolling window size in years (default: 5).
#'   Only used when \code{mode = "rolling"}.
#'
#' @return A list with three components:
#'   \item{data}{A tibble containing entropy values for each group and year}
#'   \item{plots}{A list of plotly objects visualizing entropy trends for each group}
#'   \item{years_range}{A vector with the start_year and end_year used in calculations}
#'
#' @details
#' The function calculates the normalized Shannon entropy (Pielou's evenness index)
#' based on Shannon's information theory (Shannon, 1948). The temporal scope of
#' keyword data depends on the \code{mode} parameter:
#' \itemize{
#'   \item \strong{annual}: entropy from keywords published in each specific year.
#'     Values tend to be high (near 1) since within-year distributions are typically even.
#'   \item \strong{cumulative}: entropy from all keywords published up to each year.
#'     Shows long-term trends in thematic concentration as the keyword pool grows.
#'   \item \strong{rolling}: entropy from a sliding window of recent years.
#'     Balances sensitivity to recent shifts with enough data for stable estimates.
#' }
#'
#' The normalized entropy (Pielou's J') is calculated as:
#' \deqn{J' = \frac{H}{H_{max}} = \frac{-\sum_{i=1}^{n} p_i \log_2 p_i}{\log_2 n}}
#' where \eqn{p_i} is the relative frequency of keyword \eqn{i}, \eqn{n} is the
#' number of unique keywords, and \eqn{H_{max} = \log_2 n} is the maximum possible
#' entropy for \eqn{n} categories.
#'
#' Entropy values range from 0 to 1, where:
#' \itemize{
#'   \item 0 indicates minimal diversity (one dominant keyword)
#'   \item 1 indicates maximal diversity (all keywords equally frequent)
#' }
#'
#' A sudden increase in entropy may signal the emergence of new research topics,
#' while a decrease suggests thematic convergence.
#'
#' @references
#' Shannon, C. E. (1948). A mathematical theory of communication. \emph{Bell System
#' Technical Journal}, 27(3), 379-423. \doi{10.1002/j.1538-7305.1948.tb01338.x}
#'
#' Pielou, E. C. (1966). The measurement of diversity in different types of
#' biological collections. \emph{Journal of Theoretical Biology}, 13, 131-144.
#'
#' @importFrom tidygraph activate
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate filter select pull group_by summarise n
#' @importFrom tidyr separate_rows
#' @importFrom purrr map map_dfr map2
#' @importFrom stringr str_squish
#' @importFrom igraph V
#' @importFrom stats na.omit
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' # Rolling window (default: 5 years)
#' entropy_results <- sniff_entropy(groups_data, scope = "groups")
#'
#' # Cumulative mode
#' entropy_results <- sniff_entropy(groups_data, mode = "cumulative")
#'
#' # Annual mode
#' entropy_results <- sniff_entropy(groups_data, mode = "annual")
#'
#' # Rolling window with custom size
#' entropy_results <- sniff_entropy(groups_data, mode = "rolling", window_size = 3)
#'
#' # Access results
#' entropy_data <- entropy_results$data
#' entropy_plots <- entropy_results$plots
#' }
#'
#' @seealso
#' \code{\link{sniff_groups}}, \code{\link{sniff_network}}, \code{\link{indexes_plots}}
sniff_entropy <- function(network, scope = "groups", start_year = NULL, end_year = NULL,
                          mode = "rolling", window_size = 5) {
  # Input validation
  if (is.null(network)) {
    stop("Network data not found in groups object", call. = FALSE)
  }

  valid_modes <- c("annual", "cumulative", "rolling")
  if (!mode %in% valid_modes) {
    stop(glue::glue("mode must be: {paste(valid_modes, collapse = ', ')}"), call. = FALSE)
  }

  if (mode == "rolling" && (!is.numeric(window_size) || length(window_size) != 1 || window_size < 2)) {
    stop("window_size must be a single integer >= 2", call. = FALSE)
  }

  required_scope <- c("network", "groups")
  if (!scope %in% required_scope) {
    stop(glue::glue("scope must be: {paste(required_scope, collapse = ' or ')}"), call. = FALSE)
  }

  if (scope == "groups") {
    list_dimensions <- c("network", "pubs_by_year", "aggregate")
    if (!all(list_dimensions %in% names(network))) {
      stop(glue::glue("network file must be generated by sniff_groups()"), call. = FALSE)
    }
    net_data <- network$network
  } else {
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
        mixed_sort()
    },
    error = function(e) {
      stop("Error extracting groups from network: ", e$message, call. = FALSE)
    }
  )

  if (length(group) == 0) {
    stop("No valid groups found for analysis", call. = FALSE)
  }

  # --- Pre-extract keyword data once (avoid repeated network queries) ---
  net_data |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::select("name", "group", "DE", "PY") |>
    dplyr::filter(!is.na(.data$DE) & .data$DE != "") |>
    dplyr::filter(.data$group %in% !!group) |>
    tidyr::separate_rows("DE", sep = ";") |>
    dplyr::mutate(DE = stringr::str_squish(tolower(.data$DE))) |>
    dplyr::filter(.data$DE != "") ->
    all_keywords

  years_seq <- start_year:end_year

  # --- Entropy computation: Pielou's J' with mode-based filtering ---
  entropy_list <- purrr::map(group, function(grp) {
    group_keywords <- all_keywords |>
      dplyr::filter(.data$group == grp)

    purrr::map_dfr(years_seq, function(current_year) {
      # Filter keywords based on mode
      year_data <- switch(mode,
        "annual" = group_keywords |>
          dplyr::filter(.data$PY == current_year),
        "cumulative" = group_keywords |>
          dplyr::filter(.data$PY <= current_year),
        "rolling" = group_keywords |>
          dplyr::filter(.data$PY >= (current_year - window_size + 1) & .data$PY <= current_year)
      )

      if (nrow(year_data) == 0) {
        return(tibble::tibble(index = NA_real_, year = current_year, group = grp))
      }

      # Compute keyword frequencies and probabilities
      year_data |>
        dplyr::group_by(.data$DE) |>
        dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(P = .data$freq / sum(.data$freq)) ->
        freq_table

      n_keywords <- nrow(freq_table)

      # Shannon entropy needs at least 2 categories for normalization
      if (n_keywords <= 1) {
        return(tibble::tibble(index = 0, year = current_year, group = grp))
      }

      # Pielou's evenness: H / log2(n)
      H <- -sum(freq_table$P * log2(freq_table$P), na.rm = TRUE)
      H_max <- log2(n_keywords)
      entropy_value <- H / H_max

      tibble::tibble(index = entropy_value, year = current_year, group = grp)
    })
  })

  names(entropy_list) <- group

  # Combine all entropy data
  dplyr::bind_rows(entropy_list) |>
    dplyr::select("group", "year", "index") ->
    entropy_data

  # Compute global y-axis ranges for consistent comparison across groups
  all_index <- entropy_data$index[!is.na(entropy_data$index)]
  y_limits <- c(
    max(0, min(all_index, na.rm = TRUE) - 0.05),
    min(1, max(all_index, na.rm = TRUE) + 0.05)
  )

  # Compute global diff range
  all_diffs <- entropy_data |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(diff = c(NA, base::diff(.data$index))) |>
    dplyr::pull(.data$diff)
  all_diffs <- all_diffs[!is.na(all_diffs)]
  diff_margin <- stats::quantile(abs(all_diffs), 0.95, na.rm = TRUE) * 1.1
  y_limits_diff <- c(-diff_margin, diff_margin)

  # Create plots for each group
  plots_list <- purrr::map2(entropy_list, group, function(x, y) {
    if (all(is.na(x$index))) {
      warning("No data available for group: ", y, " - skipping plot")
      return(NULL)
    }
    tryCatch(
      indexes_plots(x, group_name = y, start_year, end_year, method = "entropy",
                    y_limits = y_limits, y_limits_diff = y_limits_diff),
      error = function(e) {
        warning("Error creating plot for group ", y, ": ", e$message)
        return(NULL)
      }
    )
  })

  valid_plots <- !sapply(plots_list, is.null)
  plots_list <- plots_list[valid_plots]
  names(plots_list) <- group[valid_plots]

  list(
    data = entropy_data,
    plots = plots_list,
    years_range = c(start_year = start_year, end_year = end_year)
  )
}
