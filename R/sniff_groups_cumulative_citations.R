#' Calculate Cumulative Citations by Group and Year
#'
#' This function calculates cumulative citations for papers within research groups,
#' tracking how citations accumulate over time for highly cited papers.
#'
#' @param groups A list containing network data with the following components:
#'   \itemize{
#'     \item{network: A tidygraph network object}
#'     \item{pubs_by_year: Publication counts by year}
#'     \item{aggregate: Aggregate network statistics}
#'   }
#' @param min_citations Minimum number of citations for a paper to be included in analysis (default: 10).
#'
#' @return A named list (by research group) where each element contains a tibble with:
#'   \itemize{
#'     \item \code{group}: Research group identifier
#'     \item \code{SR}: Paper identifier
#'     \item \code{TC}: Total citations
#'     \item \code{PY}: Publication year
#'     \item \code{Ki}: Total network citations
#'     \item \code{citations_by_year}: A tibble with annual citation counts
#'       (PY: year, citations: count)
#'     \item \code{growth_power}: Growth power score (0-100)
#'     \item \code{growth_consistency}: Percentage of years with citations
#'     \item \code{peak_momentum}: Highest 3-year rolling average citation count
#'     \item \code{early_impact}: Citations in first 5 years
#'     \item \code{recent_momentum}: Citations in last 3 years
#'     \item \code{acceleration_factor}: Ratio of late to early citations
#'   }
#'
#' @details
#' For each research group, the function:
#' \itemize{
#'   \item Identifies papers with citations above the threshold
#'   \item Tracks citations to these papers year by year
#'   \item Calculates cumulative citation patterns
#'   \item Computes various growth metrics for citation analysis
#' }
#'
#' Works with both Web of Science (WOS) and OpenAlex data formats.
#'
#' @examples
#' \dontrun{
#' # Assuming groups is output from sniff_groups()
#' # Calculate cumulative citations
#' groups_cumulative_citations <- sniff_groups_cumulative_citations(groups, min_citations = 5)
#' # View results for first group
#' head(groups_cumulative_citations[[1]])
#' }
#'
#' @importFrom dplyr filter select mutate arrange desc group_by tally slice_head left_join
#' @importFrom tidygraph activate
#' @importFrom tibble tibble
#' @importFrom purrr map map_int map2 compact
#' @importFrom tidyr unnest
#' @importFrom stringr str_count
#' @importFrom stats setNames quantile
#' @importFrom utils tail
#' @importFrom rlang .data
#' @export

sniff_groups_cumulative_citations <- function(groups, min_citations = 5) {
  # Validate input structure
  if (!is.list(groups)) {
    stop("groups must be a list containing network data", call. = FALSE)
  }

  # Check for required components
  required_components <- c("network", "pubs_by_year", "aggregate")
  missing_components <- setdiff(required_components, names(groups))
  if (length(missing_components) > 0) {
    stop("groups is missing required components: ", paste(missing_components, collapse = ", "), call. = FALSE)
  }

  # Validate min_citations parameter
  if (!is.numeric(min_citations) || length(min_citations) != 1 || min_citations < 0 || !is.finite(min_citations)) {
    stop("min_citations must be a single non-negative finite numeric value", call. = FALSE)
  }

  # Extract and validate network data
  net <- groups$network
  net |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::mutate(TC = as.numeric(TC), PY = as.numeric(PY)) ->
    data

  if (nrow(data) == 0) {
    stop("Network contains no nodes", call. = FALSE)
  }

  # Check for required columns
  required_cols <- c("SR", "group", "TC", "PY", "DB")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Network nodes are missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Validate and process data source
  data_source <- gsub("_.*$", "", data$DB[1])
  if (!data_source %in% c("wos", "openalex")) {
    stop("Unsupported data source. Currently only 'wos' and 'openalex' are supported.", call. = FALSE)
  }

  # Initialize results structure
  grupos <- sort(unique(data$group))
  results <- stats::setNames(vector("list", length(grupos)), grupos)

  # Process each group
  for (h in seq_along(grupos)) {
    current_group <- grupos[h]

    # Filter and prepare group data
    data |>
      dplyr::filter(group == current_group) |>
      dplyr::filter(TC > min_citations) ->
      group_data

    if (nrow(group_data) == 0) {
      results[[h]] <- NULL
      next
    }

    # Handle different data sources
    if (data_source == "wos") {
      group_data |>
        dplyr::select(group, SR, PY, TC, to_search = name) ->
        TIpost
    } else { # openalex
      group_data |>
        dplyr::select(group, SR, PY, TC, SR) |>
        dplyr::mutate(to_search = SR) ->
        TIpost
    }

    # Prepare citation data by publication year
    data |>
      dplyr::select(SR, PY, CR) |>
      dplyr::filter(!is.na(CR)) |>
      dplyr::filter(!is.na(SR)) |>
      split(~PY) ->
      yearly_citation_data

    # Handle different data sources
    if (data_source == "wos") {
      yearly_citation_data |>
        purrr::map(\(x) x |> dplyr::mutate(CR = toupper(gsub("[[:punct:]]", "", CR)))) ->
        yearly_citation_data
    }

    # Calculate citations by year for each paper
    citation_results <- vector(mode = "list", length = nrow(TIpost))
    names(citation_results) <- TIpost$SR

    for (i in seq_along(citation_results)) {
      start_year <- TIpost$PY[i]
      searchable_years <- names(yearly_citation_data)[as.numeric(names(yearly_citation_data)) >= start_year]

      tibble::tibble(
        SR = TIpost$SR[i],
        PY = as.numeric(searchable_years),
        citations = NA_integer_
      ) ->
        citation_results[[i]]

      for (k in seq_along(searchable_years)) {
        year <- searchable_years[k]
        citation_results[[i]]$citations[k] <- sum(stringr::str_count(yearly_citation_data[[year]]$CR, TIpost$to_search[i]), na.rm = TRUE)
      }
    }
    
    # Compile final results for this group
    TIpost |>
      dplyr::select(- to_search) |>
      dplyr::mutate(
        Ki = purrr::map_int(citation_results, \(x) sum(x$citations, na.rm = TRUE)),
        citations_by_year = citation_results
      ) |>
      dplyr::filter(Ki > 0) |>
      dplyr::arrange(dplyr::desc(Ki)) ->
      results[[h]]

    results[[h]] |>
      dplyr::mutate(q3 = stats::quantile(Ki, 0.75, na.rm = TRUE)) |>
      dplyr::mutate(citations_by_year = purrr::map2(citations_by_year, q3, function(citations_tibble, added_variable) {
        citations_tibble |> dplyr::mutate(Ki_q3 = added_variable)
      })) |>
      dplyr::mutate(median = median(Ki, na.rm = TRUE)) |>
      dplyr::mutate(citations_by_year = purrr::map2(citations_by_year, median, function(citations_tibble, added_variable) {
        citations_tibble |> dplyr::mutate(Ki_median = added_variable)
      })) |>
      dplyr::mutate(citations_by_year = purrr::map2(citations_by_year, PY, function(citations_tibble, added_variable) {
        citations_tibble |> dplyr::mutate(publications_year = added_variable)
      })) |>
      dplyr::select(- q3, - median) ->
      results[[h]]

      # dplyr::select(citations_by_year) |>
      # tidyr::unnest(citations_by_year) ->
      # tt

      # tt |> 
      # dplyr::filter(SR == 'WOS:000276867500003') ->
      # citations_year_df

    results[[h]] |>
      dplyr::mutate(growth_metrics = purrr::map(citations_by_year, calculate_growth_power)) |>
      tidyr::unnest(growth_metrics) |>
      dplyr::select(- .data$total_citations) ->
      results[[h]]
  }

  # Remove empty groups
  results <- purrr::compact(results)

  return(results)
}

#' Calculate Growth Metrics for Citation Data
#'
#' Internal function to calculate various growth metrics from citation data.
#'
#' @param citations_year_df Data frame with citation data by year
#' @param publications_year Publication year of the paper
#'
#' @return A tibble with growth metrics
#'
#' @importFrom dplyr arrange mutate filter summarise pull
#' @importFrom tibble tibble
#' @importFrom utils tail
#' @keywords internal
calculate_growth_power <- function(citations_year_df, publications_year) {
  
  df <- citations_year_df |>
    dplyr::arrange(.data$PY) |>
    dplyr::mutate(
      years_since_pub = .data$PY - .data$publications_year,
      cumulative_cites = cumsum(.data$citations)
    )
  
  total_cites <- sum(df$citations)
  
  # Handle uncited papers
  if (total_cites == 0) {
    return(tibble::tibble(
      total_citations = 0,
      growth_power = 0,
      growth_consistency = 0,
      peak_momentum = 0,
      early_impact = 0,
      recent_momentum = 0,
      acceleration_factor = 1
    ))
  }
  
  # Metric 1: Growth Consistency
  # Measures what percentage of years the paper received citations
  active_years <- sum(df$citations > 0)
  total_years <- nrow(df)
  consistency <- active_years / total_years
  
  # Metric 2: Peak Momentum (3-year rolling average maximum)
  df_peak <- df |>
    dplyr::mutate(
      three_yr_avg = zoo::rollmean(.data$citations, k = 3, fill = NA, align = "right")
    )
  peak_momentum <- max(df_peak$three_yr_avg, na.rm = TRUE)
  
  # Metric 3: Early Impact (citations in first 5 years)
  early_impact <- df |>
    dplyr::filter(.data$years_since_pub <= 5) |>
    dplyr::summarise(early_cites = sum(.data$citations)) |>
    dplyr::pull(.data$early_cites)
  
  # Metric 4: Recent Momentum (citations in last 3 years)
  recent_momentum <- df |>
    utils::tail(3) |>
    dplyr::summarise(recent_cites = sum(.data$citations)) |>
    dplyr::pull(.data$recent_cites)
  
  # Metric 5: Acceleration Factor
  first_half <- df |>
    dplyr::filter(.data$years_since_pub <= total_years / 2) |>
    dplyr::summarise(mean_cites = mean(.data$citations)) |>
    dplyr::pull(.data$mean_cites)
  
  second_half <- df |>
    dplyr::filter(.data$years_since_pub > total_years / 2) |>
    dplyr::summarise(mean_cites = mean(.data$citations)) |>
    dplyr::pull(.data$mean_cites)
  
  acceleration <- ifelse(first_half > 0, second_half / first_half, 1)
  
  # Comprehensive Growth Power Score (0-100)
  growth_power <- (
    # 1. Base Impact (40%)
    (log1p(total_cites) / log1p(df$Ki_q3[[1]])) * 40 +
      
      # 2. Growth Pattern (25%)
      consistency * 25 +
      
      # 3. Peak Momentum (15%)
      (log1p(peak_momentum) / log1p(df$Ki_median[[1]])) * 15 +
      
      # 4. Recent Momentum (10%)
      (log1p(recent_momentum) / log1p(peak_momentum)) * 10 +
      
      # 5. Acceleration Bonus (10%)
      pmin(acceleration * 10, 10)
  )
  
  tibble::tibble(
    total_citations = total_cites,
    growth_power = round(growth_power, 1),
    growth_consistency = round(consistency, 3),
    peak_momentum = round(peak_momentum, 2),
    early_impact = early_impact,
    recent_momentum = recent_momentum,
    acceleration_factor = round(acceleration, 2)
  )
}
