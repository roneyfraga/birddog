#' Calculate Citation Cycle Time (CCT) indicator
#'
#' @description
#' Calculates the Citation Cycle Time (CCT) to measure the pace of scientific
#' or technological progress in a publication network. Based on Kayal (1999),
#' the indicator measures the median age of cited publications, where lower
#' values indicate faster knowledge replacement cycles.
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
#' @param min_papers_per_year Minimum number of papers required in a given year
#'   to compute CCT. Years with fewer papers are reported as NA (default: 3).
#' @param rolling_window Optional integer for rolling window smoothing.
#'   If provided, CCT values are smoothed using a centered moving average of the
#'   specified width (e.g., 3 for a 3-year window). Default is NULL (no smoothing).
#'
#' @details
#' The Citation Cycle Time (CCT) is calculated following Kayal (1999):
#' \enumerate{
#'   \item Extract citation IDs from the network's CR column
#'   \item Fetch publication years for cited works from OpenAlex API using `get_openalex_fields()`
#'   \item For each publication, calculate the age of each cited reference (PY - CR_PY)
#'   \item Calculate the median citation age per publication
#'   \item For each year, calculate the median of per-publication medians across
#'         all publications in that year (annual mode)
#' }
#'
#' Lower CCT values indicate that publications are citing more recent work, suggesting
#' a faster pace of knowledge replacement. A sudden drop in CCT within a group signals
#' potential scientific emergence.
#'
#' The function automatically handles:
#' \itemize{
#'   \item Splitting semicolon-separated citation IDs
#'   \item Batch processing of OpenAlex API requests
#'   \item Filtering invalid citations (where cited work was published after citing work)
#'   \item Skipping years with too few papers (`min_papers_per_year`)
#'   \item Optional rolling window smoothing for noisy time series
#'   \item Creating temporal plots for each group
#' }
#'
#' @return
#' A list with the following components:
#' \item{data}{Tibble with CCT data containing columns: group, year, index}
#' \item{plots}{Named list of plotly objects showing temporal evolution of CCT for each group.
#'   Each plot shows both absolute CCT values and year-over-year differences.}
#' \item{years_range}{Named vector with start_year and end_year used in the analysis}
#' \item{tracked_cr_py}{Citation year data with columns CR and CR_PY.
#'   Can be saved and reused in subsequent analyses to avoid repeated API calls.}
#'
#' @references
#' Kayal AA, Waters RC. An empirical evaluation of the technology cycle time indicator
#' as a measure of the pace of technological progress in superconductor technology.
#' IEEE Transactions on Engineering Management. 1999;46(2):127-31.
#' \doi{10.1109/17.759138}
#'
#' @examples
#' \dontrun{
#' # Group analysis
#' results <- sniff_citations_cycle_time(network_groups, scope = "groups")
#'
#' # Network analysis
#' results_network <- sniff_citations_cycle_time(complete_network, scope = "network")
#'
#' # With rolling window smoothing
#' results_smooth <- sniff_citations_cycle_time(
#'   network_groups,
#'   scope = "groups",
#'   rolling_window = 3
#' )
#'
#' # Accessing results
#' cct_data <- results$data
#' plots <- results$plots
#' plots$c1g1  # View plot for specific group
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
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter mutate left_join group_by summarise pull n
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_trim
#' @importFrom purrr map map_dfr
#' @importFrom igraph V
#' @importFrom glue glue
#' @importFrom stats median
#' @export
sniff_citations_cycle_time <- function(
  network,
  scope = "groups",
  start_year = NULL,
  end_year = NULL,
  tracked_cr_py = NULL,
  batch_size = 50,
  min_papers_per_year = 3,
  rolling_window = NULL) {

  # Input validation
  if (is.null(network)) {
    stop("Network data not found in groups object", call. = FALSE)
  }

  required_scope <- c("network", "groups")
  if (!scope %in% required_scope) {
    stop(glue::glue("scope must be: {paste(required_scope, collapse = ' or ')}"), call. = FALSE)
  }

  if (!is.null(rolling_window)) {
    if (!is.numeric(rolling_window) || rolling_window < 2) {
      stop("rolling_window must be a numeric value >= 2", call. = FALSE)
    }
    rolling_window <- as.integer(rolling_window)
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

  # --- Fetch/extract citation years ---
  if (is.null(tracked_cr_py)) {
    # Detect data source
    data_source <- gsub("_.*$", "", igraph::V(net_data)$DB[!is.na(igraph::V(net_data)$DB)][1])

    net_data |>
      tidygraph::activate(nodes) |>
      tibble::as_tibble() |>
      dplyr::select(.data$name, .data$PY, .data$CR) |>
      dplyr::filter(!is.na(.data$CR) & .data$CR != "") ->
      citation_data

    citation_data |>
      tidyr::separate_rows(.data$CR, sep = ";") |>
      dplyr::mutate(CR = stringr::str_trim(.data$CR)) |>
      dplyr::filter(.data$CR != "") ->
      citation_data_split

    if (data_source == "openalex") {
      # OpenAlex: fetch years via API
      get_openalex_fields(
        citation_data_split$CR,
        variables = "publication_year",
        batch_size = batch_size
      ) ->
        openalex_years

      citation_data_split |>
        dplyr::left_join(openalex_years, by = c("CR" = "id")) |>
        dplyr::rename(CR_PY = .data$publication_year) |>
        dplyr::filter(!is.na(.data$CR_PY)) |>
        dplyr::filter(.data$PY > .data$CR_PY) |>
        dplyr::select(.data$CR, .data$CR_PY) |>
        dplyr::distinct() ->
        tracked_cr_py_clean
    } else {
      # WoS: extract year directly from citation string
      # WoS CR format: "AUTHOR, YEAR, JOURNAL, VOL, PAGE, DOI ..."
      citation_data_split |>
        dplyr::mutate(
          CR_PY = as.numeric(stringr::str_extract(.data$CR, "(?<=,\\s?)\\d{4}(?=,)"))
        ) |>
        dplyr::filter(!is.na(.data$CR_PY)) |>
        dplyr::filter(.data$PY > .data$CR_PY) |>
        dplyr::select(.data$CR, .data$CR_PY) |>
        dplyr::distinct() ->
        tracked_cr_py_clean
    }
  } else {
    # Ensure only CR and CR_PY columns are used
    tracked_cr_py |>
      dplyr::select(dplyr::any_of(c("CR", "CR_PY"))) ->
      tracked_cr_py_clean
  }

  # --- Prepare citation data for CCT computation ---
  net_data |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::select(.data$name, .data$group, .data$PY, .data$CR) |>
    dplyr::filter(!is.na(.data$CR) & .data$CR != "") |>
    tidyr::separate_rows(.data$CR, sep = ";") |>
    dplyr::mutate(CR = stringr::str_trim(.data$CR)) |>
    dplyr::filter(.data$CR != "") |>
    dplyr::left_join(tracked_cr_py_clean, by = "CR") |>
    dplyr::filter(!is.na(.data$CR_PY)) |>
    dplyr::filter(.data$PY > .data$CR_PY) |>
    dplyr::mutate(diff = .data$PY - .data$CR_PY) |>
    dplyr::filter(.data$group %in% !!group) ->
    all_citation_data

  years_seq <- start_year:end_year

  # --- CCT computation: annual, median-based ---
  cct_list <- purrr::map(group, function(grp) {
    group_citations <- all_citation_data |>
      dplyr::filter(.data$group == grp)

    purrr::map_dfr(years_seq, function(current_year) {
      # Annual: only papers published in this year
      year_data <- group_citations |>
        dplyr::filter(.data$PY == current_year)

      # Count unique papers in this year
      n_papers <- length(unique(year_data$name))

      if (n_papers < min_papers_per_year) {
        return(tibble::tibble(index = NA_real_, year = current_year, group = grp))
      }

      # Per-paper median citation age, then median across papers
      year_data |>
        dplyr::group_by(.data$name) |>
        dplyr::summarise(paper_median = stats::median(.data$diff, na.rm = TRUE), .groups = "drop") |>
        dplyr::summarise(index = stats::median(.data$paper_median, na.rm = TRUE)) |>
        dplyr::pull(.data$index) ->
        cct_value

      tibble::tibble(index = cct_value, year = current_year, group = grp)
    })
  })

  names(cct_list) <- group

  # --- Optional rolling window smoothing ---
  if (!is.null(rolling_window)) {
    cct_list <- purrr::map(cct_list, function(df) {
      idx <- df$index
      n <- length(idx)
      half_w <- floor(rolling_window / 2)
      smoothed <- rep(NA_real_, n)

      for (i in seq_len(n)) {
        lo <- max(1, i - half_w)
        hi <- min(n, i + half_w)
        window_vals <- idx[lo:hi]
        non_na <- window_vals[!is.na(window_vals)]
        if (length(non_na) >= 2) {
          smoothed[i] <- stats::median(non_na, na.rm = TRUE)
        }
      }

      df$index <- smoothed
      df
    })
  }

  # --- Plots ---
  dplyr::bind_rows(cct_list) |>
    dplyr::select("group", "year", "index") ->
    cct_data

  plots_list <- purrr::map(group, function(group_name) {
    group_data <- cct_list[[group_name]]
    if (all(is.na(group_data$index))) {
      warning("No data available for group: ", group_name, " - skipping plot")
      return(NULL)
    }
    tryCatch(
      {
        indexes_plots(group_data, group_name = group_name, start_year, end_year, method = "cct")
      },
      error = function(e) {
        warning("Error creating plot for group ", group_name, ": ", e$message)
        return(NULL)
      }
    )
  })

  valid_plots <- !sapply(plots_list, is.null)
  plots_list <- plots_list[valid_plots]
  names(plots_list) <- group[valid_plots]

  list(
    data = cct_data,
    plots = plots_list,
    years_range = c(start_year = start_year, end_year = end_year),
    tracked_cr_py = tracked_cr_py_clean
  )
}
