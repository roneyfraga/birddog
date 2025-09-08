#' Track Publication Years from References
#'
#' Processes citation data from network objects to extract and fetch publication years
#' of cited references. Handles both Web of Science and OpenAlex data sources
#' automatically.
#'
#' @param network A `tbl_graph` object from tidygraph containing network data
#' @param cr_column Name of the column containing citation references (default: "CR")
#' @param py_column Name of the column containing publication years (default: "PY")
#'
#' @return A `tbl_graph` object with added `CR_PY` column containing publication years
#' of cited references, separated by pipes ("|") for multiple references
#'
#' @importFrom tidygraph activate
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr select mutate rename distinct group_by summarise filter left_join
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom stringr str_squish str_extract
#' @importFrom igraph V
#' @export
#'
#' @examples
#' \dontrun{
#' # Process Web of Science network data
#' network_with_years <- track_publications_year_from_references(network_wos)
#'
#' # Process OpenAlex network data
#' network_with_years <- track_publications_year_from_references(network_oa)
#'
#' # Specify custom column names
#' network_with_years <- track_publications_year_from_references(
#'   network, 
#'   cr_column = "references", 
#'   py_column = "year"
#' )
#' }
track_publications_year_from_references <- function(network, cr_column = "CR", py_column = "PY") {
  # Input validation
  if (!inherits(network, "tbl_graph")) {
    stop("network must be a tidygraph object", call. = FALSE)
  }

  network |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    names() ->
    colunas

  # Check if required columns exist
  if (!cr_column %in% colunas) {
    stop("Column '", colunas, "' not found in network data", call. = FALSE)
  }

  if (!py_column %in% colunas) {
    stop("Column '", colunas, "' not found in network data", call. = FALSE)
  }

  data_base <- stringr::str_squish(gsub("_.*$", "", igraph::V(network)$DB[[1]]))

  if (data_base == "wos") {
    network |>
      tidygraph::activate(nodes) |>
      tibble::as_tibble() |>
      dplyr::select("name", PY = {{ py_column }}, CR = {{ cr_column }}) |>
      tidyr::separate_rows(.data$CR, sep = ';') |>
      dplyr::mutate(CR = stringr::str_squish(.data$CR)) |>
      dplyr::mutate(CR_PY = as.numeric(stringr::str_extract(.data$CR, "\\b(19|20)\\d{2}\\b"))) |>
      dplyr::filter(!is.na(.data$CR_PY)) |>
      dplyr::filter(.data$PY > .data$CR_PY) ->
      node_data
  }

  if (data_base == "openalex") {
    network |>
      tidygraph::activate(nodes) |>
      tibble::as_tibble() |>
      dplyr::select("name", PY = {{ py_column }}, CR = {{ cr_column }}) |>
      dplyr::mutate(CR = strsplit(.data$CR, "\\|")) |>
      tidyr::unnest("CR") ->
      oa_name_cr

    node_data <- fetch_publication_years(oa_name_cr)

  }

  return(node_data)
}

#' Fetch Publication Years for OpenAlex IDs
#'
#' Internal function to retrieve publication years for OpenAlex work IDs using
#' the OpenAlex API. Processes data in batches to avoid API rate limits.
#'
#' @param oa_data A tibble containing OpenAlex IDs in a column named "CR"
#' @param batch_size Number of IDs to process per API call (default: 100)
#'
#' @return A tibble with added CR_PY column containing publication years
#'
#' @importFrom openalexR oa_fetch
#' @importFrom dplyr mutate left_join
#' @importFrom tibble as_tibble
#' @keywords internal
fetch_publication_years <- function(oa_data, batch_size = 100) {
  
  # oa_data <- oa_name_cr_without_py

  message(glue::glue('Downloading Publications Years for {nrow(oa_data)} cited references using openalexR package.'))

  # Get unique OpenAlex IDs
  ids <- unique(oa_data$CR)
  valid_ids <- grep("^W\\d+$", ids, value = TRUE)
  
  if (length(valid_ids) == 0) {
    return(dplyr::mutate(oa_data, CR_PY = NA_integer_))
  }
  
  # Process in batches
  all_years <- list()
  
  for (i in seq(1, length(valid_ids), by = batch_size)) {
    batch_ids <- valid_ids[i:min(i + batch_size - 1, length(valid_ids))]
    
    batch_data <- openalexR::oa_fetch(
      entity = "works",
      identifier = batch_ids,
      options = list(select = c("id", "publication_year")),
      verbose = FALSE
    )
    
    if (!is.null(batch_data) && nrow(batch_data) > 0) {
      batch_df <- data.frame(
        CR = batch_data$id,
        CR_PY = as.integer(batch_data$publication_year)
      )
      all_years[[length(all_years) + 1]] <- batch_df
    }
    
    # Small delay to be API-friendly
    Sys.sleep(0.1)
  }
  
  # Combine results
  do.call(rbind, all_years) |>
    dplyr::mutate(CR = gsub("https://openalex.org/", "", .data$CR)) |>
    tibble::as_tibble() ->
    years_df
  
  # Merge with original data
  result <- dplyr::left_join(oa_data, years_df, by = "CR")
  
  message(glue::glue("Found Publication Year for {nrow(result)} references"))
  
  return(result)
}
