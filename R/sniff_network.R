#' Create Citation Networks from Bibliographic Data
#'
#' Constructs different types of citation networks from bibliographic data imported
#' from Web of Science or OpenAlex using `birddog's` reading functions.
#'
#' @param dataframe A data frame imported via `read_openalex()` or `read_wos()`
#' @param type Type of network to create. One of:
#'   - "direct citation": Direct citation links between documents
#'   - "bibliographic coupling": Documents linked by shared references
#' @param external_references Logical indicating whether to include external references
#'   (references not in the original dataset) as nodes in the network
#'
#' @return A `tbl_graph` object from the tidygraph package representing the citation network.
#'   Node attributes include bibliographic information from the input data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using OpenAlex data
#' oa_data <- read_openalex("works.csv", format = "csv")
#' net <- sniff_network(oa_data, type = "direct citation")
#'
#' # Using WoS data
#' wos_data <- read_wos("savedrecs.txt")
#' net <- sniff_network(wos_data, type = "bibliographic coupling", external_references = TRUE)
#' }
#' @importFrom rlang .data
sniff_network <- function(dataframe, type = "direct citation", external_references = FALSE) {

  # Validate input parameters
  network_types <- c("direct citation", "bibliographic coupling")
  type <- match.arg(tolower(type), network_types)
  
  if (!inherits(dataframe, "data.frame")) {
    stop("Input must be a data frame", call. = FALSE)
  }
  
  if (!"DB" %in% names(dataframe)) {
    stop("Input data frame must contain 'DB' column. Use birddog's read functions.", call. = FALSE)
  }
  
  dataframe |>
    tibble::as_tibble() |>
    dplyr::distinct(.data$SR, .keep_all = TRUE) ->
    M

  data_source_format <- M$DB[1]
  data_source <- gsub("_.*$", "", M$DB[1])

  if (data_source_format == "openalex_api") {
    M |>
      dplyr::select(.data$SR, .data$CR) |>
      dplyr::filter(!is.na(SR), !is.na(.data$CR)) |>
      dplyr::filter(.data$SR != "", .data$CR != "") |>
      tidyr::separate_rows(.data$CR, sep = ";") |>
      dplyr::rename(from = .data$SR, to = .data$CR) ->
      edges_df

    if (type == "direct citation") {
      if (external_references == FALSE) {
        edges_df |>
          dplyr::filter(.data$to %in% M$SR) ->
          edges_df

        igraph::graph_from_data_frame(edges_df, directed = TRUE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M |> dplyr::mutate(name = .data$SR), by = "name") ->
          net
      }

      if (external_references == TRUE) {
        tibble::tibble(name = unique(c(edges_df$from, edges_df$to))) |>
          dplyr::left_join(M |> dplyr::mutate(name = .data$SR, main_query = TRUE) |> dplyr::select(.data$name, .data$main_query), by = "name") ->
          M_extended

        igraph::graph_from_data_frame(edges_df, directed = TRUE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_extended, by = "name") |>
          dplyr::mutate(main_query = ifelse(is.na(.data$main_query), FALSE, TRUE)) ->
          net
      }
    }

    if (type == "bibliographic coupling") {
      if (external_references == FALSE) {
        citing <- factor(edges_df$from)
        cited <- factor(edges_df$to)

        Matrix::sparseMatrix(
          i = as.integer(citing),
          j = as.integer(cited),
          x = 1,
          dims = c(length(levels(citing)), length(levels(cited))),
          dimnames = list(levels(citing), levels(cited))
        ) |>
          Matrix::Matrix(A, sparse = TRUE) ->
          A

        B <- A %*% Matrix::t(A)

        igraph::graph_from_adjacency_matrix(B, mode = "undirected", weighted = TRUE, diag = FALSE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M |> dplyr::mutate(name = .data$SR), by = "name") ->
          net
      }
      if (external_references == TRUE) {
        citing <- factor(edges_df$from)
        cited <- factor(edges_df$to)

        Matrix::sparseMatrix(
          i = as.integer(citing),
          j = as.integer(cited),
          x = 1,
          dims = c(length(levels(citing)), length(levels(cited))),
          dimnames = list(levels(citing), levels(cited))
        ) |>
          Matrix::Matrix(A, sparse = TRUE) ->
          A

        B <- Matrix::t(A) %*% A

        tibble::tibble(name = unique(c(edges_df$from, edges_df$to))) |>
          dplyr::left_join(M |> dplyr::mutate(name = .data$SR, main_query = TRUE) |> dplyr::select(.data$name, .data$main_query), by = "name") ->
          M_extended

        igraph::graph_from_adjacency_matrix(B, mode = "undirected", weighted = TRUE, diag = FALSE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_extended, by = "name") |>
          dplyr::mutate(main_query = ifelse(is.na(.data$main_query), FALSE, TRUE)) ->
          net
      }
    }

    return(net)
  }

  if (data_source_format == "openalex_csv") {

    if (!all(c("CR", "SR") %in% names(M))) stop("Error. The dataframe must generated by birddog function: read_openalex() or read_wos() with normalized_names = TRUE.")

    M |>
      dplyr::select(.data$SR, .data$CR) |>
      dplyr::filter(!is.na(.data$SR), !is.na(.data$CR)) |>
      dplyr::filter(.data$SR != "", .data$CR != "") |>
      tidyr::separate_rows(.data$CR, sep = "\\|") |>
      dplyr::rename(from = .data$SR, to = .data$CR) ->
      edges_df

    if (type == "direct citation") {
      if (external_references == FALSE) {
        edges_df |>
          dplyr::filter(.data$to %in% M$SR) ->
          edges_df

        igraph::graph_from_data_frame(edges_df, directed = TRUE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M |> dplyr::mutate(name = .data$SR), by = "name") ->
          net
      }

      if (external_references == TRUE) {
        tibble::tibble(name = unique(c(edges_df$from, edges_df$to))) |>
          dplyr::left_join(M |> dplyr::mutate(name = SR, main_query = TRUE) |> dplyr::select(.data$name, .data$main_query), by = "name") |>
          dplyr::distinct(.data$name, .keep_all = TRUE) ->
          M_extended

        igraph::graph_from_data_frame(edges_df, directed = TRUE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_extended, by = "name") |>
          dplyr::mutate(main_query = ifelse(is.na(.data$main_query), FALSE, TRUE)) ->
          net
      }
    }

    if (type == "bibliographic coupling") {
      if (external_references == FALSE) {
        citing <- factor(edges_df$from)
        cited <- factor(edges_df$to)

        Matrix::sparseMatrix(
          i = as.integer(citing),
          j = as.integer(cited),
          x = 1,
          dims = c(length(levels(citing)), length(levels(cited))),
          dimnames = list(levels(citing), levels(cited))
        ) |>
          Matrix::Matrix(A, sparse = TRUE) ->
          A

        B <- A %*% Matrix::t(A)

        igraph::graph_from_adjacency_matrix(B, mode = "undirected", weighted = TRUE, diag = FALSE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M |> dplyr::mutate(name = .data$SR), by = "name") ->
          net
      }
      if (external_references == TRUE) {
        citing <- factor(edges_df$from)
        cited <- factor(edges_df$to)

        Matrix::sparseMatrix(
          i = as.integer(citing),
          j = as.integer(cited),
          x = 1,
          dims = c(length(levels(citing)), length(levels(cited))),
          dimnames = list(levels(citing), levels(cited))
        ) |>
          Matrix::Matrix(A, sparse = TRUE) ->
          A

        B <- Matrix::t(A) %*% A

        tibble::tibble(name = unique(c(edges_df$from, edges_df$to))) |>
          dplyr::left_join(M |> dplyr::mutate(name = .data$SR, main_query = TRUE) |> dplyr::select(.data$name, .data$main_query), by = "name") ->
          M_extended

        igraph::graph_from_adjacency_matrix(B, mode = "undirected", weighted = TRUE, diag = FALSE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_extended, by = "name") |>
          dplyr::mutate(main_query = ifelse(is.na(.data$main_query), FALSE, TRUE)) ->
          net
      }
    }

    return(net)
  }

  if (data_source == "wos") {
    if (!all(c("CR", "SR") %in% names(M))) stop("Error. The dataframe must generated by birddog function: read_openalex() or read_wos() with normalized_names = TRUE.")

    M |>
      dplyr::select(.data$SR, .data$DI, .data$DI2, .data$CR) |>
      dplyr::filter(!is.na(.data$DI)) |>
      dplyr::distinct(.data$DI2, .keep_all = TRUE) ->
      M_id

    M_id |>
      dplyr::select(.data$DI2, .data$CR) |>
      tidyr::separate_rows(.data$CR, sep = ";") |>
      dplyr::mutate(CR = stringr::str_replace_all(.data$CR, "^.*DOI ", "")) |>
      dplyr::mutate(CR_DI = .data$CR) |>
      dplyr::mutate(CR = stringr::str_squish(toupper(stringr::str_replace_all(.data$CR, "[:punct:]", "")))) |>
      dplyr::filter(!nchar(.data$CR) <= 6) |>
      dplyr::rename(from = .data$DI2, to = .data$CR) ->
      edges_df

    if (type == "direct citation") {
      if (external_references == FALSE) {
        edges_df |>
          dplyr::filter(.data$to %in% M_id$DI2) ->
          edges_df

        igraph::graph_from_data_frame(edges_df, directed = TRUE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_id |> dplyr::mutate(name = .data$DI2), by = "name") |>
          dplyr::left_join(M |> dplyr::select(.data$AU, .data$TI, .data$PY, .data$DE, .data$AB, .data$SO, .data$TC, .data$SR, .data$DB), by = "SR") ->
          net
      }

      if (external_references == TRUE) {
        tibble::tibble(name = unique(c(edges_df$from, edges_df$to))) |>
          dplyr::left_join(M_id |> dplyr::mutate(name = .data$DI2, main_query = TRUE) |> dplyr::select(-.data$CR), by = "name") ->
          M_extended

        edges_df |>
          dplyr::select(name = .data$to, .data$CR_DI) |>
          dplyr::distinct(.data$name, .keep_all = TRUE) ->
          external_references_doi

        M_extended |>
          dplyr::left_join(external_references_doi, by = "name") |>
          dplyr::mutate(DI = ifelse(is.na(.data$DI), .data$CR_DI, .data$DI)) |>
          dplyr::select(-.data$CR_DI, -.data$DI2) |>
          dplyr::mutate(main_query = ifelse(is.na(.data$main_query), FALSE, TRUE)) ->
          M_extended_v2

        igraph::graph_from_data_frame(edges_df, directed = TRUE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_extended_v2, by = "name") ->
          net
      }
    }

    if (type == "bibliographic coupling") {
      if (external_references == FALSE) {
        citing <- factor(edges_df$from)
        cited <- factor(edges_df$to)

        Matrix::sparseMatrix(
          i = as.integer(citing),
          j = as.integer(cited),
          x = 1,
          dims = c(length(levels(citing)), length(levels(cited))),
          dimnames = list(levels(citing), levels(cited))
        ) |>
          Matrix::Matrix(A, sparse = TRUE) ->
          A

        B <- A %*% Matrix::t(A)

        igraph::graph_from_adjacency_matrix(B, mode = "undirected", weighted = TRUE, diag = FALSE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_id |> dplyr::mutate(name = .data$DI2), by = "name") |>
          dplyr::left_join(M |> dplyr::select(.data$AU, .data$TI, .data$PY, .data$DE, .data$AB, .data$SO, .data$TC, .data$SR, .data$DB), by = "SR") |>
          dplyr::select(-.data$DI2) ->
          net
      }
      if (external_references == TRUE) {
        citing <- factor(edges_df$from)
        cited <- factor(edges_df$to)

        Matrix::sparseMatrix(
          i = as.integer(citing),
          j = as.integer(cited),
          x = 1,
          dims = c(length(levels(citing)), length(levels(cited))),
          dimnames = list(levels(citing), levels(cited))
        ) |>
          Matrix::Matrix(A, sparse = TRUE) ->
          A

        B <- Matrix::t(A) %*% A

        tibble::tibble(name = unique(c(edges_df$from, edges_df$to))) |>
          dplyr::left_join(M_id |> dplyr::mutate(name = .data$DI2, main_query = TRUE) |> dplyr::select(-.data$CR), by = "name") ->
          M_extended

        edges_df |>
          dplyr::select(name = .data$to, .data$CR_DI) |>
          dplyr::distinct(.data$name, .keep_all = TRUE) ->
          external_references_doi

        M_extended |>
          dplyr::left_join(external_references_doi, by = "name") |>
          dplyr::mutate(DI = ifelse(is.na(.data$DI), .data$CR_DI, .data$DI)) |>
          dplyr::select(-.data$CR_DI, -.data$DI2) |>
          dplyr::mutate(main_query = ifelse(is.na(.data$main_query), FALSE, TRUE)) ->
          M_extended_v2

        igraph::graph_from_adjacency_matrix(B, mode = "undirected", weighted = TRUE, diag = FALSE) |>
          igraph::simplify() |>
          tidygraph::as_tbl_graph() |>
          tidygraph::activate(nodes) |>
          dplyr::left_join(M_extended_v2, by = "name") ->
          net
      }
    }

    return(net)
  }
}
