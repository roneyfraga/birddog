#' Identify Hub Papers in Research Groups
#'
#' This function analyzes citation networks to identify hub papers within research groups
#' based on their citation patterns. It calculates several metrics (Zi, Pi) to classify
#' papers into different hub categories.
#'
#' @param groups A list containing network data with the following components:
#'   \itemize{
#'     \item{network: A tidygraph network object}
#'     \item{pubs_by_year: Publication counts by year}
#'     \item{aggregate: Aggregate network statistics}
#'   }
#' @param min_citations Minimum number of citations for a paper to be considered (default: 1)
#'
#' @return A tibble containing:
#'   \itemize{
#'     \item{group: Research group identifier}
#'     \item{SR: Paper identifier}
#'     \item{TC: Total citations}
#'     \item{Ki: Total citations from all groups}
#'     \item{ki: Citations from within the same group}
#'     \item{Zi: Standardized within-group citation score}
#'     \item{Pi: Citation diversity index}
#'     \item{zone: Hub classification ("noHub", "R5", "R6", "R7")}
#'   }
#'
#' @details
#' The function classifies papers into hub categories based on:
#' \itemize{
#'   \item{R5: Knowledge hubs (Zi ≥ 2.5 and Pi ≤ 0.3)}
#'   \item{R6: Bridging hubs (Zi ≥ 2.5 and 0.3 < Pi ≤ 0.75)}
#'   \item{R7: Boundary-spanning hubs (Zi ≥ 2.5 and Pi > 0.75)}
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Assuming 'groups' is output from sniff_groups()
#'
#' # Identify hub papers
#' hubs <- sniff_groups_hubs(groups, min_citations = 5)
#'
#' # View results
#' head(hubs)
#' }
#'
#' @importFrom dplyr select mutate filter arrange starts_with right_join bind_rows
#' @importFrom tidygraph activate
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' @importFrom stringr str_count
#' @export
sniff_groups_hubs <- function(groups, min_citations = 1) {
  # Input validation
  if (!is.list(groups)) {
    stop("groups must be a list containing network data", call. = FALSE)
  }

  required_components <- c("network", "pubs_by_year", "aggregate")
  if (!all(required_components %in% names(groups))) {
    missing <- setdiff(required_components, names(groups))
    stop("groups is missing required components: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (!is.numeric(min_citations) || length(min_citations) != 1 || min_citations < 0) {
    stop("min_citations must be a single non-negative numeric value", call. = FALSE)
  }

  net <- groups$network

  # Convert network nodes to tibble
  net |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() ->
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

  data_source <- gsub("_.*$", "", data$DB[1])

  if (!data_source %in% c("wos", "openalex")) {
    stop("Unsupported data source. Currently only 'wos' and 'openalex' are supported.", call. = FALSE)
  }

  # Process data based on source
  if (data_source == "wos") {

    if (!all(c("TI", "DI") %in% colnames(data))) { stop("WOS data requires columns: TI, DI", call. = FALSE) }

    data |>
      dplyr::select(SR, group, TC, PY, TI, DI, DI2 = name, CR, DB) |>
      dplyr::mutate(
        PY = as.numeric(PY),
        TC = as.numeric(TC)
      ) |>
      dplyr::filter(TC >= min_citations) |>
      dplyr::arrange(group, dplyr::desc(TC)) |>
      dplyr::mutate(TIpost = DI2) |>
      dplyr::select(SR, group, TC, TIpost) ->
      M

  } else if (data_source == "openalex") {
    data |>
      dplyr::select(SR, group, TC, PY, CR, DB) |>
      dplyr::mutate(
        PY = as.numeric(PY),
        TC = as.numeric(TC)
      ) |>
      dplyr::filter(TC >= min_citations) |>
      dplyr::arrange(group, dplyr::desc(TC)) |>
      dplyr::mutate(TIpost = SR) |>
      dplyr::select(SR, group, TC, TIpost) ->
      M
  }

  # Prepare citation data by group
  net |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::select(SR, group, CR) |>
    dplyr::filter(!is.na(CR)) ->
    Ml

  if (data_source == "wos") {
    Ml <- Ml |> dplyr::mutate(CR = toupper(gsub("[[:punct:]]", "", CR)))
  }

  Ml <- split(Ml, Ml$group)

  # Calculate cross-group citations
  for (i in seq_len(nrow(M))) {
    for (k in seq_along(Ml)) {
      M[i, paste0("cited_from_", names(Ml)[k])] <- sum(stringr::str_count(Ml[[k]]$CR, M$TIpost[i]), na.rm = TRUE)
    }
  }

  # Calculate total citations (Ki)
  M$Ki <- rowSums(dplyr::select(M, dplyr::starts_with("cited_from")), na.rm = TRUE)

  # Calculate within-group citations (ki)
  M |>
    dplyr::select(SR, group, dplyr::starts_with("cited_from")) |>
    tidyr::pivot_longer(
      !c(SR, group),
      names_to = "cited_from",
      values_to = "ki"
    ) |>
    dplyr::mutate(cited_from = gsub("cited_from_", "", cited_from)) |>
    dplyr::filter(group == cited_from) |>
    dplyr::select(-cited_from) ->
    M_long

  dplyr::right_join(
    M_long,
    M,
    by = c("SR", "group"),
    relationship = "many-to-many"
  ) |>
    dplyr::select(SR, group, TC, Ki, ki, dplyr::starts_with("cited_from")) |>
    dplyr::arrange(group, dplyr::desc(Ki)) ->
    M

  # Calculate standardized within-group citation score (Zi)
  M_split <- split(M, M$group)

  lapply(M_split, function(x) {
    if (nrow(x) > 1 && stats::sd(x$ki, na.rm = TRUE) != 0) {
      x$Zi <- (x$ki - mean(x$ki, na.rm = TRUE)) / stats::sd(x$ki, na.rm = TRUE)
    } else {
      x$Zi <- NA_real_
    }
    dplyr::arrange(x, dplyr::desc(Zi))
  }) |>
    dplyr::bind_rows() ->
    M

  # Calculate citation diversity index (Pi)
  tt <- dplyr::select(M, dplyr::starts_with("cited_from"), Ki, SR)
  cite_cols <- grep("^cited_from_", names(tt), value = TRUE)

  for (col in cite_cols) {
    tt[[col]] <- (M[[col]] / M$Ki)^2
  }

  tt$Pi <- 1 - rowSums(dplyr::select(tt, dplyr::starts_with("cited_from")), na.rm = TRUE)

  # Combine results and classify hubs
  M |>
    dplyr::left_join(
      dplyr::select(tt, SR, Pi),
      by = "SR",
      relationship = "many-to-many"
    ) |>
    dplyr::arrange(group, dplyr::desc(Ki)) |>
    dplyr::select(group, SR, TC, Ki, ki, Zi, Pi) |>
    dplyr::mutate(
      zone = dplyr::case_when(
        Zi >= 2.5 & Pi <= 0.3 ~ "R5",
        Zi >= 2.5 & Pi > 0.3 & Pi <= 0.75 ~ "R6",
        Zi >= 2.5 & Pi > 0.75 ~ "R7",
        TRUE ~ "noHub"
      )
    ) ->
    M

  return(M)
}
