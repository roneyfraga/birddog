#' Extract representative keywords from grouped nodes
#'
#' This function processes nodes grouped in a network (typically by community detection),
#' and extracts the most frequent and the most distinctive keywords (using TF-IDF) from
#' a descriptor field such as keywords or subject terms.
#'
#' @param net_groups A list containing a `network` component of class `tbl_graph`,
#'   where each node has at least two attributes: `group` and `DE`.
#' @param n_terms Integer. The number of top terms to return per group, both by frequency and by TF-IDF. Default is 15.
#' @param min_freq Integer. Minimum frequency a term must have in a group to be considered. Default is 2.
#' @param sep Character. Separator used in the `DE` field to split multiple terms. Default is `";"`.
#'
#' @return A tibble with one row per group, containing two columns:
#'   \itemize{
#'     \item `term_freq`: the most frequent terms (with raw frequency).
#'     \item `term_tfidf`: the most distinctive terms (with TF-IDF scores).
#'   }
#' @examples
#' \dontrun{
#' # Assuming 'groups' is output from sniff_groups()
#' groups_keywords <- sniff_groups_keywords(groups)
#' }
#' @importFrom dplyr %>%
#' @importFrom tidygraph activate
#' @importFrom stringr str_trim str_to_lower
#' @importFrom tidyr separate_rows
#' @importFrom tibble as_tibble
#' @export
sniff_groups_keywords <- function(net_groups,
                                  n_terms = 15,
                                  min_freq = 1,
                                  sep = ";") {

  if (!is.list(net_groups)) {
    stop("net_groups must be a list containing network data")
  }

  required_components <- c("network", "pubs_by_year", "aggregate")
  if (!all(required_components %in% names(net_groups))) {
    stop(paste("net_groups must contain:", paste(required_components, collapse = ", ")))
  }

  data <- net_groups$network |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::select(.data$group, .data$DE) |>
    dplyr::filter(!is.na(.data$group), !is.na(.data$DE))

  grupoDEfreq <- data |>
    tidyr::separate_rows(.data$DE, sep = sep) |>
    dplyr::mutate(
      DE = stringr::str_trim(stringr::str_to_lower(.data$DE))
    ) |>
    dplyr::group_by(.data$group, .data$DE) |>
    dplyr::tally(name = "freq", sort = TRUE) |>
    dplyr::ungroup()

  keywords_freq <- grupoDEfreq |>
    dplyr::group_by(.data$group) |>
    dplyr::arrange(dplyr::desc(.data$freq)) |>
    dplyr::filter(.data$freq > min_freq) |>
    dplyr::slice_head(n = n_terms) |>
    dplyr::mutate(keywords_freq = paste0(.data$DE, " (", .data$freq, ")")) |>
    dplyr::select(.data$group, .data$keywords_freq) |>
    dplyr::ungroup()

  total_DE <- grupoDEfreq |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(total = sum(.data$freq), .groups = "drop")

  tfidf <- grupoDEfreq |>
    dplyr::left_join(total_DE, by = "group") |>
    tidytext::bind_tf_idf(.data$DE, .data$group, .data$freq)

  tfidf_freq <- tfidf |>
    dplyr::filter(.data$freq > min_freq) |>
    dplyr::arrange(.data$group, dplyr::desc(.data$tf_idf)) |>
    dplyr::group_by(.data$group) |>
    dplyr::slice_head(n = n_terms) |>
    dplyr::mutate(keywords_tfidf = paste0(.data$DE, " [", round(.data$tf_idf, 4), "] (", .data$freq, ")")) |>
    dplyr::select(.data$group, .data$keywords_tfidf) |>
    dplyr::ungroup()

  keywords_freq2 <- keywords_freq |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(term_freq = paste(.data$keywords_freq, collapse = "; "), .groups = "drop")

  tfidf_freq2 <- tfidf_freq |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(term_tfidf = paste(.data$keywords_tfidf, collapse = "; "), .groups = "drop")

  dplyr::full_join(keywords_freq2, tfidf_freq2, by = "group")
}
