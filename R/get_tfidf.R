
#' @title get_tfidf
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param groups PARAM_DESCRIPTION
#' @param keywords PARAM_DESCRIPTION
#' @param sep PARAM_DESCRIPTION, Default: ';'
#' @param n_keywords PARAM_DESCRIPTION, Default: 15
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()) {
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{nse-defuse}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{rename}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{count}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}},\code{\link[dplyr]{top_n}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{separate_rows}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[tidytext]{bind_tf_idf}}
#' @rdname get_tfidf
#' @export 
#' @importFrom rlang enquo
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename filter select mutate group_by tally ungroup arrange desc top_n summarise left_join full_join
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_trim
#' @importFrom tidytext bind_tf_idf
#' @importFrom rlang .data
get_tfidf <- function(data, groups, keywords, sep = ';',  n_keywords = 15) {

    group <- rlang::enquo(groups)
    DE <- rlang::enquo(keywords)

    data |>
        tibble::as_tibble() |>
        dplyr::rename(group = !!group, DE = !!DE) |> 
        dplyr::filter(!is.na(.data$group)) |> 
        dplyr::filter(!is.na(.data$DE)) |> 
        dplyr::select(.data$group, .data$DE) |>
        tidyr::separate_rows(.data$DE, sep = sep) |>
        dplyr::mutate(DE = stringr::str_trim(.data$DE)) |>
        dplyr::group_by(.data$group, .data$DE) |> 
        dplyr::tally(sort = T) |> 
        dplyr::ungroup() |>
        dplyr::arrange(.data$group, dplyr::desc(.data$n)) |> 
        dplyr::mutate(DE = stringr::str_trim(.data$DE)) ->
        grupoDEfreq

    grupoDEfreq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::arrange(.data$group, dplyr::desc(.data$n)) |> 
        dplyr::top_n(n_keywords) |> 
        dplyr::filter(.data$n > 1) |> 
        dplyr::mutate(keywords_freq = paste0(.data$DE, ' (', .data$n, ')')) |> 
        dplyr::select(-.data$n) |> 
        dplyr::ungroup() ->
        keywords_freq

    grupoDEfreq |>
        dplyr::group_by(.data$group) |>
        dplyr::summarise(total = sum(.data$n)) ->
        total_DE

    dplyr::left_join(grupoDEfreq, total_DE) |>
        tidytext::bind_tf_idf(.data$DE, .data$group, .data$n) ->
        tfidf

    tfidf |>
        dplyr::arrange(.data$group, dplyr::desc(.data$tf_idf)) |>
        dplyr::group_by(.data$group) |> 
        dplyr::top_n(n_keywords) |> 
        dplyr::filter(.data$n > 1) |> 
        dplyr::mutate(keywords_tfidf = paste0(.data$DE, ' (', .data$n, ')')) |> 
        dplyr::select(-.data$n) |> 
        dplyr::ungroup() |>
        dplyr::select(.data$group, .data$keywords_tfidf) ->
        tfidf_freq

    keywords_freq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::summarise(keywords_freq = paste(.data$keywords_freq, collapse = ', ')) -> 
        keywords_freq2

    tfidf_freq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::summarise(keywords_tfidf = paste(.data$keywords_tfidf, collapse = ', ')) -> 
        tfidf_freq2

    dplyr::full_join(keywords_freq2, tfidf_freq2) 

}

