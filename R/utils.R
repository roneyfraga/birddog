#' Natural sort for alphanumeric strings
#'
#' Sort character vectors containing embedded numbers in natural numeric
#' order rather than lexicographic order. For example, `"c1g2"` comes before
#' `"c1g10"`, which standard [sort()] would not guarantee.
#'
#' @details
#' The function extracts all numeric segments from each string using
#' [stringr::str_extract_all()] and sorts by them sequentially (first
#' number, then second). Designed for two-segment identifiers such as
#' `"c1g1"`, `"c2g10"`, etc.
#'
#' @param x Character vector to sort.
#'
#' @return A character vector sorted in natural numeric order.
#'
#' @seealso [sort()] for standard lexicographic sorting.
#'
#' @examples
#' mixed_sort(c("c1g10", "c1g2", "c1g1", "c1g9"))
#'
#' mixed_sort(c("group12", "group1", "group3"))
#'
#' @export
mixed_sort <- function(x) {
  nums <- stringr::str_extract_all(x, "\\d+")
  ord <- order(
    as.numeric(vapply(nums, `[`, character(1), 1)),
    as.numeric(vapply(nums, `[`, character(1), 2))
  )
  x[ord]
}
