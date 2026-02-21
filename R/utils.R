#' Natural sort for alphanumeric group names
#'
#' Sorts strings like "c1g1", "c1g2", "c1g10" in natural numeric order
#' by extracting and comparing embedded numbers sequentially.
#'
#' @param x Character vector to sort
#' @return Sorted character vector
#' @keywords internal
mixed_sort <- function(x) {
  nums <- stringr::str_extract_all(x, "\\d+")
  ord <- order(
    as.numeric(vapply(nums, `[`, character(1), 1)),
    as.numeric(vapply(nums, `[`, character(1), 2))
  )
  x[ord]
}
