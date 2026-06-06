#' One-mode projection of the trajectory x group bipartite
#'
#' Projects the bipartite graph from [sniff_trajectory_group_contribution()] onto
#' one side. `mode = "trajectory"` links trajectories that feed common final
#' groups (competition / co-construction of lineages); `mode = "group"` links
#' final groups that share trajectory sources (common intellectual heritage).
#'
#' @param contribution Output of [sniff_trajectory_group_contribution()].
#' @param mode `"trajectory"` (default) or `"group"`.
#' @return A weighted one-mode igraph.
#'
#' @seealso [sniff_trajectory_group_contribution()]
#'
#' @export
#' @importFrom igraph bipartite_projection
sniff_trajectory_projection <- function(contribution, mode = c("trajectory", "group")) {
  mode <- match.arg(mode)
  if (!is.list(contribution) || is.null(contribution$graph) ||
      !igraph::is_igraph(contribution$graph)) {
    stop("'contribution' must be the output of sniff_trajectory_group_contribution()",
         call. = FALSE)
  }
  pr <- igraph::bipartite_projection(contribution$graph, multiplicity = TRUE)
  # pr[[1]] = type FALSE = trajectory rows; pr[[2]] = type TRUE = group columns
  if (mode == "trajectory") pr[[1]] else pr[[2]]
}
