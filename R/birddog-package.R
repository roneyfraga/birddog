#' birddog: sniffing emergence and trajectories in academic papers and patents
#'
#' Tools to detect emergence and trace technological/scientific trajectories
#' in papers and patents. It reads OpenAlex and Web of Science data, builds
#' citation-based networks, identifies groups, and summarizes their dynamics.
#'
#' @section Links:
#' - Website: <http://roneyfraga.com/birddog/>
#' - GitHub: <https://github.com/roneyfraga/birddog>
#' - Issues: <https://github.com/roneyfraga/birddog/issues>
#'
#' @section Pipeline:
#' `read_openalex()`/`read_wos()` -> `sniff_network()` -> `sniff_components()` ->
#' `sniff_groups_cumulative()` -> `sniff_groups_lineage()` ->
#' `sniff_trajectory_dag()` -> `sniff_trajectory_braid()` -> analysis
#' (`sniff_trajectory_confluence()`, `_formation()`, `_destination()`,
#' `_contribution()`, `_self_sufficiency()`, `_cct()`, `_entropy()`, `_hubs()`,
#' `_dynamics()`), each paired with a `plot_*` function. Focus an analysis with
#' `subset(flow, ...)`. "Flow" names the object kind, not an algorithm (stock
#' vs flow: groups are stock, trajectories are flow): every detector returns a
#' flow, and alternative detectors (`sniff_trajectory_<algo>()`) return the
#' same contract, checked by `validate_flow()`.
#'
#' @section Label grammar:
#' `cNgN` is a group (component N, group N); `y<YYYY><cNgN>` is a group-year
#' node; `tr::<cNgN>` is a central trajectory (reaches the last year, one per
#' final group); `tr1..trN` are absorbed trajectories. This grammar is part of
#' the public API and only changes at major versions.
#'
#' @section Theoretical background:
#' Trajectories are detected as system-level objects (Dosi, 1982): disjoint
#' chains in the temporal DAG of cumulative clusterings. A trajectory that stops
#' being detected loses its identity, not its papers; tracking its terminal
#' cohort to the last year classifies the outcome as convergence (one
#' destination), divergence (many), or dormancy (none) — with emergence
#' indicators on the living chains (Rotolo et al., 2015; Carley et al., 2017).
#'
#' @seealso Useful links:
#' \itemize{
#'   \item Report bugs at \url{https://github.com/roneyfraga/birddog/issues}
#' }
#'
#' @keywords package
"_PACKAGE"
