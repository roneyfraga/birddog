#' Hub roles aggregated to each flow trajectory (provincial vs bridging)
#'
#' Summarizes the document-level hub roles of [sniff_groups_hubs()] (the
#' Guimera--Amaral within-module z-score `Zi` and participation coefficient `Pi`)
#' over the documents of every [sniff_trajectory_braid()] trajectory. Provincial
#' hubs (low `Pi`) keep a lineage self-contained, consistent with convergence /
#' paradigmatic homogeneity; connector and boundary-spanning hubs (high `Pi`)
#' bridge across groups, consistent with divergence / branching.
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param hubs A [sniff_groups_hubs()] tibble: a document-id column (`name`, as
#'   returned, or `SR`), plus `Zi`, `Pi`, `zone` (`zone` one of `"noHub"`, `"R5"`,
#'   `"R6"`, `"R7"`). Documents are matched to a trajectory by that id against the
#'   trajectory's node documents.
#'
#' @return A tibble, one row per trajectory: `traj_id`, `type`, `group`, `n_docs`
#'   (documents of the trajectory), `mean_Zi`, `mean_Pi`, `hub_share` (share that
#'   are hubs), `connector_share` (share that are connector/boundary hubs `R6`/`R7`,
#'   the bridging signal), `provincial_share` (share that are provincial hubs `R5`).
#'
#' @seealso [sniff_groups_hubs()], [sniff_trajectory_dynamics()],
#'   [sniff_trajectory_braid()]
#' @family trajectory analysis
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
sniff_trajectory_hubs <- function(flow, hubs) {
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  id_col <- if (is.data.frame(hubs)) intersect(c("name", "SR"), names(hubs))[1] else NA
  if (!is.data.frame(hubs) || is.na(id_col) ||
      !all(c("Zi", "Pi", "zone") %in% names(hubs))) {
    stop("'hubs' must be a sniff_groups_hubs() tibble (a document-id column ",
         "'name' or 'SR', plus Zi, Pi, zone)", call. = FALSE)
  }
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  node_docs <- split(dpg$document_id, dpg$group_id)
  h <- hubs[!duplicated(hubs[[id_col]]), , drop = FALSE]
  hid <- h[[id_col]]

  rows <- lapply(seq_len(nrow(tr)), function(i) {
    docs <- unique(unlist(node_docs[tr$nodes[[i]]], use.names = FALSE))
    out <- tibble::tibble(traj_id = tr$traj_id[i], type = tr$type[i], group = tr$group[i],
                          n_docs = length(docs), mean_Zi = NA_real_, mean_Pi = NA_real_,
                          hub_share = NA_real_, connector_share = NA_real_,
                          provincial_share = NA_real_)
    if (length(docs) == 0) return(out)
    j <- match(docs, hid)
    zone <- h$zone[j]; zone[is.na(zone)] <- "noHub"
    out$mean_Zi <- mean(h$Zi[j], na.rm = TRUE)
    out$mean_Pi <- mean(h$Pi[j], na.rm = TRUE)
    out$hub_share <- mean(zone != "noHub")
    out$connector_share <- mean(zone %in% c("R6", "R7"))
    out$provincial_share <- mean(zone == "R5")
    out
  })
  dplyr::bind_rows(rows)
}
