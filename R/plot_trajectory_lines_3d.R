#' Plot the global trajectories as 3D variable-width lines
#'
#' The 3D counterpart of [plot_trajectory_lines_2d()]. Every global trajectory
#' is drawn over the
#' year-aware Sugiyama layout as a growing-thickness line: **x = publication
#' year**, **y = route** (the Sugiyama branching coordinate that separates
#' trajectories), and **z = cumulative tracked documents** along the path. The
#' central finals `tr::cNgN` and the intermediate `trN` that feed them share one
#' colour-per-trajectory scheme; the legend reports, for every intermediate, how
#' many papers of its terminal cohort reached each final (a single `trN` can feed
#' more than one final, which the legend makes explicit). Selection and thresholds
#' are identical to [plot_trajectory_lines_2d()]; only the rendering adds the
#' document z-axis and interactive hover.
#'
#' @param flow A [sniff_trajectory_braid()] object, or a `docs_per_group` tibble /
#'   [sniff_trajectory_dag()] object the flow is built from.
#' @param target What to draw. `"all"` (default) draws every central and the
#'   trajectories feeding it; a central `traj_id` (e.g. `"tr::c1g1"`) or a vector
#'   of them draws only those finals and their feeders.
#' @param conf An optional precomputed [sniff_trajectory_confluence()] object for
#'   `flow`; pass it to skip recomputing the confluence on every call. `NULL`
#'   (default) computes it.
#' @param min_n,min_prop,min_total_size,min_duration_years Feeder thresholds,
#'   passed to the same pruning used by [plot_trajectory_lines_2d()]: an
#'   intermediate is drawn only if it transfers `>= min_n` papers and `>= min_prop`
#'   of its cohort, has total size `>= min_total_size` and lifespan
#'   `>= min_duration_years`; a pruned intermediate takes its own feeders with it.
#' @param min_target_n Minimum papers an intermediate must leave in the selected
#'   final(s) by the last year to be drawn (default `0`, no filter). Only applies
#'   to a targeted final view.
#' @param width_range Line-width range `c(min, max)` (default `c(4, 12)`), scaled
#'   per trajectory by its cumulative tracked documents.
#' @param use_raw_papers Scale z and width by raw `quantity_papers` (`TRUE`) or by
#'   the weighted `quantity_papers * prop_tracked_intra_group` (`FALSE`, default).
#' @param log_scale Apply `log1p()` to the z-axis (cumulative documents), which
#'   keeps large trajectories from dwarfing small ones (default `FALSE`).
#' @param label_size Font size of the `trN` / `tr::cNgN` end labels (default `16`).
#' @param hover_font_size Font size for hover tooltips (default `12`).
#' @param dest_min_prop In the `target = "all"` and single-target views, the
#'   minimum share of an intermediate's terminal cohort that must reach a final
#'   for that final to appear in its legend split (default `0.05`). In the
#'   single-target view the targeted final is always kept regardless of this
#'   threshold.
#' @param lowlight_alpha,lowlight_color,lowlight_width Opacity, colour and width of
#'   the light-grey links between trajectories.
#' @param show_background Draw the light-grey links between trajectories -- the
#'   edges of the DAG that are not on any single trajectory's path (the hand-offs
#'   and forks), at the nodes' real heights, as in [plot_trajectory_lines_2d()]
#'   (default `TRUE`).
#' @param year_range Optional `c(from, to)` years fixing the x-axis range, so a
#'   short trajectory can be shown on the same window as a long one. `NULL`
#'   (default) fits the drawn trajectories.
#' @param palette Optional named colour vector overriding the default hues (keyed
#'   by trajectory `traj_id`).
#' @param title Plot title; `NULL` (default) derives one from `target`.
#' @param legend_position Where to place the legend: `"bottom"` (default, matching
#'   [plot_trajectory_lines_2d()]), `"right"`, `"top"`, `"left"`, or `"none"` to
#'   hide it.
#' @param legend_ncol Number of columns in the legend (default `NULL`, plotly
#'   chooses). Applies to the horizontal legends (`"bottom"` / `"top"`): each
#'   entry is given `1 / legend_ncol` of the width, so `legend_ncol` entries fit
#'   per row. Ignored for the vertical legends (`"right"` / `"left"`).
#' @param labels_text Optional `data.frame` with columns `id` and `text` mapping
#'   group ids (e.g. `"c1g1"`) to descriptions, as in
#'   [plot_trajectory_confluence()]. When supplied, hovering a trajectory's nodes
#'   shows the description of its group beneath the feeding legend. `NULL`
#'   (default) shows the bare group ids.
#'
#' @return A plotly 3D plot object.
#'
#' @details
#' - **z = cumulative tracked documents** along each trajectory (raw or weighted),
#'   accumulated node by node; `log_scale = TRUE` compresses it.
#' - **Line width** grows per trajectory with the same cumulative measure, so each
#'   line is thinnest at its birth and thickest at its terminal node (the width is
#'   rescaled within each trajectory, matching [plot_trajectory_lines_2d()]).
#' - A single-year **central** (e.g. a final-year-only community like `tr::c1g17`)
#'   has no segment to draw and is rendered as a terminal ball; a single-year
#'   **feeder** is dropped (it would show a label with no visible line).
#' - **Colour** is per trajectory and the legend carries the feeding split: a
#'   central reads `tr::cNgN (final, n)` (n = documents that remained in its
#'   final-year community), an intermediate reads `trN -> c1g1:n, c1g2:n` (its
#'   terminal cohort split across finals). In a single-target view it reads
#'   `trN: p% (n) -> c1g15:n, c1g1:n, ...` -- `p%` and the parenthesised `(n)` are
#'   the share and the document count that reached the target (as in
#'   [plot_trajectory_lines_2d()]); the split then shows the feeder's full
#'   destination, so a feeder of `c1g1` reveals that more of it may flow elsewhere.
#' - **Hovering a node** shows that same feeding-legend text, the trajectory's
#'   `labels_text` description (when supplied), the node's year, and the absolute
#'   documents in that cluster-year (`quantity_papers`, not the running cumulative
#'   that sets the height).
#'
#' @seealso [plot_trajectory_lines_2d()], [sniff_trajectory_braid()],
#'   [sniff_trajectory_confluence()]
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' plot_trajectory_lines_3d(flow, min_n = 20)
#' plot_trajectory_lines_3d(flow, target = "tr::c1g1")
#' }
#'
#' @family visualization
#' @export
#' @importFrom igraph induced_subgraph vcount ecount V as_edgelist
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales hue_pal rescale alpha
#' @importFrom stats setNames
#' @importFrom tibble tibble
plot_trajectory_lines_3d <- function(flow, target = "all", conf = NULL,
                                     min_n = 5, min_prop = 0.05,
                                     min_total_size = 0, min_duration_years = 0,
                                     min_target_n = 0,
                                     width_range = c(4, 12), use_raw_papers = FALSE,
                                     log_scale = FALSE,
                                     label_size = 16, hover_font_size = 12,
                                     dest_min_prop = 0.05,
                                     lowlight_alpha = 0.75, lowlight_color = "#9AA5B1",
                                     lowlight_width = 3, show_background = TRUE,
                                     year_range = NULL, palette = NULL, title = NULL,
                                     legend_position = c("bottom", "right", "top", "left", "none"),
                                     legend_ncol = NULL,
                                     labels_text = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly is required for 3D plotting. install.packages('plotly').", call. = FALSE)
  }
  legend_position <- match.arg(legend_position)

  # ---- coerce to a flow + confluence (same contract as plot_trajectory_lines_2d) ----
  if (!is_flow(flow)) flow <- sniff_trajectory_braid(flow)
  if (is.null(conf)) conf <- sniff_trajectory_confluence(flow)

  # ---- selection / pruning: identical to plot_trajectory_lines_2d ----
  tgt <- if (is.null(target) || identical(target, "all")) NULL else target
  target_groups <- if (!is.null(tgt)) sub("^tr::", "", tgt) else NULL
  pr <- .confluence_prune(conf$rivers, conf$confluences, target = tgt,
                          min_n = min_n, min_prop = min_prop,
                          min_total_size = min_total_size,
                          min_duration_years = min_duration_years)
  if (nrow(pr$rivers) == 0) {
    stop("no trajectories pass the thresholds (min_n / min_prop / min_total_size / ",
         "min_duration_years)", call. = FALSE)
  }
  kept <- pr$rivers$traj_id
  if (!is.null(target_groups) && min_target_n > 0) {
    dst <- conf$destinations
    contrib <- vapply(pr$rivers$traj_id, function(id) {
      d <- dst[dst$traj_id == id & dst$g_final %in% target_groups, , drop = FALSE]
      if (nrow(d)) sum(d$n) else 0L
    }, numeric(1))
    kept <- pr$rivers$traj_id[pr$rivers$type == "central" | contrib >= min_target_n]
  }
  # a single-year FEEDER draws no line and is dropped; a single-year CENTRAL
  # (the selected target) is kept and drawn as a terminal ball below.
  trj <- flow$trajectories[flow$trajectories$traj_id %in% kept, , drop = FALSE]
  trj <- trj[lengths(trj$nodes) >= 2 | trj$type == "central", , drop = FALSE]
  if (nrow(trj) == 0) {
    stop("no trajectory to draw for this target / thresholds", call. = FALSE)
  }
  keep_ids <- trj$traj_id
  riv <- pr$rivers[match(keep_ids, pr$rivers$traj_id), , drop = FALSE]

  # ---- colour + feeding-legend text, keyed by traj_id (centrals first) ----
  levs <- c(mixed_sort(keep_ids[startsWith(keep_ids, "tr::")]),
            mixed_sort(keep_ids[!startsWith(keep_ids, "tr::")]))
  pal <- if (!is.null(palette)) palette else if (length(levs) <= 8) {
    stats::setNames(RColorBrewer::brewer.pal(max(3, length(levs)), "Set2")[seq_along(levs)], levs)
  } else {
    stats::setNames(scales::hue_pal()(length(levs)), levs)
  }
  final_size_of <- stats::setNames(vapply(riv$size_curve, function(s)
    if (nrow(s)) as.integer(s$size[which.max(s$year)]) else NA_integer_, integer(1)),
    riv$traj_id)
  type_of <- stats::setNames(riv$type, riv$traj_id)
  size_of <- stats::setNames(riv$size, riv$traj_id)
  dest <- conf$destinations
  feed_label <- function(id) {
    if (identical(type_of[[id]], "central")) {
      return(paste0(id, " (final, ", final_size_of[[id]], ")"))
    }
    dd <- dest[dest$traj_id == id & dest$n > 0, , drop = FALSE]
    if (!is.null(target_groups)) {
      if (length(target_groups) == 1) {
        # lead with the share that reached the target, then the feeder's FULL
        # destination split, so a feeder of c1g1 also reveals where the rest of
        # its documents went. The target final is always kept; the others must
        # pass dest_min_prop.
        n_t <- sum(dd$n[dd$g_final %in% target_groups])
        pct <- if (size_of[[id]] > 0) round(100 * n_t / size_of[[id]]) else 0
        if (nrow(dd)) {
          keep <- (dd$g_final %in% target_groups) | (dd$n / sum(dd$n) >= dest_min_prop)
          sp <- dd[keep, , drop = FALSE]
          sp <- sp[order(-sp$n), , drop = FALSE]
        } else {
          sp <- dd
        }
        split_str <- if (nrow(sp))
          paste0(" \u2192 ", paste0(sp$g_final, ":", sp$n, collapse = ", ")) else ""
        # (n_t) = documents transferred to the target, in parentheses as in
        # plot_trajectory_lines_2d().
        return(paste0(id, ": ", pct, "% (", n_t, ")", split_str))
      }
      # multiple targets: documents to each selected target
      dd <- dd[dd$g_final %in% target_groups, , drop = FALSE]
      if (!nrow(dd)) return(paste0(id, ": 0"))
      dd <- dd[order(-dd$n), , drop = FALSE]
      return(paste0(id, " \u2192 ", paste0("tr::", dd$g_final, ":", dd$n, collapse = ", ")))
    }
    if (!nrow(dd)) return(id)
    tot <- sum(dd$n)
    dd <- dd[dd$n / tot >= dest_min_prop, , drop = FALSE]
    dd <- dd[order(-dd$n), , drop = FALSE]
    paste0(id, " \u2192 ", paste0("tr::", dd$g_final, ":", dd$n, collapse = ", "))
  }

  # optional manual descriptions; resolved once so labels_text warns at most once.
  # central rivers (tr::cNgN) are keyed by their final group (cNgN); absorbed
  # tributaries (trN) are keyed by their own trajectory id, so each carries its own
  # description rather than inheriting its destination central's.
  grp_of <- stats::setNames(trj$group, trj$traj_id)
  key_of <- ifelse(startsWith(levs, "tr::"), unname(grp_of[levs]), levs)
  desc_map <- stats::setNames(.dag_label_text(key_of, labels_text), levs)
  desc_of <- function(id) {
    k <- if (startsWith(id, "tr::")) grp_of[[id]] else id
    d <- desc_map[[id]]
    if (is.null(d) || is.na(d) || is.na(k) || identical(d, k)) NA_character_ else d
  }

  # ---- layout: the same year-aware Sugiyama as the 2D view; x = year, y = route ----
  nodes <- unique(unlist(trj$nodes, use.names = FALSE))
  g <- igraph::induced_subgraph(flow$graph, nodes)
  if (igraph::vcount(g) == 0) return(plotly::plot_ly())
  ml <- mk_layout_and_year_scale(g); lay <- ml$lay
  v_year <- stats::setNames(.extract_year(lay$name), lay$name)
  v_route <- stats::setNames(lay$y, lay$name)

  node_measure <- if (use_raw_papers) {
    (igraph::V(g)$quantity_papers %||% 0)
  } else {
    (igraph::V(g)$quantity_papers %||% 0) * (igraph::V(g)$prop_tracked_intra_group %||% 0)
  }
  # absolute documents in each cluster-year, for the hover (z stays cumulative)
  docs_abs <- igraph::V(g)$quantity_papers %||% 0
  nm <- igraph::V(g)$name

  # per-trajectory sequence: x = year, y = route, z = cumulative documents
  td <- lapply(seq_len(nrow(trj)), function(i) {
    nd <- trj$nodes[[i]]
    nd <- nd[order(.extract_year(nd), nd)]
    step_p <- node_measure[match(nd, nm)]; step_p[is.na(step_p)] <- 0
    docs <- docs_abs[match(nd, nm)]; docs[is.na(docs)] <- 0
    zraw <- cumsum(step_p)
    list(traj_id = trj$traj_id[i], nodes = nd,
         years = unname(v_year[nd]), route = unname(v_route[nd]),
         step_p = step_p, docs = docs, zraw = zraw,
         z = if (log_scale) log1p(zraw) else zraw)
  })
  names(td) <- trj$traj_id
  td <- td[levs]   # legend / draw order: centrals first, then mixed_sort

  # node -> z lookup (flow trajectories are node-disjoint, so z is unique per node)
  # and the within-trajectory path edges (drawn coloured below); every other edge
  # of g is an inter-trajectory link (a hand-off / fork).
  z_by_node <- numeric(0)
  for (tt in td) z_by_node[tt$nodes] <- tt$z
  path_keys <- character(0)
  for (tt in td) if (length(tt$nodes) >= 2) {
    path_keys <- c(path_keys, paste0(tt$nodes[-length(tt$nodes)], "\r", tt$nodes[-1]))
  }
  path_keys <- unique(path_keys)

  p <- plotly::plot_ly()

  # light-grey links between trajectories: the edges of g not on any trajectory's
  # path, drawn at the nodes' real heights so a hand-off connects the two coloured
  # lines where it occurs (cf. plot_trajectory_lines_2d's NA-traj_id edges).
  if (show_background && igraph::ecount(g) > 0) {
    el <- igraph::as_edgelist(g, names = TRUE)
    el <- el[!(paste0(el[, 1], "\r", el[, 2]) %in% path_keys), , drop = FALSE]
    if (nrow(el)) {
      bx <- by <- bz <- numeric(0)
      for (i in seq_len(nrow(el))) {
        f <- el[i, 1]; v <- el[i, 2]
        bx <- c(bx, v_year[f], v_year[v], NA)
        by <- c(by, v_route[f], v_route[v], NA)
        bz <- c(bz, z_by_node[f], z_by_node[v], NA)
      }
      p <- plotly::add_trace(
        p, x = bx, y = by, z = bz, type = "scatter3d", mode = "lines",
        line = list(color = scales::alpha(lowlight_color, lowlight_alpha),
                    width = max(0.5, lowlight_width)),
        hoverinfo = "skip", showlegend = FALSE)
    }
  }

  for (t in td) {
    col <- unname(pal[t$traj_id])
    leg <- feed_label(t$traj_id)
    dsc <- desc_of(t$traj_id)
    hdr <- paste0("<b>", leg, "</b>", if (!is.na(dsc)) paste0("<br>", dsc) else "")

    # single-node central: no line, just a terminal ball + label
    if (length(t$nodes) < 2) {
      p <- plotly::add_trace(
        p, x = t$years[1], y = t$route[1], z = t$z[1],
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = col), name = leg, showlegend = TRUE,
        hoverinfo = "text", text = paste0(hdr, "<br>Documents: ", round(t$docs[1])),
        hoverlabel = list(bgcolor = "rgba(50,50,50,0.9)",
                          font = list(color = "white", size = hover_font_size)))
      p <- plotly::add_trace(
        p, x = t$years[1], y = t$route[1], z = t$z[1],
        type = "scatter3d", mode = "text", text = t$traj_id,
        textfont = list(size = label_size, color = col),
        showlegend = FALSE, hoverinfo = "skip")
      next
    }

    # segment widths grow with cumulative documents, rescaled within the trajectory
    seg_w <- if (length(unique(t$z)) <= 1) {
      rep(mean(width_range), length(t$nodes) - 1)
    } else {
      scales::rescale(t$z[-1], to = width_range, from = range(t$z, na.rm = TRUE))
    }
    for (i in seq_len(length(t$nodes) - 1)) {
      p <- plotly::add_trace(
        p, x = c(t$years[i], t$years[i + 1]), y = c(t$route[i], t$route[i + 1]),
        z = c(t$z[i], t$z[i + 1]), type = "scatter3d", mode = "lines",
        line = list(color = col, width = seg_w[i]),
        name = leg, legendgroup = t$traj_id, showlegend = (i == 1),
        hoverinfo = "skip")
    }

    # invisible hover markers along the path
    hov <- paste0(hdr, "<br>Year: ", t$years,
                  "<br>Documents: ", round(t$docs))
    p <- plotly::add_trace(
      p, x = t$years, y = t$route, z = t$z, type = "scatter3d", mode = "markers",
      marker = list(size = 1.5, opacity = 0), legendgroup = t$traj_id,
      hoverinfo = "text", text = hov,
      hoverlabel = list(bgcolor = "rgba(50,50,50,0.9)",
                        font = list(color = "white", size = hover_font_size)),
      showlegend = FALSE)

    # terminal ball + end label at the trajectory's last node. The ball carries
    # the legend hover (it sits on top of the invisible markers, so without its
    # own hover the trajectory's end would show nothing).
    n <- length(t$nodes)
    end_hov <- paste0(hdr, "<br>Year: ", t$years[n],
                      "<br>Documents: ", round(t$docs[n]))
    p <- plotly::add_trace(
      p, x = t$years[n], y = t$route[n], z = t$z[n],
      type = "scatter3d", mode = "markers", marker = list(size = 5, color = col),
      legendgroup = t$traj_id, showlegend = FALSE,
      hoverinfo = "text", text = end_hov,
      hoverlabel = list(bgcolor = "rgba(50,50,50,0.9)",
                        font = list(color = "white", size = hover_font_size)))
    p <- plotly::add_trace(
      p, x = t$years[n], y = t$route[n], z = t$z[n],
      type = "scatter3d", mode = "text", text = t$traj_id,
      textfont = list(size = label_size, color = col),
      legendgroup = t$traj_id, showlegend = FALSE,
      hoverinfo = "text", hovertext = end_hov,
      hoverlabel = list(bgcolor = "rgba(50,50,50,0.9)",
                        font = list(color = "white", size = hover_font_size)))
  }

  if (is.null(title)) {
    title <- if (is.null(tgt)) "Trajectory lines 3D (global)"
      else paste0("Feeders of ", paste(tgt, collapse = ", "))
  }
  xaxis <- list(title = "Publication year")
  if (!is.null(year_range)) xaxis$range <- range(year_range)

  # legend placement
  fnt <- list(size = 14)
  leg <- switch(legend_position,
    right  = list(font = fnt),
    left   = list(orientation = "v", x = -0.02, xanchor = "right",
                  y = 0.5, yanchor = "middle", font = fnt),
    top    = list(orientation = "h", x = 0.5, xanchor = "center",
                  y = 1.02, yanchor = "bottom", font = fnt),
    bottom = list(orientation = "h", x = 0.5, xanchor = "center",
                  y = -0.02, yanchor = "top", font = fnt),
    none   = list(font = fnt))
  # legend_ncol: plotly has no native column count, but giving each entry
  # 1/ncol of the width forces ncol entries per row in a horizontal legend.
  if (!is.null(legend_ncol) && identical(leg$orientation, "h")) {
    leg$entrywidthmode <- "fraction"
    leg$entrywidth <- 1 / legend_ncol
  }

  plotly::layout(
    p,
    scene = list(
      xaxis = xaxis,
      yaxis = list(title = "Route"),
      zaxis = list(title = paste0(
        if (use_raw_papers) "Cumulative documents" else "Cumulative weighted documents",
        if (log_scale) " (log)" else "")),
      camera = list(eye = list(x = -1.5, y = -2, z = 1))
    ),
    legend = leg,
    showlegend = legend_position != "none",
    title = title
  )
}
