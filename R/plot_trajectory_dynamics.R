#' Strategic map of trajectory dynamic states
#'
#' A quadrant scatter of [sniff_trajectory_dynamics()] output, one figure per
#' population, designed to make the dynamic states legible at a glance. Each point
#' is a trajectory, its area the community size, its colour the state; the regions
#' are shaded and named after the state they classify, with the thresholds drawn as
#' reference lines.
#'
#' @param target What to draw, following [plot_trajectory_confluence()]. `"finals"`
#'   (default) maps the **living (central)** trajectories on a life-cycle plane
#'   (novelty x growth) that separates emergence, maturity and dormancy.
#'   `"intermediary"` maps the **declining (absorbed)** trajectories on a
#'   destination plane (entropy x dormant share) that separates convergence,
#'   divergence and dormancy. The two populations live on different axes, so there
#'   is no combined view.
#' @param dyn A [sniff_trajectory_dynamics()] tibble.
#' @param thresholds The [fixed_state_thresholds()]-shaped list whose cut values
#'   bound the shaded regions and the reference lines. `NULL` (default) reuses the
#'   cuts the classification was built with -- `attr(dyn, "state_thresholds")`,
#'   attached by [sniff_trajectory_dynamics()] -- so regions and colours always
#'   agree; if that attribute is absent it derives them from `dyn` via
#'   [data_state_thresholds()]. Pass an explicit list to override.
#' @param labels_text Optional `data.frame` with columns `id` and `text` mapping
#'   group ids (`cNgN`) to descriptions; used to label the living cores in the
#'   `"finals"` view (as in [plot_trajectory_dag()]). `NULL` (default) labels by id.
#' @param label_id Prefix each living-core label with its group id, as
#'   `cNgN: description` (default `FALSE`, description only). No effect when
#'   `labels_text` is `NULL` (the label is already the id) or in the
#'   `"intermediary"` view (labelled by `traj_id`).
#' @param size_range Point-area range `c(min, max)` passed to [ggplot2::scale_size()]
#'   (default `c(2, 14)`).
#' @param label_size Text size of the point labels (default `3.5`); the region
#'   labels are drawn one point larger.
#' @param axis_text_size Font size of the axis tick labels; `NULL` (default) keeps
#'   the theme default.
#' @param palette Optional named colour vector overriding the default semantic
#'   hues, keyed by state (`emergence`/`maturity`/`dormancy` for `"finals"`,
#'   `convergence`/`divergence`/`dormancy` for `"intermediary"`).
#' @param show_thresholds Draw the classification cut lines (default `TRUE`).
#' @param show_quadrants Shade and name the state regions (default `TRUE`).
#' @param xlim,ylim Optional `c(lo, hi)` to zoom the axes via
#'   [ggplot2::coord_cartesian()] without dropping data from the fit; `NULL`
#'   (default) expands the view to keep every state region visible (the data, the
#'   threshold cuts, and the `[0,1]` share bounds, with padding), so the bands are
#'   shown even when the points cluster. Pass a range to override, e.g. to zoom
#'   past a single young core's outlying `growth_rate`.
#' @param legend_position Legend placement (default `"right"`).
#' @param title Plot title; `NULL` (default) derives one from `target`.
#'
#' @return A `ggplot` object.
#'
#' @details
#' - **`"finals"`** -- x = `novelty`, y = `growth_rate`; emergence is the
#'   novel-and-growing region, dormancy the loss-of-momentum band, maturity the
#'   saturated middle.
#' - **`"intermediary"`** -- x = `dest_entropy`, y = `dormant_share`; convergence is
#'   the low-entropy (single destination) side, divergence the high-entropy side,
#'   and dormancy the high-dropout band (rare, so this band is usually sparse).
#'   Extinct (group `NA`) lineages have no coordinates and are reported in the
#'   caption rather than drawn.
#'
#' @seealso [sniff_trajectory_dynamics()], [fixed_state_thresholds()],
#'   [plot_trajectory_confluence()]
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' dyn  <- sniff_trajectory_dynamics(flow)
#' descr <- data.frame(id = "c1g1", text = "Anaerobic digestion")
#'
#' plot_trajectory_dynamics(dyn, target = "finals", labels_text = descr)
#' plot_trajectory_dynamics(dyn, target = "intermediary")
#' }
#'
#' @family visualization
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_vline annotate
#' @importFrom ggplot2 scale_fill_manual scale_size coord_cartesian labs
#' @importFrom ggplot2 theme_minimal theme element_text element_line
#' @importFrom rlang .data
plot_trajectory_dynamics <- function(dyn, target = c("finals", "intermediary"),
                                     thresholds = NULL, labels_text = NULL,
                                     label_id = FALSE,
                                     size_range = c(2, 14), label_size = 3.5,
                                     axis_text_size = NULL, palette = NULL,
                                     show_thresholds = TRUE, show_quadrants = TRUE,
                                     xlim = NULL, ylim = NULL,
                                     legend_position = "right", title = NULL) {
  target <- match.arg(target)
  need <- c("traj_id", "type", "group", "phase", "fate", "novelty", "growth_rate",
            "dest_entropy", "dormant_share", "size")
  if (!is.data.frame(dyn) || !all(need %in% names(dyn))) {
    stop("'dyn' must be the output of sniff_trajectory_dynamics()", call. = FALSE)
  }
  # reuse the cuts the classification was built with (attached by
  # sniff_trajectory_dynamics); else derive them from the data, as the sniff does.
  if (is.null(thresholds)) thresholds <- attr(dyn, "state_thresholds")
  if (is.null(thresholds)) thresholds <- data_state_thresholds(dyn)
  th <- thresholds

  if (target == "finals") {
    d <- dyn[dyn$type == "central", , drop = FALSE]
    xcol <- "novelty"; ycol <- "growth_rate"; scol <- "phase"
    xlab <- "Novelty (recent-arrival share)"; ylab <- "Growth rate"
    state_levels <- c("emergence", "maturity", "dormancy")
    vlines <- th$emergence_novelty; hlines <- c(th$emergence_growth, th$decline_growth)
    pal <- if (!is.null(palette)) palette else
      c(emergence = "#2CA02C", maturity = "#1F77B4", dormancy = "#8C8C8C")
    def_title <- "Emerging, mature and dormant research fronts"
    # shaded regions tile the whole plane so none reads as an undefined gap:
    # emergence (novel & growing), dormancy (declining band), and maturity as the
    # saturated middle band plus the grow-but-not-novel wedge.
    rects <- data.frame(
      xmin = c(th$emergence_novelty, -Inf, -Inf,               -Inf),
      xmax = c(Inf,                  Inf,  Inf,                 th$emergence_novelty),
      ymin = c(th$emergence_growth,  -Inf, th$decline_growth,  th$emergence_growth),
      ymax = c(Inf,                  th$decline_growth, th$emergence_growth, Inf),
      fill = unname(pal[c("emergence", "dormancy", "maturity", "maturity")]),
      stringsAsFactors = FALSE)
    rlabs <- data.frame(
      pos = c("topright", "bottomleft", "leftmid"),
      lab = c("emergence", "dormancy (stall)", "maturity"),
      col = unname(pal[c("emergence", "dormancy", "maturity")]), stringsAsFactors = FALSE)
    ymid_lab <- mean(c(th$decline_growth, th$emergence_growth))
  } else {
    d <- dyn[dyn$type == "absorbed", , drop = FALSE]
    xcol <- "dest_entropy"; ycol <- "dormant_share"; scol <- "fate"
    xlab <- "Destination entropy (concentration \u2192 dispersion)"
    ylab <- "Dormant share (cohort dropping out)"
    state_levels <- c("convergence", "divergence", "dormancy")
    vlines <- th$convergence_entropy; hlines <- th$dormancy_share
    pal <- if (!is.null(palette)) palette else
      c(convergence = "#1F77B4", divergence = "#FF7F0E", dormancy = "#8C8C8C")
    def_title <- "How declining lineages resolve: consolidation vs branching"
    rects <- data.frame(
      xmin = c(-Inf, th$convergence_entropy, -Inf),
      xmax = c(th$convergence_entropy, Inf, Inf),
      ymin = c(-Inf, -Inf, th$dormancy_share),
      ymax = c(th$dormancy_share, th$dormancy_share, Inf),
      fill = unname(pal[c("convergence", "divergence", "dormancy")]), stringsAsFactors = FALSE)
    rlabs <- data.frame(
      pos = c("cutleft", "cutright", "topleft"),
      lab = c("convergence", "divergence", "dormancy"),
      col = unname(pal[c("convergence", "divergence", "dormancy")]), stringsAsFactors = FALSE)
    ymid_lab <- NA_real_
  }

  drop_n <- sum(is.na(d[[xcol]]) | is.na(d[[ycol]]))
  d <- d[!is.na(d[[xcol]]) & !is.na(d[[ycol]]), , drop = FALSE]
  if (nrow(d) == 0) {
    stop("no ", target, " trajectories to plot (their metrics are all NA)", call. = FALSE)
  }
  d$.state <- factor(d[[scol]], levels = state_levels)
  if (target == "finals") {
    desc <- .dag_label_text(d$group, labels_text)
    d$.lab <- if (label_id) ifelse(desc == d$group, d$group, paste0(d$group, ": ", desc)) else desc
  } else {
    d$.lab <- d$traj_id
  }

  # always-show range: include the data, the cuts, and the [0,1] share bounds, so
  # every state region keeps a visible band even when the points cluster.
  xr <- if (!is.null(xlim)) xlim else .dyn_show_range(d[[xcol]], vlines, bound01 = TRUE)
  yr <- if (!is.null(ylim)) ylim else
    .dyn_show_range(d[[ycol]], hlines, bound01 = target == "intermediary")
  # extra headroom on the unbounded growth axis so the top-corner label clears the
  # highest core.
  if (target == "finals" && is.null(ylim)) yr[2] <- yr[2] + 0.12 * diff(yr)
  # region-label anchors parked away from the point cloud: finals at the outer
  # corners / middle-band left edge; intermediary just under the dormancy cut (above
  # the low-dropout cluster) and in the empty dropout band on top.
  cxy <- function(pos) {
    ix <- 0.02 * diff(xr); iy <- 0.02 * diff(yr)
    switch(pos,
      topright = list(x = xr[2] - ix, y = yr[2] - iy, h = 1, v = 1),
      topleft  = list(x = xr[1] + ix, y = yr[2] - iy, h = 0, v = 1),
      bottomleft = list(x = xr[1] + ix, y = yr[1] + iy, h = 0, v = 0),
      cutleft  = list(x = xr[1] + ix, y = th$dormancy_share - iy, h = 0, v = 1),
      cutright = list(x = xr[2] - ix, y = th$dormancy_share - iy, h = 1, v = 1),
      leftmid  = list(x = xr[1] + ix, y = ymid_lab, h = 0, v = 0.5))
  }

  # per-point marker size (matching scale_size's area scaling), handed to ggrepel
  # as point.size so each label's leader starts at the bubble's outer edge, not its
  # centre.
  sr <- range(d$size)
  pt_size <- if (diff(sr) == 0) rep(mean(size_range), nrow(d)) else
    scales::rescale(sqrt(d$size), to = size_range, from = sqrt(sr))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]]))
  # region shading first, so it sits behind everything else.
  if (show_quadrants) {
    for (k in seq_len(nrow(rects))) {
      p <- p + ggplot2::annotate("rect", xmin = rects$xmin[k], xmax = rects$xmax[k],
        ymin = rects$ymin[k], ymax = rects$ymax[k], fill = rects$fill[k], alpha = 0.10)
    }
  }
  if (show_thresholds) {
    p <- p +
      ggplot2::geom_vline(xintercept = vlines, linetype = "dashed", colour = "grey70") +
      ggplot2::geom_hline(yintercept = hlines, linetype = "dashed", colour = "grey70")
  }
  p <- p +
    ggplot2::geom_point(ggplot2::aes(size = .data$size, fill = .data$.state),
      shape = 21, colour = "grey25", alpha = 0.85, stroke = 0.4) +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE, name = NULL) +
    ggplot2::scale_size(range = size_range, name = "Documents")

  p <- p + (if (requireNamespace("ggrepel", quietly = TRUE))
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$.lab), size = label_size,
      max.overlaps = Inf, min.segment.length = 0, segment.colour = "grey60",
      box.padding = 0.5, point.padding = 0, point.size = pt_size)
    else ggplot2::geom_text(ggplot2::aes(label = .data$.lab), size = label_size,
      vjust = -1))

  # region names in the padded corners, on a translucent box so they stay legible
  # over the shading and clear of the points.
  if (show_quadrants) {
    for (k in seq_len(nrow(rlabs))) {
      xy <- cxy(rlabs$pos[k])
      p <- p + ggplot2::annotate("label", x = xy$x, y = xy$y, label = rlabs$lab[k],
        colour = rlabs$col[k], hjust = xy$h, vjust = xy$v, fontface = "bold",
        size = label_size + 1, fill = "white", alpha = 0.85,
        label.padding = ggplot2::unit(0.18, "lines"))
    }
  }

  p <- p + ggplot2::coord_cartesian(xlim = xr, ylim = yr)
  if (is.null(title)) title <- def_title
  p <- p +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = legend_position,
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "grey20", linewidth = 0.6),
      axis.ticks = ggplot2::element_line(colour = "grey20", linewidth = 0.5),
      axis.text = ggplot2::element_text(size = axis_text_size))
  if (drop_n > 0) {
    p <- p + ggplot2::labs(caption = paste0(
      drop_n, " trajectory(ies) omitted (state metric undefined)"))
  }
  p
}

#' Axis range that always shows every state region
#'
#' Spans the data, the threshold cuts, and (for a `[0,1]` share axis) the unit
#' interval, with padding so the outer state bands keep a visible height even when
#' the points cluster.
#' @keywords internal
.dyn_show_range <- function(vals, cuts, bound01 = FALSE, pad_frac = 0.1, min_pad = 0.04) {
  v <- c(vals, cuts, if (bound01) c(0, 1))
  v <- v[is.finite(v)]
  lo <- min(v); hi <- max(v)
  pad <- max(pad_frac * (hi - lo), min_pad)
  c(lo - pad, hi + pad)
}
