#' Interactive strategic map of trajectory dynamic states (plotly)
#'
#' The plotly twin of [plot_trajectory_dynamics()]. The same quadrant map -- shaded
#' and named state regions, classification thresholds as reference lines, points
#' sized by community and coloured by state -- rendered as an HTML widget. Hovering
#' a point reveals the full indicator panel (growth, novelty, recruitment, inflow,
#' destination entropy, dormant share, and any CCT / hub-role columns), and the
#' state legend is clickable to isolate one state at a time.
#'
#' @param dyn A [sniff_trajectory_dynamics()] tibble.
#' @param target `"finals"` (default) maps the **living (central)** trajectories
#'   (novelty x growth), `"intermediary"` the **declining (absorbed)** ones
#'   (destination entropy x dormant share), as in [plot_trajectory_dynamics()].
#' @param thresholds The [fixed_state_thresholds()]-shaped list bounding the
#'   regions and the reference lines. `NULL` (default) reuses the cuts attached to
#'   `dyn` by [sniff_trajectory_dynamics()] (`attr(dyn, "state_thresholds")`), else
#'   derives them via [data_state_thresholds()]. Pass an explicit list to override.
#' @param labels_text Optional `data.frame` (`id`, `text`) mapping ids to
#'   descriptions, used in the hover (and labels). The `"finals"` view matches by
#'   group id (`cNgN`); the `"intermediaries"` view matches by trajectory id
#'   (`trN`).
#' @param label_id Put the id on the label's own first line, above the
#'   description (`cNgN` then the text for finals, `trN` then the text for
#'   intermediaries), so the hover and pinned labels stay identifiable; the line
#'   break is plotly's `<br>`. Default `FALSE`.
#' @param marker_range Marker diameter range in px `c(min, max)` (default
#'   `c(6, 42)`); area scales with the square root of `size`.
#' @param show_labels Pin a text label to each point (default `FALSE`; the detail
#'   lives in the hover). Text may overlap when points are dense.
#' @param label_size Font size (px) of the pinned labels (default `11`).
#' @param palette Optional named colour vector overriding the default semantic
#'   hues, keyed by state.
#' @param show_thresholds Draw the classification cut lines (default `TRUE`).
#' @param show_quadrants Shade and name the state regions (default `TRUE`).
#' @param xlim,ylim Optional `c(lo, hi)` axis ranges; `NULL` (default) expands to
#'   keep every state region visible (the data, the threshold cuts, and the
#'   `[0,1]` share bounds, with padding).
#' @param title Plot title; `NULL` (default) derives one from `target`.
#'
#' @return A `plotly` htmlwidget.
#'
#' @seealso [plot_trajectory_dynamics()], [sniff_trajectory_dynamics()],
#'   [fixed_state_thresholds()]
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' dyn  <- sniff_trajectory_dynamics(flow)
#' plot_trajectory_dynamics_interactive(dyn, target = "finals", labels_text = descr)
#' }
#'
#' @family visualization
#' @export
#' @importFrom plotly plot_ly add_trace layout
plot_trajectory_dynamics_interactive <- function(dyn, target = c("finals", "intermediary"),
                                                 thresholds = NULL, labels_text = NULL,
                                                 label_id = FALSE, marker_range = c(6, 42),
                                                 show_labels = FALSE, label_size = 11,
                                                 palette = NULL, show_thresholds = TRUE,
                                                 show_quadrants = TRUE, xlim = NULL,
                                                 ylim = NULL, title = NULL) {
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
  }

  drop_n <- sum(is.na(d[[xcol]]) | is.na(d[[ycol]]))
  d <- d[!is.na(d[[xcol]]) & !is.na(d[[ycol]]), , drop = FALSE]
  if (nrow(d) == 0) {
    stop("no ", target, " trajectories to plot (their metrics are all NA)", call. = FALSE)
  }
  d$.state <- factor(d[[scol]], levels = state_levels)
  if (target == "finals") {
    desc <- .dag_label_text(d$group, labels_text)
    d$.lab <- if (label_id) ifelse(desc == d$group, d$group, paste0(d$group, "<br>", desc)) else desc
  } else {
    desc <- .dag_label_text(d$traj_id, labels_text)
    d$.lab <- if (label_id) ifelse(desc == d$traj_id, d$traj_id, paste0(d$traj_id, "<br>", desc)) else desc
  }

  # marker sizes: area proportional to community size.
  s <- sqrt(pmax(d$size, 0))
  msize <- if (diff(range(s)) == 0) rep(mean(marker_range), nrow(d)) else
    marker_range[1] + (marker_range[2] - marker_range[1]) * (s - min(s)) / (max(s) - min(s))

  # hover panel: the full indicator set, with the optional cct / hub-role columns.
  num <- function(x, dg = 2) ifelse(is.na(x), "\u2014", formatC(x, format = "f", digits = dg))
  gr <- paste0("<br>growth rate: ", num(d$growth_rate),
    ifelse(is.na(d$doubling_time), "", paste0(" (doubling ", num(d$doubling_time, 1), " yr)")))
  hov <- paste0("<b>", d$.lab, "</b><br>state: ", d[[scol]],
    "<br>size: ", d$size, " docs", gr, "<br>novelty: ", num(d$novelty))
  if (target == "finals") {
    hov <- paste0(hov, "<br>recruitment: ", d$recruitment,
      "<br>inflow (consolidation): ", ifelse(is.na(d$attraction_inflow), "\u2014", d$attraction_inflow))
  } else {
    hov <- paste0(hov, "<br>\u2192 tr::", d$group,
      "<br>dest entropy: ", num(d$dest_entropy), "<br>dormant share: ", num(d$dormant_share))
  }
  if ("cct_recent" %in% names(d)) hov <- paste0(hov, "<br>CCT recent: ", num(d$cct_recent, 1))
  if ("cct_delta" %in% names(d)) hov <- paste0(hov, "<br>CCT slope: ", num(d$cct_delta, 2))
  if ("mean_Pi" %in% names(d)) hov <- paste0(hov, "<br>mean Pi: ", num(d$mean_Pi))
  if ("connector_share" %in% names(d)) hov <- paste0(hov, "<br>connector hubs: ", num(d$connector_share))
  d$.hover <- hov

  # axis ranges always include the threshold cuts (and the [0,1] share bounds), so
  # every state region keeps a visible band even when the points cluster.
  xr <- if (!is.null(xlim)) xlim else .dyn_show_range(d[[xcol]], vlines, bound01 = TRUE)
  yr <- if (!is.null(ylim)) ylim else
    .dyn_show_range(d[[ycol]], hlines, bound01 = target == "intermediary")
  # extra headroom on the unbounded growth axis so the emergence label clears the
  # highest core.
  if (target == "finals" && is.null(ylim)) yr[2] <- yr[2] + 0.12 * diff(yr)

  rgba <- function(hex, a) {
    cc <- grDevices::col2rgb(hex); sprintf("rgba(%d,%d,%d,%g)", cc[1], cc[2], cc[3], a)
  }

  # one trace per state -> discrete colours and a clickable legend.
  p <- plotly::plot_ly()
  for (st in state_levels) {
    idx <- which(d$.state == st)
    if (!length(idx)) next
    p <- plotly::add_trace(p, x = d[[xcol]][idx], y = d[[ycol]][idx],
      type = "scatter", mode = if (show_labels) "markers+text" else "markers",
      marker = list(size = msize[idx], color = pal[[st]], opacity = 0.85,
                    line = list(color = "rgba(40,40,40,0.8)", width = 1)),
      text = if (show_labels) d$.lab[idx] else NULL,
      textposition = if (show_labels) "top center" else NULL,
      textfont = if (show_labels) list(size = label_size, color = "black") else NULL,
      hovertext = d$.hover[idx], hoverinfo = "text", name = st)
  }

  # region rectangles (below) tiling the panel + threshold lines.
  shapes <- list()
  if (show_quadrants) {
    rg <- if (target == "finals") list(
      list(xr[1], xr[2], yr[1], hlines[2], pal[["dormancy"]]),     # dormancy (bottom, full width)
      list(xr[1], xr[2], hlines[2], hlines[1], pal[["maturity"]]), # maturity band (middle, full width)
      list(xr[1], vlines, hlines[1], yr[2], pal[["maturity"]]),    # maturity wedge (top-left only)
      list(vlines, xr[2], hlines[1], yr[2], pal[["emergence"]]))   # emergence (top-right)
    else list(
      list(xr[1], vlines, yr[1], hlines, pal[["convergence"]]),  # convergence (left-bottom)
      list(vlines, xr[2], yr[1], hlines, pal[["divergence"]]),   # divergence (right-bottom)
      list(xr[1], xr[2], hlines, yr[2], pal[["dormancy"]]))      # dormancy (top)
    for (r in rg) shapes[[length(shapes) + 1]] <- list(type = "rect", xref = "x", yref = "y",
      x0 = r[[1]], x1 = r[[2]], y0 = r[[3]], y1 = r[[4]], layer = "below",
      fillcolor = rgba(r[[5]], 0.12), line = list(width = 0))
  }
  if (show_thresholds) {
    for (v in vlines) shapes[[length(shapes) + 1]] <- list(type = "line", xref = "x",
      yref = "paper", x0 = v, x1 = v, y0 = 0, y1 = 1,
      line = list(color = "grey60", width = 1, dash = "dash"))
    for (h in hlines) shapes[[length(shapes) + 1]] <- list(type = "line", xref = "paper",
      yref = "y", x0 = 0, x1 = 1, y0 = h, y1 = h,
      line = list(color = "grey60", width = 1, dash = "dash"))
  }

  # region names on a translucent box, parked away from the point cloud: finals at
  # the panel corners; intermediary's convergence/divergence just under the dormancy
  # cut (above the low-dropout cluster), dormancy in the empty band on top.
  anns <- list()
  if (show_quadrants) {
    # x, y, xanchor, yanchor, yref, label
    specs <- if (target == "finals") list(
      list(1, 1, "right", "top", "paper", "emergence"),
      list(0, 0, "left", "bottom", "paper", "dormancy (stall)"),
      list(0, 1, "left", "top", "paper", "maturity"))
    else list(
      list(0, th$dormancy_share, "left", "top", "y", "convergence"),
      list(1, th$dormancy_share, "right", "top", "y", "divergence"),
      list(0, 1, "left", "top", "paper", "dormancy"))
    for (cc in specs) anns[[length(anns) + 1]] <- list(x = cc[[1]], y = cc[[2]],
      xref = "paper", yref = cc[[5]], xanchor = cc[[3]], yanchor = cc[[4]],
      text = paste0("<b>", cc[[6]], "</b>"),
      font = list(color = pal[[sub(" .*$", "", cc[[6]])]], size = label_size + 2),
      bgcolor = "rgba(255,255,255,0.7)", borderpad = 2, showarrow = FALSE)
  }
  if (drop_n > 0) anns[[length(anns) + 1]] <- list(x = 1, y = -0.13, xref = "paper",
    yref = "paper", xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0(drop_n, " omitted (state metric undefined)"),
    font = list(color = "grey50", size = 10))

  if (is.null(title)) title <- def_title
  plotly::layout(p,
    title = list(text = title),
    xaxis = list(title = xlab, range = xr, zeroline = FALSE, showline = TRUE,
                 linecolor = "grey20", ticks = "outside", tickcolor = "grey20"),
    yaxis = list(title = ylab, range = yr, zeroline = FALSE, showline = TRUE,
                 linecolor = "grey20", ticks = "outside", tickcolor = "grey20"),
    shapes = shapes, annotations = anns, legend = list(title = list(text = "")))
}
