#' Interactive trajectory confluence (plotly): hover a stream for its trajectory
#'
#' The plotly twin of [plot_trajectory_confluence()]. Every trajectory is a filled
#' stream over the publication year, laid out by the same centered lane packing and
#' coloured by the same rule (per destination central in the whole-forest / multi
#' view, per tributary in a single-central view). There is no legend: hovering a
#' **central** river shows `tr::cNgN (size)` (plus its `labels_text` description),
#' and hovering a **tributary** shows `trN (n/cohort -> tr::cNgN)`. With
#' `label_terminal = TRUE` the central (or selected) rivers are also named in a
#' de-overlapped column on the right, joined to their stream by a leader line.
#' Returns a `plotly` htmlwidget.
#'
#' Unlike the static function this covers the **forest views only** (`"all"`,
#' `"finals"`, `"intermediary"`, one or more centrals, and an intermediate shown as
#' its upstream feeders); the single-intermediate **focus view** and the faint
#' secondary-destination links are not drawn.
#'
#' @param conf A [sniff_trajectory_confluence()] object.
#' @param target What to draw, as in [plot_trajectory_confluence()] minus the focus
#'   view: `"all"` (default) the whole forest, `"finals"` the central backbones,
#'   `"intermediary"` the tributary streams, a single central `tr::cNgN` (its
#'   feeders, coloured per tributary), or a vector of trajectory ids (centrals
#'   stacked, or intermediates shown as their feeders).
#' @param depth,min_n,min_prop,min_total_size,min_duration_years,width_range,smooth
#'   Passed through to the same pruning / geometry helpers as
#'   [plot_trajectory_confluence()]; see there for details.
#' @param palette Optional named colour vector overriding the default hues (keyed by
#'   destination central in the all/multi view, else by tributary `traj_id`).
#' @param labels_text Optional `data.frame` with columns `id` and `text` mapping
#'   ids to descriptions, appended to the hover (`id: description ...`). Central
#'   rivers are matched by their group id (`cNgN`), absorbed tributaries by their
#'   own trajectory id (`trN`); `NULL` (default) uses the bare id.
#' @param label_terminal Name the central (or selected) rivers in a de-overlapped
#'   column to the right, each joined to its stream by a leader line (default
#'   `TRUE`).
#' @param label_size Font size (px) of those right-hand labels (default `12`).
#' @param title Plot title; `NULL` (default) derives one from `target`.
#'
#' @return A `plotly` htmlwidget.
#'
#' @details
#' Built with plotly, following the same principles as
#' [plot_trajectory_dag_interactive()]:
#' - The **x-axis is the publication year**; the **y-axis has no intrinsic meaning**
#'   (it only packs streams to minimise merge crossings).
#' - Each stream is a filled polygon (`fill = "toself"`, hoverable over its area);
#'   a tributary is **grey** before the first paper it transfers, then in its colour.
#' - **There is no legend**: detail is revealed on hover, and only the terminal
#'   (central or selected) rivers carry a printed label.
#' - An intermediate that delivers **0 documents** to the final it flows into is
#'   always excluded (as in [plot_trajectory_confluence()]).
#'
#' The widget renders in the RStudio Viewer, embeds in Quarto/Shiny, or saves to a
#' standalone file with `htmlwidgets::saveWidget()`.
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' conf <- sniff_trajectory_confluence(flow)
#'
#' plot_trajectory_confluence_interactive(conf, min_n = 20)
#'
#' # one central's formation, hover the tributaries for their n/cohort
#' plot_trajectory_confluence_interactive(conf, target = "tr::c1g1")
#' }
#'
#' @seealso [plot_trajectory_confluence()], [plot_trajectory_dag_interactive()],
#'   [sniff_trajectory_confluence()]
#'
#' @family visualization
#' @export
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats setNames
plot_trajectory_confluence_interactive <- function(conf, target = "all", depth = NULL,
                                                   min_n = 5, min_prop = 0.05,
                                                   min_total_size = 0,
                                                   min_duration_years = 0,
                                                   width_range = c(0, 0.42),
                                                   palette = NULL, labels_text = NULL,
                                                   label_terminal = TRUE,
                                                   label_size = 12, smooth = TRUE,
                                                   title = NULL) {
  # keyword targets pick which layers to draw over the whole-forest layout.
  draw_spines <- draw_ribbons <- draw_merges <- TRUE
  view <- NULL
  if (is.character(target) && length(target) == 1L &&
      target %in% c("all", "finals", "intermediary")) {
    view <- target
    if (view == "finals")       { draw_ribbons <- FALSE; draw_merges <- FALSE }
    if (view == "intermediary") { draw_spines  <- FALSE; draw_merges <- FALSE }
    target <- NULL
  }
  if (!is.list(conf) || is.null(conf$rivers) || is.null(conf$confluences) ||
      !is.data.frame(conf$rivers)) {
    stop("'conf' must be the output of sniff_trajectory_confluence()", call. = FALSE)
  }

  single <- !is.null(target) && length(target) == 1
  max_depth <- if (!is.null(depth)) depth else if (is.null(target)) Inf else 1L
  pr <- .confluence_prune(conf$rivers, conf$confluences, target = target,
                          min_n = min_n, min_prop = min_prop,
                          min_total_size = min_total_size,
                          min_duration_years = min_duration_years,
                          max_depth = max_depth)
  if (nrow(pr$rivers) == 0) {
    stop("no trajectories pass the thresholds (min_n / min_prop / min_total_size / ",
         "min_duration_years)", call. = FALSE)
  }
  # an intermediate that delivers 0 documents to its final cannot appear.
  zf <- .confluence_drop_zero_dest(pr$rivers, pr$confluences, conf, target, single)
  pr$rivers <- zf$rivers; pr$confluences <- zf$confluences
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = target)
  g  <- .confluence_polygons(pr$rivers, pr$confluences, ln,
                             last_year = conf$last_year, smooth = smooth,
                             width_range = width_range)
  lane <- stats::setNames(ln$lane, ln$traj_id)

  # colour key: same rule as the static plot (per destination central, or per
  # tributary for a single-central view); grey is the pre-feed fill.
  if (!single) {
    keys <- conf$centrals
    key_of <- stats::setNames(pr$rivers$central, pr$rivers$traj_id)
  } else {
    keys <- mixed_sort(pr$rivers$traj_id[pr$rivers$type != "central"])
    key_of <- stats::setNames(pr$rivers$traj_id, pr$rivers$traj_id)
  }
  pal <- if (!is.null(palette)) palette else if (length(keys) <= 8 && length(keys) >= 1) {
    stats::setNames(RColorBrewer::brewer.pal(max(3, length(keys)), "Set2")[seq_along(keys)], keys)
  } else if (length(keys) >= 1) {
    stats::setNames(scales::hue_pal()(length(keys)), keys)
  } else {
    character(0)
  }
  hue <- function(id) {
    k <- key_of[[id]]
    unname(if (!is.null(k) && k %in% names(pal)) pal[[k]] else "grey50")
  }
  g$ribbons$fill <- ifelse(g$ribbons$seg == "pre", "grey60",
                           vapply(g$ribbons$traj_id, hue, character(1)))
  g$merges$fill  <- vapply(g$merges$child, hue, character(1))
  g$spines$fill  <- if (!single)
    vapply(g$spines$traj_id, hue, character(1)) else rep("#2E3440", nrow(g$spines))

  # hover text builders.
  cf_by_child <- stats::setNames(seq_len(nrow(pr$confluences)), pr$confluences$child)
  size_of <- stats::setNames(pr$rivers$size, pr$rivers$traj_id)
  central_of <- stats::setNames(pr$rivers$central, pr$rivers$traj_id)
  trib_hover <- function(id) {
    ci <- cf_by_child[id]
    desc <- if (id %in% names(trib_desc)) trib_desc[[id]] else id
    base <- if (!is.na(desc) && desc != id) paste0(id, ": ", desc) else id
    if (is.na(ci)) return(base)
    paste0(base, " (", pr$confluences$n[ci], "/", pr$confluences$cohort_size[ci],
           " -> tr::", central_of[[id]], ")")
  }
  # resolve all central descriptions at once (one matched/unmatched warning at most,
  # not one per central) and key them by central traj_id.
  cen_ids <- pr$rivers$traj_id[pr$rivers$type == "central"]
  cen_desc <- if (length(cen_ids))
    stats::setNames(.dag_label_text(unname(central_of[cen_ids]), labels_text), cen_ids) else
    character(0)
  # tributary descriptions: keyed by the tributary's own traj_id (trN), so each
  # carries its own description rather than its destination central's.
  trib_ids <- pr$rivers$traj_id[pr$rivers$type != "central"]
  trib_desc <- if (length(trib_ids))
    stats::setNames(.dag_label_text(trib_ids, labels_text), trib_ids) else character(0)
  cen_hover <- function(id) {
    grp <- central_of[[id]]
    desc <- if (id %in% names(cen_desc)) cen_desc[[id]] else grp
    base <- if (!is.na(desc) && desc != grp) paste0(id, ": ", desc) else id
    paste0(base, " (", size_of[[id]], ")")
  }

  # one filled polygon (top edge then reversed bottom edge -> closed path).
  poly <- function(p, x, lo, hi, fillcolor, text = NULL) {
    if (length(x) < 2) return(p)
    plotly::add_trace(p, x = c(x, rev(x)), y = c(hi, rev(lo)),
      type = "scatter", mode = "lines", fill = "toself", fillcolor = fillcolor,
      line = list(width = 0, color = "rgba(0,0,0,0)"), hoveron = "fills",
      hoverinfo = if (is.null(text)) "skip" else "text", text = text,
      showlegend = FALSE)
  }
  # several polygons sharing a colour with no hover, as one NA-separated trace.
  polys <- function(p, df, by, fillcolor) {
    xs <- ys <- numeric(0)
    for (k in unique(by)) {
      d <- df[by == k, , drop = FALSE]
      if (nrow(d) < 2) next
      xs <- c(xs, d$x, rev(d$x), NA); ys <- c(ys, d$hi, rev(d$lo), NA)
    }
    if (!length(xs)) return(p)
    plotly::add_trace(p, x = xs, y = ys, type = "scatter", mode = "lines",
      fill = "toself", fillcolor = fillcolor,
      line = list(width = 0, color = "rgba(0,0,0,0)"), hoverinfo = "skip",
      showlegend = FALSE)
  }

  p <- plotly::plot_ly()
  # draw order: spines (back), merges, then ribbons (front), matching the static.
  if (draw_spines && nrow(g$spines)) {
    for (id in unique(g$spines$traj_id)) {
      d <- g$spines[g$spines$traj_id == id, , drop = FALSE]
      p <- poly(p, d$x, d$lo, d$hi, d$fill[1], text = cen_hover(id))
    }
  }
  if (draw_merges && nrow(g$merges)) {
    for (col in unique(g$merges$fill)) {
      sub <- g$merges[g$merges$fill == col, , drop = FALSE]
      p <- polys(p, sub, sub$child, col)
    }
  }
  if (draw_ribbons && nrow(g$ribbons)) {
    pre <- g$ribbons[g$ribbons$seg == "pre", , drop = FALSE]
    if (nrow(pre)) p <- polys(p, pre, pre$traj_id, "grey60")
    feed <- g$ribbons[g$ribbons$seg == "feed", , drop = FALSE]
    for (id in unique(feed$traj_id)) {
      d <- feed[feed$traj_id == id, , drop = FALSE]
      p <- poly(p, d$x, d$lo, d$hi, d$fill[1], text = trib_hover(id))
    }
  }

  # terminal labels: the central (or selected) rivers, in a de-overlapped column.
  if (label_terminal) {
    roots <- if (is.null(target)) {
      if (draw_spines) pr$rivers$traj_id[pr$rivers$type == "central"] else character(0)
    } else {
      intersect(target, pr$rivers$traj_id)
    }
    if (length(roots)) {
      rx_spine <- if (nrow(g$spines)) tapply(g$spines$x, g$spines$traj_id, max) else NULL
      rx_ribb  <- if (nrow(g$ribbons)) tapply(g$ribbons$x, g$ribbons$traj_id, max) else NULL
      rxf <- function(id) {
        if (!is.null(rx_spine) && id %in% names(rx_spine)) return(rx_spine[[id]])
        if (!is.null(rx_ribb)  && id %in% names(rx_ribb))  return(rx_ribb[[id]])
        conf$last_year
      }
      rdf <- data.frame(id = roots, stringsAsFactors = FALSE)
      rdf$x <- vapply(rdf$id, rxf, numeric(1))
      rdf$y <- vapply(rdf$id, function(i) lane[[i]], numeric(1))
      gap <- if (nrow(rdf) > 1) diff(range(rdf$y)) / (nrow(rdf) - 1) else 0
      rdf$ly <- .spread_y(rdf$y, gap)
      rdf$lx <- rdf$x + 0.4
      seg_x <- as.numeric(t(cbind(rdf$x, rdf$lx, NA)))
      seg_y <- as.numeric(t(cbind(rdf$y, rdf$ly, NA)))
      p <- plotly::add_trace(p, x = seg_x, y = seg_y, type = "scatter", mode = "lines",
        line = list(color = "grey60", width = 0.7), hoverinfo = "skip", showlegend = FALSE)
      p <- plotly::add_trace(p, x = rdf$lx, y = rdf$ly, type = "scatter", mode = "text",
        text = rdf$id, textposition = "middle right",
        textfont = list(size = label_size, color = "black"), cliponaxis = FALSE,
        hoverinfo = "skip", showlegend = FALSE)
    }
  }

  if (is.null(title)) {
    title <- if (identical(view, "finals")) "Final trajectories"
      else if (identical(view, "intermediary")) "Intermediate trajectories"
      else if (is.null(target)) "Trajectory confluence"
      else paste0("Formation of ", paste(sub("^tr::", "", target), collapse = ", "))
  }
  x_lo <- min(pr$rivers$start); x_hi <- conf$last_year
  xax <- list(showgrid = FALSE, zeroline = FALSE, title = "Year", tickmode = "array",
              tickvals = seq(x_lo, x_hi, by = 2), ticktext = seq(x_lo, x_hi, by = 2),
              range = c(x_lo - 0.5, x_hi + 3))
  yax <- list(visible = FALSE, showgrid = FALSE, zeroline = FALSE)
  ttl <- if (is.null(title)) list(text = "") else list(text = title)
  plotly::layout(p, xaxis = xax, yaxis = yax, title = ttl, showlegend = FALSE)
}
