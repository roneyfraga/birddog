#' Plot the trajectories that fed into a target in 3D (confluence)
#'
#' Interactive 3D (plotly) version of [plot_trajectory_formation_2d()], in the
#' idiom of [plot_group_trajectories_lines_3d()]. The target trajectory is a
#' cumulative "river" line at lane `y = 0` whose height (z) grows with the papers
#' absorbed. Each feeder sits on its own lane and rises along a measured curve
#' (see `feeder_curve`); a connector then drops from the feeder's end onto the
#' river at its handoff year, its width scaled by the papers `n` that actually
#' transferred. So the slope shows how each feeder built up and the connector
#' shows how much of it flowed into the target.
#'
#' @param formation Output of [sniff_trajectory_formation()] (or one element of
#'   [sniff_trajectory_formations()]).
#' @param feeder_curve What the feeder slope traces: `"size"` (default) is the
#'   feeder's measured cluster size per year, ending at `cohort_size` (can rise
#'   and fall as the community merges and splits; best read with
#'   `log_scale = TRUE`); `"inflow"` is the cumulative arrival of just the `n`
#'   contributed papers, a monotone curve ending at `n` on the river's scale.
#' @param max_feeders Maximum number of feeders to draw, by descending papers
#'   (default: 8). Papers in omitted feeders are noted in the title.
#' @param log_scale Apply `log1p()` to the z-axis (papers), which keeps large
#'   feeders from dwarfing the river (default: TRUE).
#' @param width_range Line-width range, scaled by papers (default: c(3, 12)).
#' @param title Plot title (default: the target trajectory key).
#' @param label_size Font size for the end labels (default: 16).
#' @param hover_font_size Font size for hover tooltips (default: 12).
#' @param descriptions Optional data frame of manual hover descriptions
#'   (default: NULL). Must have columns `id` (trajectory key `"group::traj_id"`
#'   or `"group:traj_id"`) and `text`. When supplied, the matching target or
#'   feeder node shows the description on hover; others keep the default hover.
#'
#' @return A plotly 3D plot object.
#'
#' @examples
#' \dontrun{
#' f <- sniff_trajectory_formation(
#'   "c1g10::tr1", groups_detected_trajectories,
#'   groups_cumulative_trajectories$docs_per_group
#' )
#' plot_trajectory_formation_3d(f)
#' }
#'
#' @seealso [plot_trajectory_formation_2d()], [sniff_trajectory_formation()],
#'   [plot_group_trajectories_lines_3d()]
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales hue_pal rescale
#' @importFrom stats approx
#' @export
plot_trajectory_formation_3d <- function(formation,
                                         feeder_curve = c("size", "inflow"),
                                         max_feeders = 8,
                                         log_scale = TRUE,
                                         width_range = c(3, 12),
                                         title = NULL,
                                         label_size = 16,
                                         hover_font_size = 12,
                                         descriptions = NULL) {
  feeder_curve <- match.arg(feeder_curve)
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly is required for 3D plotting. install.packages('plotly').", call. = FALSE)
  }
  if (!is.list(formation) ||
        is.null(formation$feeders) ||
        is.null(formation$target_info)) {
    stop("'formation' must be the output of sniff_trajectory_formation()", call. = FALSE)
  }

  # Optional manual hover descriptions, keyed by trajectory key (":" or "::").
  desc_lookup <- NULL
  if (!is.null(descriptions)) {
    if (!all(c("id", "text") %in% names(descriptions))) {
      stop("'descriptions' must be a data.frame with columns 'id' and 'text'", call. = FALSE)
    }
    desc_lookup <- stats::setNames(
      as.character(descriptions$text),
      gsub("::", ":", as.character(descriptions$id))
    )
  }
  desc_of <- function(key) {
    if (is.null(desc_lookup)) return(NA_character_)
    unname(desc_lookup[gsub("::", ":", key)])
  }

  feeders <- formation$feeders
  feeders <- feeders[feeders$kept %in% TRUE, , drop = FALSE]
  if (nrow(feeders) == 0) {
    stop("no feeders pass the min_papers/min_prop thresholds to plot", call. = FALSE)
  }
  feeders <- feeders[order(-feeders$n, feeders$source_key), , drop = FALSE]

  omitted_n <- 0L
  if (nrow(feeders) > max_feeders) {
    omitted_n <- sum(feeders$n[(max_feeders + 1):nrow(feeders)])
    feeders <- feeders[seq_len(max_feeders), , drop = FALSE]
  }

  ti <- formation$target_info
  last_year <- formation$last_year
  target_label <- if (is.null(ti$group) || is.na(ti$group) || ti$group == "") {
    ti$traj_id
  } else {
    paste0(ti$group, "::", ti$traj_id)
  }
  k <- nrow(feeders)
  feeders$lane <- seq.int(k, 1L)   # strongest feeder on the highest lane

  zfun <- function(z) if (log_scale) log1p(z) else z

  # Cumulative river: inflow accumulated by each handoff year.
  ev <- sort(unique(feeders$handoff_year))
  cum_after <- vapply(ev, function(e) sum(feeders$n[feeders$handoff_year <= e]), numeric(1))
  rx <- c(ti$start, ev, last_year)
  rz_raw <- c(0, cum_after, max(cum_after))
  o <- order(rx); rx <- rx[o]; rz_raw <- rz_raw[o]
  keep <- !duplicated(rx, fromLast = TRUE)   # on ties keep the higher (cumulative) z
  rx <- rx[keep]; rz_raw <- rz_raw[keep]
  rz <- zfun(rz_raw)

  p <- plotly::plot_ly()

  # Which feeder curve to draw: "inflow" rises to the contributed papers n
  # (monotone, on the river's scale); "size" follows the feeder's real cluster
  # size up to cohort_size (can rise and fall).
  curve_col <- if (feeder_curve == "inflow") "inflow_curve" else "size_curve"
  has_curve <- curve_col %in% names(feeders)
  if (!has_curve) {
    warning("formation lacks '", curve_col, "' (old object); drawing a linear ",
            "ramp. Regenerate with sniff_trajectory_formation().", call. = FALSE)
  }
  feeder_top_raw <- if (feeder_curve == "inflow") feeders$n else feeders$cohort_size

  # per-feeder series (linear-ramp fallback for objects without the curve)
  feeder_series <- function(j) {
    g <- if (has_curve) feeders[[curve_col]][[j]] else NULL
    if (is.null(g) || nrow(g) == 0) {
      g <- data.frame(year = c(feeders$start_year[j], feeders$handoff_year[j]),
                      size = c(0L, feeder_top_raw[j]))
    }
    g[order(g$year), , drop = FALSE]
  }

  # Shared paper -> width scale across the river and every feeder.
  zr <- range(c(rz, zfun(feeder_top_raw), zfun(feeders$n)))
  wscale <- function(v) {
    if (diff(zr) == 0) rep(mean(width_range), length(v))
    else scales::rescale(zfun(v), to = width_range, from = zr)
  }

  # --- target river: growing-width dark line at y = 0 ---
  river_w <- if (length(unique(rz)) <= 1) {
    rep(mean(width_range), max(0, length(rx) - 1))
  } else {
    scales::rescale(rz[-1], to = width_range, from = zr)
  }
  for (i in seq_len(length(rx) - 1)) {
    p <- plotly::add_trace(
      p, x = c(rx[i], rx[i + 1]), y = c(0, 0), z = c(rz[i], rz[i + 1]),
      type = "scatter3d", mode = "lines",
      line = list(color = "#2E3440", width = river_w[i]),
      showlegend = FALSE, hoverinfo = "skip"
    )
  }

  # colors per feeder
  pal <- if (k <= 8) {
    RColorBrewer::brewer.pal(max(3, k), "Set2")[seq_len(k)]
  } else {
    scales::hue_pal()(k)
  }
  line_w <- wscale(feeder_top_raw)   # feeder line width ~ its end value
  conn_w <- wscale(feeders$n)        # merge width ~ papers transferred

  for (j in seq_len(k)) {
    fd <- feeders[j, ]
    col <- pal[j]

    g <- feeder_series(j)
    gz <- zfun(g$size)
    ztop <- gz[length(gz)]   # feeder end: n (inflow) or cohort_size (size)

    # feeder line on its lane, rising along the chosen curve
    if (nrow(g) >= 2) {
      p <- plotly::add_trace(
        p, x = g$year, y = rep(fd$lane, nrow(g)), z = gz,
        type = "scatter3d", mode = "lines",
        line = list(color = col, width = line_w[j]),
        name = fd$source_key, showlegend = TRUE, hoverinfo = "skip"
      )
    } else {
      p <- plotly::add_trace(
        p, x = g$year, y = fd$lane, z = gz,
        type = "scatter3d", mode = "markers",
        marker = list(size = 4, color = col),
        name = fd$source_key, showlegend = TRUE, hoverinfo = "skip"
      )
    }

    # merge connector: from the feeder's end down to the river at the handoff.
    # Light grey so the thick 3D lines stay subtle; a hex, since plotly.js does
    # not understand R colour names (e.g. "grey55").
    cum_at <- stats::approx(rx, rz, xout = fd$handoff_year, rule = 2)$y
    p <- plotly::add_trace(
      p, x = c(fd$handoff_year, fd$handoff_year), y = c(fd$lane, 0),
      z = c(ztop, cum_at), type = "scatter3d", mode = "lines",
      line = list(color = "#C8C8C8", width = conn_w[j]),
      showlegend = FALSE, hoverinfo = "skip"
    )

    # hover marker at the handoff point
    hover <- paste0(
      "<b>", fd$source_key, "</b><br>",
      "to target: ", fd$n, "/", fd$cohort_size,
      " (", round(100 * fd$prop_of_source), "%)<br>",
      "handoff: ", fd$handoff_year
    )
    fd_desc <- desc_of(fd$source_key)
    if (!is.na(fd_desc)) hover <- paste0(hover, "<br>", fd_desc)
    p <- plotly::add_trace(
      p, x = fd$handoff_year, y = fd$lane, z = ztop,
      type = "scatter3d", mode = "markers",
      marker = list(size = 3, color = col),
      hoverinfo = "text", text = hover,
      hoverlabel = list(bgcolor = "rgba(50,50,50,0.9)",
                        font = list(color = "white", size = hover_font_size)),
      showlegend = FALSE
    )

    # feeder label at its start
    p <- plotly::add_trace(
      p, x = g$year[1], y = fd$lane, z = gz[1],
      type = "scatter3d", mode = "text",
      text = paste0(fd$source_key, " (", fd$n, "/", fd$cohort_size, ")"),
      textfont = list(size = label_size - 2, color = col),
      showlegend = FALSE, hoverinfo = "skip"
    )
  }

  # terminal node: a ball on the target river's end
  target_hover <- if (!is.null(ti$size)) {
    paste0("<b>", target_label, "</b><br>documents: ", ti$size)
  } else {
    target_label
  }
  tgt_desc <- desc_of(target_label)
  if (!is.na(tgt_desc)) target_hover <- paste0(target_hover, "<br>", tgt_desc)
  p <- plotly::add_trace(
    p, x = rx[length(rx)], y = 0, z = rz[length(rz)],
    type = "scatter3d", mode = "markers",
    marker = list(size = 6, color = "#2E3440"),
    hoverinfo = "text", text = target_hover,
    hoverlabel = list(bgcolor = "rgba(50,50,50,0.9)",
                      font = list(color = "white", size = hover_font_size)),
    showlegend = FALSE
  )

  # target label at the river end: name with the main trajectory's paper count
  # on a line below (e.g. "c1g10::tr1\n(214)")
  target_text <- if (!is.null(ti$size)) {
    paste0(target_label, "<br>(", ti$size, ")")
  } else {
    target_label
  }
  p <- plotly::add_trace(
    p, x = rx[length(rx)], y = 0, z = rz[length(rz)],
    type = "scatter3d", mode = "text", text = target_text,
    textfont = list(size = label_size, color = "#2E3440"),
    showlegend = FALSE, hoverinfo = "skip"
  )

  if (is.null(title)) title <- target_label
  if (omitted_n > 0) {
    title <- paste0(title, "  (+", omitted_n, " papers in omitted feeders)")
  }

  plotly::layout(
    p,
    scene = list(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Feeder lane"),
      zaxis = list(title = paste0("Papers", if (log_scale) " (log)" else "")),
      camera = list(eye = list(x = -1.5, y = -2, z = 1))
    ),
    legend = list(font = list(size = 14)),
    title = title
  )
}
