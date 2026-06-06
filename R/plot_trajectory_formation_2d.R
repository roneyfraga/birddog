#' Plot the trajectories that fed into a target (confluence timeline)
#'
#' Draws the target trajectory as a central spine and each feeder as a parallel
#' timeline that grows along its length (see `feeder_curve`) and merges into the
#' spine at its handoff year. The spine is a cumulative river: it widens smoothly
#' as each drawn feeder joins, so by the final year its width reflects the total
#' papers absorbed. Spine and feeder widths share one strictly proportional scale
#' (width 0 at zero papers), so their thickness is directly comparable. It
#' generalises [plot_trajectory_handoff()] (one
#' source into one absorber) to many feeders converging on one trajectory,
#' answering visually "which trajectories formed this one?".
#'
#' @param formation Output of [sniff_trajectory_formation()].
#' @param feeder_curve What the feeder's growing width traces: `"inflow"`
#'   (default) is the cumulative arrival of just the papers that entered the
#'   target, a monotone curve ending at `n` on the river's scale; `"size"` is the
#'   feeder's measured cluster size per year, ending at `cohort_size` (its whole
#'   community, which can dwarf and shrink the river since the 2D scale is
#'   linear). Each feeder rises from zero at its start year to this end value, on
#'   the same proportional scale as the spine, so it grows like the target does.
#'   Mirrors the `feeder_curve` of [plot_trajectory_formation_3d()] (whose
#'   default is `"size"`, mitigated there by a log z-axis).
#' @param max_feeders Maximum number of feeders to draw, taken by descending
#'   papers (default: 8). Feeders beyond this are summarised in the caption.
#' @param title Plot title. Defaults to the target trajectory key.
#' @param label_size Text size for the feeder and target labels (default: 4.5).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' f <- sniff_trajectory_formation(
#'   "c1g10::tr1", groups_detected_trajectories,
#'   groups_cumulative_trajectories$docs_per_group
#' )
#' plot_trajectory_formation_2d(f)
#' }
#'
#' @seealso [sniff_trajectory_formation()], [plot_trajectory_handoff()],
#'   [plot_trajectory_destination()]
#'
#' @importFrom dplyr filter arrange desc mutate transmute
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_segment geom_text
#' @importFrom ggplot2 annotate scale_fill_discrete scale_colour_discrete
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous labs theme_minimal
#' @importFrom ggplot2 theme element_blank element_text expansion
#' @importFrom grid arrow unit
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
plot_trajectory_formation_2d <- function(formation,
                                      feeder_curve = c("inflow", "size"),
                                      max_feeders = 8,
                                      title = NULL, label_size = 4.5) {
  feeder_curve <- match.arg(feeder_curve)
  if (!is.list(formation) ||
        is.null(formation$feeders) ||
        is.null(formation$target_info)) {
    stop("'formation' must be the output of sniff_trajectory_formation()", call. = FALSE)
  }

  feeders <- formation$feeders
  feeders <- feeders[feeders$kept %in% TRUE, , drop = FALSE]
  if (nrow(feeders) == 0) {
    stop("no feeders pass the min_papers/min_prop thresholds to plot", call. = FALSE)
  }
  feeders <- feeders |> dplyr::arrange(dplyr::desc(.data$n), .data$source_key)

  omitted <- NULL
  if (nrow(feeders) > max_feeders) {
    omitted <- feeders[(max_feeders + 1):nrow(feeders), , drop = FALSE]
    feeders <- feeders[seq_len(max_feeders), , drop = FALSE]
  }

  ti <- formation$target_info
  last_year <- formation$last_year
  target_label <- if (is.null(ti$group) || is.na(ti$group) || ti$group == "") {
    ti$traj_id
  } else {
    paste0(ti$group, "::", ti$traj_id)
  }
  # name with the main trajectory's paper count on a line below
  target_text <- if (!is.null(ti$size)) {
    paste0(target_label, "\n(", ti$size, ")")
  } else {
    target_label
  }

  k <- nrow(feeders)
  spine_y <- 0
  feeders <- feeders |> dplyr::mutate(y = seq.int(k, 1L))  # strongest on top

  # Per-feeder growth series (year, size): the feeder rises from zero at its
  # start year to its end value (n for "inflow", cohort_size for "size") on the
  # river's scale, so it grows like the spine instead of being a flat bar. Falls
  # back to a linear 0 -> end ramp for old objects lacking the curve column.
  curve_col <- if (feeder_curve == "size") "size_curve" else "inflow_curve"
  has_curve <- curve_col %in% names(feeders)
  if (!has_curve) {
    warning("formation lacks '", curve_col, "' (old object); drawing a linear ",
            "ramp. Regenerate with sniff_trajectory_formation().", call. = FALSE)
  }
  feeder_series <- function(j) {
    g <- if (has_curve) feeders[[curve_col]][[j]] else NULL
    end_val <- if (feeder_curve == "size") feeders$cohort_size[j] else feeders$n[j]
    if (is.null(g) || nrow(g) == 0) {
      g <- tibble::tibble(
        year = c(feeders$start_year[j], feeders$handoff_year[j]),
        size = c(0L, as.integer(end_val))
      )
    }
    g <- g[order(g$year), , drop = FALSE]
    if (g$year[1] > feeders$start_year[j]) {  # grow from zero at the feeder's start
      g <- tibble::tibble(year = c(feeders$start_year[j], g$year),
                          size = c(0L, g$size))
    }
    g
  }
  series <- lapply(seq_len(k), feeder_series)

  # Cumulative river: the spine is a filled polygon that widens smoothly as each
  # drawn feeder merges. Width is in y-data units, strictly proportional to
  # papers (0 papers -> 0 width), on the same scale as the feeder ribbons, so
  # spine and feeder thicknesses are directly comparable. The scale is set so the
  # widest object (the final river, or a feeder's peak size under "size") fills
  # max_half. The step cumulative is smoothed so the river grows gradually.
  total_drawn <- sum(feeders$n)
  feeder_peak <- max(vapply(series, function(g) max(g$size), numeric(1)), 0)
  peak <- max(total_drawn, feeder_peak)
  max_half <- 0.42                    # half-height of the widest object
  scale_h <- max_half / peak          # y-data units per paper

  # Step cumulative on a fine grid, then a Gaussian smooth so the river edge is a
  # natural curve, not an angular staircase. The grid is padded with its end
  # values so the convolution stays defined; smoothing a monotone step keeps it
  # monotone.
  x0 <- min(ti$start, min(feeders$handoff_year))
  n_grid <- 600
  grid_x <- seq(x0, last_year, length.out = n_grid)
  step_cum <- vapply(
    grid_x, function(xx) sum(feeders$n[feeders$handoff_year <= xx]), numeric(1)
  )
  dx <- (last_year - x0) / (n_grid - 1)
  sigma_pts <- max(1, 1.0 / dx)                 # ~1-year Gaussian sd, in grid pts
  hk <- ceiling(3 * sigma_pts)
  kern <- stats::dnorm(seq(-hk, hk), sd = sigma_pts)
  kern <- kern / sum(kern)
  padded <- c(rep(step_cum[1], hk), step_cum, rep(step_cum[n_grid], hk))
  sm <- as.numeric(stats::filter(padded, kern, sides = 2))
  smooth_cum <- pmax(0, sm[(hk + 1):(hk + n_grid)])

  river <- tibble::tibble(
    x = grid_x,
    lo = spine_y - scale_h * smooth_cum,
    hi = spine_y + scale_h * smooth_cum
  )
  # Each feeder is a ribbon on its own lane, growing from zero at its start year
  # to its end value along the chosen curve, half-height = scale_h * size.
  feeder_ribbon <- do.call(rbind, lapply(seq_len(k), function(j) {
    g <- series[[j]]
    tibble::tibble(
      source_key = feeders$source_key[j],
      x = g$year,
      lo = feeders$y[j] - scale_h * g$size,
      hi = feeders$y[j] + scale_h * g$size
    )
  }))
  # Each feeder's half-height at its handoff (the underside the arrow starts at).
  end_half <- vapply(
    seq_len(k), function(j) scale_h * series[[j]]$size[nrow(series[[j]])], numeric(1)
  )

  # Vertical merge arrows: from the underside of each feeder ribbon's end down to
  # the river's top edge at the handoff year, so the arrowhead lands on the river
  # surface instead of being buried in it.
  river_top <- spine_y + scale_h *
    stats::approx(grid_x, smooth_cum, xout = feeders$handoff_year, rule = 2)$y
  arrows <- tibble::tibble(
    x = feeders$handoff_year, xend = feeders$handoff_year,
    y = feeders$y - end_half, yend = river_top
  )

  if (is.null(title)) {
    title <- target_label
  }
  title <- gsub("\\s*->\\s*", paste0(" ", intToUtf8(8594), " "), title)

  year_breaks <- seq(min(feeders$start_year, ti$start), last_year, by = 2)

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = river,
      ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi),
      fill = "#2E3440"
    ) +
    ggplot2::geom_ribbon(
      data = feeder_ribbon,
      ggplot2::aes(
        x = .data$x, ymin = .data$lo, ymax = .data$hi,
        fill = .data$source_key, group = .data$source_key
      )
    ) +
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed"),
      colour = "grey55", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = feeders,
      ggplot2::aes(
        x = .data$handoff_year, y = .data$y, colour = .data$source_key,
        label = paste0(.data$source_key, "\n(", .data$n, "/", .data$cohort_size, ")")
      ),
      hjust = 0, nudge_x = 0.5, size = label_size, fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text", x = last_year + 0.5, y = spine_y, label = target_text,
      hjust = 0, size = label_size + 0.5, fontface = "bold", colour = "#2E3440"
    ) +
    ggplot2::scale_fill_discrete(guide = "none") +
    ggplot2::scale_colour_discrete(guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = year_breaks,
      expand = ggplot2::expansion(mult = c(0.05, 0.30))
    ) +
    ggplot2::scale_y_continuous(limits = c(-0.6, k + 0.6), breaks = NULL) +
    ggplot2::labs(x = "Year", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
      plot.title = ggplot2::element_text(size = 18, face = "bold")
    )

  if (!is.null(omitted) && nrow(omitted) > 0) {
    p <- p + ggplot2::labs(
      caption = paste0("+ ", nrow(omitted), " more feeders (",
                       sum(omitted$n), " papers) omitted")
    )
  }
  p
}
