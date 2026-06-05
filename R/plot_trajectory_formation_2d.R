#' Plot the trajectories that fed into a target (confluence timeline)
#'
#' Draws the target trajectory as a central spine and each feeder as a parallel
#' timeline that merges into the spine at its handoff year. The spine is a
#' cumulative river: it widens smoothly as each drawn feeder joins, so by the final year
#' its width reflects the total papers absorbed. Spine and feeder widths share
#' one strictly proportional scale (width 0 at zero papers), so their thickness
#' is directly comparable. It generalises [plot_trajectory_handoff()] (one
#' source into one absorber) to many feeders converging on one trajectory,
#' answering visually "which trajectories formed this one?".
#'
#' @param formation Output of [sniff_trajectory_formation()].
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
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_rect geom_segment geom_text
#' @importFrom ggplot2 annotate scale_fill_discrete scale_colour_discrete
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous labs theme_minimal
#' @importFrom ggplot2 theme element_blank element_text expansion
#' @importFrom grid arrow unit
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
plot_trajectory_formation_2d <- function(formation, max_feeders = 8,
                                      title = NULL, label_size = 4.5) {
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
  target_label <- paste0(ti$group, "::", ti$traj_id)
  # name with the main trajectory's paper count on a line below
  target_text <- if (!is.null(ti$size)) {
    paste0(target_label, "\n(", ti$size, ")")
  } else {
    target_label
  }

  k <- nrow(feeders)
  spine_y <- 0
  feeders <- feeders |> dplyr::mutate(y = seq.int(k, 1L))  # strongest on top

  # Cumulative river: the spine is a filled polygon that widens smoothly as each
  # drawn feeder merges. Width is in y-data units, strictly proportional to
  # papers (0 papers -> 0 width), on the same scale as the feeder bars, so spine
  # and feeder thicknesses are directly comparable. The step cumulative is
  # smoothed (two box passes) so the river grows gradually, not in sharp steps.
  total_drawn <- sum(feeders$n)
  max_half <- 0.42                    # half-height of the widest object
  scale_h <- max_half / total_drawn   # y-data units per paper

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
  feeders <- feeders |> dplyr::mutate(half = scale_h * .data$n)

  # Vertical merge arrows: from the underside of each feeder bar down to the
  # river's top edge at the handoff year, so the arrowhead lands on the river
  # surface instead of being buried in it.
  river_top <- spine_y + scale_h *
    stats::approx(grid_x, smooth_cum, xout = feeders$handoff_year, rule = 2)$y
  arrows <- tibble::tibble(
    x = feeders$handoff_year, xend = feeders$handoff_year,
    y = feeders$y - feeders$half, yend = river_top
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
    ggplot2::geom_rect(
      data = feeders,
      ggplot2::aes(
        xmin = .data$start_year, xmax = .data$handoff_year,
        ymin = .data$y - .data$half, ymax = .data$y + .data$half,
        fill = .data$source_key
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
        x = .data$start_year, y = .data$y, colour = .data$source_key,
        label = paste0(.data$source_key, " (", .data$n, "/", .data$cohort_size, ")")
      ),
      hjust = 1, nudge_x = -0.3, size = label_size, fontface = "bold",
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
      expand = ggplot2::expansion(mult = c(0.28, 0.18))
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
