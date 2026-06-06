#' Plot the handoff from a stagnant trajectory to the one that absorbed it
#'
#' Draws the stagnant (source) trajectory and the dominant absorbing trajectory
#' as two timelines on a shared year axis, with an arrow marking the handoff
#' year (the source's terminal year). It answers, visually, "which living
#' trajectory continued this one?".
#'
#' @param destination Output of [sniff_trajectory_destination()] built with
#'   `all_detected` (so that `source_info` and `continuation_info` are present).
#' @param title Plot title. Defaults to a `source -> absorber` summary.
#' @param label_size Text size for the in-plot labels (the two trajectory names
#'   and the papers-carried annotation). Default: 5.5.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' dest <- sniff_trajectory_destination(
#'   detected, "tr3", docs_per_group,
#'   all_detected = groups_detected_trajectories, group = "c1g10"
#' )
#' plot_trajectory_handoff(dest)
#' }
#'
#' @seealso [sniff_trajectory_destination()], [plot_trajectory_destination()]
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_text annotate
#' @importFrom ggplot2 scale_color_manual scale_linewidth scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous labs theme_minimal theme element_blank
#' @importFrom ggplot2 element_text
#' @importFrom grid arrow unit
#' @importFrom tibble tibble
#' @export
plot_trajectory_handoff <- function(destination, title = NULL, label_size = 5.5) {
  if (!is.list(destination) ||
        is.null(destination$source_info) ||
        is.null(destination$continuation_info)) {
    stop("plot_trajectory_handoff() requires a destination built with sniff_trajectory_destination(..., all_detected = ) and a non-empty continuation", call. = FALSE)
  }

  src <- destination$source_info
  abs_ <- destination$continuation_info
  handoff_year <- src$end

  src_label <- if (is.null(src$group) || is.na(src$group) || src$group == "") {
    src$traj_id
  } else paste(src$group, src$traj_id)
  abs_label <- if (is.null(abs_$group) || is.na(abs_$group) || abs_$group == "") {
    abs_$traj_id
  } else paste(abs_$group, abs_$traj_id)

  seg <- tibble::tibble(
    role = c("stagnant (source)", "absorbing (living)"),
    y = c(2, 1),
    label_y = c(2.34, 1.20),
    x = c(src$start, abs_$start),
    xend = c(src$end, abs_$end),
    label = c(src_label, abs_label),
    papers = c(src$cohort_size, abs_$n_papers)
  )

  if (is.null(title)) {
    title <- paste0(
      src_label, " -> ", abs_label, "  (handoff ", handoff_year, ")"
    )
  }
  title <- gsub("\\s*->\\s*", paste0(" ", intToUtf8(8594), " "), title)

  # Year axis every two years across the spanned period
  year_breaks <- seq(min(seg$x), max(seg$xend), by = 2)

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg,
      ggplot2::aes(
        x = .data$x, xend = .data$xend, y = .data$y, yend = .data$y,
        color = .data$role, linewidth = .data$papers
      ),
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = seg,
      ggplot2::aes(x = .data$xend, y = .data$y, color = .data$role),
      size = 2.5
    ) +
    ggplot2::geom_text(
      data = seg,
      ggplot2::aes(x = .data$x, y = .data$label_y, label = .data$label),
      hjust = 0, size = label_size, fontface = "bold", color = "grey20"
    ) +
    ggplot2::annotate(
      "segment",
      x = handoff_year, xend = handoff_year, y = 2 - 0.08, yend = 1 + 0.08,
      arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed"),
      color = "grey40"
    ) +
    ggplot2::annotate(
      "text",
      x = handoff_year, y = 1.5, label = paste0(abs_$n_papers, " papers"),
      hjust = -0.1, size = label_size - 0.5, color = "grey30"
    ) +
    ggplot2::scale_color_manual(
      values = c("stagnant (source)" = "#D1495B", "absorbing (living)" = "#2E8B57"),
      name = NULL
    ) +
    ggplot2::scale_linewidth(range = c(1, 6), guide = "none") +
    ggplot2::scale_x_continuous(breaks = year_breaks) +
    ggplot2::scale_y_continuous(limits = c(0.6, 2.8), breaks = NULL) +
    ggplot2::labs(x = "Year", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
      plot.title = ggplot2::element_text(size = 20, face = "bold")
    )
}
