#' Plot where a stagnant trajectory's cohort dispersed (its dispersion fan)
#'
#' Draws the selected intermediate (stagnant) trajectory as a short spine on the
#' left and fans out, from its handoff year, one curved link to **every final
#' trajectory its terminal cohort reached** (the full `destination` split), each
#' link's width scaled by the papers that landed there and labelled
#' `tr::cNgN (n)`. It answers, visually, "where did this trajectory go?" -- not
#' just the dominant absorber but the complete dispersion. Styling follows the
#' other flow plots ([plot_trajectory_lines_2d()], [plot_trajectory_confluence()]):
#' a Set2/hue palette keyed by final group, a year-aware x-axis, and the shared
#' `year_range` / `axis_text_size` / `palette` controls.
#'
#' @param destination Output of [sniff_trajectory_destination()]. Uses its
#'   `destination` table (`g_final`, `n`) and `source_info`.
#' @param title Plot title. Defaults to a `source -> N finals` summary.
#' @param label_size Text size for the in-plot labels (default: 4.5).
#' @param year_range Optional `c(from, to)` years fixing the time axis, as in
#'   [plot_trajectory_lines_2d()]. `NULL` (default) fits the drawn data; a wider
#'   range only extends the axis.
#' @param axis_text_size Font size of the x-axis (year) tick labels; `NULL`
#'   (default) keeps the built-in size.
#' @param palette Optional named colour vector overriding the default hues (keyed
#'   by final group `cNgN`). `NULL` (default) uses the package's Set2 (<= 8) / hue
#'   scheme.
#' @param min_n Minimum papers a final must receive to be drawn (default `0`, all
#'   impacted finals). Raise it to hide the long tail of tiny destinations.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' d <- sniff_trajectory_destination(flow, "tr1")
#' plot_trajectory_dispersion(d)
#' }
#'
#' @seealso [sniff_trajectory_destination()], [plot_trajectory_confluence()],
#'   [plot_trajectory_lines_2d()]
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_curve geom_point geom_text annotate
#' @importFrom ggplot2 geom_blank scale_colour_identity scale_linewidth scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous expansion labs theme_minimal theme element_blank
#' @importFrom ggplot2 element_text
#' @importFrom grid arrow unit
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @family visualization
#' @export
plot_trajectory_dispersion <- function(destination, title = NULL, label_size = 4.5,
                                       year_range = NULL, axis_text_size = NULL,
                                       palette = NULL, min_n = 0) {
  if (!is.list(destination) || is.null(destination$source_info) ||
        is.null(destination$destination) || !is.data.frame(destination$destination)) {
    stop("plot_trajectory_dispersion() requires the output of sniff_trajectory_destination()",
         call. = FALSE)
  }
  src <- destination$source_info
  ti <- destination$target_info
  # a central trajectory reaches the final year, so it has no handoff / continuation
  if (!is.null(ti) && identical(ti$type, "central")) {
    stop("'", src$traj_id, "' is a central trajectory: it reaches the final year and ",
         "has no handoff (no continuation) to plot", call. = FALSE)
  }

  handoff <- src$end
  last_year <- destination$last_year %||% (handoff + 6L)

  dest <- destination$destination
  dest <- dest[dest$n >= min_n, , drop = FALSE]
  dest <- dest[order(-dest$n), , drop = FALSE]
  if (nrow(dest) == 0) {
    stop("no destinations pass min_n; lower it to show where the cohort went",
         call. = FALSE)
  }

  k <- nrow(dest)
  dest$lane <- seq_len(k)                       # biggest destination nearest the spine
  is_drop <- dest$g_final == "(dropped)"
  dest$label <- ifelse(is_drop, paste0("(dropped) (", dest$n, ")"),
                       paste0("tr::", dest$g_final, " (", dest$n, ")"))

  # colours: the package's Set2 (<= 8) / hue scheme keyed by final group (with a
  # caller override), dropped grey, source dark.
  fin_keys <- dest$g_final[!is_drop]
  pal <- if (!is.null(palette)) palette else if (length(fin_keys) >= 1 && length(fin_keys) <= 8) {
    stats::setNames(RColorBrewer::brewer.pal(max(3, length(fin_keys)), "Set2")[seq_along(fin_keys)], fin_keys)
  } else if (length(fin_keys) >= 1) {
    stats::setNames(scales::hue_pal()(length(fin_keys)), fin_keys)
  } else {
    character(0)
  }
  dest$colour <- ifelse(is_drop, "#9AA5B1",
                        vapply(dest$g_final, function(g)
                          if (g %in% names(pal)) unname(pal[[g]]) else "grey50", character(1)))

  src_label <- if (is.null(src$group) || is.na(src$group) || src$group == "") {
    src$traj_id
  } else {
    paste(src$group, src$traj_id)
  }

  if (is.null(title)) {
    title <- paste0(src_label, " -> ", k, " final", if (k > 1) "s" else "",
                    " (handoff ", handoff, ")")
  }
  title <- gsub("\\s*->\\s*", paste0(" ", intToUtf8(8594), " "), title)

  yr <- if (!is.null(year_range)) range(year_range) else c(src$start, last_year)
  year_breaks <- seq(yr[1], yr[2], by = 2)

  p <- ggplot2::ggplot() +
    # the dying trajectory: a short dark spine ending at the handoff year
    ggplot2::geom_segment(
      data = data.frame(x = src$start, xend = handoff, y = 0),
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$y),
      linewidth = 2.5, colour = "#2E3440", lineend = "round") +
    # one curved link per impacted final, width ~ papers, coloured per final group
    ggplot2::geom_curve(
      data = dest,
      ggplot2::aes(x = handoff, y = 0, xend = last_year, yend = .data$lane,
                   linewidth = .data$n, colour = .data$colour),
      curvature = -0.18, lineend = "round", alpha = 0.85,
      arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")) +
    ggplot2::geom_point(
      data = dest, ggplot2::aes(x = last_year, y = .data$lane, colour = .data$colour),
      size = 2) +
    ggplot2::geom_text(
      data = dest,
      ggplot2::aes(x = last_year, y = .data$lane, label = .data$label, colour = .data$colour),
      hjust = 0, nudge_x = 0.4, size = label_size, fontface = "bold") +
    ggplot2::annotate(
      "text", x = src$start, y = 0, label = paste0(src_label, " (", src$cohort_size, ")"),
      hjust = 0, vjust = 2, size = label_size + 0.3, fontface = "bold", colour = "#2E3440") +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_linewidth(range = c(0.4, 5), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = year_breaks, expand = ggplot2::expansion(mult = c(0.02, 0.22))) +
    ggplot2::scale_y_continuous(limits = c(-0.8, k + 0.8), breaks = NULL) +
    ggplot2::labs(x = "Year", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
        size = if (is.null(axis_text_size)) 14 else axis_text_size),
      plot.title = ggplot2::element_text(size = 18, face = "bold"))

  # extend the time axis to the requested window without dropping any data
  if (!is.null(year_range)) {
    p <- p + ggplot2::geom_blank(data = data.frame(x = yr, y = c(0, 0)),
                                 ggplot2::aes(x = .data$x, y = .data$y))
  }
  p
}
