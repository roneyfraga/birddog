#' Plot where an incomplete trajectory's papers go (variable-width lines)
#'
#' Renders the destinations of a stagnant trajectory in the same visual idiom as
#' [plot_group_trajectories_lines_2d()]: a Sugiyama time layout (year on the
#' x-axis) with one variable-width line per destination, so the reader follows
#' the cohort left to right as a continuation of the 2d/3d trajectory plots.
#' Each line tracks, year by year, the group that holds most of that
#' destination's papers; line width grows with the number of papers carried.
#'
#' @param destination Output of [sniff_trajectory_destination()] (must contain a
#'   `flow` tibble; `color_by = "trajectory"` additionally needs the
#'   `dest_traj` column produced when `all_detected` was supplied).
#' @param color_by Colour the lines by destination `"group"` (default) or by
#'   destination `"trajectory"`.
#' @param min_prop Destinations capturing less than `min_prop` of the cohort are
#'   merged into a single grey `(other)` line (default: 0, keep all).
#' @param title Plot title. Defaults to a summary of the terminal node and the
#'   dominant continuation.
#' @param width_range Range for line widths, scaled by papers carried
#'   (default: `c(0.8, 7)`).
#' @param show_only_highlighted Hide the grey background skeleton, drawing only
#'   the destination lines (default: FALSE).
#' @param label_size Text size for the end labels (default: 4).
#' @param lowlight_color,lowlight_alpha,lowlight_width Style of the background
#'   skeleton edges.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' detected <- detect_main_trajectories(groups_cumulative_trajectories, "c1g10")
#' dest <- sniff_trajectory_destination(
#'   detected, "tr3", groups_cumulative_trajectories$docs_per_group,
#'   all_detected = groups_detected_trajectories, group = "c1g10"
#' )
#' plot_trajectory_destination(dest, color_by = "trajectory", min_prop = 0.03)
#' }
#'
#' @seealso [sniff_trajectory_destination()], [plot_trajectory_handoff()],
#'   [plot_group_trajectories_lines_2d()]
#'
#' @export
#' @importFrom dplyr filter mutate arrange group_by ungroup slice_head slice_max
#' @importFrom dplyr left_join transmute summarise desc bind_rows lead select
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_label labs
#' @importFrom ggplot2 scale_colour_manual scale_linewidth scale_size
#' @importFrom ggplot2 scale_x_continuous theme_minimal theme element_blank
#' @importFrom ggplot2 element_text expansion
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales hue_pal
#' @importFrom stats setNames
#' @importFrom tibble tibble
plot_trajectory_destination <- function(destination,
                                        color_by = c("group", "trajectory"),
                                        min_prop = 0,
                                        title = NULL,
                                        width_range = c(0.8, 7),
                                        show_only_highlighted = FALSE,
                                        label_size = 4,
                                        lowlight_color = "#9AA5B1",
                                        lowlight_alpha = 0.25,
                                        lowlight_width = 0.6) {
  color_by <- match.arg(color_by)
  if (!is.list(destination) || is.null(destination$flow)) {
    stop("'destination' must contain a 'flow' component, as returned by sniff_trajectory_destination()", call. = FALSE)
  }

  if (color_by == "trajectory") {
    if (!"dest_traj" %in% names(destination$flow)) {
      stop("color_by = 'trajectory' requires sniff_trajectory_destination(..., all_detected = ) so that 'flow' has a 'dest_traj' column", call. = FALSE)
    }
    key_col <- "dest_traj"
    id_col <- "traj_key"
    share_tbl <- destination$destination_traj
  } else {
    key_col <- "final_group"
    id_col <- "g_final"
    share_tbl <- destination$destination
  }

  flow <- destination$flow
  if (nrow(flow) == 0) {
    stop("'flow' is empty: the trajectory has no forward transitions to plot", call. = FALSE)
  }

  # Modal year-group path per destination (this is the testable core)
  paths <- .destination_paths(flow, key_col, share_tbl, id_col, min_prop)

  # Node positions from the shared Sugiyama time layout
  edges <- flow |>
    dplyr::group_by(from = .data$from_id, to = .data$to_id) |>
    dplyr::summarise(w = sum(.data$n), .groups = "drop")
  nodes <- tibble::tibble(name = unique(c(edges$from, edges$to)))
  g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
  layinfo <- mk_layout_and_year_scale(g)
  pos <- tibble::tibble(
    name = layinfo$lay$name, x = layinfo$lay$x, y = layinfo$lay$y
  )

  paths <- paths |>
    dplyr::left_join(pos, by = c("node" = "name")) |>
    dplyr::filter(!is.na(.data$x)) |>
    dplyr::arrange(.data$dest, .data$x)

  # Per-destination segments (variable width along each line)
  segs <- paths |>
    dplyr::group_by(.data$dest) |>
    dplyr::mutate(
      x2 = dplyr::lead(.data$x), y2 = dplyr::lead(.data$y),
      papers2 = dplyr::lead(.data$papers)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$x2))

  # Colours: real destinations get a palette, specials stay grey
  specials <- c("(other)", "(dropped)", "(none)")
  levs <- sort(setdiff(unique(paths$dest), specials))
  nlev <- length(levs)
  pal <- if (nlev == 0) {
    character(0)
  } else if (nlev <= 8) {
    stats::setNames(RColorBrewer::brewer.pal(max(3, nlev), "Set2")[seq_len(nlev)], levs)
  } else {
    stats::setNames(scales::hue_pal()(nlev), levs)
  }
  pal["(other)"] <- "#B0B0B0"
  pal["(dropped)"] <- "#7A7A7A"
  pal["(none)"] <- "#9AA5B1"

  if (is.null(title)) {
    cont <- if (color_by == "trajectory" && !is.null(destination$continuation_traj)) {
      destination$continuation_traj
    } else {
      destination$continuation
    }
    si <- destination$source_info
    src <- if (!is.null(si) && !is.null(si$group)) {
      paste(si$group, si$traj_id)
    } else {
      destination$terminal_node
    }
    title <- paste0(src, " -> ", cont)
  }
  title <- gsub("\\s*->\\s*", paste0(" ", intToUtf8(8594), " "), title)

  # Background skeleton
  skel <- edges |>
    dplyr::left_join(pos, by = c("from" = "name")) |>
    dplyr::rename(x1 = "x", y1 = "y") |>
    dplyr::left_join(pos, by = c("to" = "name")) |>
    dplyr::rename(x2 = "x", y2 = "y")

  p <- ggplot2::ggplot()
  if (!show_only_highlighted) {
    p <- p + ggplot2::geom_segment(
      data = skel,
      ggplot2::aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2),
      colour = lowlight_color, alpha = lowlight_alpha, linewidth = lowlight_width,
      lineend = "round"
    )
  }

  p <- p +
    ggplot2::geom_segment(
      data = segs,
      ggplot2::aes(
        x = .data$x, y = .data$y, xend = .data$x2, yend = .data$y2,
        colour = .data$dest, linewidth = .data$papers
      ),
      lineend = "round", alpha = 0.9
    ) +
    ggplot2::geom_point(
      data = paths,
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$dest, size = .data$papers),
      alpha = 0.9
    )

  # End labels for the real destinations
  labs_end <- paths |>
    dplyr::filter(!.data$dest %in% specials) |>
    dplyr::group_by(.data$dest) |>
    dplyr::slice_max(.data$x, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  if (nrow(labs_end) > 0) {
    p <- p + ggplot2::geom_label(
      data = labs_end,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$dest, colour = .data$dest),
      size = label_size, fontface = "bold", fill = "white",
      hjust = 0, nudge_x = 0.15, show.legend = FALSE
    )
  }

  p +
    ggplot2::scale_colour_manual(values = pal, guide = "none") +
    ggplot2::scale_linewidth(range = width_range, guide = "none") +
    ggplot2::scale_size(range = c(1.5, max(width_range)), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = layinfo$all_breaks, labels = layinfo$all_labels,
      expand = ggplot2::expansion(mult = c(0.02, 0.16))
    ) +
    ggplot2::labs(title = title, x = "Publication year", y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
      plot.title = ggplot2::element_text(size = 20, face = "bold")
    )
}

#' Modal year-group path of each destination
#'
#' From a `flow` tibble (see [sniff_trajectory_destination()]) derives, for each
#' destination and year, the group that holds most of that destination's papers
#' and the number of papers present that year. Sub-threshold destinations are
#' merged into `"(other)"`.
#'
#' @param flow Flow tibble with `from_id`, `to_id`, `from_year`, `to_year`, the
#'   colour key column, and `n`.
#' @param key_col Colour key column in `flow` (`"final_group"` or `"dest_traj"`).
#' @param share_tbl Destination share tibble with an id column and `prop`.
#' @param id_col Id column in `share_tbl` (`"g_final"` or `"traj_key"`).
#' @param min_prop Minimum cohort share to keep a destination in its own band.
#' @return Tibble `dest`, `year`, `node`, `papers`.
#' @keywords internal
.destination_paths <- function(flow, key_col = "final_group", share_tbl = NULL,
                               id_col = "g_final", min_prop = 0) {
  fl <- flow
  if (!is.null(min_prop) && min_prop > 0 && !is.null(share_tbl)) {
    keep <- share_tbl[[id_col]][share_tbl$prop >= min_prop]
    fl[[key_col]] <- ifelse(fl[[key_col]] %in% keep, fl[[key_col]], "(other)")
  }

  last_year <- max(fl$to_year)
  from_side <- fl |>
    dplyr::transmute(
      dest = .data[[key_col]], year = .data$from_year,
      node = .data$from_id, n = .data$n
    )
  to_side <- fl |>
    dplyr::filter(.data$to_year == last_year) |>
    dplyr::transmute(
      dest = .data[[key_col]], year = .data$to_year,
      node = .data$to_id, n = .data$n
    )

  node_counts <- dplyr::bind_rows(from_side, to_side) |>
    dplyr::group_by(.data$dest, .data$year, .data$node) |>
    dplyr::summarise(k = sum(.data$n), .groups = "drop")

  totals <- node_counts |>
    dplyr::group_by(.data$dest, .data$year) |>
    dplyr::summarise(papers = sum(.data$k), .groups = "drop")

  node_counts |>
    dplyr::group_by(.data$dest, .data$year) |>
    dplyr::arrange(dplyr::desc(.data$k), .data$node, .by_group = TRUE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(.data$dest, .data$year, .data$node) |>
    dplyr::left_join(totals, by = c("dest", "year")) |>
    dplyr::arrange(.data$dest, .data$year)
}
