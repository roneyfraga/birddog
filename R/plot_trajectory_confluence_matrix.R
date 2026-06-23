#' Trajectory x final-group paper-overlap incidence for a flow object
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
.coupling_incidence_flow <- function(flow) {
  tr <- flow$trajectories
  dpg <- flow$docs_per_group
  last_year <- flow$last_year
  node_docs <- split(dpg$document_id, dpg$group_id)

  fin <- dpg[dpg$network_until == last_year, c("document_id", "group"), drop = FALSE]
  fin <- fin[!duplicated(fin$document_id), , drop = FALSE]
  final_groups <- mixed_sort(unique(fin$group))
  final_size <- table(fin$group)
  doc_final <- stats::setNames(fin$group, fin$document_id)

  long_list <- list()
  for (i in seq_len(nrow(tr))) {
    docs <- unique(unlist(node_docs[tr$nodes[[i]]], use.names = FALSE))
    n_traj <- length(docs)
    if (n_traj == 0) next
    fg <- doc_final[docs]
    fg <- fg[!is.na(fg)]
    if (length(fg) == 0) next
    tab <- table(fg)
    long_list[[length(long_list) + 1]] <- tibble::tibble(
      traj_id = tr$traj_id[i],
      terminal_group = if (is.na(tr$group[i])) "(extinct)" else tr$group[i],
      group_final = names(tab),
      n_shared = as.integer(tab),
      prop_of_group = as.integer(tab) / as.integer(final_size[names(tab)]),
      prop_of_traj = as.integer(tab) / n_traj
    )
  }
  long <- if (length(long_list)) dplyr::bind_rows(long_list) else tibble::tibble(
    traj_id = character(), terminal_group = character(), group_final = character(),
    n_shared = integer(), prop_of_group = double(), prop_of_traj = double())

  incidence <- matrix(0L, nrow = length(unique(long$traj_id)),
                      ncol = length(final_groups),
                      dimnames = list(unique(long$traj_id), final_groups))
  if (nrow(long) > 0) incidence[cbind(long$traj_id, long$group_final)] <- long$n_shared
  reach <- stats::setNames(rowSums(incidence), rownames(incidence))
  list(long = long, incidence = incidence, reach = reach)
}

#' Seriation orders of both matrix margins by overlap-profile similarity
#'
#' Hierarchical clustering (average linkage) of the intermediates by their
#' destination profile across the finals, and of the finals by their source
#' profile across the intermediates, so alike rows and alike columns sit together
#' (a co-clustered heatmap).
#' @keywords internal
#' @importFrom stats dist hclust
.matrix_cluster_orders <- function(long, inter_ids, final_ids) {
  m <- matrix(0, nrow = length(inter_ids), ncol = length(final_ids),
              dimnames = list(inter_ids, final_ids))
  m[cbind(as.character(long$traj_id), as.character(long$group_final))] <- long$n_shared
  seriate <- function(x) {                 # order the rows of x by their profile
    if (nrow(x) < 3) return(rownames(x))
    prof <- x / pmax(rowSums(x), 1)
    rownames(x)[stats::hclust(stats::dist(prof), method = "average")$order]
  }
  list(inter = seriate(m), final = seriate(t(m)))
}

#' Plot the trajectory confluence matrix (finals vs intermediates)
#'
#' A heatmap of how each intermediate (absorbed) trajectory's papers spread across
#' the final trajectories. By default **rows are the final trajectories** `tr::cNgN`
#' and **columns are the intermediate trajectories** `trN` (swap with
#' `orientation`); the fill is the coupling strength (shared documents) and each
#' intermediate's dominant-final cell is outlined. Which intermediates appear is
#' decided by the same pruning the other flow plots use
#' ([plot_trajectory_lines_2d()], [plot_trajectory_confluence()]), so the column
#' set is consistent across views.
#'
#' @param flow A [sniff_trajectory_braid()] object.
#' @param conf An optional precomputed [sniff_trajectory_confluence()] object for
#'   `flow`; `NULL` (default) computes it.
#' @param fill `"n_shared"` (default, log-scaled), `"prop_of_group"`, or
#'   `"prop_of_traj"`.
#' @param orientation `"finals-cols"` (default): final trajectories `tr::cNgN` on
#'   the rows (y-axis), intermediate trajectories `trN` on the columns (x-axis) --
#'   the "finals (rows) x intermediates (cols)" matrix. `"finals-rows"` is the
#'   transpose (finals on the columns, intermediates on the rows).
#' @param order_by How the intermediate trajectories are ordered along their axis:
#'   `"terminal"` (default, grouped by dominant final then reach), `"reach"` (total
#'   shared documents), `"size"` (trajectory size at handoff), `"alpha"`
#'   (alphabetical), or `"cluster"` (seriation that reorders **both** axes by
#'   hierarchical clustering -- intermediates by their destination profile and
#'   finals by their source profile -- so alike rows and columns form blocks).
#' @param min_n,min_prop,min_total_size,min_duration_years Thresholds selecting
#'   which intermediate trajectories are shown, passed to the same prune as
#'   [plot_trajectory_lines_2d()]: an intermediate is kept only if it transfers
#'   `>= min_n` papers and `>= min_prop` of its cohort, has total size
#'   `>= min_total_size` and lifespan `>= min_duration_years`.
#' @param min_target_n Minimum shared documents a cell must have to be drawn
#'   (default `0`, all cells); raise it to hide the long tail of small overlaps.
#'   Cells with zero shared documents are never drawn. Final trajectories are
#'   always kept on their axis, even when the thresholds leave them with no cell.
#' @param mark_terminal Outline each intermediate's dominant-final cell
#'   (default `TRUE`).
#' @param show_values Print the value inside each cell (default `FALSE`). The text
#'   colour auto-contrasts (dark on light cells, light on dark) for legibility.
#' @param label_size Font size of the in-cell values (default `2.5`).
#' @param axis_text_size Font size of the axis (trajectory id) tick labels; `NULL`
#'   (default) keeps the theme default.
#' @param axis_title_size Font size of the axis titles ("Final Trajectory" /
#'   "Intermediate Trajectory"); `NULL` (default) keeps the theme default.
#' @param legend_position Where to put the colourbar: `"right"` (default),
#'   `"left"`, `"top"`, `"bottom"`, `"none"`, or a `c(x, y)` coordinate inside the
#'   panel (each in `0..1`). `"top"`/`"bottom"` draw the colourbar horizontally.
#' @param legend_text_size Font size of the legend text; `NULL` (default) keeps the
#'   theme default.
#' @param bg Panel/plot background colour (default `"white"`). A darker background
#'   (e.g. `"grey20"`) raises contrast against the bright cells; the grid lines and
#'   the terminal outline auto-adapt to its luminance.
#' @param title Plot title (default `"Trajectory matrix"`); `NULL` removes it.
#'
#' @return A `ggplot` object.
#'
#' @seealso [sniff_trajectory_braid()], [plot_trajectory_lines_2d()],
#'   [plot_trajectory_confluence()]
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' plot_trajectory_confluence_matrix(flow, min_n = 20, show_values = TRUE)
#' plot_trajectory_confluence_matrix(flow, order_by = "cluster", bg = "grey20")
#' }
#'
#' @family visualization
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_viridis_c
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete scale_colour_identity
#' @importFrom ggplot2 guides guide_colourbar labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank element_rect
#' @importFrom grid unit
#' @importFrom rlang .data
plot_trajectory_confluence_matrix <- function(flow, conf = NULL,
                                   fill = c("n_shared", "prop_of_group", "prop_of_traj"),
                                   orientation = c("finals-cols", "finals-rows"),
                                   order_by = c("terminal", "reach", "size", "alpha", "cluster"),
                                   min_n = 5, min_prop = 0.05, min_total_size = 0,
                                   min_duration_years = 0, min_target_n = 0,
                                   mark_terminal = TRUE,
                                   show_values = FALSE, label_size = 2.5,
                                   axis_text_size = NULL, axis_title_size = NULL,
                                   legend_position = "right", legend_text_size = NULL,
                                   bg = "white", title = "Trajectory matrix") {
  fill <- match.arg(fill)
  orientation <- match.arg(orientation)
  order_by <- match.arg(order_by)
  if (!is_flow(flow)) {
    stop("'flow' must be a sniff_trajectory_braid() object", call. = FALSE)
  }
  if (is.null(conf)) conf <- sniff_trajectory_confluence(flow)

  # columns/rows of intermediates = the absorbed trajectories that pass the prune
  pr <- .confluence_prune(conf$rivers, conf$confluences, target = NULL,
                          min_n = min_n, min_prop = min_prop,
                          min_total_size = min_total_size,
                          min_duration_years = min_duration_years)
  inter <- pr$rivers$traj_id[pr$rivers$type == "absorbed"]
  if (length(inter) == 0) {
    stop("no intermediate trajectories pass the thresholds (min_n / min_prop / ",
         "min_total_size / min_duration_years)", call. = FALSE)
  }

  ci <- .coupling_incidence_flow(flow)
  long <- ci$long[ci$long$traj_id %in% inter, , drop = FALSE]
  if (min_target_n > 0) long <- long[long$n_shared >= min_target_n, , drop = FALSE]
  if (nrow(long) == 0) {
    stop("no coupling cells to draw for these thresholds", call. = FALSE)
  }

  # order intermediates per order_by; "cluster" also reorders the finals axis.
  # every final trajectory is kept on its axis (even with no surviving cell), so
  # thresholds never drop a final -- absent finals trail the present ones.
  reach <- tapply(long$n_shared, long$traj_id, sum)
  inter_ids <- names(reach)
  all_finals <- colnames(ci$incidence)
  present_finals <- unique(as.character(long$group_final))
  absent_finals <- mixed_sort(setdiff(all_finals, present_finals))
  tg <- long$terminal_group[match(inter_ids, long$traj_id)]
  size_of <- stats::setNames(pr$rivers$size, pr$rivers$traj_id)[inter_ids]
  if (order_by == "cluster") {
    so <- .matrix_cluster_orders(long, inter_ids, present_finals)
    inter_order <- so$inter
    final_order <- c(so$final, absent_finals)
  } else {
    inter_order <- switch(order_by,
      terminal = inter_ids[order(tg, -reach)],
      reach    = inter_ids[order(-reach)],
      size     = inter_ids[order(-size_of, -reach)],
      alpha    = mixed_sort(inter_ids))
    final_order <- mixed_sort(all_finals)
  }
  long$is_terminal <- long$terminal_group == as.character(long$group_final)
  long$cell_lab <- if (fill == "n_shared") as.character(long$n_shared) else
    paste0(round(100 * long[[fill]]), "%")

  # auto-contrast in-cell text: light text on dark (low) cells, dark on bright
  fv <- if (fill == "n_shared") log10(long$n_shared) else long[[fill]]
  rng <- range(fv, na.rm = TRUE)
  nrm <- if (diff(rng) > 0) (fv - rng[1]) / diff(rng) else rep(0.5, length(fv))
  long$txt_col <- ifelse(nrm > 0.62, "grey10", "grey95")

  # orientation: which variable is on each axis, and which axis is the finals
  if (orientation == "finals-cols") {
    # finals on the rows (y), intermediates on the columns (x): the
    # "finals (rows) x intermediates (cols)" matrix.
    long$xx <- factor(long$traj_id, levels = inter_order)
    long$yy <- factor(long$group_final, levels = rev(final_order))
    x_lab <- "Intermediate Trajectory"; y_lab <- "Final Trajectory"
    x_pref <- ""; y_pref <- "tr::"
  } else {
    # finals on the columns (x), intermediates on the rows (y): the transpose.
    long$xx <- factor(long$group_final, levels = final_order)
    long$yy <- factor(long$traj_id, levels = rev(inter_order))
    x_lab <- "Final Trajectory"; y_lab <- "Intermediate Trajectory"
    x_pref <- "tr::"; y_pref <- ""
  }
  # keep every final on its axis (drop = FALSE there) but drop unused intermediates
  drop_x <- orientation == "finals-cols"   # x = intermediates here; finals on x are kept
  drop_y <- orientation == "finals-rows"

  # background-aware grid and terminal-outline colours
  bg_lum <- sum(grDevices::col2rgb(bg) * c(0.299, 0.587, 0.114)) / 255
  grid_col <- if (bg_lum < 0.5) "grey30" else "grey92"
  term_col <- if (bg_lum < 0.5) "white" else "black"

  p <- ggplot2::ggplot(long, ggplot2::aes(.data$xx, .data$yy, fill = .data[[fill]])) +
    ggplot2::geom_tile(colour = grid_col)
  if (mark_terminal) {
    p <- p + ggplot2::geom_tile(
      data = long[long$is_terminal, , drop = FALSE],
      fill = NA, colour = term_col, linewidth = 0.8)
  }
  if (isTRUE(show_values)) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label = .data$cell_lab, colour = .data$txt_col),
                         size = label_size, show.legend = FALSE) +
      ggplot2::scale_colour_identity()
  }
  # a horizontal colourbar when the legend sits on the top/bottom, vertical else
  horiz <- length(legend_position) == 1L && legend_position %in% c("top", "bottom")
  cbar <- if (horiz) {
    ggplot2::guide_colourbar(title = fill, direction = "horizontal",
      title.position = "top", barwidth = grid::unit(4, "cm"),
      barheight = grid::unit(0.45, "cm"))
  } else {
    ggplot2::guide_colourbar(title = fill,
      barheight = grid::unit(4, "cm"), barwidth = grid::unit(0.45, "cm"))
  }
  p +
    (if (fill == "n_shared") {
      ggplot2::scale_fill_viridis_c(transform = "log10", option = "C")
    } else {
      ggplot2::scale_fill_viridis_c(option = "C")
    }) +
    ggplot2::scale_x_discrete(labels = function(b) paste0(x_pref, b), drop = drop_x) +
    ggplot2::scale_y_discrete(labels = function(b) paste0(y_pref, b), drop = drop_y) +
    ggplot2::guides(fill = cbar) +
    ggplot2::labs(x = x_lab, y = y_lab, title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = legend_text_size),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = axis_text_size),
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = bg, colour = bg),
      plot.background = ggplot2::element_rect(fill = bg, colour = bg))
}
