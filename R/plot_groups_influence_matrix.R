#' Plot the directed group-influence matrix (heatmap)
#'
#' *Experimental.* The directed counterpart of [plot_trajectory_confluence_matrix()]:
#' a heatmap of the cross-citation channels from a [sniff_groups_influence()]
#' object. **Rows are the citing group** ("is influenced by") and **columns the
#' cited group** ("the influencer"), so a cell and its mirror across the diagonal
#' are the two directions of one pair. Confluence is symmetric; influence is not,
#' and that broken symmetry is the bidirectional relationship.
#'
#' @param influence A [sniff_groups_influence()] object.
#' @param fill Which value to colour by: `"surprise"` (default, the flow against
#'   the size null, on a diverging scale centred at `1`), `"raw"` (citation
#'   counts), `"debt"`, `"audience"` or `"salton"`.
#' @param diagonal How to treat the intra-group cohesion cells: `"mute"`
#'   (default, drawn grey, off the colour scale, so the between-group channels
#'   keep the contrast), `"show"` (on the scale like any cell) or `"drop"` (not
#'   drawn).
#' @param show_values Print the value inside each cell (default `TRUE`).
#' @param label_size Font size of the in-cell values (default `4`).
#' @param order_by Group order on both axes: `"group"` (default, `mixed_sort()`)
#'   or `"balance"` (sources first, sinks last).
#' @param axis_text_size,axis_title_size Font sizes of the tick labels and axis
#'   titles; `NULL` (default) keeps the theme defaults.
#' @param x_angle Rotation in degrees of the column (cited-group) labels on the
#'   top axis. Default `0` (horizontal); `90` stacks the `cNgN` ids vertically
#'   so they stay legible on a small figure with many groups.
#' @param legend_position Legend placement: `"right"` (default), `"left"`,
#'   `"top"`, `"bottom"` or `"none"`.
#' @param bg Panel/plot background colour (default `"white"`).
#' @param title Plot title; `NULL` (default) removes it.
#'
#' @return A `ggplot` object.
#'
#' @seealso [sniff_groups_influence()], [plot_groups_influence_network()],
#'   [plot_trajectory_confluence_matrix()]
#'
#' @examples
#' \dontrun{
#' infl <- sniff_groups_influence(groups)
#' plot_groups_influence_matrix(infl)
#' plot_groups_influence_matrix(infl, fill = "debt", diagonal = "drop")
#' }
#'
#' @family visualization
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2
#' @importFrom ggplot2 scale_fill_gradient scale_x_discrete scale_y_discrete
#' @importFrom ggplot2 coord_equal labs theme_minimal theme element_text
#' @importFrom ggplot2 element_blank element_rect
#' @importFrom tibble tibble
#' @importFrom rlang .data
plot_groups_influence_matrix <- function(influence,
                                         fill = c("surprise", "raw", "debt",
                                                  "audience", "salton"),
                                         diagonal = c("mute", "show", "drop"),
                                         show_values = TRUE, label_size = 4,
                                         order_by = c("group", "balance"),
                                         axis_text_size = NULL, axis_title_size = NULL,
                                         x_angle = 0,
                                         legend_position = "right",
                                         bg = "white", title = NULL) {
  fill <- match.arg(fill)
  diagonal <- match.arg(diagonal)
  order_by <- match.arg(order_by)
  if (!is_influence(influence)) {
    stop("'influence' must be a sniff_groups_influence() object.", call. = FALSE)
  }

  C <- influence$matrix
  glev <- rownames(C)
  o <- rowSums(C); iota <- colSums(C); m <- sum(C)

  # value grid over every cell (so zeros show in the right colour)
  val <- switch(fill,
    raw      = C * 1.0,
    debt     = C / o,                                   # divide row i by o_i
    audience = sweep(C, 2, iota, "/"),                  # divide col j by iota_j
    salton   = C / outer(sqrt(o), sqrt(iota)),
    surprise = C / (outer(o, iota) / m))
  val[!is.finite(val)] <- NA_real_
  # self = FALSE carries no intra-group data (the diagonal was dropped), so the
  # diagonal is blank rather than a meaningless zero
  if (!isTRUE(influence$params$self)) diag(val) <- NA_real_

  lab <- if (fill == "raw") formatC(C, format = "d") else formatC(val, format = "f", digits = 2)
  lab[is.na(val)] <- ""

  ord <- if (order_by == "balance") {
    influence$groups$group[order(-influence$groups$balance, influence$groups$group)]
  } else {
    glev
  }
  ord <- ord[ord %in% glev]

  # i = row = citing (recipient), j = column = cited (influencer); cell = val[i, j]
  ij <- expand.grid(i = seq_along(glev), j = seq_along(glev))
  df <- tibble::tibble(
    influencer = factor(glev[ij$j], levels = ord),       # cited -> x axis
    recipient  = factor(glev[ij$i], levels = rev(ord)),  # citing -> y axis
    value = val[cbind(ij$i, ij$j)],
    label = lab[cbind(ij$i, ij$j)],
    is_diag = ij$i == ij$j
  )

  off  <- df[!df$is_diag, , drop = FALSE]
  diag <- df[df$is_diag, , drop = FALSE]

  scale_fill <- if (fill == "surprise") {
    # diverging: surprise has a neutral point at 1 (over- vs under-represented)
    ggplot2::scale_fill_gradient2(midpoint = 1, low = "#2c7bb6", mid = "grey96",
                                  high = "#d7191c", name = "surprise", na.value = "grey88")
  } else {
    # sequential (no neutral point): same warm family as surprise, light low end
    # so the black labels stay legible
    ggplot2::scale_fill_gradient(low = "grey96", high = "#d7191c",
                                 name = fill, na.value = "grey88")
  }

  fill_df <- if (diagonal == "show") df else off

  p <- ggplot2::ggplot()
  if (diagonal == "mute") {
    p <- p + ggplot2::geom_tile(data = diag,
      ggplot2::aes(.data$influencer, .data$recipient),
      fill = "grey88", colour = "white", linewidth = 1.2)
  }
  p <- p + ggplot2::geom_tile(data = fill_df,
      ggplot2::aes(.data$influencer, .data$recipient, fill = .data$value),
      colour = "white", linewidth = 1.2) +
    scale_fill

  if (isTRUE(show_values)) {
    text_df <- if (diagonal == "drop") off else df
    text_df$txt_col <- ifelse(text_df$is_diag & diagonal == "mute", "grey40", "grey10")
    p <- p + ggplot2::geom_text(data = text_df,
      ggplot2::aes(.data$influencer, .data$recipient, label = .data$label,
                   colour = .data$txt_col),
      fontface = "bold", size = label_size, show.legend = FALSE) +
      ggplot2::scale_colour_identity()
  }

  p <- p +
    ggplot2::scale_x_discrete(position = "top", drop = FALSE) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "cited group  (the influencer)",
                  y = "citing group  (is influenced)", title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = legend_position,
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = bg, colour = bg),
      plot.background = ggplot2::element_rect(fill = bg, colour = bg),
      axis.text = ggplot2::element_text(face = "bold", size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size, colour = "grey35"))

  # rotate the top (cited-group) labels when asked: x_angle = 90 stacks the
  # cNgN ids vertically so they stay legible on a small, many-group figure.
  # near-vertical labels centre on the tick; shallower angles (e.g. 45) anchor
  # low so the diagonal text clears the axis title above it.
  if (x_angle != 0) {
    x_vjust <- if (abs(x_angle) >= 80) 0.5 else 0
    p <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(
      angle = x_angle, hjust = 0, vjust = x_vjust))
  }
  p
}
