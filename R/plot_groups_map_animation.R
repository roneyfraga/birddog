# First year each node appears in the cumulative networks: the minimum
# network_until_<Y> whose $documents$name contains the node. Returns a named
# integer vector name -> appearance year.
.groups_map_appearance <- function(groups_cumulative) {
  yrs <- as.integer(sub("network_until_", "", names(groups_cumulative)))
  appear <- integer(0)
  for (i in order(yrs)) {
    nm <- unique(groups_cumulative[[i]]$documents$name)
    nm <- nm[!is.na(nm)]
    new <- nm[!nm %in% names(appear)]
    if (length(new)) {
      add <- rep(yrs[i], length(new))
      names(add) <- new
      appear <- c(appear, add)
    }
  }
  appear
}

#' Animate the stock map: the citation network forming year by year
#'
#' One layout of the final network, with every paper revealed at the year it
#' first enters the cumulative network and held at its final position, so the
#' cloud grows without nodes moving. Returns a `gganim` object; render it to a
#' GIF/MP4 with [gganimate::animate()] and [gganimate::anim_save()]. Reveal and
#' colour reuse the static [plot_groups_map()] engine.
#'
#' Rendering: the transition is one frame per year. Render with `duration` (not
#' `fps`), which works with either renderer: `gganimate::animate(anim, duration = 8)`
#' gives `nframes/duration` fps (with the `magick` renderer that ratio must divide
#' 100, e.g. 32 years / 8 = 4). Save the rendered object, not the `gganim`:
#' `a <- gganimate::animate(anim, duration = 8); gganimate::anim_save("x.gif", a)`.
#' Installing `gifski` removes the fps constraint.
#'
#' @param groups_cumulative A stock object from [sniff_groups_cumulative()].
#' @param labels_text Optional `data.frame` with columns `id` and `text`; `NULL`
#'   (default) uses the bare group id. If no `id` matches any group, a warning is
#'   issued and the group ids are used.
#' @param layout Force-directed layout passed to [ggraph::create_layout()]
#'   (default `"drl"`).
#' @param seed Integer seed for the stochastic layout (default `888L`).
#' @param show_unassigned Reveal nodes with no group (`NA`) as grey (default `TRUE`).
#' @param label Draw persistent group labels at the final centroids (default `TRUE`).
#' @param label_size Font size of the centroid labels (default `3.2`).
#' @param title_size Font size of the per-frame `"Year: ..."` title (default `14`).
#' @param point_size Node point size (default `0.9`).
#' @return A `gganim` object (render with [gganimate::animate()]).
#' @seealso [plot_groups_map()], [plot_groups_map_interactive()]
#' @family visualization
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_text scale_colour_manual labs theme_void theme element_text
#' @importFrom rlang .data
plot_groups_map_animation <- function(groups_cumulative,
                                      labels_text = NULL,
                                      layout = "drl",
                                      seed = 888L,
                                      show_unassigned = TRUE,
                                      label = TRUE,
                                      label_size = 3.2,
                                      title_size = 14,
                                      point_size = 0.9) {
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    stop("plot_groups_map_animation() needs the 'gganimate' package; ",
         "install it with install.packages('gganimate')", call. = FALSE)
  }
  if (!is.null(labels_text) &&
      (!is.data.frame(labels_text) || !all(c("id", "text") %in% names(labels_text)))) {
    stop("'labels_text' must be a data.frame with columns 'id' and 'text'", call. = FALSE)
  }
  ml <- .groups_map_layout(groups_cumulative, NULL, layout, seed, FALSE, 0.99)
  assigned   <- ml$assigned
  unassigned <- ml$unassigned
  pal        <- ml$pal
  .groups_map_check_labels(labels_text, ml$groups)

  appear <- .groups_map_appearance(groups_cumulative)
  last_year <- max(as.integer(sub("network_until_", "", names(groups_cumulative))))
  attach_year <- function(d) {
    y <- appear[as.character(d$name)]
    y[is.na(y)] <- last_year
    d$appear_year <- as.integer(y)
    d
  }
  assigned   <- attach_year(assigned)
  unassigned <- attach_year(unassigned)

  p <- ggplot2::ggplot()
  if (show_unassigned && nrow(unassigned) > 0) {
    p <- p + ggplot2::geom_point(
      data = unassigned,
      ggplot2::aes(x = .data$x, y = .data$y),
      colour = "grey85", size = point_size, alpha = 0.5
    )
  }
  p <- p +
    ggplot2::geom_point(
      data = assigned,
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$group),
      size = point_size, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = pal)

  if (label && nrow(assigned) > 0) {
    cent <- .groups_map_centroids(assigned, labels_text)
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      # Repel the labels apart (white halo for legibility). A fixed seed and
      # point.size = NA make the de-overlap deterministic and independent of the
      # accumulating points, so the labels stay put across animation frames.
      p <- p + ggrepel::geom_text_repel(
        data = cent,
        ggplot2::aes(x = .data$cx, y = .data$cy, label = .data$label_text),
        size = label_size, colour = "black",
        bg.color = "white", bg.r = 0.18, seed = seed,
        point.size = NA, max.overlaps = Inf,
        min.segment.length = grid::unit(Inf, "lines")
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = cent,
        ggplot2::aes(x = .data$cx, y = .data$cy, label = .data$label_text),
        size = label_size, colour = "black"
      )
    }
  }

  p <- p +
    ggplot2::labs(title = "Year: {current_frame}") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
  p + gganimate::transition_manual(.data$appear_year, cumulative = TRUE)
}
