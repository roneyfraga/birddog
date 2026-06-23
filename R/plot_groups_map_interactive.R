# Build one HTML tooltip string per assigned node from the layout-frame columns.
# Fields fall back gracefully when absent (e.g. WoS data, or no title -> name).
.groups_map_tooltip <- function(assigned, labels_text) {
  col_or <- function(nm, fallback) {
    if (nm %in% names(assigned)) as.character(assigned[[nm]]) else fallback
  }
  title   <- col_or("TI", col_or("title", assigned$name))
  year    <- col_or("PY", col_or("publication_year", ""))
  authors <- col_or("AU", "")
  keyws   <- col_or("DE", "")
  trunc_semi <- function(s, k) {
    vapply(strsplit(s, ";"), function(parts) {
      parts <- trimws(parts[nzchar(trimws(parts))])
      if (length(parts) > k) paste0(paste(parts[seq_len(k)], collapse = ", "), ", ...")
      else paste(parts, collapse = ", ")
    }, character(1))
  }
  grp <- as.character(assigned$group)
  if (!is.null(labels_text)) {
    lt <- labels_text$text[match(grp, labels_text$id)]
    grp <- ifelse(is.na(lt), grp, paste0(grp, " \u2014 ", lt))
  }
  paste0("<b>", title, "</b><br>",
         year, " \u00b7 ", grp, "<br>",
         trunc_semi(authors, 6), "<br>",
         trunc_semi(keyws, 8))
}

#' Interactive stock map: the citation network as a plotly HTML widget
#'
#' The interactive twin of [plot_groups_map()]. Every paper is a `scattergl`
#' point laid out by a force-directed layout of the citation network and coloured
#' by its final group; hovering a node shows its title, year, authors, keywords,
#' and final group. Returns a `plotly` htmlwidget -- it renders in the RStudio
#' Viewer, embeds in Quarto/Shiny, or saves to a standalone file with
#' `htmlwidgets::saveWidget()`.
#'
#' @param groups_cumulative A stock object from [sniff_groups_cumulative()].
#' @param labels_text Optional `data.frame` with columns `id` and `text`; `NULL`
#'   (default) uses the bare group id. If no `id` matches any group, a warning is
#'   issued and the group ids are used.
#' @param network_until Snapshot year. `NULL` (default) uses the last element.
#' @param layout Force-directed layout passed to [ggraph::create_layout()]
#'   (default `"drl"`).
#' @param seed Integer seed for the stochastic layout (default `888L`).
#' @param show_unassigned Draw nodes with no group (`NA`) as grey background
#'   (default `TRUE`).
#' @param label Draw group labels at centroids as annotations (default `TRUE`).
#' @param label_size Font size (px) of the centroid label annotations (default `12`).
#' @param title Plot title: the default `"Group map"`, any string, or `NULL` for
#'   no title.
#' @param title_size Title font size in px (default `16`).
#' @param normalize Rescale to a fixed unit window for cross-year comparison
#'   (default `FALSE`); see [plot_groups_map()].
#' @param normalize_q Quantile of the radial distance kept inside the normalised
#'   window (default `0.99`).
#' @param marker_size Node marker size (default `5`).
#' @return A `plotly` htmlwidget.
#' @seealso [plot_groups_map()], [sniff_groups_cumulative()]
#' @family visualization
#' @export
#' @importFrom plotly plot_ly add_trace layout
plot_groups_map_interactive <- function(groups_cumulative,
                                        labels_text = NULL,
                                        network_until = NULL,
                                        layout = "drl",
                                        seed = 888L,
                                        show_unassigned = TRUE,
                                        label = TRUE,
                                        label_size = 12,
                                        title = "Group map",
                                        title_size = 16,
                                        normalize = FALSE,
                                        normalize_q = 0.99,
                                        marker_size = 5) {
  if (!is.null(labels_text) &&
      (!is.data.frame(labels_text) || !all(c("id", "text") %in% names(labels_text)))) {
    stop("'labels_text' must be a data.frame with columns 'id' and 'text'", call. = FALSE)
  }
  ml <- .groups_map_layout(groups_cumulative, network_until, layout, seed,
                           normalize, normalize_q)
  assigned   <- ml$assigned
  unassigned <- ml$unassigned
  pal        <- ml$pal
  .groups_map_check_labels(labels_text, ml$groups)

  assigned$tooltip <- .groups_map_tooltip(assigned, labels_text)
  # Colour each node explicitly from our palette. plotly's discrete `color=`/
  # `colors=` mapping ignores the supplied palette once a group count exceeds 8
  # (it re-derives its own interpolated Set2 and warns), which would desync the
  # colours from the static plot_groups_map(). Setting marker$color per node
  # keeps one trace, our exact hue_pal/Set2 colours, and no warning. Vectors are
  # passed directly (not the ~formula interface) to avoid the R CMD check
  # global-variable NOTE and to keep the layout frame's list-columns out of plotly.
  node_colour <- unname(pal[as.character(assigned$group)])

  p <- plotly::plot_ly()
  if (show_unassigned && nrow(unassigned) > 0) {
    p <- plotly::add_trace(
      p, x = unassigned$x, y = unassigned$y, type = "scattergl", mode = "markers",
      marker = list(size = marker_size * 0.8, color = "grey85"),
      hoverinfo = "skip", showlegend = FALSE
    )
  }
  p <- plotly::add_trace(
    p, x = assigned$x, y = assigned$y, type = "scattergl", mode = "markers",
    marker = list(size = marker_size, color = node_colour),
    text = assigned$tooltip, hoverinfo = "text", showlegend = FALSE
  )

  ax  <- list(visible = FALSE, showgrid = FALSE, zeroline = FALSE)
  xax <- ax
  yax <- ax
  if (normalize) {
    xax$range <- c(-1, 1)
    yax$range <- c(-1, 1)
    yax$scaleanchor <- "x"
    yax$scaleratio  <- 1
  }
  ttl <- if (is.null(title)) list(text = "") else list(text = title, font = list(size = title_size))
  p <- plotly::layout(p, xaxis = xax, yaxis = yax, title = ttl)

  if (label && nrow(assigned) > 0) {
    cent <- .groups_map_centroids(assigned, labels_text)
    anns <- lapply(seq_len(nrow(cent)), function(i) {
      list(x = cent$cx[i], y = cent$cy[i], text = cent$label_text[i],
           showarrow = FALSE, font = list(size = label_size),
           bgcolor = "rgba(255,255,255,0.6)")
    })
    p <- plotly::layout(p, annotations = anns)
  }
  p
}
