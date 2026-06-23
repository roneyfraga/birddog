# Shared layout + palette for the stock maps (static and interactive).
# Returns assigned/unassigned layout rows, the sorted group levels, and the palette.
.groups_map_layout <- function(groups_cumulative, network_until, layout, seed,
                               normalize, normalize_q) {
  if (!is.list(groups_cumulative) || length(groups_cumulative) == 0) {
    stop("'groups_cumulative' must be a non-empty sniff_groups_cumulative() list",
         call. = FALSE)
  }
  el <- if (is.null(network_until)) {
    names(groups_cumulative)[length(groups_cumulative)]
  } else {
    paste0("network_until_", network_until)
  }
  if (is.null(el) || !el %in% names(groups_cumulative)) {
    stop("no '", el, "' element; available years: ",
         paste(sub("network_until_", "", names(groups_cumulative)), collapse = ", "),
         call. = FALSE)
  }
  snap <- groups_cumulative[[el]]
  if (!is.list(snap) || is.null(snap$network) || is.null(snap$documents)) {
    stop("element '", el, "' must carry $network and $documents", call. = FALSE)
  }
  g <- snap$network
  if (!igraph::is_igraph(g)) {
    stop("$network must be an igraph/tbl_graph", call. = FALSE)
  }
  if (igraph::vcount(g) == 0) {
    stop("the network has no nodes", call. = FALSE)
  }
  docs <- snap$documents
  if (!all(c("name", "group") %in% names(docs))) {
    stop("$documents must have columns 'name' and 'group'", call. = FALSE)
  }

  grp <- docs$group[match(igraph::vertex_attr(g, "name"), docs$name)]
  g <- igraph::set_vertex_attr(g, "group", value = as.character(grp))

  set.seed(seed)
  lay <- ggraph::create_layout(g, layout = layout)
  lay$group <- as.character(lay$group)
  if (normalize) {
    cx <- stats::median(lay$x)
    cy <- stats::median(lay$y)
    rad <- sqrt((lay$x - cx)^2 + (lay$y - cy)^2)
    s <- stats::quantile(rad, normalize_q, names = FALSE)
    lay$x <- (lay$x - cx) / s
    lay$y <- (lay$y - cy) / s
  }
  is_na <- is.na(lay$group)
  assigned   <- lay[!is_na, , drop = FALSE]
  unassigned <- lay[is_na, , drop = FALSE]

  groups <- mixed_sort(unique(assigned$group))
  nlev <- length(groups)
  pal <- if (nlev <= 8) {
    RColorBrewer::brewer.pal(max(3, nlev), "Set2")[seq_len(nlev)]
  } else {
    scales::hue_pal()(nlev)
  }
  names(pal) <- groups
  assigned$group <- factor(assigned$group, levels = groups)

  list(assigned = assigned, unassigned = unassigned, groups = groups, pal = pal)
}

# Per-group centroid (median x/y) plus the resolved label text (CSV or bare id).
.groups_map_centroids <- function(assigned, labels_text) {
  cent <- do.call(rbind, lapply(split(assigned, assigned$group), function(d) {
    data.frame(
      group = as.character(d$group[1]),
      cx = stats::median(d$x), cy = stats::median(d$y),
      stringsAsFactors = FALSE
    )
  }))
  cent$label_text <- if (is.null(labels_text)) {
    cent$group
  } else {
    mapped <- labels_text$text[match(cent$group, labels_text$id)]
    ifelse(is.na(mapped), cent$group, mapped)
  }
  cent
}

# Warn (and fall back to the bare group ids) when a supplied labels_text shares
# no id with the actual group ids -- a wrong key, not a real description table.
.groups_map_check_labels <- function(labels_text, groups) {
  if (!is.null(labels_text) && !any(groups %in% labels_text$id)) {
    warning("none of labels_text$id match the group ids; using the group ids as labels",
            call. = FALSE)
  }
  invisible(NULL)
}

#' Plot the stock map: the complete citation network coloured by final group
#'
#' The birddog stock map: every paper in the cumulative network is a point, laid
#' out by a force-directed layout of the citation network and coloured by its
#' final group, with a text label at each group's centroid. It is the stock
#' snapshot -- all final groups, the whole network at once -- positioned by
#' citation structure rather than text embeddings.
#'
#' @param groups_cumulative A stock object from [sniff_groups_cumulative()]; each
#'   `network_until_<year>` element must carry `$network` (an igraph/tbl_graph
#'   whose vertices have a `name` attribute) and `$documents` (with columns
#'   `name`, `group`).
#' @param labels_text Optional `data.frame` with columns `id` (matching the group
#'   ids, e.g. `"c1g1"`) and `text` (the human description). `NULL` (default)
#'   uses the bare group id as the label. If no `id` matches any group, a warning
#'   is issued and the group ids are used.
#' @param network_until Snapshot year to map. `NULL` (default) uses the last
#'   element (the final groups).
#' @param layout Force-directed layout passed to [ggraph::create_layout()]
#'   (default `"drl"`, the OpenOrd layout, which separates communities into
#'   legible territories on dense citation networks; `"fr"` and `"kk"` tend to
#'   collapse a large network into a single hairball).
#' @param seed Integer seed for the stochastic layout (default `888L`).
#' @param point_size Node point size (default `0.9`).
#' @param point_alpha Node point alpha (default `0.7`).
#' @param show_unassigned Draw nodes with no group (`NA`) as grey background
#'   (default `TRUE`); `FALSE` drops them.
#' @param label Draw the group text labels at centroids (default `TRUE`).
#' @param label_size Font size of the centroid labels (default `3.2`).
#' @param title Plot title: the default `"Group map"`, any string, or `NULL` for
#'   no title.
#' @param title_size Title font size (default `14`).
#' @param normalize Rescale the layout for cross-year comparison (default
#'   `FALSE`). When `TRUE`, coordinates are centred on the median and divided by
#'   the robust radius (the `normalize_q` quantile of the distance to the centre),
#'   then framed with a fixed unit window, so the dense core fills the plot
#'   identically across snapshots. This equalises scale only, not orientation:
#'   the force-directed layout assigns each graph an arbitrary rotation, so node
#'   positions are not correspondable across years.
#' @param normalize_q Quantile of the radial distance kept inside the normalised
#'   window (default `0.99`); only used when `normalize = TRUE`.
#' @return A `ggplot` object.
#' @seealso [sniff_groups_cumulative()], [plot_trajectory_dag()]
#' @family visualization
#' @export
#' @importFrom ggraph create_layout
#' @importFrom ggplot2 ggplot aes geom_point scale_colour_manual labs theme_void geom_text coord_fixed theme element_text
#' @importFrom igraph is_igraph vcount vertex_attr set_vertex_attr
#' @importFrom rlang .data
plot_groups_map <- function(groups_cumulative,
                            labels_text = NULL,
                            network_until = NULL,
                            layout = "drl",
                            seed = 888L,
                            point_size = 0.9,
                            point_alpha = 0.7,
                            show_unassigned = TRUE,
                            label = TRUE,
                            label_size = 3.2,
                            title = "Group map",
                            title_size = 14,
                            normalize = FALSE,
                            normalize_q = 0.99) {
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

  p <- ggplot2::ggplot()
  if (show_unassigned && nrow(unassigned) > 0) {
    p <- p + ggplot2::geom_point(
      data = unassigned,
      ggplot2::aes(x = .data$x, y = .data$y),
      colour = "grey85", size = point_size, alpha = point_alpha * 0.6
    )
  }
  p <- p +
    ggplot2::geom_point(
      data = assigned,
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$group),
      size = point_size, alpha = point_alpha, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = pal)

  if (label && nrow(assigned) > 0) {
    cent <- .groups_map_centroids(assigned, labels_text)
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        data = cent,
        ggplot2::aes(x = .data$cx, y = .data$cy, label = .data$label_text),
        bg.color = "white", bg.r = 0.18, size = label_size,
        point.size = NA, max.overlaps = Inf,
        min.segment.length = grid::unit(Inf, "lines")
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = cent,
        ggplot2::aes(x = .data$cx, y = .data$cy, label = .data$label_text),
        size = label_size
      )
    }
  }

  if (normalize) {
    p <- p + ggplot2::coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1), expand = FALSE)
  }

  p <- p + ggplot2::theme_void(base_size = 12)
  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
  }
  p
}
