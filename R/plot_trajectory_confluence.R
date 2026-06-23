# --- internal layout/geometry helpers for plot_trajectory_confluence ------

# Threshold + subtree pruning. An edge survives when it transfers >= min_n papers
# and >= min_prop of the child's cohort. A trajectory is kept only if it is a
# root (a central, or the chosen target) or it is reached from a root through
# surviving edges, so a tributary below threshold prunes its whole feeder
# subtree (everything that flows into it). Returns the kept rivers/confluences
# plus the dropped edges (child, n)
# for the caption.
.confluence_prune <- function(rivers, confluences, target = NULL,
                              min_n = 5, min_prop = 0, min_total_size = 0,
                              min_duration_years = 0, max_depth = Inf) {
  conf <- confluences
  prop <- ifelse(conf$cohort_size > 0, conf$n / conf$cohort_size, 0)
  size_of <- stats::setNames(rivers$size, rivers$traj_id)
  dur_of <- stats::setNames(vapply(rivers$size_curve, function(s)
    if (nrow(s)) max(s$year) - min(s$year) + 1 else 0, numeric(1)), rivers$traj_id)
  child_size <- as.numeric(size_of[conf$child]); child_size[is.na(child_size)] <- 0
  child_dur  <- as.numeric(dur_of[conf$child]);  child_dur[is.na(child_dur)]   <- 0
  pass <- conf$n >= min_n & prop >= min_prop &
    child_size >= min_total_size & child_dur >= min_duration_years

  if (is.null(target)) {
    roots <- rivers$traj_id[rivers$type == "central"]
  } else {
    if (!all(target %in% rivers$traj_id)) {
      stop("'target' is not a trajectory in this confluence object", call. = FALSE)
    }
    roots <- target
  }

  kids_of <- split(which(pass), conf$parent[pass])   # parent -> passing edge rows
  reachable <- character(0)
  used_edge <- logical(nrow(conf))
  frontier <- roots
  level <- 0L
  while (length(frontier)) {
    reachable <- union(reachable, frontier)
    if (level >= max_depth) break          # stop expanding past max_depth levels
    nxt <- character(0)
    for (p in frontier) {
      idx <- kids_of[[p]]
      if (!is.null(idx)) {
        used_edge[idx] <- TRUE
        nxt <- c(nxt, conf$child[idx])
      }
    }
    frontier <- setdiff(unique(nxt), reachable)
    level <- level + 1L
  }

  rivers_kept <- rivers[rivers$traj_id %in% reachable, , drop = FALSE]
  confluences_kept <- conf[used_edge, , drop = FALSE]
  # caption counts threshold failures whose parent is drawn (NOT depth-excluded,
  # which merely fold into their direct parent and are intentionally not shown).
  in_scope <- conf$parent %in% reachable
  dropped <- tibble::tibble(child = conf$child[in_scope & !pass],
                            n = conf$n[in_scope & !pass])
  list(rivers = rivers_kept, confluences = confluences_kept, dropped = dropped)
}

# Destination filter. Drop every intermediate (absorbed) tributary that delivers
# 0 documents to the final it flows into, then drop whatever is left unreachable
# from the roots (its whole upstream subtree, since the forest is a tree per
# central). "Contribution" is the tributary's terminal-cohort documents that END
# in the relevant final's community (conf$destinations$n), NOT the trunk transfer
# (confluences$n): a stream can hand papers to its absorber yet have none survive
# to the final year, and such a stream must not appear. The relevant final is the
# single target's group in a single-central view, else each tributary's own
# dominant central. No-op for older `conf` objects without a `destinations` field.
.confluence_drop_zero_dest <- function(rivers, confluences, conf, target, single) {
  dest <- conf$destinations
  if (is.null(dest) || !is.data.frame(dest) || nrow(dest) == 0) {
    return(list(rivers = rivers, confluences = confluences))
  }
  abs_ids <- rivers$traj_id[rivers$type != "central"]
  if (!length(abs_ids)) return(list(rivers = rivers, confluences = confluences))
  central_of <- stats::setNames(rivers$central, rivers$traj_id)
  single_grp <- if (single) sub("^tr::", "", target) else NA_character_
  contrib <- vapply(abs_ids, function(id) {
    g <- if (single) single_grp else central_of[[id]]
    if (is.null(g) || is.na(g)) return(NA_real_)
    sum(dest$n[dest$traj_id == id & dest$g_final == g])
  }, numeric(1))
  zero <- abs_ids[!is.na(contrib) & contrib == 0]
  if (!length(zero)) return(list(rivers = rivers, confluences = confluences))

  # cut the zero tributaries' inbound edges, then keep only what is still reachable
  # from the roots through the surviving edges (drops orphaned upstream subtrees).
  roots <- if (is.null(target)) rivers$traj_id[rivers$type == "central"] else target
  edges <- confluences[!(confluences$child %in% zero), , drop = FALSE]
  kids_of <- split(seq_len(nrow(edges)), edges$parent)
  reachable <- character(0); used <- logical(nrow(edges)); frontier <- roots
  while (length(frontier)) {
    reachable <- union(reachable, frontier)
    nxt <- character(0)
    for (p in frontier) {
      idx <- kids_of[[p]]
      if (!is.null(idx)) { used[idx] <- TRUE; nxt <- c(nxt, edges$child[idx]) }
    }
    frontier <- setdiff(unique(nxt), reachable)
  }
  list(rivers = rivers[rivers$traj_id %in% reachable, , drop = FALSE],
       confluences = edges[used, , drop = FALSE])
}

# Centered recursive packing. Returns tibble(traj_id, lane); x is always the year
# (set by the caller), only y/lane is solved here. A node is centered among its
# children: order children by descending size, alternate them outward (strongest
# nearest the spine) above and below, each child reserving its own recursively
# packed block. Roots (centrals, or the single target) are stacked top to bottom
# by descending size with a one-lane gap, so nothing collides.
.confluence_lanes <- function(rivers, confluences, target = NULL, root_gap = 3) {
  size_of <- stats::setNames(rivers$size, rivers$traj_id)
  kids_of <- split(confluences$child, confluences$parent)

  # block(node) -> character vector of traj_ids, top to bottom, node centered.
  block <- function(node) {
    kids <- kids_of[[node]]
    if (is.null(kids) || length(kids) == 0) return(node)
    kids <- kids[order(-size_of[kids], kids)]          # strongest first
    above <- kids[seq_along(kids) %% 2 == 1]            # 1st, 3rd, ... above
    below <- kids[seq_along(kids) %% 2 == 0]            # 2nd, 4th, ... below
    above_block <- unlist(lapply(rev(above), block), use.names = FALSE)  # outermost first
    below_block <- unlist(lapply(below, block), use.names = FALSE)
    c(above_block, node, below_block)
  }

  if (is.null(target)) {
    roots <- rivers$traj_id[rivers$type == "central"]
  } else {
    roots <- target
  }
  roots <- roots[order(-size_of[roots], roots)]

  sep <- rep(NA_character_, root_gap)
  order_tb <- character(0)
  for (j in seq_along(roots)) {
    if (j > 1L) order_tb <- c(order_tb, sep)           # gap between watersheds
    order_tb <- c(order_tb, block(roots[j]))
  }
  lanes <- rev(seq_along(order_tb))                    # first -> highest
  out <- tibble::tibble(traj_id = order_tb,
                        lane = as.numeric(lanes) - mean(lanes))
  out[!is.na(out$traj_id), , drop = FALSE]             # drop spacer rows
}

# Gaussian low-pass over a uniformly spaced series: a ~1-year sigma (in x units)
# that rounds the year-to-year kinks, with the ends edge-padded so the smoothed
# curve still starts and ends on its own value.
.gauss_smooth <- function(y, dx, smooth = TRUE) {
  n <- length(y)
  if (!smooth || n < 3) return(pmax(0, y))
  sigma <- max(1, 1.0 / dx)
  hk <- ceiling(3 * sigma)
  kern <- stats::dnorm(seq(-hk, hk), sd = sigma); kern <- kern / sum(kern)
  padded <- c(rep(y[1], hk), y, rep(y[n], hk))
  sm <- as.numeric(stats::filter(padded, kern, sides = 2))
  pmax(0, sm[(hk + 1):(hk + n)])
}

# Smoothed cumulative step on a fine x-grid (the spine smoothing from
# plot_trajectory_formation.R). step_fun maps a year to cumulative papers.
.smooth_cum <- function(x0, x1, step_fun, smooth = TRUE, n_grid = 400) {
  grid_x <- seq(x0, x1, length.out = n_grid)
  step <- vapply(grid_x, step_fun, numeric(1))
  if (!smooth || x1 <= x0) return(list(x = grid_x, y = pmax(0, step)))
  dx <- (x1 - x0) / (n_grid - 1)
  list(x = grid_x, y = .gauss_smooth(step, dx, smooth))
}

# Smoothed resample of a (year, value) series onto a fine x-grid: linearly
# interpolate to the grid (flat past the ends), then Gaussian-smooth, so a ribbon
# outline flows like a brush stroke instead of a kinked polygon.
.smooth_series <- function(x_in, y_in, x0, x1, smooth = TRUE, n_grid = 200) {
  grid_x <- seq(x0, x1, length.out = n_grid)
  y <- stats::approx(x_in, y_in, xout = grid_x, rule = 2)$y
  if (!smooth || x1 <= x0) return(list(x = grid_x, y = pmax(0, y)))
  dx <- (x1 - x0) / (n_grid - 1)
  list(x = grid_x, y = .gauss_smooth(y, dx, smooth))
}

# Build the ggplot-ready polygons. One shared scale maps papers -> y half-height
# (0 papers -> 0). Tributary ribbons follow size_curve, split grey (before
# first_feed_year) / colour (after). Central spines follow the smoothed
# cumulative inflow of their kept direct children. Merge bands carry width n.
.confluence_polygons <- function(rivers, confluences, lanes, last_year,
                                 smooth = TRUE, width_range = c(0, 0.42)) {
  lane <- stats::setNames(lanes$lane, lanes$traj_id)

  # widths map papers -> half-height affinely into width_range = c(floor, max):
  # the widest object reaches `max`, every band is at least `floor` (so thin
  # streams stay visible). Candidates for the widest: each central's total kept
  # inflow, and each non-central trajectory's peak size_curve.
  inflow_by_parent <- tapply(confluences$n, confluences$parent, sum)
  trib_peak <- vapply(rivers$size_curve[rivers$type != "central"], function(s)
    if (nrow(s)) max(s$size) else 0, numeric(1))
  # self-sufficient centrals (no tributaries) are drawn from their own size curve,
  # so their peak size must also be a candidate for the shared scale.
  central_ids <- rivers$traj_id[rivers$type == "central"]
  childless <- setdiff(central_ids, confluences$parent)
  endo_peak <- if (length(childless)) vapply(
    rivers$size_curve[rivers$traj_id %in% childless],
    function(s) if (nrow(s)) max(s$size) else 0, numeric(1)) else 0
  peak <- max(c(unname(inflow_by_parent), trib_peak, endo_peak, 1))
  width_range <- sort(width_range)
  w0 <- width_range[1]                      # minimum half-width (floor)
  scale <- (width_range[2] - w0) / peak     # proportional slope above the floor

  # merge bend length per edge: grows ~1:1 with the vertical distance travelled so
  # the bend slope (and therefore the band's apparent thickness) is roughly
  # constant regardless of how far a feeder sits from the central. Keyed by child;
  # the ribbon ends where its merge bend begins, so the two flow as one.
  span_of <- stats::setNames(numeric(nrow(rivers)), rivers$traj_id)
  for (i in seq_len(nrow(confluences))) {
    ch <- confluences$child[i]; pa <- confluences$parent[i]
    if (!(ch %in% names(lane)) || !(pa %in% names(lane))) next
    span_of[ch] <- max(1.4, min(8, abs(lane[[ch]] - lane[[pa]])))
  }

  # --- tributary ribbons (non-central): width = size_curve, split by onset -----
  # Resample each size_curve onto an annual grid out to the handoff (flat past the
  # last measured year) and force the onset year to be a grid point, so the grey
  # (pre-feed) and coloured (feeding) segments split exactly at first_feed_year
  # even when the raw curve has no sample at that year.
  ribbons <- list()
  ribbon_end <- stats::setNames(numeric(0), character(0))   # x where each ribbon stops
  cf_by_child <- stats::setNames(seq_len(nrow(confluences)), confluences$child)
  for (i in which(rivers$type != "central")) {
    id <- rivers$traj_id[i]
    s <- rivers$size_curve[[i]]
    if (nrow(s) == 0) next
    hy <- rivers$handoff_year[i]
    if (nrow(s) == 1) {                  # constant-width for single-point curves
      s <- rbind(s, tibble::tibble(year = hy, size = s$size[1]))
      if (s$year[1] == s$year[2]) s$year[2] <- s$year[1] + 1L
    }
    ci <- cf_by_child[id]
    onset <- if (!is.na(ci)) confluences$first_feed_year[ci] else min(s$year)
    yc <- lane[[id]]
    x0 <- min(s$year)
    # the bend descends into the river over a span ~ the lane distance, so its slope
    # stays gentle (~1) instead of near-vertical. It begins at `bstart`, where the
    # ribbon's flat part ends, so the two join. A short-lived feeder that sits far
    # from the river has no room for a flat part first (the bend may start up to
    # 1.5 yr before it, as a lead-in); it gets a small node cap at the bend start so
    # the trajectory still reads as a node, then it is all bend.
    bstart <- min(max(hy - span_of[[id]], x0 - 1.5), hy - 0.5)
    ribbon_end[id] <- bstart                              # the merge starts here, so they join
    if (bstart <= x0 + 0.25) {
      capw <- w0 + scale * s$size[which.min(s$year)]
      ribbons[[length(ribbons) + 1]] <- tibble::tibble(
        traj_id = id, x = c(bstart - 0.7, bstart),
        lo = yc - capw, hi = yc + capw, seg = "feed")
      next
    }
    ser <- .smooth_series(s$year, s$size, x0, bstart, smooth = smooth)
    gx <- ser$x; hw <- w0 + scale * ser$y                 # smoothed half-widths
    poly <- tibble::tibble(traj_id = id, x = gx, lo = yc - hw, hi = yc + hw,
                           seg = ifelse(gx <= onset, "pre", "feed"))
    if (onset > x0 && onset < bstart) {   # split the two fills exactly at onset
      by <- stats::approx(gx, ser$y, xout = onset, rule = 2)$y
      bh <- w0 + scale * by
      bnd <- tibble::tibble(traj_id = id, x = onset, lo = yc - bh, hi = yc + bh,
                            seg = c("pre", "feed"))
      poly <- rbind(poly[poly$x != onset, ], bnd)
    }
    poly <- poly[order(poly$seg, poly$x), ]               # x-ascending within fill
    ribbons[[length(ribbons) + 1]] <- poly
  }
  ribbons <- if (length(ribbons)) dplyr::bind_rows(ribbons) else
    tibble::tibble(traj_id = character(), x = numeric(), lo = numeric(),
                   hi = numeric(), seg = character())

  # --- central spines: width = smoothed cumulative inflow ----------------------
  spines <- list()
  for (i in which(rivers$type == "central")) {
    id <- rivers$traj_id[i]
    kids <- confluences[confluences$parent == id, , drop = FALSE]
    yc <- lane[[id]]
    if (nrow(kids) > 0) {
      # spine = smoothed cumulative inflow of the central's kept tributaries.
      x0 <- min(kids$handoff_year); x1 <- last_year
      step_fun <- local({
        k <- kids
        function(xx) sum(k$n[k$handoff_year <= xx])
      })
      sm <- .smooth_cum(x0, x1, step_fun, smooth = smooth)
      xs <- sm$x; vals <- sm$y
    } else {
      # self-sufficient central (no tributaries): draw its own lineage growth so it
      # still appears instead of vanishing from the plot. Pad a single-year curve
      # so the one-point ribbon (which renders nothing) becomes a visible segment.
      s <- rivers$size_curve[[i]]
      if (nrow(s) == 0) next
      if (nrow(s) == 1) s <- rbind(tibble::tibble(year = s$year - 1L, size = s$size), s)
      xs <- s$year; vals <- s$size
    }
    spines[[length(spines) + 1]] <- tibble::tibble(
      traj_id = id, x = xs,
      lo = yc - (w0 + scale * vals), hi = yc + (w0 + scale * vals))
  }
  spines <- if (length(spines)) dplyr::bind_rows(spines) else
    tibble::tibble(traj_id = character(), x = numeric(),
                   lo = numeric(), hi = numeric())

  # --- merge bands: an S-curve carrying width n from child lane into parent -----
  merges <- list()
  for (i in seq_len(nrow(confluences))) {
    child <- confluences$child[i]; parent <- confluences$parent[i]
    if (!(child %in% names(lane)) || !(parent %in% names(lane))) next
    hy <- confluences$handoff_year[i]; nn <- confluences$n[i]
    # start the bend exactly where the child's ribbon stops, so the two always join.
    # A short-lived tributary (its size_curve begins close to the handoff) has a tiny
    # ribbon; anchoring at hy - span_of would strand it far to the left of the bend.
    mstart <- if (child %in% names(ribbon_end)) ribbon_end[[child]] else hy - span_of[[child]]
    if (hy - mstart < 0.25) mstart <- hy - 0.5  # guard: keep a visible bend
    y_child <- lane[[child]]; y_parent <- lane[[parent]]
    xs <- seq(mstart, hy, length.out = 48)
    t  <- (xs - mstart) / (hy - mstart)
    sstep <- t * t * (3 - 2 * t)                # smoothstep 0->1
    yc <- y_child + (y_parent - y_child) * sstep
    half <- w0 + scale * nn                     # river-scale width = transferred papers
    merges[[length(merges) + 1]] <- tibble::tibble(
      child = child, x = xs, lo = yc - half, hi = yc + half)
  }
  merges <- if (length(merges)) dplyr::bind_rows(merges) else
    tibble::tibble(child = character(), x = numeric(),
                   lo = numeric(), hi = numeric())

  list(spines = spines, ribbons = ribbons, merges = merges, scale = scale)
}

# Focus view for one intermediate (absorbed) trajectory: its feeders merge into it
# on the left (a mini-formation), and the finals its terminal cohort fed fan out on
# the right (its multi-destination split). Returns a ggplot.
.confluence_focus <- function(conf, target, min_n = 5, min_prop = 0.05,
                              width_range = c(0, 0.42),
                              label_intermediary = "id_count",
                              label_terminal = "id_count",
                              label_intermediary_size = 4, label_terminal_size = 4.5,
                              axis_text_size = NULL, palette = NULL, title = NULL) {
  if (is.null(conf$destinations)) {
    stop("this 'conf' has no 'destinations' field, which the focus view needs; ",
         "rebuild it with sniff_trajectory_confluence() (older objects predate it).",
         call. = FALSE)
  }
  ti <- conf$rivers[conf$rivers$traj_id == target, , drop = FALSE]
  hy <- as.numeric(ti$handoff_year); start <- as.numeric(ti$start)

  # --- upstream: feeders of the target, target reclassified as a pseudo-central --
  feeders <- conf$confluences[conf$confluences$parent == target, , drop = FALSE]
  if (nrow(feeders)) feeders <- feeders[
    feeders$n >= min_n & feeders$n / feeders$cohort_size >= min_prop, , drop = FALSE]
  sub_r <- conf$rivers[conf$rivers$traj_id %in% c(target, feeders$child), , drop = FALSE]
  sub_r$type[sub_r$traj_id == target] <- "central"
  sub_r$central[sub_r$traj_id == target] <- target
  ln <- .confluence_lanes(sub_r, feeders, target = target)
  g  <- .confluence_polygons(sub_r, feeders, ln, last_year = hy, smooth = TRUE,
                             width_range = width_range)
  lane <- stats::setNames(ln$lane, ln$traj_id)
  y_t <- lane[[target]]; w0 <- min(width_range); scale <- g$scale

  fkeys <- mixed_sort(feeders$child)
  fpal <- if (!is.null(palette)) palette else if (length(fkeys) >= 1 && length(fkeys) <= 8) {
    stats::setNames(RColorBrewer::brewer.pal(max(3, length(fkeys)), "Set2")[seq_along(fkeys)], fkeys)
  } else if (length(fkeys) >= 1) {
    stats::setNames(scales::hue_pal()(length(fkeys)), fkeys)
  } else character(0)
  fhue <- function(id) if (id %in% names(fpal)) unname(fpal[[id]]) else "grey50"
  g$ribbons$fill <- ifelse(g$ribbons$seg == "pre", "grey60",
                           vapply(g$ribbons$traj_id, fhue, character(1)))
  g$ribbons$grp <- paste(g$ribbons$traj_id, g$ribbons$seg)
  g$merges$fill <- vapply(g$merges$child, fhue, character(1))
  g$spines$fill <- rep("#2E3440", nrow(g$spines))

  p <- ggplot2::ggplot()
  if (nrow(g$spines)) p <- p + ggplot2::geom_ribbon(data = g$spines,
    ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
                 group = .data$traj_id, fill = .data$fill))
  if (nrow(g$merges)) p <- p + ggplot2::geom_ribbon(data = g$merges,
    ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
                 group = .data$child, fill = .data$fill))
  if (nrow(g$ribbons)) p <- p + ggplot2::geom_ribbon(data = g$ribbons,
    ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
                 group = .data$grp, fill = .data$fill))

  # --- downstream: destination finals fan out to the right of the handoff --------
  dests <- conf$destinations[conf$destinations$traj_id == target & conf$destinations$n > 0, , drop = FALSE]
  if (nrow(dests)) dests <- dests[dests$n / sum(dests$n) >= 0.04, , drop = FALSE]
  dests <- dests[order(-dests$n), , drop = FALSE]
  stub_df <- NULL
  if (nrow(dests)) {
    fan <- max(3, round(conf$last_year - hy)); x_stub <- hy + fan
    nd <- nrow(dests)
    gap <- max(w0 + scale * dests$n) * 2 + 0.4
    ys <- y_t + (seq_len(nd) - (nd + 1) / 2) * gap
    ckeys <- conf$centrals
    cpal <- if (length(ckeys) <= 8) stats::setNames(RColorBrewer::brewer.pal(max(3, length(ckeys)), "Set2")[seq_along(ckeys)], ckeys) else stats::setNames(scales::hue_pal()(length(ckeys)), ckeys)
    link_rows <- list()
    for (j in seq_len(nd)) {
      col <- if (dests$g_final[j] %in% names(cpal)) unname(cpal[[dests$g_final[j]]]) else "grey50"
      half <- w0 + scale * dests$n[j]
      xs <- seq(hy, x_stub, length.out = 40)
      t <- (xs - hy) / (x_stub - hy); sstep <- t * t * (3 - 2 * t)
      yc <- y_t + (ys[j] - y_t) * sstep
      link_rows[[j]] <- tibble::tibble(grp = dests$g_final[j], x = xs,
                                       lo = yc - half, hi = yc + half, fill = col)
    }
    p <- p + ggplot2::geom_ribbon(data = dplyr::bind_rows(link_rows),
      ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
                   group = .data$grp, fill = .data$fill))
    dest_lab <- if (identical(label_terminal, "id")) paste0("tr::", dests$g_final) else
      paste0("tr::", dests$g_final, " (", dests$n, ")")
    stub_df <- tibble::tibble(x = x_stub, y = ys, lab = dest_lab)
  }
  p <- p + ggplot2::scale_fill_identity()

  # --- labels --------------------------------------------------------------------
  if (nrow(feeders) && !is.null(label_intermediary)) {
    fr <- conf$rivers[conf$rivers$traj_id %in% feeders$child, , drop = FALSE]
    fr$lane <- lane[fr$traj_id]
    re <- tapply(g$ribbons$x, g$ribbons$traj_id, max)
    fr$x <- as.numeric(re[fr$traj_id]); fr$x[is.na(fr$x)] <- start
    fn <- stats::setNames(feeders$n, feeders$child)
    fc <- stats::setNames(feeders$cohort_size, feeders$child)
    fr$lab <- if (identical(label_intermediary, "id")) fr$traj_id else
      paste0(fr$traj_id, " (", fn[fr$traj_id], "/", fc[fr$traj_id], ")")
    out <- ifelse(fr$lane >= y_t, 1, -1)
    p <- p + (if (requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(data = fr,
        ggplot2::aes(x = .data$x, y = .data$lane, label = .data$lab),
        direction = "y", hjust = 0, nudge_x = 0.2, nudge_y = out * 0.25,
        size = label_intermediary_size, segment.colour = "grey45",
        segment.size = 0.4, min.segment.length = 0, point.padding = 0.15,
        box.padding = 0.35, max.overlaps = Inf)
      else ggplot2::geom_text(data = fr,
        ggplot2::aes(x = .data$x, y = .data$lane, label = .data$lab),
        hjust = 0, size = label_intermediary_size))
  }
  tlab <- if (identical(label_terminal, "id")) target else paste0(target, " (", ti$size, ")")
  # label sits on the dark target river, so draw it in white (the dark "#2E3440"
  # text was invisible against the same-coloured trunk).
  p <- p + ggplot2::annotate("text", x = hy - 0.2, y = y_t, label = tlab, hjust = 1,
    vjust = 0.5, size = label_terminal_size + 0.5, fontface = "bold", colour = "white")
  if (!is.null(stub_df)) p <- p + ggplot2::geom_text(data = stub_df,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$lab),
    hjust = 0, nudge_x = 0.3, size = label_terminal_size, fontface = "bold")

  if (is.null(title)) title <- paste0("Focus: ", target, " (feeders and destinations)")
  x_lo <- min(start, if (nrow(g$ribbons)) min(g$ribbons$x) else start)
  x_hi <- if (!is.null(stub_df)) max(stub_df$x) else hy
  p +
    ggplot2::scale_x_continuous(breaks = seq(round(x_lo), round(x_hi), by = 2),
      expand = ggplot2::expansion(mult = c(0.02, 0.16))) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::labs(x = "Year", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = axis_text_size),
      axis.text.y = ggplot2::element_blank())
}

#' Plot the trajectory confluence (how the central trajectories form)
#'
#' A braided river system over [sniff_trajectory_confluence()]: every trajectory is
#' a stream flowing left to right along the publication year, merging into its
#' absorber where it hands off, until the central trajectories (`tr::cNgN`) reach
#' the final year. Each tributary is drawn **grey** until the first paper it
#' transfers is published, then in its colour; at the merge a band of width equal
#' to the **transferred** papers (`n` of the tributary's `cohort_size`) bends into
#' the river, which widens by that amount. The central spine is centered on its
#' band with tributaries packed above and below. With `target = NULL` the whole
#' system is drawn (one colour per destination central); with `target = "tr::c1g1"`
#' only that central's direct feeders are drawn (one colour per tributary).
#'
#' @param conf A [sniff_trajectory_confluence()] object.
#' @param target What to draw. `"all"` (default, same as `NULL`) draws the full
#'   confluence (the whole nested forest). `"finals"` draws only the **central
#'   backbones** (the inflow spines, tributaries hidden); `"intermediary"` draws
#'   only the **tributary streams** (the ribbons, central spines hidden). A single
#'   central `traj_id` (e.g. `"tr::c1g1"`) draws only that central's tributaries; a
#'   vector of central ids (e.g. `c("tr::c1g1", "tr::c1g5")`) draws the selected
#'   centrals stacked. With a single central the streams are coloured per
#'   tributary; otherwise per destination central. A single **intermediate**
#'   trajectory id (e.g. `"tr3"`) switches to a **focus view**: its feeders merge
#'   in on the left (who fed it) and the finals its terminal cohort reached fan out
#'   on the right (who it fed).
#' @param depth How many confluence levels upstream of the roots to draw; `NULL`
#'   (default) means the whole forest when `target = NULL`, or just the direct
#'   feeders (`1`) when one or more `target`s are given. Set e.g. `depth = 2` to
#'   expand a target view to feeders-of-feeders.
#' @param min_n Minimum papers a tributary must transfer to its absorber to be
#'   counted; below this it is pruned from the drawing **and** the river width,
#'   together with its own upstream subtree (default `5`).
#' @param min_prop Minimum share of a tributary's own cohort that must transfer
#'   (default `0.05`); guards against incidental trickles.
#' @param min_total_size Minimum **total size** of a tributary (its own distinct
#'   paper count across all years) for it to be drawn (default `0`, no filter);
#'   prunes small lineages and their upstream subtree, complementing `min_n`
#'   (which gates the transfer, not the trajectory's own size).
#' @param min_duration_years Minimum **lifespan in years** of a tributary (its
#'   last year minus its first year, plus one) for it to be drawn (default `0`, no
#'   filter); use it to drop short-lived lineages.
#' @param width_range Half-width range `c(floor, max)` in y-data units (lanes are
#'   spaced `1` apart, so keep `max` below ~`0.5` to avoid overlap). The widest
#'   stream reaches `max`; every band is at least `floor`, so thin streams stay
#'   visible. Default `c(0, 0.42)` is strictly proportional (no floor); a positive
#'   floor (e.g. `c(0.05, 0.45)`) lifts the small streams, raising `max` thickens
#'   everything. Note these are lane-fraction units, **not** pixel sizes like
#'   `marker_range`.
#' @param label_terminal Label content for the **final** (terminal) trajectories:
#'   `"id_count"` (default) `tr::cNgN (size)`, `"id"` `tr::cNgN`, or `NULL` to hide
#'   them. The count is the central's **final-year** size (what remains in the
#'   final community), matching [plot_trajectory_lines_3d()], not its lifetime
#'   total.
#' @param label_intermediary Label content for the **intermediate** (tributary)
#'   trajectories: `"id_count"` (default) `trN (n)`, `"id"` `trN`, or `NULL` to
#'   hide them. `n` is the tributary's documents that **end** in its target final
#'   (the destination count from [plot_trajectory_lines_3d()]) -- the single target
#'   in a single-target view, else the tributary's own dominant central -- not the
#'   trunk transfer. Objects without destination data fall back to `trN (n/cohort)`.
#' @param labels_text Optional `data.frame` with columns `id` and `text` mapping
#'   central group ids (`cNgN`) to descriptions; when supplied a bottom legend
#'   `tr::cNgN: description` is added (as in [plot_trajectory_dag()]). `NULL`
#'   (default) adds no legend.
#' @param legend_ncol Number of columns in the `labels_text` legend; `NULL`
#'   (default) lets ggplot choose.
#' @param label_terminal_size Text size of the **final** (terminal) labels
#'   (default `4.5`).
#' @param label_intermediary_size Text size of the **intermediate** (tributary)
#'   labels (default `4`).
#' @param palette Optional named colour vector overriding the default hues (keyed
#'   by destination central in the all/multi view, else by tributary `traj_id`).
#' @param axis_text_size Font size of the x-axis (year) tick labels; `NULL`
#'   (default) keeps the theme default.
#' @param year_range Optional `c(from, to)` years fixing the time axis (as in
#'   [plot_trajectory_lines_2d()]), so a short confluence can share the window of a
#'   longer one. `NULL` (default) fits the drawn rivers; a wider range only extends
#'   the axis (it never drops data). Ignored in the single-intermediate focus view.
#' @param show_secondary Draw faint links from a tributary to the **other** finals
#'   its terminal cohort fed, beyond the dominant one it merges into (default
#'   `TRUE`). A tributary's papers can land in several final groups; the forest
#'   keeps only the dominant, and these links reveal the rest. Only drawn where both
#'   spines and ribbons are shown and the destination central is in view (so the
#'   all-view or several selected centrals, not a single-central view). Needs the
#'   `destinations` field of [sniff_trajectory_confluence()].
#' @param secondary_min_prop Minimum share of a tributary's terminal cohort that
#'   must reach a secondary final for its link to be drawn (default `0.2`); raise it
#'   to keep only the strongest secondary destinations.
#' @param smooth Gaussian-smooth the stream outlines (default `TRUE`): the central
#'   spine's cumulative inflow and each tributary's width follow a ~1-year low-pass
#'   so the edges flow as brush strokes instead of kinked polygons. `FALSE` keeps
#'   the raw year-to-year steps.
#' @param title Plot title; `NULL` (default) derives one from `target`.
#'
#' @return A `ggplot` object.
#'
#' @details
#' - **x = publication year** (never reassigned); the **y-axis has no intrinsic
#'   meaning**, it only packs streams to minimise merge crossings.
#' - **Tributary width** follows its community size each year; **grey** before the
#'   first transferred paper, then its **colour**. **Central spine width** is the
#'   smoothed cumulative inflow of its counted tributaries (papers transferred),
#'   on one proportional scale shared with the tributaries (0 papers give 0 width).
#' - A tributary hands `n` of its `cohort_size` papers over; only `n` enters the
#'   river, so the merge band keeps a **constant width equal to the transferred
#'   papers** (the `n/cohort` label makes the split explicit) and bends over a span
#'   that scales with the distance travelled, so its thickness reads the same for
#'   near and far feeders.
#' - `min_n` / `min_prop` decide what is **counted**, not merely hidden: dropped
#'   tributaries leave the river narrower and are summarised in the caption.
#' - An intermediate that delivers **0 documents** to the final it flows into (its
#'   terminal cohort lands entirely outside that final's community) is always
#'   excluded, together with its upstream subtree, regardless of the thresholds: a
#'   stream contributing nothing to the destination cannot appear.
#'
#' @seealso [sniff_trajectory_confluence()], [plot_trajectory_formation()],
#'   [sniff_trajectory_braid()]
#'
#' @examples
#' \dontrun{
#' flow <- sniff_trajectory_braid(docs_per_group)
#' conf <- sniff_trajectory_confluence(flow)
#'
#' # the whole system: how every central forms
#' plot_trajectory_confluence(conf, min_n = 20)
#'
#' # one central's direct feeders, coloured per tributary
#' plot_trajectory_confluence(conf, target = "tr::c1g1")
#'
#' # focus on one intermediate trajectory: its feeders + the finals it fed
#' plot_trajectory_confluence(conf, target = "tr7")
#'
#' # two selected centrals, a description legend, bigger year labels, bare ids,
#' #  a visible floor on thin streams, and only sizeable tributaries
#' descr <- data.frame(id = c("c1g1", "c1g5"), text = c("Topic A", "Topic B"))
#' plot_trajectory_confluence(conf, target = c("tr::c1g1", "tr::c1g5"),
#'                               labels_text = descr, axis_text_size = 14,
#'                               label_intermediary = "id", min_total_size = 30,
#'                               min_duration_years = 3, width_range = c(0.05, 0.45))
#' }
#'
#' @family visualization
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_blank geom_text geom_label geom_point scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous scale_colour_manual guides guide_legend
#' @importFrom ggplot2 labs theme_minimal theme element_blank element_text expansion
#' @importFrom ggplot2 annotate
#' @importFrom rlang .data
plot_trajectory_confluence <- function(conf, target = "all", depth = NULL,
                                          min_n = 5, min_prop = 0.05,
                                          min_total_size = 0, min_duration_years = 0,
                                          width_range = c(0, 0.42),
                                          label_terminal = c("id_count", "id"),
                                          label_intermediary = c("id_count", "id"),
                                          label_terminal_size = 4.5,
                                          label_intermediary_size = 4,
                                          labels_text = NULL, legend_ncol = NULL,
                                          palette = NULL, axis_text_size = NULL,
                                          year_range = NULL,
                                          show_secondary = TRUE,
                                          secondary_min_prop = 0.2,
                                          smooth = TRUE, title = NULL) {
  if (!is.null(label_terminal)) label_terminal <- match.arg(label_terminal)
  if (!is.null(label_intermediary)) label_intermediary <- match.arg(label_intermediary)
  # Keyword targets pick which layers to draw over the whole-forest layout:
  # "all" = everything, "finals" = central backbones (spines only), "intermediary"
  # = tributary streams (ribbons only). A vector of central ids selects centrals.
  draw_spines <- draw_ribbons <- draw_merges <- TRUE
  if (is.character(target) && length(target) == 1L &&
      target %in% c("all", "finals", "intermediary")) {
    view <- target
    if (view == "finals")       { draw_ribbons <- FALSE; draw_merges <- FALSE }
    if (view == "intermediary") { draw_spines  <- FALSE; draw_merges <- FALSE }
    target <- NULL
  } else {
    view <- NULL
  }
  if (!is.list(conf) || is.null(conf$rivers) || is.null(conf$confluences) ||
      !is.data.frame(conf$rivers)) {
    stop("'conf' must be the output of sniff_trajectory_confluence()", call. = FALSE)
  }
  # focus view: a single intermediate (absorbed) trajectory -> its feeders
  # (upstream) and the finals its cohort fed (downstream).
  if (is.character(target) && length(target) == 1L &&
      target %in% conf$rivers$traj_id &&
      conf$rivers$type[match(target, conf$rivers$traj_id)] == "absorbed") {
    return(.confluence_focus(conf, target, min_n = min_n, min_prop = min_prop,
      width_range = width_range, label_intermediary = label_intermediary,
      label_terminal = label_terminal,
      label_intermediary_size = label_intermediary_size,
      label_terminal_size = label_terminal_size, axis_text_size = axis_text_size,
      palette = palette, title = title))
  }
  single <- !is.null(target) && length(target) == 1
  max_depth <- if (!is.null(depth)) depth else if (is.null(target)) Inf else 1L
  pr <- .confluence_prune(conf$rivers, conf$confluences, target = target,
                          min_n = min_n, min_prop = min_prop,
                          min_total_size = min_total_size,
                          min_duration_years = min_duration_years,
                          max_depth = max_depth)
  if (nrow(pr$rivers) == 0) {
    stop("no trajectories pass the thresholds (min_n / min_prop / min_total_size / ",
         "min_duration_years)", call. = FALSE)
  }
  # an intermediate that delivers 0 documents to its final cannot appear.
  zf <- .confluence_drop_zero_dest(pr$rivers, pr$confluences, conf, target, single)
  pr$rivers <- zf$rivers; pr$confluences <- zf$confluences
  ln <- .confluence_lanes(pr$rivers, pr$confluences, target = target)
  g  <- .confluence_polygons(pr$rivers, pr$confluences, ln,
                             last_year = conf$last_year, smooth = smooth,
                             width_range = width_range)
  lane <- stats::setNames(ln$lane, ln$traj_id)

  # colour key: a single central -> one hue per tributary; the all-view or several
  # selected centrals -> one hue per destination central (tributaries inherit it).
  # Grey is the pre-feed fill.
  if (!single) {
    keys <- conf$centrals
    key_of <- stats::setNames(pr$rivers$central, pr$rivers$traj_id)
  } else {
    keys <- mixed_sort(pr$rivers$traj_id[pr$rivers$type != "central"])
    key_of <- stats::setNames(pr$rivers$traj_id, pr$rivers$traj_id)
  }
  pal <- if (!is.null(palette)) palette else if (length(keys) <= 8 && length(keys) >= 1) {
    stats::setNames(RColorBrewer::brewer.pal(max(3, length(keys)), "Set2")[seq_along(keys)], keys)
  } else if (length(keys) >= 1) {
    stats::setNames(scales::hue_pal()(length(keys)), keys)
  } else {
    character(0)
  }
  hue <- function(id) {
    k <- key_of[[id]]
    col <- if (!is.null(k) && k %in% names(pal)) pal[[k]] else "grey50"
    unname(col)
  }
  g$ribbons$fill <- ifelse(g$ribbons$seg == "pre", "grey60",
                           vapply(g$ribbons$traj_id, hue, character(1)))
  g$ribbons$grp <- paste(g$ribbons$traj_id, g$ribbons$seg)
  g$merges$fill <- vapply(g$merges$child, hue, character(1))
  g$spines$fill <- if (!single)
    vapply(g$spines$traj_id, hue, character(1)) else rep("#2E3440", nrow(g$spines))

  p <- ggplot2::ggplot()
  if (draw_spines && nrow(g$spines)) p <- p + ggplot2::geom_ribbon(
    data = g$spines, ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
      group = .data$traj_id, fill = .data$fill))
  if (draw_merges && nrow(g$merges)) p <- p + ggplot2::geom_ribbon(
    data = g$merges, ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
      group = .data$child, fill = .data$fill))
  if (draw_ribbons && nrow(g$ribbons)) p <- p + ggplot2::geom_ribbon(
    data = g$ribbons, ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
      group = .data$grp, fill = .data$fill))
  # secondary destinations: faint links from a tributary to the OTHER finals its
  # terminal cohort fed (beyond its dominant parent), coloured by the destination
  # central. Drawn only when both spines and ribbons are shown and the destination
  # central is in view.
  if (show_secondary && draw_ribbons && draw_spines &&
      !is.null(conf$destinations) && nrow(conf$destinations) > 0 &&
      nrow(g$ribbons) > 0) {
    re <- tapply(g$ribbons$x, g$ribbons$traj_id, max)
    spine_x0 <- tapply(g$spines$x, g$spines$traj_id, min)  # where each river begins
    spine_x1 <- tapply(g$spines$x, g$spines$traj_id, max)  # where each river ends
    w0 <- min(width_range)
    abs_riv <- pr$rivers[pr$rivers$type != "central", , drop = FALSE]
    sec_rows <- list()
    for (k in seq_len(nrow(abs_riv))) {
      id <- abs_riv$traj_id[k]; prim <- abs_riv$central[k]
      if (!(id %in% names(re))) next
      dd <- conf$destinations[conf$destinations$traj_id == id, , drop = FALSE]
      if (!nrow(dd)) next
      tot <- sum(dd$n)
      dd <- dd[dd$g_final != prim & dd$n / tot >= secondary_min_prop, , drop = FALSE]
      if (!nrow(dd)) next
      y0 <- lane[[id]]; x_end <- as.numeric(re[id])
      for (j in seq_len(nrow(dd))) {
        sc <- paste0("tr::", dd$g_final[j])
        if (!(sc %in% names(lane))) next
        y1 <- lane[[sc]]
        span <- max(1.4, min(8, abs(y1 - y0)))
        # land on the destination river where it actually exists: if it forms after
        # x_end + span, extend the link to that river's start instead of leaving a
        # loose end in empty space.
        sx0 <- if (sc %in% names(spine_x0)) spine_x0[[sc]] else x_end
        sx1 <- if (sc %in% names(spine_x1)) spine_x1[[sc]] else conf$last_year
        lx <- min(max(x_end + span, sx0), sx1)
        if (lx <= x_end) next                    # river ends before this tributary
        xs <- seq(x_end, lx, length.out = 40)
        t  <- if (lx > x_end) (xs - x_end) / (lx - x_end) else rep(0, length(xs))
        sstep <- t * t * (3 - 2 * t)
        yc <- y0 + (y1 - y0) * sstep
        half <- w0 + g$scale * dd$n[j]
        col <- if (dd$g_final[j] %in% names(pal)) unname(pal[[dd$g_final[j]]]) else "grey50"
        sec_rows[[length(sec_rows) + 1]] <- tibble::tibble(
          grp = paste(id, sc), x = xs, lo = yc - half, hi = yc + half, fill = col)
      }
    }
    if (length(sec_rows)) {
      sec <- dplyr::bind_rows(sec_rows)
      p <- p + ggplot2::geom_ribbon(
        data = sec, ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
          group = .data$grp, fill = .data$fill), alpha = 0.3)
    }
  }
  p <- p + ggplot2::scale_fill_identity()

  # labels: central -> "cNgN (size)" at the right end; tributary -> "trN (n/coh)"
  riv <- pr$rivers
  riv$lane <- lane[riv$traj_id]
  cen <- riv[riv$type == "central", , drop = FALSE]
  if (!is.null(label_terminal)) {
    if (identical(label_terminal, "id_count")) {
      # the central's final-year size (what remains in the final community),
      # matching plot_trajectory_lines_3d(), not its lifetime total. Applies to
      # every view; falls back to the total if a curve is missing.
      fy <- vapply(cen$size_curve, function(s)
        if (is.data.frame(s) && nrow(s)) as.integer(s$size[which.max(s$year)]) else NA_integer_,
        integer(1))
      fy[is.na(fy)] <- cen$size[is.na(fy)]
      cen$lab <- paste0(cen$traj_id, " (", fy, ")")
    } else {
      cen$lab <- cen$traj_id
    }
  }
  trb <- riv[riv$type != "central", , drop = FALSE]
  cf_by_child <- stats::setNames(seq_len(nrow(pr$confluences)), pr$confluences$child)
  central_of <- stats::setNames(riv$central, riv$traj_id)
  # the count is the documents of the tributary's terminal cohort that END in its
  # target final (like plot_trajectory_lines_3d), not the trunk transfer. The
  # target is the single target in a single-target view, else the tributary's own
  # dominant central. Objects without destinations fall back to the trunk n/cohort.
  has_dest <- !is.null(conf$destinations) && is.data.frame(conf$destinations) &&
    nrow(conf$destinations) > 0
  if (!is.null(label_intermediary)) {
    trb$lab <- vapply(trb$traj_id, function(id) {
      if (label_intermediary == "id") return(id)
      g <- if (single) sub("^tr::", "", target) else central_of[[id]]
      if (has_dest && !is.null(g) && !is.na(g)) {
        dn <- sum(conf$destinations$n[conf$destinations$traj_id == id &
                                      conf$destinations$g_final == g])
        return(paste0(id, " (", dn, ")"))
      }
      ci <- cf_by_child[id]
      if (is.na(ci)) return(sub("^tr::", "", id))
      paste0(id, " (", pr$confluences$n[ci], "/", pr$confluences$cohort_size[ci], ")")
    }, character(1))
  }
  if (nrow(cen) && !is.null(label_terminal) && draw_spines) {
    cen$x <- conf$last_year
    # boxed labels in the plot_trajectory_lines_2d() style (dark fill, white text):
    # a solid background keeps each article count legible, and repel separates them
    # vertically instead of letting the counts overlap at the right edge.
    cen_geom <- if (nrow(cen) > 1 && requireNamespace("ggrepel", quietly = TRUE)) {
      ggrepel::geom_label_repel(
        data = cen, ggplot2::aes(x = .data$x, y = .data$lane, label = .data$lab),
        direction = "y", hjust = 0, nudge_x = 0.6, size = label_terminal_size,
        fontface = "bold", fill = "gray25", colour = "white", label.size = 0,
        alpha = 0.9, segment.size = 0.3, segment.colour = "grey70",
        min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf)
    } else {
      ggplot2::geom_label(
        data = cen, ggplot2::aes(x = .data$x, y = .data$lane, label = .data$lab),
        hjust = 0, nudge_x = 0.4, size = label_terminal_size, fontface = "bold",
        fill = "gray25", colour = "white", linewidth = 0, alpha = 0.9)
    }
    p <- p + cen_geom
  }
  if (nrow(trb) && !is.null(label_intermediary) && draw_ribbons) {
    # anchor each label at the end of its drawn ribbon (the last point the stream
    # sits on its own lane, before bending into the river), so the leader line
    # points straight at the trajectory and the label sits next to it.
    re <- tapply(g$ribbons$x, g$ribbons$traj_id, max)
    trb$x <- as.numeric(re[trb$traj_id])
    trb$x[is.na(trb$x)] <- trb$handoff_year[is.na(trb$x)]
    # nudge each label gently off its stream, outward from the spine (up for
    # tributaries above the centre, down for those below); repel then resolves any
    # remaining overlaps. Kept small so labels stay next to their own trajectory.
    out <- ifelse(trb$lane >= 0, 1, -1)
    lab_geom <- if (requireNamespace("ggrepel", quietly = TRUE)) {
      ggrepel::geom_text_repel(
        data = trb, ggplot2::aes(x = .data$x, y = .data$lane, label = .data$lab),
        direction = "y", hjust = 0, nudge_x = 0.2, nudge_y = out * 0.2,
        size = label_intermediary_size, segment.size = 0.4,
        segment.colour = "grey45", min.segment.length = 0, point.padding = 0.15,
        box.padding = 0.35, max.overlaps = Inf)
    } else {
      ggplot2::geom_text(
        data = trb, ggplot2::aes(x = .data$x, y = .data$lane, label = .data$lab),
        hjust = 0, nudge_x = 0.2, nudge_y = out * 0.2,
        size = label_intermediary_size)
    }
    p <- p + lab_geom
  }

  # optional bottom legend "cNgN: description" (like plot_trajectory_dag), keyed by
  # the central colours present (dark when a single central is coloured per
  # tributary). The legend is driven by an invisible point layer so the
  # identity-filled rivers are untouched.
  if (!is.null(labels_text) && nrow(cen) > 0) {
    ckeys <- cen$central
    desc <- .dag_label_text(ckeys, labels_text)
    leg_lab <- ifelse(desc == ckeys, paste0("tr::", ckeys),
                      paste0("tr::", ckeys, ": ", desc))
    leg_cols <- if (!single) {
      vapply(ckeys, function(k) if (k %in% names(pal)) unname(pal[[k]]) else "grey50",
             character(1))
    } else {
      rep("#2E3440", length(ckeys))
    }
    names(leg_cols) <- ckeys
    leg_df <- data.frame(x = conf$last_year, y = cen$lane, key = ckeys,
                         stringsAsFactors = FALSE)
    guide_args <- list(override.aes = list(alpha = 1, size = 4), title = NULL)
    if (!is.null(legend_ncol)) guide_args$ncol <- legend_ncol
    p <- p +
      ggplot2::geom_point(
        data = leg_df,
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$key), alpha = 0) +
      ggplot2::scale_colour_manual(
        values = leg_cols, breaks = mixed_sort(ckeys),
        labels = function(b) leg_lab[match(b, ckeys)]) +
      ggplot2::guides(colour = do.call(ggplot2::guide_legend, guide_args))
  }

  if (is.null(title)) {
    title <- if (identical(view, "finals")) "Final trajectories"
      else if (identical(view, "intermediary")) "Intermediate trajectories"
      else if (is.null(target)) "Trajectory confluence"
      else paste0("Formation of ", paste(sub("^tr::", "", target), collapse = ", "))
  }
  yr <- if (!is.null(year_range)) range(year_range) else
    c(min(riv$start), conf$last_year)
  x_lo <- yr[1]; x_hi <- yr[2]
  p <- p +
    # extend the time axis to the requested window without dropping any data
    ggplot2::geom_blank(data = data.frame(x = yr, y = c(0, 0)),
                        ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::scale_x_continuous(
      breaks = seq(x_lo, x_hi, by = 2),
      expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::labs(x = "Year", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = if (!is.null(labels_text)) "bottom" else "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = axis_text_size),
      axis.text.y = ggplot2::element_blank())

  if (nrow(pr$dropped) > 0) {
    p <- p + ggplot2::labs(caption = paste0(
      "+ ", nrow(pr$dropped), " tributaries (", sum(pr$dropped$n),
      " papers) below threshold"))
  }
  p
}
