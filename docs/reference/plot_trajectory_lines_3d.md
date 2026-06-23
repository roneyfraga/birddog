# Plot the global trajectories as 3D variable-width lines

The 3D counterpart of
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md).
Every global trajectory is drawn over the year-aware Sugiyama layout as
a growing-thickness line: **x = publication year**, **y = route** (the
Sugiyama branching coordinate that separates trajectories), and **z =
cumulative tracked documents** along the path. The central finals
`tr::cNgN` and the intermediate `trN` that feed them share one
colour-per-trajectory scheme; the legend reports, for every
intermediate, how many papers of its terminal cohort reached each final
(a single `trN` can feed more than one final, which the legend makes
explicit). Selection and thresholds are identical to
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md);
only the rendering adds the document z-axis and interactive hover.

## Usage

``` r
plot_trajectory_lines_3d(
  flow,
  target = "all",
  conf = NULL,
  min_n = 5,
  min_prop = 0.05,
  min_total_size = 0,
  min_duration_years = 0,
  min_target_n = 0,
  width_range = c(4, 12),
  use_raw_papers = FALSE,
  log_scale = FALSE,
  label_size = 16,
  hover_font_size = 12,
  dest_min_prop = 0.05,
  lowlight_alpha = 0.75,
  lowlight_color = "#9AA5B1",
  lowlight_width = 3,
  show_background = TRUE,
  year_range = NULL,
  palette = NULL,
  title = NULL,
  legend_position = c("bottom", "right", "top", "left", "none"),
  legend_ncol = NULL,
  labels_text = NULL
)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object, or a `docs_per_group` tibble /
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  object the flow is built from.

- target:

  What to draw. `"all"` (default) draws every central and the
  trajectories feeding it; a central `traj_id` (e.g. `"tr::c1g1"`) or a
  vector of them draws only those finals and their feeders.

- conf:

  An optional precomputed
  [`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)
  object for `flow`; pass it to skip recomputing the confluence on every
  call. `NULL` (default) computes it.

- min_n, min_prop, min_total_size, min_duration_years:

  Feeder thresholds, passed to the same pruning used by
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md):
  an intermediate is drawn only if it transfers `>= min_n` papers and
  `>= min_prop` of its cohort, has total size `>= min_total_size` and
  lifespan `>= min_duration_years`; a pruned intermediate takes its own
  feeders with it.

- min_target_n:

  Minimum papers an intermediate must leave in the selected final(s) by
  the last year to be drawn (default `0`, no filter). Only applies to a
  targeted final view.

- width_range:

  Line-width range `c(min, max)` (default `c(4, 12)`), scaled per
  trajectory by its cumulative tracked documents.

- use_raw_papers:

  Scale z and width by raw `quantity_papers` (`TRUE`) or by the weighted
  `quantity_papers * prop_tracked_intra_group` (`FALSE`, default).

- log_scale:

  Apply [`log1p()`](https://rdrr.io/r/base/Log.html) to the z-axis
  (cumulative documents), which keeps large trajectories from dwarfing
  small ones (default `FALSE`).

- label_size:

  Font size of the `trN` / `tr::cNgN` end labels (default `16`).

- hover_font_size:

  Font size for hover tooltips (default `12`).

- dest_min_prop:

  In the `target = "all"` and single-target views, the minimum share of
  an intermediate's terminal cohort that must reach a final for that
  final to appear in its legend split (default `0.05`). In the
  single-target view the targeted final is always kept regardless of
  this threshold.

- lowlight_alpha, lowlight_color, lowlight_width:

  Opacity, colour and width of the light-grey links between
  trajectories.

- show_background:

  Draw the light-grey links between trajectories – the edges of the DAG
  that are not on any single trajectory's path (the hand-offs and
  forks), at the nodes' real heights, as in
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)
  (default `TRUE`).

- year_range:

  Optional `c(from, to)` years fixing the x-axis range, so a short
  trajectory can be shown on the same window as a long one. `NULL`
  (default) fits the drawn trajectories.

- palette:

  Optional named colour vector overriding the default hues (keyed by
  trajectory `traj_id`).

- title:

  Plot title; `NULL` (default) derives one from `target`.

- legend_position:

  Where to place the legend: `"bottom"` (default, matching
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)),
  `"right"`, `"top"`, `"left"`, or `"none"` to hide it.

- legend_ncol:

  Number of columns in the legend (default `NULL`, plotly chooses).
  Applies to the horizontal legends (`"bottom"` / `"top"`): each entry
  is given `1 / legend_ncol` of the width, so `legend_ncol` entries fit
  per row. Ignored for the vertical legends (`"right"` / `"left"`).

- labels_text:

  Optional `data.frame` with columns `id` and `text` mapping group ids
  (e.g. `"c1g1"`) to descriptions, as in
  [`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md).
  When supplied, hovering a trajectory's nodes shows the description of
  its group beneath the feeding legend. `NULL` (default) shows the bare
  group ids.

## Value

A plotly 3D plot object.

## Details

- **z = cumulative tracked documents** along each trajectory (raw or
  weighted), accumulated node by node; `log_scale = TRUE` compresses it.

- **Line width** grows per trajectory with the same cumulative measure,
  so each line is thinnest at its birth and thickest at its terminal
  node (the width is rescaled within each trajectory, matching
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)).

- A single-year **central** (e.g. a final-year-only community like
  `tr::c1g17`) has no segment to draw and is rendered as a terminal
  ball; a single-year **feeder** is dropped (it would show a label with
  no visible line).

- **Colour** is per trajectory and the legend carries the feeding split:
  a central reads `tr::cNgN (final, n)` (n = documents that remained in
  its final-year community), an intermediate reads
  `trN -> c1g1:n, c1g2:n` (its terminal cohort split across finals). In
  a single-target view it reads `trN: p% (n) -> c1g15:n, c1g1:n, ...` –
  `p%` and the parenthesised `(n)` are the share and the document count
  that reached the target (as in
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md));
  the split then shows the feeder's full destination, so a feeder of
  `c1g1` reveals that more of it may flow elsewhere.

- **Hovering a node** shows that same feeding-legend text, the
  trajectory's `labels_text` description (when supplied), the node's
  year, and the absolute documents in that cluster-year
  (`quantity_papers`, not the running cumulative that sets the height).

## See also

[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md),
[`plot_groups_lineage_3d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_3d.md),
[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md),
[`plot_groups_map_animation()`](https://roneyfraga.com/birddog/reference/plot_groups_map_animation.md),
[`plot_groups_map_interactive()`](https://roneyfraga.com/birddog/reference/plot_groups_map_interactive.md),
[`plot_groups_per_year()`](https://roneyfraga.com/birddog/reference/plot_groups_per_year.md),
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md),
[`plot_trajectory_confluence_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence_interactive.md),
[`plot_trajectory_confluence_matrix()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence_matrix.md),
[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md),
[`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md),
[`plot_trajectory_dispersion()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dispersion.md),
[`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md),
[`plot_trajectory_dynamics_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics_interactive.md),
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flow <- sniff_trajectory_braid(docs_per_group)
plot_trajectory_lines_3d(flow, min_n = 20)
plot_trajectory_lines_3d(flow, target = "tr::c1g1")
} # }
```
