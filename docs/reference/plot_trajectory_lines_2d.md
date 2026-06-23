# Plot the global trajectories as 2D variable-width lines

The line counterpart of
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)
over the **global** flow DAG. Every global trajectory is drawn as a
variable-width line through the year-aware Sugiyama layout: the central
finals `tr::cNgN` and the intermediate `trN` that feed them, each line
widening with the documents accumulated along its path. A bottom legend
reports, for every intermediate, how many papers of its terminal cohort
reached each final group – a single `trN` can feed more than one final
(its terminal cohort splits across groups), which the legend makes
explicit.

## Usage

``` r
plot_trajectory_lines_2d(
  flow,
  target = "all",
  conf = NULL,
  min_n = 5,
  min_prop = 0.05,
  min_total_size = 0,
  min_duration_years = 0,
  min_target_n = 0,
  width_range = c(0.8, 6),
  use_raw_papers = FALSE,
  label_size = 4,
  dest_min_prop = 0.05,
  lowlight_alpha = 0.18,
  lowlight_color = "#9AA5B1",
  legend_ncol = NULL,
  legend_text_size = NULL,
  axis_text_size = NULL,
  year_range = NULL,
  palette = NULL,
  title = NULL
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
  call (e.g. in a Shiny app that re-renders as a selector changes).
  `NULL` (default) computes it.

- min_n, min_prop, min_total_size, min_duration_years:

  Feeder thresholds, passed to the same pruning used by
  [`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md):
  an intermediate is drawn only if it transfers `>= min_n` papers and
  `>= min_prop` of its cohort, has total size `>= min_total_size` and
  lifespan `>= min_duration_years`; a pruned intermediate takes its own
  feeders with it.

- min_target_n:

  Minimum papers an intermediate must leave in the selected final(s) by
  the last year to be drawn (default `0`, no filter). Unlike `min_n`
  (the tree-edge transfer to its absorber), this counts the
  intermediate's own papers that **end up** in the target group, so a
  feeder that merges in but barely persists there (e.g. `tr39` with 12
  of its papers in `c1g1`) is dropped by `min_target_n = 20`. Only
  applies to a targeted final view.

- width_range:

  Line-width range `c(min, max)` (default `c(0.8, 6)`), scaled by the
  cumulative tracked documents along each trajectory.

- use_raw_papers:

  Scale widths by raw `quantity_papers` (`TRUE`) or by the weighted
  `quantity_papers * prop_tracked_intra_group` (`FALSE`, default).

- label_size:

  Text size of the `trN` / `tr::cNgN` end labels (default `4`).

- dest_min_prop:

  In the `target = "all"` view only, the minimum share of an
  intermediate's terminal cohort that must reach a final for that final
  to appear in its legend entry (default `0.05`). A targeted view
  reports only the selected final(s), so this does not apply.

- lowlight_alpha, lowlight_color:

  Opacity and colour of the background (non-trajectory) edges (defaults
  `0.18`, neutral grey).

- legend_ncol:

  Number of columns in the feeding legend; `NULL` (default) lets ggplot
  choose.

- legend_text_size:

  Font size (pt) of the legend text; `NULL` (default) keeps the theme
  default.

- axis_text_size:

  Font size (pt) of the x-axis (year) tick labels; `NULL` (default)
  keeps the theme default.

- year_range:

  Optional `c(from, to)` years fixing the time axis, so a short
  trajectory can be shown on the same window as a long one (e.g. compare
  a young `tr::c1g14` against `tr::c1g1`). `NULL` (default) fits the
  drawn trajectories; a wider range only extends the axis (it never
  drops data).

- palette:

  Optional named colour vector overriding the default hues (keyed by
  trajectory `traj_id`).

- title:

  Plot title; `NULL` (default) derives one from `target`.

## Value

A `ggplot` object.

## Details

- **x = publication year** (Sugiyama layers); the **y-axis has no
  intrinsic meaning** (a branching position).

- **Line width** grows with cumulative tracked documents along the
  trajectory.

- A single-year **feeder** has no edge to draw as a line and is dropped
  (it would otherwise show a label/legend with no visible line); a
  single-year **central** (the selected final, e.g. a final-year-only
  community like `tr::c1g17`) is kept and drawn as a point.

- **Colour** is per trajectory. With a single targeted final the legend
  is summarised to `trN: p% (n)` – `n` of `trN`'s papers landed in that
  final, which is `p%` of `trN`'s size (the feeders' counts overlap, so
  they do not sum to the final's total). With `target = "all"` it shows
  the full split `trN -> c1g1:n, c1g2:n`. A central is
  `tr::cNgN (final, n)`, where `n` is the documents that **remained** in
  its final-year community (the last point of its size curve), not the
  cumulative documents the trajectory ever held.

## See also

[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md),
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md),
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
[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flow <- sniff_trajectory_braid(docs_per_group)
plot_trajectory_lines_2d(flow, min_n = 20)
plot_trajectory_lines_2d(flow, target = "tr::c1g1")
} # }
```
