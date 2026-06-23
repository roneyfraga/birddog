# Strategic map of trajectory dynamic states

A quadrant scatter of
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
output, one figure per population, designed to make the dynamic states
legible at a glance. Each point is a trajectory, its area the community
size, its colour the state; the regions are shaded and named after the
state they classify, with the thresholds drawn as reference lines.

## Usage

``` r
plot_trajectory_dynamics(
  dyn,
  target = c("finals", "intermediary"),
  thresholds = NULL,
  labels_text = NULL,
  label_id = FALSE,
  size_range = c(2, 14),
  label_size = 3.5,
  axis_text_size = NULL,
  palette = NULL,
  show_thresholds = TRUE,
  show_quadrants = TRUE,
  xlim = NULL,
  ylim = NULL,
  legend_position = "right",
  title = NULL
)
```

## Arguments

- dyn:

  A
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  tibble.

- target:

  What to draw, following
  [`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md).
  `"finals"` (default) maps the **living (central)** trajectories on a
  life-cycle plane (novelty x growth) that separates emergence, maturity
  and dormancy. `"intermediary"` maps the **declining (absorbed)**
  trajectories on a destination plane (entropy x dormant share) that
  separates convergence, divergence and dormancy. The two populations
  live on different axes, so there is no combined view.

- thresholds:

  The
  [`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)-shaped
  list whose cut values bound the shaded regions and the reference
  lines. `NULL` (default) reuses the cuts the classification was built
  with – `attr(dyn, "state_thresholds")`, attached by
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  – so regions and colours always agree; if that attribute is absent it
  derives them from `dyn` via
  [`data_state_thresholds()`](https://roneyfraga.com/birddog/reference/data_state_thresholds.md).
  Pass an explicit list to override.

- labels_text:

  Optional `data.frame` with columns `id` and `text` mapping group ids
  (`cNgN`) to descriptions; used to label the living cores in the
  `"finals"` view (as in
  [`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md)).
  `NULL` (default) labels by id.

- label_id:

  Prefix each living-core label with its group id, as
  `cNgN: description` (default `FALSE`, description only). No effect
  when `labels_text` is `NULL` (the label is already the id) or in the
  `"intermediary"` view (labelled by `traj_id`).

- size_range:

  Point-area range `c(min, max)` passed to
  [`ggplot2::scale_size()`](https://ggplot2.tidyverse.org/reference/scale_size.html)
  (default `c(2, 14)`).

- label_size:

  Text size of the point labels (default `3.5`); the region labels are
  drawn one point larger.

- axis_text_size:

  Font size of the axis tick labels; `NULL` (default) keeps the theme
  default.

- palette:

  Optional named colour vector overriding the default semantic hues,
  keyed by state (`emergence`/`maturity`/`dormancy` for `"finals"`,
  `convergence`/`divergence`/`dormancy` for `"intermediary"`).

- show_thresholds:

  Draw the classification cut lines (default `TRUE`).

- show_quadrants:

  Shade and name the state regions (default `TRUE`).

- xlim, ylim:

  Optional `c(lo, hi)` to zoom the axes via
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
  without dropping data from the fit; `NULL` (default) expands the view
  to keep every state region visible (the data, the threshold cuts, and
  the `[0,1]` share bounds, with padding), so the bands are shown even
  when the points cluster. Pass a range to override, e.g. to zoom past a
  single young core's outlying `growth_rate`.

- legend_position:

  Legend placement (default `"right"`).

- title:

  Plot title; `NULL` (default) derives one from `target`.

## Value

A `ggplot` object.

## Details

- **`"finals"`** – x = `novelty`, y = `growth_rate`; emergence is the
  novel-and-growing region, dormancy the loss-of-momentum band, maturity
  the saturated middle.

- **`"intermediary"`** – x = `dest_entropy`, y = `dormant_share`;
  convergence is the low-entropy (single destination) side, divergence
  the high-entropy side, and dormancy the high-dropout band (rare, so
  this band is usually sparse). Extinct (group `NA`) lineages have no
  coordinates and are reported in the caption rather than drawn.

## See also

[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md),
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)

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
[`plot_trajectory_dynamics_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics_interactive.md),
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flow <- sniff_trajectory_braid(docs_per_group)
dyn  <- sniff_trajectory_dynamics(flow)
descr <- data.frame(id = "c1g1", text = "Anaerobic digestion")

plot_trajectory_dynamics(dyn, target = "finals", labels_text = descr)
plot_trajectory_dynamics(dyn, target = "intermediary")
} # }
```
