# Plot the trajectories that fed into a target (confluence timeline)

Draws the target trajectory as a central spine and each feeder as a
parallel timeline that grows along its length (see `feeder_curve`) and
merges into the spine at its handoff year. The spine is a cumulative
river: it widens smoothly as each drawn feeder joins, so by the final
year its width reflects the total papers absorbed. Spine and feeder
widths share one strictly proportional scale (width 0 at zero papers),
so their thickness is directly comparable. It generalises
[`plot_trajectory_dispersion()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dispersion.md)
(one source into one absorber) to many feeders converging on one
trajectory, answering visually "which trajectories formed this one?".

## Usage

``` r
plot_trajectory_formation(
  formation,
  feeder_curve = c("inflow", "size"),
  max_feeders = 8,
  title = NULL,
  label_size = 4.5,
  year_range = NULL,
  axis_text_size = NULL,
  palette = NULL
)
```

## Arguments

- formation:

  Output of
  [`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md).

- feeder_curve:

  What the feeder's growing width traces: `"inflow"` (default) is the
  cumulative arrival of just the papers that entered the target, a
  monotone curve ending at `n` on the river's scale; `"size"` is the
  feeder's measured cluster size per year, ending at `cohort_size` (its
  whole community, which can dwarf and shrink the river since the 2D
  scale is linear). Each feeder rises from zero at its start year to
  this end value, on the same proportional scale as the spine, so it
  grows like the target does.

- max_feeders:

  Maximum number of feeders to draw, taken by descending papers
  (default: 8). Feeders beyond this are summarised in the caption.

- title:

  Plot title. Defaults to the target trajectory key.

- label_size:

  Text size for the feeder and target labels (default: 4.5).

- year_range:

  Optional `c(from, to)` years fixing the time axis, so a short
  formation can be shown on the same window as a long one (as in
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)).
  `NULL` (default) fits the drawn data; a wider range only extends the
  axis (it never drops data).

- axis_text_size:

  Font size of the x-axis (year) tick labels; `NULL` (default) keeps the
  built-in size.

- palette:

  Optional named colour vector overriding the default hues (keyed by
  feeder `source_key`), to match colours across plots. `NULL` (default)
  uses the package's Set2 (\<= 8 feeders) / hue scheme.

## Value

A `ggplot` object.

## See also

[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_dispersion()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dispersion.md)

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
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flow <- sniff_trajectory_braid(groups_lineage)
f <- sniff_trajectory_formation(flow, target = "tr::c1g3")
plot_trajectory_formation(f)
} # }
```
