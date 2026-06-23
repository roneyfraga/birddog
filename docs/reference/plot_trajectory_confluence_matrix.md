# Plot the trajectory confluence matrix (finals vs intermediates)

A heatmap of how each intermediate (absorbed) trajectory's papers spread
across the final trajectories. By default **rows are the final
trajectories** `tr::cNgN` and **columns are the intermediate
trajectories** `trN` (swap with `orientation`); the fill is the coupling
strength (shared documents) and each intermediate's dominant-final cell
is outlined. Which intermediates appear is decided by the same pruning
the other flow plots use
([`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)),
so the column set is consistent across views.

## Usage

``` r
plot_trajectory_confluence_matrix(
  flow,
  conf = NULL,
  fill = c("n_shared", "prop_of_group", "prop_of_traj"),
  orientation = c("finals-cols", "finals-rows"),
  order_by = c("terminal", "reach", "size", "alpha", "cluster"),
  min_n = 5,
  min_prop = 0.05,
  min_total_size = 0,
  min_duration_years = 0,
  min_target_n = 0,
  mark_terminal = TRUE,
  show_values = FALSE,
  label_size = 2.5,
  axis_text_size = NULL,
  axis_title_size = NULL,
  legend_position = "right",
  legend_text_size = NULL,
  bg = "white",
  title = "Trajectory matrix"
)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- conf:

  An optional precomputed
  [`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)
  object for `flow`; `NULL` (default) computes it.

- fill:

  `"n_shared"` (default, log-scaled), `"prop_of_group"`, or
  `"prop_of_traj"`.

- orientation:

  `"finals-cols"` (default): final trajectories `tr::cNgN` on the rows
  (y-axis), intermediate trajectories `trN` on the columns (x-axis) –
  the "finals (rows) x intermediates (cols)" matrix. `"finals-rows"` is
  the transpose (finals on the columns, intermediates on the rows).

- order_by:

  How the intermediate trajectories are ordered along their axis:
  `"terminal"` (default, grouped by dominant final then reach),
  `"reach"` (total shared documents), `"size"` (trajectory size at
  handoff), `"alpha"` (alphabetical), or `"cluster"` (seriation that
  reorders **both** axes by hierarchical clustering – intermediates by
  their destination profile and finals by their source profile – so
  alike rows and columns form blocks).

- min_n, min_prop, min_total_size, min_duration_years:

  Thresholds selecting which intermediate trajectories are shown, passed
  to the same prune as
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md):
  an intermediate is kept only if it transfers `>= min_n` papers and
  `>= min_prop` of its cohort, has total size `>= min_total_size` and
  lifespan `>= min_duration_years`.

- min_target_n:

  Minimum shared documents a cell must have to be drawn (default `0`,
  all cells); raise it to hide the long tail of small overlaps. Cells
  with zero shared documents are never drawn. Final trajectories are
  always kept on their axis, even when the thresholds leave them with no
  cell.

- mark_terminal:

  Outline each intermediate's dominant-final cell (default `TRUE`).

- show_values:

  Print the value inside each cell (default `FALSE`). The text colour
  auto-contrasts (dark on light cells, light on dark) for legibility.

- label_size:

  Font size of the in-cell values (default `2.5`).

- axis_text_size:

  Font size of the axis (trajectory id) tick labels; `NULL` (default)
  keeps the theme default.

- axis_title_size:

  Font size of the axis titles ("Final Trajectory" / "Intermediate
  Trajectory"); `NULL` (default) keeps the theme default.

- legend_position:

  Where to put the colourbar: `"right"` (default), `"left"`, `"top"`,
  `"bottom"`, `"none"`, or a `c(x, y)` coordinate inside the panel (each
  in `0..1`). `"top"`/`"bottom"` draw the colourbar horizontally.

- legend_text_size:

  Font size of the legend text; `NULL` (default) keeps the theme
  default.

- bg:

  Panel/plot background colour (default `"white"`). A darker background
  (e.g. `"grey20"`) raises contrast against the bright cells; the grid
  lines and the terminal outline auto-adapt to its luminance.

- title:

  Plot title (default `"Trajectory matrix"`); `NULL` removes it.

## Value

A `ggplot` object.

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
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
[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md),
[`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md),
[`plot_trajectory_dispersion()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dispersion.md),
[`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md),
[`plot_trajectory_dynamics_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics_interactive.md),
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flow <- sniff_trajectory_braid(docs_per_group)
plot_trajectory_confluence_matrix(flow, min_n = 20, show_values = TRUE)
plot_trajectory_confluence_matrix(flow, order_by = "cluster", bg = "grey20")
} # }
```
