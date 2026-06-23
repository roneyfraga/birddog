# Plot the directed group-influence matrix (heatmap)

*Experimental.* The directed counterpart of
[`plot_trajectory_confluence_matrix()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence_matrix.md):
a heatmap of the cross-citation channels from a
[`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md)
object. **Rows are the citing group** ("is influenced by") and **columns
the cited group** ("the influencer"), so a cell and its mirror across
the diagonal are the two directions of one pair. Confluence is
symmetric; influence is not, and that broken symmetry is the
bidirectional relationship.

## Usage

``` r
plot_groups_influence_matrix(
  influence,
  fill = c("surprise", "raw", "debt", "audience", "salton"),
  diagonal = c("mute", "show", "drop"),
  show_values = TRUE,
  label_size = 4,
  order_by = c("group", "balance"),
  axis_text_size = NULL,
  axis_title_size = NULL,
  x_angle = 0,
  legend_position = "right",
  bg = "white",
  title = NULL
)
```

## Arguments

- influence:

  A
  [`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md)
  object.

- fill:

  Which value to colour by: `"surprise"` (default, the flow against the
  size null, on a diverging scale centred at `1`), `"raw"` (citation
  counts), `"debt"`, `"audience"` or `"salton"`.

- diagonal:

  How to treat the intra-group cohesion cells: `"mute"` (default, drawn
  grey, off the colour scale, so the between-group channels keep the
  contrast), `"show"` (on the scale like any cell) or `"drop"` (not
  drawn).

- show_values:

  Print the value inside each cell (default `TRUE`).

- label_size:

  Font size of the in-cell values (default `4`).

- order_by:

  Group order on both axes: `"group"` (default,
  [`mixed_sort()`](https://roneyfraga.com/birddog/reference/mixed_sort.md))
  or `"balance"` (sources first, sinks last).

- axis_text_size, axis_title_size:

  Font sizes of the tick labels and axis titles; `NULL` (default) keeps
  the theme defaults.

- x_angle:

  Rotation in degrees of the column (cited-group) labels on the top
  axis. Default `0` (horizontal); `90` stacks the `cNgN` ids vertically
  so they stay legible on a small figure with many groups.

- legend_position:

  Legend placement: `"right"` (default), `"left"`, `"top"`, `"bottom"`
  or `"none"`.

- bg:

  Panel/plot background colour (default `"white"`).

- title:

  Plot title; `NULL` (default) removes it.

## Value

A `ggplot` object.

## See also

[`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_trajectory_confluence_matrix()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence_matrix.md)

Other visualization:
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
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
infl <- sniff_groups_influence(groups)
plot_groups_influence_matrix(infl)
plot_groups_influence_matrix(infl, fill = "debt", diagonal = "drop")
} # }
```
