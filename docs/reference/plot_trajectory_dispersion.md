# Plot where a stagnant trajectory's cohort dispersed (its dispersion fan)

Draws the selected intermediate (stagnant) trajectory as a short spine
on the left and fans out, from its handoff year, one curved link to
**every final trajectory its terminal cohort reached** (the full
`destination` split), each link's width scaled by the papers that landed
there and labelled `tr::cNgN (n)`. It answers, visually, "where did this
trajectory go?" – not just the dominant absorber but the complete
dispersion. Styling follows the other flow plots
([`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)):
a Set2/hue palette keyed by final group, a year-aware x-axis, and the
shared `year_range` / `axis_text_size` / `palette` controls.

## Usage

``` r
plot_trajectory_dispersion(
  destination,
  title = NULL,
  label_size = 4.5,
  year_range = NULL,
  axis_text_size = NULL,
  palette = NULL,
  min_n = 0
)
```

## Arguments

- destination:

  Output of
  [`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md).
  Uses its `destination` table (`g_final`, `n`) and `source_info`.

- title:

  Plot title. Defaults to a `source -> N finals` summary.

- label_size:

  Text size for the in-plot labels (default: 4.5).

- year_range:

  Optional `c(from, to)` years fixing the time axis, as in
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md).
  `NULL` (default) fits the drawn data; a wider range only extends the
  axis.

- axis_text_size:

  Font size of the x-axis (year) tick labels; `NULL` (default) keeps the
  built-in size.

- palette:

  Optional named colour vector overriding the default hues (keyed by
  final group `cNgN`). `NULL` (default) uses the package's Set2 (\<= 8)
  / hue scheme.

- min_n:

  Minimum papers a final must receive to be drawn (default `0`, all
  impacted finals). Raise it to hide the long tail of tiny destinations.

## Value

A `ggplot` object.

## See also

[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)

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
[`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md),
[`plot_trajectory_dynamics_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics_interactive.md),
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flow <- sniff_trajectory_braid(docs_per_group)
d <- sniff_trajectory_destination(flow, "tr1")
plot_trajectory_dispersion(d)
} # }
```
