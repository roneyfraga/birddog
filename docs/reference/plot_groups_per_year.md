# Plot groups per year: the clustering fan converging to the final groups

The stock-side bridge view. One column per year; within a column the
cluster-year nodes are ranked by paper count with the largest at the
baseline, dot area encodes paper count, and (optionally) the number of
groups that year is printed atop the column. No succession edges, no
birth rings – it shows the annual + cumulative clustering fan settling
onto the final-year groups. It is the stock-side counterpart of the
flow-side
[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md),
and reads the
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
object directly.

## Usage

``` r
plot_groups_per_year(
  dag,
  color_terminal = FALSE,
  point_size = 6,
  show_count = TRUE,
  count_size = 3.2,
  count_colour = "white",
  count_fill = "grey45",
  label_terminal = TRUE,
  label_size = 3,
  axis_text_size = NULL,
  title = "Groups per year"
)
```

## Arguments

- dag:

  A
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  object.

- color_terminal:

  Colour the dots by `terminal_group` to foreshadow the braid (default
  `FALSE`, a single neutral grey that reads as stock). The colour legend
  is suppressed either way.

- point_size:

  Upper bound of the dot-size range; paper count is mapped to
  `[1, point_size]` (default `6`).

- show_count:

  Print the number of groups atop each year column, on a filled label
  for emphasis (default `TRUE`).

- count_size:

  Font size of the per-year group-count text (default `3.2`).

- count_colour:

  Font colour of the count text (default `"white"`).

- count_fill:

  Background colour of the count label (default `"grey45"`, a grey chip
  that makes the count stand out); use `"white"` for a plain box.

- label_terminal:

  Label the final-year nodes with their group id (`c1gN`) to the right
  of the last column, with a leader line (default `TRUE`).

- label_size:

  Font size of the final-node labels (default `3`).

- axis_text_size:

  Font size of the x-axis (year) tick labels; `NULL` (default) keeps the
  theme default.

- title:

  Plot title (default `"Groups per year"`); `NULL` for none.

## Value

A `ggplot` object.

## Details

The visual encoding:

- **One column per year** along the **x-axis**; the **y-axis has no
  intrinsic meaning** (nodes are stacked by rank).

- Within a column the cluster-year nodes are **ranked by paper count**
  with the largest at the baseline, and **dot area encodes the paper
  count**.

- With `show_count = TRUE` the **number of groups that year** is printed
  atop the column on a filled label (`count_fill` / `count_colour`).

- There are **no succession edges and no births**: this is the
  stock-side view of how many groups exist each year and how large they
  are, settling onto the final-year groups. For the flow (edges,
  trajectories) use
  [`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md).

- With `label_terminal = TRUE` the final-year nodes are labelled `c1gN`
  to the right of the last column.

## See also

[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md),
[`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md)

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md),
[`plot_groups_lineage_3d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_3d.md),
[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md),
[`plot_groups_map_animation()`](https://roneyfraga.com/birddog/reference/plot_groups_map_animation.md),
[`plot_groups_map_interactive()`](https://roneyfraga.com/birddog/reference/plot_groups_map_interactive.md),
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
# docs_per_group comes from sniff_groups_lineage()
dag <- sniff_trajectory_dag(docs_per_group)

plot_groups_per_year(dag)

# colour the dots by destination, bigger count chips and year labels
plot_groups_per_year(dag, color_terminal = TRUE, count_size = 5, axis_text_size = 14)
} # }
```
