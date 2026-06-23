# Interactive trajectory DAG (plotly): hover a node for its trajectory

The plotly twin of
[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md).
Nodes are laid out by the same year-aware Sugiyama layout, sized by
paper count, and coloured by the final group their lineage reaches.
Dormant lineages that never reach the final year are drawn grey. There
is no legend: hovering a final-year node shows its group id and
`labels_text` description (`c1gN: description`), and hovering any other
(intermediate or dormant) node shows its raw id (e.g. `y2021c1g2`). When
`label_terminal` is `TRUE`, the final group id (`c1gN`) is also printed
to the right of each final-year node. Returns a `plotly` htmlwidget.

## Usage

``` r
plot_trajectory_dag_interactive(
  dag,
  labels_text = NULL,
  marker_range = c(4, 14),
  edge_alpha = 0.3,
  label_terminal = TRUE,
  label_size = 12,
  title = "Cumulative network groups"
)
```

## Arguments

- dag:

  A
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  object.

- labels_text:

  Optional `data.frame` with columns `id` and `text` mapping final group
  ids to descriptions, shown after the group id when hovering a
  final-year node (`c1gN: description`); `NULL` (default) falls back to
  the bare group id.

- marker_range:

  Min/max marker size in pixels; paper count is scaled into this range
  (default `c(4, 14)`).

- edge_alpha:

  Edge opacity, `0`-`1` (default `0.3`).

- label_terminal:

  Show the final group id (`c1gN`) to the right of each final-year node
  (default `TRUE`).

- label_size:

  Font size (px) of the final-node labels (default `12`).

- title:

  Plot title (default `"Cumulative network groups"`); `NULL` for none.

## Value

A `plotly` htmlwidget.

## Details

The interactive twin of
[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md),
built with plotly:

- The **x-axis is the publication year** (Sugiyama layout); the **y-axis
  has no intrinsic meaning**.

- **Node colour** is the trajectory basin (the final group reached);
  **dormant lineages** that never reach the final year are grey. **Node
  size** scales with paper count over `marker_range`.

- **There is no legend.** Hovering a **final-year node** shows
  `c1gN: description` (its group id plus the `labels_text` description);
  hovering any **other node** shows its raw id (`yYYYYcXgN`).

- **Edges** are a single faint layer (`edge_alpha`) and are excluded
  from hover. With `label_terminal = TRUE` the final group ids (`c1gN`)
  are printed in a de-overlapped column to the right, each joined to its
  node by a leader line.

The widget renders in the RStudio Viewer, embeds in Quarto/Shiny, or
saves to a standalone file with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).

## See also

[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md),
[`plot_groups_per_year()`](https://roneyfraga.com/birddog/reference/plot_groups_per_year.md)

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

plot_trajectory_dag_interactive(dag)

# hover the final nodes for descriptions; fainter edges
descr <- data.frame(id = c("c1g1", "c1g2"), text = c("Topic A", "Topic B"))
p <- plot_trajectory_dag_interactive(dag, labels_text = descr, edge_alpha = 0.2)
# htmlwidgets::saveWidget(p, "trajectory_dag.html", selfcontained = TRUE)
} # }
```
