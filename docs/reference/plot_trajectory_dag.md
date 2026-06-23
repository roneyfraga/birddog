# Plot the soft trajectory DAG (all intermediate nodes)

The braided-river panorama: every cluster-year node laid out by year
(x), with succession edges, node colour = `terminal_group` (or `group`),
node size = paper count, and births ringed. Uses the year-aware Sugiyama
layout from
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)'s
graph. Nodes from dormant lineages that never reach the final year (a
colour value outside the final groups) are drawn grey and kept out of
the colour legend.

## Usage

``` r
plot_trajectory_dag(
  dag,
  color_by = c("terminal_group", "group"),
  edge_alpha = 0.25,
  point_size = 2.5,
  show_legend = FALSE,
  label_terminal = TRUE,
  labels_text = NULL,
  label_size = 3,
  title = "Cumulative network groups",
  legend_text_size = NULL,
  legend_ncol = NULL,
  axis_text_size = NULL
)
```

## Arguments

- dag:

  A
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  object.

- color_by:

  How to colour the nodes (default `"terminal_group"`).
  `"terminal_group"` uses the final group each node's lineage flows into
  (its heaviest-successor destination), so a whole trajectory keeps one
  colour from source to mouth and the colour legend has one entry per
  final group. `"group"` uses the node's own per-year id (`c1gN`), which
  is a within-year size rank rather than a stable identity, so the same
  trajectory changes colour across years. Pick `"terminal_group"` to
  read how the final groups formed, `"group"` to inspect the raw annual
  clustering labels.

- edge_alpha:

  Base edge transparency, scaled by Jaccard weight (default 0.25).

- point_size:

  Base node size (default 2.5).

- show_legend:

  Show the colour legend (default `FALSE`; many final groups make a busy
  legend).

- label_terminal:

  Label the final-year nodes with their bare group id (`c1gN`), to the
  right of each vertex, with a leader line back to the vertex (default
  `TRUE`).

- labels_text:

  Optional `data.frame` with columns `id` and `text`. When supplied, the
  node-colour legend moves to the bottom and each group is relabelled
  `"c1gN: text"` (groups without a match keep the bare id); `NULL`
  (default) leaves the legend governed by `show_legend`. The final-node
  labels (`label_terminal`) are always the bare group id, independent of
  this.

- label_size:

  Font size of the final-node labels (default `3`).

- title:

  Plot title (default `"Cumulative network groups"`); `NULL` for no
  title.

- legend_text_size:

  Font size of the colour-legend text; `NULL` (default) keeps the theme
  default.

- legend_ncol:

  Number of columns in the colour legend; `NULL` (default) lets ggplot
  choose.

- axis_text_size:

  Font size of the x-axis (year) tick labels; `NULL` (default) keeps the
  theme default.

## Value

A `ggplot` object.

## Details

The visual encoding:

- The **x-axis is the publication year** (a Sugiyama layered layout);
  the **y-axis has no intrinsic meaning**, it only spreads nodes to
  reduce edge crossings.

- **Node colour** is the trajectory basin (see `color_by`), **node
  size** is the paper count, and **succession edges** fade with their
  Jaccard weight.

- **Births** (nodes with no strong predecessor) are ringed in black.

- **Dormant lineages** that never reach the final year are drawn grey
  and kept out of the colour legend, so the colours stand for the final
  groups only.

- With `label_terminal = TRUE` the final-year nodes are labelled `c1gN`
  to the right (with a leader line); with `labels_text` the colour
  legend moves to the bottom as `"c1gN: description"` entries.

## See also

[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`plot_groups_per_year()`](https://roneyfraga.com/birddog/reference/plot_groups_per_year.md),
[`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md)

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

# the whole braid, coloured by destination group
plot_trajectory_dag(dag)

# human-readable bottom legend, two columns, larger year labels
descr <- data.frame(id = c("c1g1", "c1g2"), text = c("Topic A", "Topic B"))
plot_trajectory_dag(dag, labels_text = descr, legend_ncol = 2, axis_text_size = 14)
} # }
```
