# Plot the group-influence network (the net-influence spine)

*Experimental.* Draws the directed influence backbone from a
[`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md)
object as a node-link graph: an arrow runs from the **source** (the
cited, more foundational group) to the **recipient** (the citing group).
By default the edges are the net flow `$net` (\\\nu\_{ij} = C\_{ij} -
C\_{ji}\\), the "who, on balance, leads whom" spine; nodes are coloured
by role (source / broker / sink) and sized by citation activity.

## Usage

``` r
plot_groups_influence_network(
  influence,
  weight = c("net", "gross", "surprise"),
  min_weight = 0,
  node_size = c("equal", "io", "balance"),
  colour_role = TRUE,
  edge_labels = TRUE,
  edge_digits = NULL,
  labels = TRUE,
  label_size = NULL,
  layout = "sugiyama",
  title = NULL
)
```

## Arguments

- influence:

  A
  [`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md)
  object.

- weight:

  Which edges and weights to draw: `"net"` (default, the net-flow spine
  `$net`, one arrow per connected pair), `"gross"` (every directed
  channel, weighted by citation count), or `"surprise"` (every directed
  channel, weighted by the size-null surprise).

- min_weight:

  Drop edges whose weight is below this (default `0`).

- node_size:

  How to size the group nodes: `"equal"` (default), `"io"` (citations
  made plus received) or `"balance"` (the absolute balance
  \\\|\beta\|\\).

- colour_role:

  Colour nodes by their source / broker / sink role (default `TRUE`).

- edge_labels:

  Print the weight on each edge (default `TRUE`).

- edge_digits:

  Decimal places for the edge weight labels; `NULL` (default) shows
  integers for `"net"`/`"gross"` and two decimals for the continuous
  `"surprise"` weight.

- labels:

  Print the group label on each node (default `TRUE`).

- label_size:

  Font size of the node labels; `NULL` (default) auto-sizes the nodes
  and the text so even the smallest node holds its group id.

- layout:

  A `ggraph`/`igraph` layout name (default `"sugiyama"`, the layered DAG
  layout).

- title:

  Plot title; `NULL` (default) removes it.

## Value

A `ggplot` object.

## See also

[`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md),
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md)

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
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
plot_groups_influence_network(infl)
plot_groups_influence_network(infl, weight = "gross", min_weight = 50)
} # }
```
