# Visualize 2D Technological Trajectories from Group Evolution

Creates a 2D visualization of technological trajectories based on group
similarity metrics, showing the evolution of research groups over time
with node size representing group importance and color representing
publication-year deviation.

## Usage

``` r
plot_groups_lineage_2d(
  groups_lineage,
  group = "c1g1",
  jaccard_similarity = 0.01,
  prop_tracked_intra_group_treshold = 0.2,
  label_type = "size",
  label_vertical_position = 0,
  label_horizontal_position = 0,
  label_angle = 0,
  time_span = NA,
  show_legend = TRUE,
  color_by = c("py_deviation", "final_group_share")
)
```

## Arguments

- groups_lineage:

  A list with components `groups_similarity` and `groups_attributes`,
  typically produced by
  [`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md).
  The `groups_similarity` element must be a named list of edge tables
  (one per group) with at least `from`, `to`, and `weight`; the
  `groups_attributes` element must be a named list of node tables
  containing, among others, `network_until`, `quantity_papers`,
  `prop_tracked_intra_group`, `tracked_documents`, and `PY.sd`.

- group:

  The specific group to visualize (default: "c1g1").

- jaccard_similarity:

  Minimum Jaccard similarity threshold for connections (default: 0.1).

- prop_tracked_intra_group_treshold:

  Minimum proportion of tracked intra-group documents for nodes to be
  included (default: 0.2).

- label_type:

  Type of labels to display on nodes ("size" for weighted size or "id"
  for group IDs).

- label_vertical_position:

  Kept for backward compatibility; no longer applied, as labels are
  auto-positioned (repelled) with a white halo so they stay readable
  over dark nodes and do not overlap.

- label_horizontal_position:

  Kept for backward compatibility; not applied (see
  `label_vertical_position`).

- label_angle:

  Kept for backward compatibility; not applied (labels are
  auto-positioned).

- time_span:

  Optional vector of years to display; if `NA`, shows all (default:
  `NA`).

- show_legend:

  Logical indicating whether to show the color legend (default: `TRUE`).

- color_by:

  What the node color encodes: `"py_deviation"` (default) colors by the
  average publication-year deviation (`PY.sd`); `"final_group_share"`
  colors each node by the share of its papers that belong to `group`'s
  last-year cluster (a plasma palette, 0-100\\ showing how much of each
  earlier cluster funnels into the final group. The latter requires
  `docs_per_group` in `groups_lineage`.

## Value

A `ggplot2` object visualizing the technological trajectories.

## Details

A descriptive stock view of one group's lineage network. A group's
lineage can share year communities with other groups (soft membership),
so this view makes no trajectory claims – for detection see
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md).

## See also

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
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
# Compute trajectories first
traj_data <- sniff_groups_lineage(groups_cumulative)

# Visualize a specific group (pass the whole object; the function extracts what it needs internally)
plot_groups_lineage_2d(
  groups_lineage = traj_data,
  group = "c1g5",
  jaccard_similarity = 0.3
)
} # }
```
