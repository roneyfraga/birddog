# Visualize 3D Technological Trajectories from Group Evolution

Creates an interactive 3D visualization of technological trajectories
showing the evolution of research groups over time with node size
representing group importance and color representing publication year
deviation.

## Usage

``` r
plot_groups_lineage_3d(
  groups_lineage,
  group = "component1_g01",
  jaccard_similarity = 0.1,
  prop_tracked_intra_group_treshold = 0.2,
  label_type = "size",
  label_vertical_position = 0,
  label_horizontal_position = 0,
  label_angle = 0,
  time_span = NA,
  show_legend = TRUE,
  last_year_keywords = NULL,
  log_scale = FALSE
)
```

## Arguments

- groups_lineage:

  A list containing two components:

  - groups_similarity: Similarity data between groups

  - groups_attributes: Attribute data for each group

- group:

  The specific group to visualize (default: "component1_g01")

- jaccard_similarity:

  Minimum Jaccard similarity threshold for connections (default: 0.1)

- prop_tracked_intra_group_treshold:

  Minimum proportion of tracked intra-group documents for nodes to be
  included (default: 0.2)

- label_type:

  Type of labels to display on nodes ("size" for weighted size or "id"
  for group IDs)

- label_vertical_position:

  Vertical adjustment for node labels (default: 0)

- label_horizontal_position:

  Horizontal adjustment for node labels (default: 0)

- label_angle:

  Angle for node labels (default: 0)

- time_span:

  Optional vector specifying the time span to display (default: NA shows
  all years)

- show_legend:

  Logical indicating whether to show the color legend (default: TRUE)

- last_year_keywords:

  Optional keywords description for the last year (default: NULL)

- log_scale:

  Whether to apply log transformation to the z-axis size values
  (default: FALSE). Uses [`log1p()`](https://rdrr.io/r/base/Log.html)
  (i.e., log(1 + x)) to compress large differences between node sizes.

## Value

A plotly 3D visualization object

## Details

A descriptive stock view of one group's lineage network. A group's
lineage can share year communities with other groups (soft membership),
so this view makes no trajectory claims – for detection see
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md).

## See also

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md),
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
# First get trajectory data
traj_data <- sniff_groups_lineage(groups_cumulative)

# Visualize a specific group in 3D
plot_groups_lineage_3d(
  groups_lineage = traj_data,
  group = "component1_g05",
  jaccard_similarity = 0.2
)
} # }
```
