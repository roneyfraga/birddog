# Visualize 2D Technological Trajectories from Group Evolution

Creates a 2D visualization of technological trajectories based on group
similarity metrics, showing the evolution of research groups over time
with node size representing group importance and color representing
publication-year deviation.

## Usage

``` r
plot_group_trajectories_2d(
  groups_cumulative_trajectories,
  group = "c1g1",
  jaccard_similarity = 0.01,
  prop_tracked_intra_group_treshold = 0.2,
  label_type = "size",
  label_vertical_position = 0,
  label_horizontal_position = 0,
  label_angle = 0,
  time_span = NA,
  show_legend = TRUE
)
```

## Arguments

- groups_cumulative_trajectories:

  A list with components `groups_similarity` and `groups_attributes`,
  typically produced by `plot_groups_trajectories()`. The
  `groups_similarity` element must be a named list of edge tables (one
  per group) with at least `from`, `to`, and `weight`; the
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

  Vertical adjustment for node labels (default: 0).

- label_horizontal_position:

  Horizontal adjustment for node labels (default: 0).

- label_angle:

  Angle for node labels (default: 0).

- time_span:

  Optional vector of years to display; if `NA`, shows all (default:
  `NA`).

- show_legend:

  Logical indicating whether to show the color legend (default: `TRUE`).

## Value

A `ggplot2` object visualizing the technological trajectories.

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute trajectories first
traj_data <- plot_groups_trajectories(groups_cumulative)

# Visualize a specific group (pass the whole object; the function extracts what it needs internally)
plot_group_trajectories_2d(
  groups_cumulative_trajectories = traj_data,
  group = "c1g5",
  jaccard_similarity = 0.3
)
} # }
```
