# Visualize 3D Technological Trajectories from Group Evolution

Creates an interactive 3D visualization of technological trajectories
showing the evolution of research groups over time with node size
representing group importance and color representing publication year
deviation.

## Usage

``` r
plot_group_trajectories_3d(
  groups_cumulative_trajectories,
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

- groups_cumulative_trajectories:

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

## Examples

``` r
if (FALSE) { # \dontrun{
# First get trajectory data
traj_data <- sniff_groups_trajectories(groups_cumulative)

# Visualize a specific group in 3D
plot_group_trajectory_3d(
  groups_cumulative_trajectories = traj_data,
  group = "component1_g05",
  jaccard_similarity = 0.2
)
} # }
```
