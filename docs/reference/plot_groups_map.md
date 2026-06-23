# Plot the stock map: the complete citation network coloured by final group

The birddog stock map: every paper in the cumulative network is a point,
laid out by a force-directed layout of the citation network and coloured
by its final group, with a text label at each group's centroid. It is
the stock snapshot – all final groups, the whole network at once –
positioned by citation structure rather than text embeddings.

## Usage

``` r
plot_groups_map(
  groups_cumulative,
  labels_text = NULL,
  network_until = NULL,
  layout = "drl",
  seed = 888L,
  point_size = 0.9,
  point_alpha = 0.7,
  show_unassigned = TRUE,
  label = TRUE,
  label_size = 3.2,
  title = "Group map",
  title_size = 14,
  normalize = FALSE,
  normalize_q = 0.99
)
```

## Arguments

- groups_cumulative:

  A stock object from
  [`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md);
  each `network_until_<year>` element must carry `$network` (an
  igraph/tbl_graph whose vertices have a `name` attribute) and
  `$documents` (with columns `name`, `group`).

- labels_text:

  Optional `data.frame` with columns `id` (matching the group ids, e.g.
  `"c1g1"`) and `text` (the human description). `NULL` (default) uses
  the bare group id as the label. If no `id` matches any group, a
  warning is issued and the group ids are used.

- network_until:

  Snapshot year to map. `NULL` (default) uses the last element (the
  final groups).

- layout:

  Force-directed layout passed to
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  (default `"drl"`, the OpenOrd layout, which separates communities into
  legible territories on dense citation networks; `"fr"` and `"kk"` tend
  to collapse a large network into a single hairball).

- seed:

  Integer seed for the stochastic layout (default `888L`).

- point_size:

  Node point size (default `0.9`).

- point_alpha:

  Node point alpha (default `0.7`).

- show_unassigned:

  Draw nodes with no group (`NA`) as grey background (default `TRUE`);
  `FALSE` drops them.

- label:

  Draw the group text labels at centroids (default `TRUE`).

- label_size:

  Font size of the centroid labels (default `3.2`).

- title:

  Plot title: the default `"Group map"`, any string, or `NULL` for no
  title.

- title_size:

  Title font size (default `14`).

- normalize:

  Rescale the layout for cross-year comparison (default `FALSE`). When
  `TRUE`, coordinates are centred on the median and divided by the
  robust radius (the `normalize_q` quantile of the distance to the
  centre), then framed with a fixed unit window, so the dense core fills
  the plot identically across snapshots. This equalises scale only, not
  orientation: the force-directed layout assigns each graph an arbitrary
  rotation, so node positions are not correspondable across years.

- normalize_q:

  Quantile of the radial distance kept inside the normalised window
  (default `0.99`); only used when `normalize = TRUE`.

## Value

A `ggplot` object.

## See also

[`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md),
[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md)

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md),
[`plot_groups_lineage_3d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_3d.md),
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
