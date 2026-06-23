# Interactive stock map: the citation network as a plotly HTML widget

The interactive twin of
[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md).
Every paper is a `scattergl` point laid out by a force-directed layout
of the citation network and coloured by its final group; hovering a node
shows its title, year, authors, keywords, and final group. Returns a
`plotly` htmlwidget – it renders in the RStudio Viewer, embeds in
Quarto/Shiny, or saves to a standalone file with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).

## Usage

``` r
plot_groups_map_interactive(
  groups_cumulative,
  labels_text = NULL,
  network_until = NULL,
  layout = "drl",
  seed = 888L,
  show_unassigned = TRUE,
  label = TRUE,
  label_size = 12,
  title = "Group map",
  title_size = 16,
  normalize = FALSE,
  normalize_q = 0.99,
  marker_size = 5
)
```

## Arguments

- groups_cumulative:

  A stock object from
  [`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md).

- labels_text:

  Optional `data.frame` with columns `id` and `text`; `NULL` (default)
  uses the bare group id. If no `id` matches any group, a warning is
  issued and the group ids are used.

- network_until:

  Snapshot year. `NULL` (default) uses the last element.

- layout:

  Force-directed layout passed to
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  (default `"drl"`).

- seed:

  Integer seed for the stochastic layout (default `888L`).

- show_unassigned:

  Draw nodes with no group (`NA`) as grey background (default `TRUE`).

- label:

  Draw group labels at centroids as annotations (default `TRUE`).

- label_size:

  Font size (px) of the centroid label annotations (default `12`).

- title:

  Plot title: the default `"Group map"`, any string, or `NULL` for no
  title.

- title_size:

  Title font size in px (default `16`).

- normalize:

  Rescale to a fixed unit window for cross-year comparison (default
  `FALSE`); see
  [`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md).

- normalize_q:

  Quantile of the radial distance kept inside the normalised window
  (default `0.99`).

- marker_size:

  Node marker size (default `5`).

## Value

A `plotly` htmlwidget.

## See also

[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md),
[`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md)

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md),
[`plot_groups_lineage_3d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_3d.md),
[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md),
[`plot_groups_map_animation()`](https://roneyfraga.com/birddog/reference/plot_groups_map_animation.md),
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
