# Animate the stock map: the citation network forming year by year

One layout of the final network, with every paper revealed at the year
it first enters the cumulative network and held at its final position,
so the cloud grows without nodes moving. Returns a `gganim` object;
render it to a GIF/MP4 with
[`gganimate::animate()`](https://gganimate.com/reference/animate.html)
and
[`gganimate::anim_save()`](https://gganimate.com/reference/anim_save.html).
Reveal and colour reuse the static
[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md)
engine.

## Usage

``` r
plot_groups_map_animation(
  groups_cumulative,
  labels_text = NULL,
  layout = "drl",
  seed = 888L,
  show_unassigned = TRUE,
  label = TRUE,
  label_size = 3.2,
  title_size = 14,
  point_size = 0.9
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

- layout:

  Force-directed layout passed to
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  (default `"drl"`).

- seed:

  Integer seed for the stochastic layout (default `888L`).

- show_unassigned:

  Reveal nodes with no group (`NA`) as grey (default `TRUE`).

- label:

  Draw persistent group labels at the final centroids (default `TRUE`).

- label_size:

  Font size of the centroid labels (default `3.2`).

- title_size:

  Font size of the per-frame `"Year: ..."` title (default `14`).

- point_size:

  Node point size (default `0.9`).

## Value

A `gganim` object (render with
[`gganimate::animate()`](https://gganimate.com/reference/animate.html)).

## Details

Rendering: the transition is one frame per year. Render with `duration`
(not `fps`), which works with either renderer:
`gganimate::animate(anim, duration = 8)` gives `nframes/duration` fps
(with the `magick` renderer that ratio must divide 100, e.g. 32 years /
8 = 4). Save the rendered object, not the `gganim`:
`a <- gganimate::animate(anim, duration = 8); gganimate::anim_save("x.gif", a)`.
Installing `gifski` removes the fps constraint.

## See also

[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md),
[`plot_groups_map_interactive()`](https://roneyfraga.com/birddog/reference/plot_groups_map_interactive.md)

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md),
[`plot_groups_lineage_3d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_3d.md),
[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md),
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
