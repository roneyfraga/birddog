# Interactive strategic map of trajectory dynamic states (plotly)

The plotly twin of
[`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md).
The same quadrant map – shaded and named state regions, classification
thresholds as reference lines, points sized by community and coloured by
state – rendered as an HTML widget. Hovering a point reveals the full
indicator panel (growth, novelty, recruitment, inflow, destination
entropy, dormant share, and any CCT / hub-role columns), and the state
legend is clickable to isolate one state at a time.

## Usage

``` r
plot_trajectory_dynamics_interactive(
  dyn,
  target = c("finals", "intermediary"),
  thresholds = NULL,
  labels_text = NULL,
  label_id = FALSE,
  marker_range = c(6, 42),
  show_labels = FALSE,
  label_size = 11,
  palette = NULL,
  show_thresholds = TRUE,
  show_quadrants = TRUE,
  xlim = NULL,
  ylim = NULL,
  title = NULL
)
```

## Arguments

- dyn:

  A
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  tibble.

- target:

  `"finals"` (default) maps the **living (central)** trajectories
  (novelty x growth), `"intermediary"` the **declining (absorbed)** ones
  (destination entropy x dormant share), as in
  [`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md).

- thresholds:

  The
  [`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)-shaped
  list bounding the regions and the reference lines. `NULL` (default)
  reuses the cuts attached to `dyn` by
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  (`attr(dyn, "state_thresholds")`), else derives them via
  [`data_state_thresholds()`](https://roneyfraga.com/birddog/reference/data_state_thresholds.md).
  Pass an explicit list to override.

- labels_text:

  Optional `data.frame` (`id`, `text`) mapping ids to descriptions, used
  in the hover (and labels). The `"finals"` view matches by group id
  (`cNgN`); the `"intermediaries"` view matches by trajectory id
  (`trN`).

- label_id:

  Put the id on the label's own first line, above the description
  (`cNgN` then the text for finals, `trN` then the text for
  intermediaries), so the hover and pinned labels stay identifiable; the
  line break is plotly's `<br>`. Default `FALSE`.

- marker_range:

  Marker diameter range in px `c(min, max)` (default `c(6, 42)`); area
  scales with the square root of `size`.

- show_labels:

  Pin a text label to each point (default `FALSE`; the detail lives in
  the hover). Text may overlap when points are dense.

- label_size:

  Font size (px) of the pinned labels (default `11`).

- palette:

  Optional named colour vector overriding the default semantic hues,
  keyed by state.

- show_thresholds:

  Draw the classification cut lines (default `TRUE`).

- show_quadrants:

  Shade and name the state regions (default `TRUE`).

- xlim, ylim:

  Optional `c(lo, hi)` axis ranges; `NULL` (default) expands to keep
  every state region visible (the data, the threshold cuts, and the
  `[0,1]` share bounds, with padding).

- title:

  Plot title; `NULL` (default) derives one from `target`.

## Value

A `plotly` htmlwidget.

## See also

[`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)

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
[`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md),
[`plot_trajectory_dispersion()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dispersion.md),
[`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md),
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md),
[`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md),
[`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flow <- sniff_trajectory_braid(docs_per_group)
dyn  <- sniff_trajectory_dynamics(flow)
plot_trajectory_dynamics_interactive(dyn, target = "finals", labels_text = descr)
} # }
```
