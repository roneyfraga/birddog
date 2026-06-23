# Interactive trajectory confluence (plotly): hover a stream for its trajectory

The plotly twin of
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md).
Every trajectory is a filled stream over the publication year, laid out
by the same centered lane packing and coloured by the same rule (per
destination central in the whole-forest / multi view, per tributary in a
single-central view). There is no legend: hovering a **central** river
shows `tr::cNgN (size)` (plus its `labels_text` description), and
hovering a **tributary** shows `trN (n/cohort -> tr::cNgN)`. With
`label_terminal = TRUE` the central (or selected) rivers are also named
in a de-overlapped column on the right, joined to their stream by a
leader line. Returns a `plotly` htmlwidget.

## Usage

``` r
plot_trajectory_confluence_interactive(
  conf,
  target = "all",
  depth = NULL,
  min_n = 5,
  min_prop = 0.05,
  min_total_size = 0,
  min_duration_years = 0,
  width_range = c(0, 0.42),
  palette = NULL,
  labels_text = NULL,
  label_terminal = TRUE,
  label_size = 12,
  smooth = TRUE,
  title = NULL
)
```

## Arguments

- conf:

  A
  [`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)
  object.

- target:

  What to draw, as in
  [`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)
  minus the focus view: `"all"` (default) the whole forest, `"finals"`
  the central backbones, `"intermediary"` the tributary streams, a
  single central `tr::cNgN` (its feeders, coloured per tributary), or a
  vector of trajectory ids (centrals stacked, or intermediates shown as
  their feeders).

- depth, min_n, min_prop, min_total_size, min_duration_years,
  width_range, smooth:

  Passed through to the same pruning / geometry helpers as
  [`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md);
  see there for details.

- palette:

  Optional named colour vector overriding the default hues (keyed by
  destination central in the all/multi view, else by tributary
  `traj_id`).

- labels_text:

  Optional `data.frame` with columns `id` and `text` mapping ids to
  descriptions, appended to the hover (`id: description ...`). Central
  rivers are matched by their group id (`cNgN`), absorbed tributaries by
  their own trajectory id (`trN`); `NULL` (default) uses the bare id.

- label_terminal:

  Name the central (or selected) rivers in a de-overlapped column to the
  right, each joined to its stream by a leader line (default `TRUE`).

- label_size:

  Font size (px) of those right-hand labels (default `12`).

- title:

  Plot title; `NULL` (default) derives one from `target`.

## Value

A `plotly` htmlwidget.

## Details

Unlike the static function this covers the **forest views only**
(`"all"`, `"finals"`, `"intermediary"`, one or more centrals, and an
intermediate shown as its upstream feeders); the single-intermediate
**focus view** and the faint secondary-destination links are not drawn.

Built with plotly, following the same principles as
[`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md):

- The **x-axis is the publication year**; the **y-axis has no intrinsic
  meaning** (it only packs streams to minimise merge crossings).

- Each stream is a filled polygon (`fill = "toself"`, hoverable over its
  area); a tributary is **grey** before the first paper it transfers,
  then in its colour.

- **There is no legend**: detail is revealed on hover, and only the
  terminal (central or selected) rivers carry a printed label.

- An intermediate that delivers **0 documents** to the final it flows
  into is always excluded (as in
  [`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)).

The widget renders in the RStudio Viewer, embeds in Quarto/Shiny, or
saves to a standalone file with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).

## See also

[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md),
[`plot_trajectory_dag_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag_interactive.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)

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
flow <- sniff_trajectory_braid(docs_per_group)
conf <- sniff_trajectory_confluence(flow)

plot_trajectory_confluence_interactive(conf, min_n = 20)

# one central's formation, hover the tributaries for their n/cohort
plot_trajectory_confluence_interactive(conf, target = "tr::c1g1")
} # }
```
