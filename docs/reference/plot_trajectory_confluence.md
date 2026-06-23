# Plot the trajectory confluence (how the central trajectories form)

A braided river system over
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md):
every trajectory is a stream flowing left to right along the publication
year, merging into its absorber where it hands off, until the central
trajectories (`tr::cNgN`) reach the final year. Each tributary is drawn
**grey** until the first paper it transfers is published, then in its
colour; at the merge a band of width equal to the **transferred** papers
(`n` of the tributary's `cohort_size`) bends into the river, which
widens by that amount. The central spine is centered on its band with
tributaries packed above and below. With `target = NULL` the whole
system is drawn (one colour per destination central); with
`target = "tr::c1g1"` only that central's direct feeders are drawn (one
colour per tributary).

## Usage

``` r
plot_trajectory_confluence(
  conf,
  target = "all",
  depth = NULL,
  min_n = 5,
  min_prop = 0.05,
  min_total_size = 0,
  min_duration_years = 0,
  width_range = c(0, 0.42),
  label_terminal = c("id_count", "id"),
  label_intermediary = c("id_count", "id"),
  label_terminal_size = 4.5,
  label_intermediary_size = 4,
  labels_text = NULL,
  legend_ncol = NULL,
  palette = NULL,
  axis_text_size = NULL,
  year_range = NULL,
  show_secondary = TRUE,
  secondary_min_prop = 0.2,
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

  What to draw. `"all"` (default, same as `NULL`) draws the full
  confluence (the whole nested forest). `"finals"` draws only the
  **central backbones** (the inflow spines, tributaries hidden);
  `"intermediary"` draws only the **tributary streams** (the ribbons,
  central spines hidden). A single central `traj_id` (e.g. `"tr::c1g1"`)
  draws only that central's tributaries; a vector of central ids (e.g.
  `c("tr::c1g1", "tr::c1g5")`) draws the selected centrals stacked. With
  a single central the streams are coloured per tributary; otherwise per
  destination central. A single **intermediate** trajectory id (e.g.
  `"tr3"`) switches to a **focus view**: its feeders merge in on the
  left (who fed it) and the finals its terminal cohort reached fan out
  on the right (who it fed).

- depth:

  How many confluence levels upstream of the roots to draw; `NULL`
  (default) means the whole forest when `target = NULL`, or just the
  direct feeders (`1`) when one or more `target`s are given. Set e.g.
  `depth = 2` to expand a target view to feeders-of-feeders.

- min_n:

  Minimum papers a tributary must transfer to its absorber to be
  counted; below this it is pruned from the drawing **and** the river
  width, together with its own upstream subtree (default `5`).

- min_prop:

  Minimum share of a tributary's own cohort that must transfer (default
  `0.05`); guards against incidental trickles.

- min_total_size:

  Minimum **total size** of a tributary (its own distinct paper count
  across all years) for it to be drawn (default `0`, no filter); prunes
  small lineages and their upstream subtree, complementing `min_n`
  (which gates the transfer, not the trajectory's own size).

- min_duration_years:

  Minimum **lifespan in years** of a tributary (its last year minus its
  first year, plus one) for it to be drawn (default `0`, no filter); use
  it to drop short-lived lineages.

- width_range:

  Half-width range `c(floor, max)` in y-data units (lanes are spaced `1`
  apart, so keep `max` below ~`0.5` to avoid overlap). The widest stream
  reaches `max`; every band is at least `floor`, so thin streams stay
  visible. Default `c(0, 0.42)` is strictly proportional (no floor); a
  positive floor (e.g. `c(0.05, 0.45)`) lifts the small streams, raising
  `max` thickens everything. Note these are lane-fraction units, **not**
  pixel sizes like `marker_range`.

- label_terminal:

  Label content for the **final** (terminal) trajectories: `"id_count"`
  (default) `tr::cNgN (size)`, `"id"` `tr::cNgN`, or `NULL` to hide
  them. The count is the central's **final-year** size (what remains in
  the final community), matching
  [`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md),
  not its lifetime total.

- label_intermediary:

  Label content for the **intermediate** (tributary) trajectories:
  `"id_count"` (default) `trN (n)`, `"id"` `trN`, or `NULL` to hide
  them. `n` is the tributary's documents that **end** in its target
  final (the destination count from
  [`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md))
  – the single target in a single-target view, else the tributary's own
  dominant central – not the trunk transfer. Objects without destination
  data fall back to `trN (n/cohort)`.

- label_terminal_size:

  Text size of the **final** (terminal) labels (default `4.5`).

- label_intermediary_size:

  Text size of the **intermediate** (tributary) labels (default `4`).

- labels_text:

  Optional `data.frame` with columns `id` and `text` mapping central
  group ids (`cNgN`) to descriptions; when supplied a bottom legend
  `tr::cNgN: description` is added (as in
  [`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md)).
  `NULL` (default) adds no legend.

- legend_ncol:

  Number of columns in the `labels_text` legend; `NULL` (default) lets
  ggplot choose.

- palette:

  Optional named colour vector overriding the default hues (keyed by
  destination central in the all/multi view, else by tributary
  `traj_id`).

- axis_text_size:

  Font size of the x-axis (year) tick labels; `NULL` (default) keeps the
  theme default.

- year_range:

  Optional `c(from, to)` years fixing the time axis (as in
  [`plot_trajectory_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_2d.md)),
  so a short confluence can share the window of a longer one. `NULL`
  (default) fits the drawn rivers; a wider range only extends the axis
  (it never drops data). Ignored in the single-intermediate focus view.

- show_secondary:

  Draw faint links from a tributary to the **other** finals its terminal
  cohort fed, beyond the dominant one it merges into (default `TRUE`). A
  tributary's papers can land in several final groups; the forest keeps
  only the dominant, and these links reveal the rest. Only drawn where
  both spines and ribbons are shown and the destination central is in
  view (so the all-view or several selected centrals, not a
  single-central view). Needs the `destinations` field of
  [`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md).

- secondary_min_prop:

  Minimum share of a tributary's terminal cohort that must reach a
  secondary final for its link to be drawn (default `0.2`); raise it to
  keep only the strongest secondary destinations.

- smooth:

  Gaussian-smooth the stream outlines (default `TRUE`): the central
  spine's cumulative inflow and each tributary's width follow a ~1-year
  low-pass so the edges flow as brush strokes instead of kinked
  polygons. `FALSE` keeps the raw year-to-year steps.

- title:

  Plot title; `NULL` (default) derives one from `target`.

## Value

A `ggplot` object.

## Details

- **x = publication year** (never reassigned); the **y-axis has no
  intrinsic meaning**, it only packs streams to minimise merge
  crossings.

- **Tributary width** follows its community size each year; **grey**
  before the first transferred paper, then its **colour**. **Central
  spine width** is the smoothed cumulative inflow of its counted
  tributaries (papers transferred), on one proportional scale shared
  with the tributaries (0 papers give 0 width).

- A tributary hands `n` of its `cohort_size` papers over; only `n`
  enters the river, so the merge band keeps a **constant width equal to
  the transferred papers** (the `n/cohort` label makes the split
  explicit) and bends over a span that scales with the distance
  travelled, so its thickness reads the same for near and far feeders.

- `min_n` / `min_prop` decide what is **counted**, not merely hidden:
  dropped tributaries leave the river narrower and are summarised in the
  caption.

- An intermediate that delivers **0 documents** to the final it flows
  into (its terminal cohort lands entirely outside that final's
  community) is always excluded, together with its upstream subtree,
  regardless of the thresholds: a stream contributing nothing to the
  destination cannot appear.

## See also

[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)

Other visualization:
[`plot_groups_influence_matrix()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_matrix.md),
[`plot_groups_influence_network()`](https://roneyfraga.com/birddog/reference/plot_groups_influence_network.md),
[`plot_groups_lineage_2d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_2d.md),
[`plot_groups_lineage_3d()`](https://roneyfraga.com/birddog/reference/plot_groups_lineage_3d.md),
[`plot_groups_map()`](https://roneyfraga.com/birddog/reference/plot_groups_map.md),
[`plot_groups_map_animation()`](https://roneyfraga.com/birddog/reference/plot_groups_map_animation.md),
[`plot_groups_map_interactive()`](https://roneyfraga.com/birddog/reference/plot_groups_map_interactive.md),
[`plot_groups_per_year()`](https://roneyfraga.com/birddog/reference/plot_groups_per_year.md),
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
flow <- sniff_trajectory_braid(docs_per_group)
conf <- sniff_trajectory_confluence(flow)

# the whole system: how every central forms
plot_trajectory_confluence(conf, min_n = 20)

# one central's direct feeders, coloured per tributary
plot_trajectory_confluence(conf, target = "tr::c1g1")

# focus on one intermediate trajectory: its feeders + the finals it fed
plot_trajectory_confluence(conf, target = "tr7")

# two selected centrals, a description legend, bigger year labels, bare ids,
#  a visible floor on thin streams, and only sizeable tributaries
descr <- data.frame(id = c("c1g1", "c1g5"), text = c("Topic A", "Topic B"))
plot_trajectory_confluence(conf, target = c("tr::c1g1", "tr::c1g5"),
                              labels_text = descr, axis_text_size = 14,
                              label_intermediary = "id", min_total_size = 30,
                              min_duration_years = 3, width_range = c(0.05, 0.45))
} # }
```
