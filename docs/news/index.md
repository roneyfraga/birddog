# Changelog

## birddog 2.0.0

birddog 2.0.0 makes
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
the single canonical trajectory detector and is a pre-launch reset: the
dynamic-programming (DP) per-group detector is REMOVED, not deprecated
(the package had not been publicized; this is the one hard break —
future removals will go through a deprecation window). Why removed: the
DP routes leak across groups (nodes that globally belong to other
centrals), the main route duplicates the flow trunk, and the genuine
tributary chaining is already explicit in `flow$tree`. The last 1.x
state is preserved at git tag `v1.0.7` and in the CRAN archive.

### Renamed

- `sniff_trajectory_flow()` is now
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md).
  “Flow” names only the object kind (the `birddog_flow` returned by
  every detector, validated by
  [`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md) /
  [`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md));
  the canonical detector is named for the braided river it decomposes, a
  symmetric sibling of
  [`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md).
  The object contract is unchanged.
- `sniff_citation_cycle_time()` is now
  [`sniff_cct()`](https://roneyfraga.com/birddog/reference/sniff_cct.md)
  – the short name pairs the per-trajectory
  [`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
  mirroring the
  [`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md)
  /
  [`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md)
  pattern.
- `sniff_trajectory_prestige()` is now
  [`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
  pairing
  [`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md);
  the
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  argument `prestige` is likewise renamed `hubs`.

### Removed (migration table)

| birddog 1.x (removed) | birddog 2.0 |
|----|----|
| `detect_main_trajectories(gct, group)` | `sniff_trajectory_braid(gct)` |
| `filter_trajectories(tr_tbl, top_n)` | `subset(flow, ...)` (object) or dplyr on `flow$trajectories` (table) |
| `sniff_trajectory_formations(all_detected, dpg)` | `sniff_trajectory_formation(flow, target)` |
| `sniff_trajectory_destinations(all_detected, dpg)` | `sniff_trajectory_destination(flow, source)` |
| `sniff_groups_trajectories(gc)` | `sniff_groups_lineage(gc)` |
| `sniff_citations_cycle_time(...)` | `sniff_cct(...)` |
| `plot_group_trajectories_2d(gct, group)` | `plot_groups_lineage_2d(gct, group)` |
| `plot_group_trajectories_3d(gct, group)` | `plot_groups_lineage_3d(gct, group)` |
| `plot_group_trajectories_lines_2d(traj, tf)` | `plot_trajectory_lines_2d(flow, target)` |
| `plot_group_trajectories_lines_3d(traj, tf)` | `plot_trajectory_lines_3d(flow, target)` |
| `plot_trajectory_destination(d)` | `plot_trajectory_dispersion(d)` with `d <- sniff_trajectory_destination(flow, source)` |
| `attach_docs_to_vertices(g, docs)` | none (internal helper) |
| legacy (DP-object) input to `sniff_trajectory_formation/destination/self_sufficiency()` | pass a flow object |

### Breaking changes

- [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md):
  first argument renamed `docs_per_group` -\> `x`; it now also accepts
  the
  [`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
  object directly.
- [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  and
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  return classed objects (`birddog_dag`, `birddog_flow`) with print
  methods. Objects saved by 1.x (RDS) are still accepted everywhere.
- `plot_trajectory_formation_linear()` renamed to
  [`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md):
  with the 3D view gone it is the single formation plot, mirroring
  [`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md).
- `plot_trajectory_formation_3d()` removed; the 3D view is covered by
  [`plot_trajectory_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_trajectory_lines_3d.md)
  and the per-target river by
  `plot_trajectory_confluence(conf, target = ...)`.

### New features

- [`subset()`](https://rdrr.io/r/base/subset.html) method for flow
  objects: contract-preserving filtering by predicate
  (`subset(flow, size >= 50)`) or watershed
  (`subset(flow, target = "tr::c1g1")` keeps the central and its whole
  tributary subtree). The result is still a valid flow; orphaned
  tributaries reattach to their nearest kept ancestor, and the pruning
  call is shown by [`print()`](https://rdrr.io/r/base/print.html). Works
  for any detector whose result inherits from `birddog_flow`.
- Per-trajectory emergence indicators in
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md):
  `emergence_index_intensive` (a rate-only ranking that drops the
  size-carrying `recruitment` pillar), `emergence_density` (staying
  power – the share of a lineage’s years still growing at the emergence
  rate, after Carley et al. 2017), and `reach_ratio` (downstream
  propagation in the DAG, after Marino & Silva 2023).
- [`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md)
  and
  [`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md):
  the FUSE community pillar – distinct-author breadth per trajectory,
  and the authors who own each living trajectory’s emergence (Total /
  sqrt-normalized, after Garner et al. 2017).
- [`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md)
  and a rewritten
  [`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md):
  per-year keyword diversity (Pielou’s J’) and citation cycle time,
  recomputed from each trajectory’s own node documents and returned as
  `tibble(year, value)` list-columns. They join into
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  via the new `cct`, `entropy` and `hubs` arguments.
- `sniff_trajectory_dag(similarity = "coupling")`: route the soft DAG by
  bibliographic coupling (shared references) instead of document
  overlap;
  [`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md)
  warns when scored on the same references.
- [`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md)
  and
  [`plot_trajectory_dynamics_interactive()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics_interactive.md):
  every state region stays in view even when the points cluster, region
  names sit on legible boxes clear of the nodes, and each point-label
  leader starts at the bubble’s outer edge.

## birddog 1.0.7

### New features

- [`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md)
  and `sniff_trajectory_formations()`: the inverse of
  [`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md).
  Given a living target trajectory, find every other trajectory whose
  terminal cohort was absorbed into it (its feeders), for one target or
  all targets at once. Each feeder carries the documents transferred,
  its cohort size and handoff year, plus two measured growth series:
  `size_curve` (its cluster size per year) and `inflow_curve` (the
  contributed papers accumulating within the feeder’s own year-nodes, so
  the curve stays inside the feeder’s lifespan instead of reaching back
  to a paper’s corpus first-appearance year); `target_info` also reports
  the target’s own document count.
- `plot_trajectory_formation_linear()` and
  `plot_trajectory_formation_3d()`: draw the confluence of feeders into
  a target trajectory – a 2D cumulative-river timeline or an interactive
  3D plotly view (X = year, Y = feeder lane, Z = documents). Each feeder
  grows along its length on the same proportional scale as the target,
  set by a `feeder_curve` argument (`"inflow"`, the papers that entered
  the target, or `"size"`, the feeder’s whole community); the 2D feeders
  are labelled on the right in the target’s two-line style. Both include
  terminal-node markers and optional manual hover descriptions.
- [`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md):
  per-trajectory endogenous-growth index, `1 - imported / size`,
  distinguishing lineages that grew on their own from confluences of
  other communities.
- `plot_group_trajectories_2d()`: new `color_by` argument with
  `"final_group_share"`, colouring each node by the share of its
  documents that belong to the group’s last-year cluster (plasma
  palette), with white-haloed labels for readability over dark nodes.
- `plot_group_trajectories_lines_3d()`: new `descriptions` argument for
  manual per-trajectory hover text, plus terminal-node markers sized by
  document count.

## birddog 1.0.6

### New features

- [`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md):
  track where the papers of an incomplete trajectory (one that stops
  before the last year) end up. Takes a `detect_main_trajectories()`
  result and a `traj_id`, follows the terminal node’s documents forward,
  and returns the final-year destiny vector, the dormant share, the
  dominant continuation group, and a per-year flow table. With the
  optional `all_detected` argument (all groups’
  `detect_main_trajectories()` outputs) it also resolves the destination
  at the trajectory level, identifying which living trajectory absorbed
  the stagnant one (`destination_traj`, `continuation_traj`).
- `plot_trajectory_destination()`: render the destinations from
  [`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md)
  as variable-width lines on a Sugiyama time layout, matching
  `plot_group_trajectories_lines_2d()`. Each destination is one line
  whose width tracks the papers carried; `color_by` switches between
  destination group and destination trajectory, and `min_prop` folds
  minor destinations into a grey `(other)` line.
- [`plot_trajectory_dispersion()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dispersion.md):
  draw the stagnant trajectory and the trajectory that absorbed it as
  two timelines on a shared year axis, with an arrow at the handoff
  year.
- `sniff_citations_cycle_time()`: new `normalize` argument (`"none"` \|
  `"domain"`) to divide each group’s annual CCT by the per-year network
  median, enabling within-domain comparability following Lee (2024).
- `sniff_citations_cycle_time()`: new `dispersion` argument (logical)
  that adds `p25` and `p75` columns to the returned data tibble, useful
  for detecting bimodal citation-age distributions.
- `sniff_citations_cycle_time()`: new `citation_scope` argument
  (`"global"` \| `"local"`) to restrict CCT to intra-corpus citations.
  Currently supported for OpenAlex data; Web of Science data falls back
  to `"global"` with a warning.

## birddog 1.0.5

### Bug fixes

- [`sniff_groups_terms()`](https://roneyfraga.com/birddog/reference/sniff_groups_terms.md):
  replace base [`tolower()`](https://rdrr.io/r/base/chartr.html) with
  [`stringr::str_to_lower()`](https://stringr.tidyverse.org/reference/case.html)
  to avoid locale-dependent failures on non-ASCII UTF-8 text.
- [`sniff_groups_terms()`](https://roneyfraga.com/birddog/reference/sniff_groups_terms.md):
  fix `tryCatch` scoping bug where error-handler assignments to
  `stats[[i]]` were lost, causing `purrr::map_int(stats, nrow)` to abort
  the whole run on a single failing group. Skipped groups are now
  dropped cleanly via
  [`purrr::compact()`](https://purrr.tidyverse.org/reference/keep.html).

## birddog 1.0.4

CRAN release: 2026-04-04

### New features

- [`mixed_sort()`](https://roneyfraga.com/birddog/reference/mixed_sort.md):
  exported for external use, with improved documentation.
- `plot_group_trajectories_lines_2d()`: new `width_by_traj_size`
  parameter to scale line width by trajectory size (default TRUE).
- `plot_group_trajectories_lines_3d()`: new `width_by_traj_size`
  parameter to scale line width by trajectory size (default TRUE).

### Improvements

- Rewrite vignette with biogas dataset and gallery-with-code format.
- Fix README: wrong package name and CRAN URL, add Features section.

## birddog 1.0.2

### New features

- [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md):
  support for multiple key-routes and natural group ordering.
- [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md):
  new `compact_gaps` and `direction` arguments.
- [`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md):
  new `mode` and `window_size` arguments.
- [`get_openalex_fields()`](https://roneyfraga.com/birddog/reference/get_openalex_fields.md):
  cache saved batches to skip duplicate API downloads.

### Improvements

- Optimize SPC and key-route search with pre-built adjacency lists.
- Pre-render horizon plots to reduce gt object size in
  [`sniff_groups_attributes()`](https://roneyfraga.com/birddog/reference/sniff_groups_attributes.md).
- Improve key-route plot layout, axis labels, and data output.
- Improve horizon plot x-axis: vertical year labels, removed title.
- Use 95th percentile for entropy diff plot y-axis range.
- Swap diff colors in index plots: red for negative, blue for positive.
- Add `y_limits_diff` parameter to
  [`indexes_plots()`](https://roneyfraga.com/birddog/reference/indexes_plots.md).

### Bug fixes

- Fix y-axis scale across entropy plots for group comparison.
- Fix dplyr variable collision in
  [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md)
  group filter.
- Fix `rlang` `.env` import to resolve R CMD check NOTE.
- Fix plot issues: diff colors and same-year node spreading.

## birddog 1.0.0

CRAN release: 2026-02-19

- Initial CRAN release.
- Rewrite of
  [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md),
  `sniff_citations_cycle_time()`, and
  [`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md).
- Added `testthat` test suite with 102 tests across 16 files.
- Fix BibTeX parser for Windows CRLF line endings.
- Fix CRAN check issues: Unicode chars, `\dontrun` examples, DOI URLs.
- Replace `\dontrun` with `\donttest` in all examples.
- Add error guards to vignettes to prevent crashes.

## birddog 0.1.0

- Initial development version.
- Added
  [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md)
  function for main path analysis.
- Includes visualization capabilities.
- Implements methodology from Liu & Lu (2012).
