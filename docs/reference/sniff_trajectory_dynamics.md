# Dynamic-state indicators and classification for flow trajectories

Characterizes each
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
trajectory along the evolutionary states of the method (emergence,
convergence, divergence, dormancy), grounding the four emergence pillars
(novelty, growth, community, persistence) in two complementary lenses
tied to the flow model.

## Usage

``` r
sniff_trajectory_dynamics(
  flow,
  thresholds = NULL,
  k = 1,
  growth_window = 3,
  novelty_window = 5,
  winsorize = 3,
  cct = NULL,
  entropy = NULL,
  hubs = NULL
)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- thresholds:

  A
  [`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)-shaped
  list. `NULL` (default) **derives the cut points from the data** via
  [`data_state_thresholds()`](https://roneyfraga.com/birddog/reference/data_state_thresholds.md)
  with the `k` below, so the classification adapts to the dataset; pass
  [`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)
  to force the fixed constants instead. The thresholds actually used are
  attached to the result as `attr(result, "state_thresholds")`, which
  the plots read back.

- k:

  Robust deviations for the data-driven thresholds when `thresholds` is
  `NULL` (default 1); see
  [`data_state_thresholds()`](https://roneyfraga.com/birddog/reference/data_state_thresholds.md).
  Ignored when an explicit `thresholds` list is supplied.

- growth_window:

  Curve points over which `growth_rate` is fitted (default 3).

- novelty_window:

  Years counted as recent for `novelty` (default 5).

- winsorize:

  Cap each pillar's robust z-score at `c(-winsorize, +winsorize)` before
  summing into `emergence_index`, so no single extreme pillar (e.g. a
  young core 18 MADs out on growth) dominates the ranking. Default `3`
  (near the Iglewicz–Hoaglin modified-z outlier boundary of 3.5); `NULL`
  leaves the robust z-scores uncapped.

- cct:

  Optional
  [`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md)
  output (one row per `traj_id`), left-joined to add the `cct`
  list-column (per-year renewal-pace series); `NULL` (default) omits it.

- entropy:

  Optional
  [`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md)
  output (one row per `traj_id`), left-joined to add the
  `keyword_entropy` list-column (per-year keyword-diversity series);
  `NULL` (default) omits it.

- hubs:

  Optional
  [`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md)
  output (one row per `traj_id`), left-joined to add the hub-role
  columns; `NULL` (default) omits them.

## Value

A tibble, one row per trajectory, sorted by descending `emergence_index`
(absorbed last): `traj_id`, `type`, `group`, `start`, `end`, `age`,
`size`, `growth_rate`, `doubling_time`, `novelty`, `recruitment`,
`emergence_density`, `attraction_inflow`, `dest_entropy`,
`dormant_share`, `phase`, `fate`, `state`, `emergence_index`,
`emergence_index_intensive` (the same index without the extensive
`recruitment` pillar), `reach_ratio`, plus any `cct` / `keyword_entropy`
(per-year list-columns) and `hubs` columns.

## Details

**Forward lens (all trajectories).** `growth_rate` and `doubling_time`
(an exponential fit of the recent size curve), `novelty` (share of
documents that arrived recently), `recruitment` (net documents gained
over the lifespan, the power of attraction), `emergence_density`
(staying power: the share of the lineage's year-steps still growing at
the emergence rate, after Carley et al. 2017), and `age`/`size`
(persistence and community).

**Backward lens (absorbed trajectories only).** Where the terminal
cohort goes, via
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md):
`dest_entropy` (normalized Shannon entropy of the destination split) and
`dormant_share` (share dropping out).

**Inflow lens (central trajectories only).** `attraction_inflow`, the
documents of the final community delivered by absorbed tributaries (the
consolidating side of convergence), from
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md).

**Reach lens (all trajectories).** `reach_ratio`, the share of the
future field a lineage's origin seeds – its birth node's forward
temporal reach in the DAG over all nodes born later (after the
temporal-reachability of Marino & Silva 2023). Structural downstream
influence, distinct from size and, via the ratio, from age.

Classification uses two axes: a life-cycle **`phase`** for living
(central) trajectories – emergence / maturity / dormancy(stall) by
growth and novelty – and a terminal **`fate`** for declining (absorbed)
ones – convergence / divergence / dormancy(death) by destination.
`state` is `phase` for centrals and `fate` for absorbed; `phase` and
`fate` are kept separate so the two senses of dormancy (a stalled living
core vs a dead cohort) stay distinguishable. The `emergence_index` (a
within-central combination of growth, novelty and recruitment) is
computed for living trajectories only, so an absorbed lineage that grew
before dying is never ranked as emerging. Because `recruitment` is an
extensive (size-carrying) pillar, the full index partly tracks community
size; `emergence_index_intensive` drops it (growth and novelty only) for
a rate-based ranking that does not reward a large core for its volume
alone.

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md),
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
