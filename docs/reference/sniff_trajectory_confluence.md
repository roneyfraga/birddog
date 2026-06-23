# Build the render-ready confluence forest of the soft DAG

Turns a
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
object into the data a trajectory confluence needs: one `rivers` row per
trajectory (with a per-year size curve) and one `confluences` row per
child to parent merge edge (papers transferred, the first year a
transferred paper was published, the handoff year, and the
cumulative-inflow curve). Every edge of the flow tree is kept;
thresholds are applied later by
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md),
so a slider can re-filter without recomputing.

## Usage

``` r
sniff_trajectory_confluence(
  x,
  min_group_size = 10,
  jaccard_min = 0.05,
  k_out = 2
)
```

## Arguments

- x:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object, or a `docs_per_group` tibble /
  [`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
  object /
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  object (the flow is built internally).

- min_group_size, jaccard_min, k_out:

  Passed to
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  when `x` is not already a flow object.

## Value

A list with:

- rivers:

  Tibble, one row per trajectory: `traj_id`, `type`
  (`"central"`/`"absorbed"`), `central` (destination group `cNgN`),
  `size`, `start`, `handoff_year` (= `last_year` for centrals, else
  absorption year), `depth`, `parent` (absorber `traj_id`, `NA` for
  central), and `size_curve` (list-col of `tibble(year, size)`, the
  trajectory's community size per year).

- confluences:

  Tibble, one row per child to parent merge: `child`, `parent`, `n`
  (papers transferred), `cohort_size` (the child's size),
  `first_feed_year` (publication year of the first transferred paper),
  `handoff_year`, and `inflow_curve` (list-col of `tibble(year, size)`).

- destinations:

  Tibble, one row per (absorbed trajectory, final group) it fed:
  `traj_id`, `g_final`, `n` (papers of the trajectory's terminal cohort
  that ended up in `g_final`). A trajectory usually appears under
  several finals, so this is the multi-destination split the forest
  collapses to one.

- centrals:

  The central group ids (`cNgN`) in
  [`mixed_sort()`](https://roneyfraga.com/birddog/reference/mixed_sort.md)
  order.

- last_year:

  Final year of the analysis.

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
