# Braid: the canonical detector of trajectories (central + absorbed)

Decomposes the cumulative-clustering DAG into trajectories via **trunk
edges** (mutual dominance: `v -> u` where `u` is `v`'s heaviest
successor and `v` is `u`'s heaviest predecessor). Trunk edges form
disjoint chains. A chain ending at a final-year group node is a
**central** trajectory (`tr::<group>`, one per final group, reaches the
final year); every other chain is **absorbed** (`tr1 … trN`), merging
into an absorber (central or another absorbed) where its head flows out
via a non-trunk edge. Absorption is transitive, forming a confluence
forest.

## Usage

``` r
sniff_trajectory_braid(x, min_group_size = 10, jaccard_min = 0.05, k_out = 2)
```

## Arguments

- x:

  A
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  object, a
  [`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
  object, or a `docs_per_group` tibble (the DAG is built internally).

- min_group_size, jaccard_min, k_out:

  Passed to
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  when `x` is not already a DAG object.

## Value

A `birddog_flow` object: a classed list with `trajectories` (tibble:
`traj_id`, `type` (`"central"`/`"absorbed"`), `group` (destination
group; NA if extinct), `start`, `end`, `size` (distinct papers), `depth`
(0 = central), `absorbed_into` (absorber `traj_id`, NA for central),
`absorption_year` (NA for central), `nodes` list-col), `tree` (edges
`child`, `parent`, `absorption_year`), `graph`, `docs_per_group`,
`last_year`.

## See also

[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md),
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)

Other trajectory detection:
[`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md),
[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md),
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`subset.birddog_flow()`](https://roneyfraga.com/birddog/reference/subset.birddog_flow.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
