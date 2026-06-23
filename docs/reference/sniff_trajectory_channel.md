# Detect channel trajectories by global optimal-path routing

An alternative to
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md).
Instead of flow's local mutual-dominance trunk edges, each node is
routed to the successor on its globally cheapest path to a final-year
node, where edge cost is `-log(weight)` (the Jaccard). The minimum-cost
path is the maximum-product Jaccard path (highest geometric-mean
coherence). Final-year nodes root one in-tree each (the watershed of a
final community); the cheapest birth -\> final chain is the **central**
trajectory (`tr::<group>`), the rest are **absorbed** tributaries
(`tr1 … trN`), mirroring the flow object exactly. "Flow" names the
object kind, not the algorithm, so the result is a `birddog_flow` and
passes
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md).

## Usage

``` r
sniff_trajectory_channel(x, min_group_size = 10, jaccard_min = 0.05, k_out = 2)
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

A `birddog_flow` object (see
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md))
with the extra `latest_departure` column.

## Details

Adds one column to `trajectories`: `latest_departure`, the year of the
most recent birth that can reach the trajectory's tail by any
time-respecting path (Wu et al. 2014 latest-departure). For a central
this is the freshest input anywhere in the final community's ancestry;
combined with `start` it separates old-but-freshly-fed communities from
genuinely young ones.

## References

Wu H, Cheng J, Huang S, Ke Y, Lu Y, Xu Y (2014). Path Problems in
Temporal Graphs. Proc. VLDB Endowment 7(9).

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md),
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)

Other trajectory detection:
[`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`subset.birddog_flow()`](https://roneyfraga.com/birddog/reference/subset.birddog_flow.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
