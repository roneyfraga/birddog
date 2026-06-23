# Coerce trajectory-layer input to a soft DAG

Resolves the flexible entry contract: a dag passes through (classed, or
a 1.x list with the dag shape), a
[`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
object contributes its `docs_per_group`, and a `docs_per_group` tibble
is built into a dag.

## Usage

``` r
as_trajectory_dag(x, min_group_size = 10, jaccard_min = 0.05, k_out = 2)
```

## Arguments

- x:

  A
  [`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md)
  object, a
  [`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
  list, or a `docs_per_group` tibble.

- min_group_size:

  Minimum distinct-document count for a node (default 10).

- jaccard_min:

  Minimum Jaccard weight to keep an edge (default 0.05).

- k_out:

  Max outgoing edges kept per source node (default 2).

## Value

A `birddog_dag` object.
