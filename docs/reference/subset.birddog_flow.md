# Subset a trajectory flow, preserving the contract

Filters the trajectories of a flow object while keeping it a valid flow:
`$tree` and `absorbed_into` are reattached by transitive bypass (a kept
child points to its nearest kept ancestor; `NA` if none survives) and
`$docs_per_group` follows the kept nodes. The result is a view, not a
census: the call is recorded in `attr(, "pruned")` and shown by
[`print()`](https://rdrr.io/r/base/print.html). Dispatches for any
detector whose result inherits from `birddog_flow` (the contract class),
so future detection methods are subsettable for free.

## Usage

``` r
# S3 method for class 'birddog_flow'
subset(x, subset, target = NULL, ...)
```

## Arguments

- x:

  A `birddog_flow` object (or a subclass from a future detector).

- subset:

  Logical expression on the columns of `x$trajectories`, e.g.
  `size >= 50` or `type == "central"`.

- target:

  Optional character vector of `traj_id`s: keep each target and its
  whole tributary subtree (the watershed). Intersects with `subset`.

- ...:

  Ignored (base generic compatibility).

## Value

An object of the same class as `x`, passing
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md).

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)

Other trajectory detection:
[`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md),
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
