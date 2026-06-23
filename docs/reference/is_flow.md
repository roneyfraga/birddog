# Is this a trajectory flow object?

"Flow" names the object kind, not the algorithm (stock vs flow: groups
are stock, trajectories are flow). Every trajectory detector returns a
flow – the temporal decomposition of the DAG into trajectories plus the
confluence tree – so this predicate accepts the result of
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
and of any future `sniff_trajectory_<algo>()` detector. `is_flow()` is
the cheap predicate;
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
is the contract's authoritative checker.

## Usage

``` r
is_flow(x)
```

## Arguments

- x:

  Any object.

## Value

A length-one logical.

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)

Other trajectory detection:
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md),
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`subset.birddog_flow()`](https://roneyfraga.com/birddog/reference/subset.birddog_flow.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
