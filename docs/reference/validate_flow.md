# Validate the trajectory flow contract

The authoritative checker of the flow contract (`trajectories`, `tree`,
`graph`, `docs_per_group`, `last_year`). "Flow" names the object kind,
not the algorithm: any detector –
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
today, alternative `sniff_trajectory_<algo>()` detectors in the future –
must return an object that passes this validator and inherits the
`birddog_flow` class.

## Usage

``` r
validate_flow(x)
```

## Arguments

- x:

  The object to validate, typically a
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object or the output of an alternative detector shaped like one.

## Value

`x`, invisibly, when valid; otherwise an error listing every violation.

## Details

Beyond the column schema, the checked invariants are the ones that
otherwise fail silently: every trajectory node label must be a
`docs_per_group$group_id` key and a `graph` vertex (the triple key
documents and analyses share), the `graph` must carry the
`quantity_papers` / `prop_tracked_intra_group` vertex attributes the
line plots read, `absorbed_into` must reference existing trajectories
without forming a cycle, each final group must have exactly one
`central` present at `last_year`, central ids must use the `tr::`
convention, and `start`/`end` must match the node years.

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md)

Other trajectory detection:
[`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md),
[`sniff_trajectory_dag()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dag.md),
[`subset.birddog_flow()`](https://roneyfraga.com/birddog/reference/subset.birddog_flow.md)
