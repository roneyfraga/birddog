# Where a trajectory's terminal cohort goes

Returns the destination of `source`'s terminal cohort: its
per-final-group split and dormant share, plus its immediate absorber and
ultimate destination.

## Usage

``` r
sniff_trajectory_destination(x, ...)
```

## Arguments

- x:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- ...:

  `source`, the trajectory whose terminal cohort to follow.

## Value

A list (`target`, `target_info`, `destination`, `cohort_size`,
`dormant_share`, `continuation_traj`, `last_year`).

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
