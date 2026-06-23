# Trajectories that fed into a target (confluence)

Returns the target trajectory's direct tributaries: its children in the
confluence tree, the trajectories whose terminal cohort was absorbed
into `target`. The result is the `formation` shape that
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md)
renders.

## Usage

``` r
sniff_trajectory_formation(x, ...)
```

## Arguments

- x:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- ...:

  `target` (a `traj_id`), then `min_papers` (default 5) and `min_prop`
  (default 0.05), the thresholds behind the feeders' `kept` flag.

## Value

A `formation` list (`target`, `target_info`, `feeders`, `total_inflow`,
`last_year`). The `feeders` carry `n_dest` (the feeder's documents that
end in the target's final community) and `target_info` carries
`final_size` (the target's final-year size);
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md)
uses these for its labels.

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`plot_trajectory_formation()`](https://roneyfraga.com/birddog/reference/plot_trajectory_formation.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
