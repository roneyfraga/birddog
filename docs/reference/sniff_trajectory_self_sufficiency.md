# Self-sufficiency of each central trajectory

Returns each central trajectory's endogenous fraction: the share of its
**final-year community** that was NOT delivered by any absorbed
tributary (1 = fully endogenous, ~0 = a confluence of tributaries).
Inflow follows the same destination semantics as
[`plot_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/plot_trajectory_confluence.md)
and
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md):
a document counts as imported when it sits in the central's final
community **and** in the terminal cohort of some absorbed tributary,
counted once even if several tributaries carried it, and **regardless of
which central the tributary is dominantly assigned to** – so secondary
inflow (tributaries owned by other centrals) is included, not only the
central's own feeders.

## Usage

``` r
sniff_trajectory_self_sufficiency(x, ...)
```

## Arguments

- x:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- ...:

  `min_size` (default 30, the minimum final-year community size).

## Value

A tibble (`central`, `group`, `final_size`, `inflow`,
`self_sufficiency`), sorted by descending `self_sufficiency`.

## See also

[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md)
