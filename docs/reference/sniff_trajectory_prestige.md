# Hub prestige aggregated to each flow trajectory (provincial vs bridging)

Summarizes the document-level hub roles of
[`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md)
(the Guimera–Amaral within-module z-score `Zi` and participation
coefficient `Pi`) over the documents of every
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
trajectory. Provincial hubs (low `Pi`) keep a lineage self-contained,
consistent with convergence / paradigmatic homogeneity; connector and
boundary-spanning hubs (high `Pi`) bridge across groups, consistent with
divergence / branching.

## Usage

``` r
sniff_trajectory_prestige(flow, hubs)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- hubs:

  A
  [`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md)
  tibble: a document-id column (`name`, as returned, or `SR`), plus
  `Zi`, `Pi`, `zone` (`zone` one of `"noHub"`, `"R5"`, `"R6"`, `"R7"`).
  Documents are matched to a trajectory by that id against the
  trajectory's node documents.

## Value

A tibble, one row per trajectory: `traj_id`, `type`, `group`, `n_docs`
(documents of the trajectory), `mean_Zi`, `mean_Pi`, `hub_share` (share
that are hubs), `connector_share` (share that are connector/boundary
hubs `R6`/`R7`, the bridging signal), `provincial_share` (share that are
provincial hubs `R5`).

## See also

[`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)

Other trajectory analysis:
[`sniff_trajectory_cct`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md)`()`,
[`sniff_trajectory_confluence`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md)`()`,
[`sniff_trajectory_contribution`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md)`()`,
[`sniff_trajectory_destination`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md)`()`,
[`sniff_trajectory_dynamics`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)`()`,
[`sniff_trajectory_formation`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md)`()`,
[`sniff_trajectory_self_sufficiency`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)`()`
