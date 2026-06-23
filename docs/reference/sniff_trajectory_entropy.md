# Per-year keyword diversity (Pielou's J') along each flow trajectory

Recomputes, from the documents in each
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
trajectory's own group-year nodes, the normalized Shannon entropy
(Pielou's evenness J') of their keyword distribution, one value per year
of the trajectory's life. A falling series marks thematic convergence, a
rising one diversification. Unlike
[`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md),
which scores a static
[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md)
partition by group-year, this follows the trajectory's actual
membership, so an absorbed tributary is scored on its own documents
rather than its destination group's.

## Usage

``` r
sniff_trajectory_entropy(flow, keywords)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- keywords:

  A long data frame with columns `document_id` and `keyword`, one row
  per document-keyword pair (e.g. the corpus `DE` field split on `;` and
  keyed by the membership id).

## Value

A tibble, one row per trajectory: `traj_id`, `type`, `group`, and
`keyword_entropy`, a list-column whose each cell is a tibble with
columns `year` and `keyword_entropy` (Pielou's J' over that node-year's
documents, in `[0, 1]`; `0` for a single keyword, `NA` when the year's
documents carry none).

## Details

This is **keyword** entropy (thematic diversity), distinct from the
dynamics `dest_entropy` (the dispersion of an absorbed cohort across
destination groups); the dynamics column is named `keyword_entropy` to
keep the two apart.

## See also

[`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md),
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
