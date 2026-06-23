# Per-year citation cycle time (renewal pace) along each flow trajectory

Recomputes the Citation Cycle Time (Kayal, 1999) from the documents in
each
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
trajectory's own group-year nodes, one value per year of the
trajectory's life: per node-year, the median over its documents of each
document's median cited-reference age. A low or falling series marks a
fast-renewing knowledge stock, a diagnostic signal of emergence.

## Usage

``` r
sniff_trajectory_cct(flow, references)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- references:

  A long data frame with columns `document_id` and `ref_age`, one row
  per document-cited-reference, `ref_age` the reference's age in years
  (`citing_year - cited_year`). Build it from the corpus `CR` field
  joined to the cited works' publication years (e.g. the `tracked_cr_py`
  of
  [`sniff_cct()`](https://roneyfraga.com/birddog/reference/sniff_cct.md)).

## Value

A tibble, one row per trajectory: `traj_id`, `type`, `group`, and `cct`,
a list-column whose each cell is a tibble with columns `year` and `cct`
(the median citation age over that node-year's documents; `NA` when none
of them carry references).

## Details

This is the faithful, per-document counterpart of
[`sniff_cct()`](https://roneyfraga.com/birddog/reference/sniff_cct.md)
(which scores a static
[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md)
partition by group-year): it follows the trajectory's actual membership,
so an absorbed tributary is scored on its own documents rather than its
destination group's.

## References

Kayal AA, Waters RC (1999). An empirical evaluation of the technology
cycle time indicator. IEEE Trans. Eng. Manag. 46(2):127-31.

## See also

[`sniff_cct()`](https://roneyfraga.com/birddog/reference/sniff_cct.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)

Other trajectory analysis:
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
