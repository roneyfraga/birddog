# Community breadth (distinct authors) of each flow trajectory

Measures the FUSE *community* pillar (Carley et al., 2018) for every
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
trajectory: how many distinct authors stand behind its documents, and
how broad that authorship is per document. A raw author count tracks
trajectory size; `authors_per_doc` is the size-independent breadth — a
high value marks a lineage carried by many distinct contributors (a wide
community), a low value a lineage concentrated in a tight group.

## Usage

``` r
sniff_trajectory_community(flow, authors)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- authors:

  A long data frame with columns `document_id` and `author`, one row per
  document-author pair (e.g. the corpus `AU` field split on `;` and
  joined to the membership key). Documents absent from `authors`
  contribute no authors but still count in `n_docs`, so keep author
  coverage high.

## Value

A tibble, one row per trajectory: `traj_id`, `type`, `group`, `n_docs`
(distinct documents of the trajectory), `n_authors` (distinct authors
across them), `authors_per_doc` (`n_authors / n_docs`, the breadth; `NA`
for an empty trajectory).

## Details

This is the document-cluster analogue of the term-level community
measure in the emergence-indicator literature, where a term counts as
emergent only when used by more than one independent author. It
complements
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
which names the community pillar but scores it only through document
counts (`recruitment`, `size`).

## See also

[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_contribution()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_contribution.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
