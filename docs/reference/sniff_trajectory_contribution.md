# Documents an intermediate trajectory contributes to a target in a given year

Selects a `source` trajectory's node in a specific `year` and returns
its documents, flagging which of them belong to the `target`
trajectory's final-year community. This is the fine-grained,
per-document view of the inter-trajectory contribution that
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md)
aggregates: where `destination` splits a terminal cohort across every
final group, `contribution` answers "which papers of `source`, as they
stood in `year`, consolidate into this one `target`".

## Usage

``` r
sniff_trajectory_contribution(flow, source, year, target)
```

## Arguments

- flow:

  A
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  object.

- source:

  Trajectory id whose year-node is read, typically an intermediate `trN`
  (e.g. `"tr14"`). Any trajectory id present in `flow` is accepted; each
  trajectory has at most one node per year.

- year:

  Integer publication year. `source` must have a node in this year.

- target:

  Trajectory id whose final-year community is the contribution basis,
  typically a central `tr::cNgN`. Its destination `group` must be known.

## Value

A tibble with one row per document in `source`'s `year` node: `source`,
`target`, `year`, `document_id`, and `in_target` (logical: the document
is in `target`'s final-year community). The `source`/`target`/`year`
columns are constant, so results for several (source, year, target)
triples row-bind cleanly. Summarise with one line, e.g.
`dplyr::summarise(x, n = dplyr::n(), contributed = sum(in_target))`.

## Details

The result is document ids only (plus the contribution flag), so it
composes with any downstream join: keywords (via the corpus `DE` field
or
[`sniff_groups_keywords()`](https://roneyfraga.com/birddog/reference/sniff_groups_keywords.md)),
authors, citations, or STM topics. Filter on `in_target` for the
contributed set.

## See also

[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md),
[`sniff_groups_keywords()`](https://roneyfraga.com/birddog/reference/sniff_groups_keywords.md)

Other trajectory analysis:
[`sniff_trajectory_cct()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_cct.md),
[`sniff_trajectory_community()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_community.md),
[`sniff_trajectory_confluence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_confluence.md),
[`sniff_trajectory_destination()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_destination.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`sniff_trajectory_emergence_owners()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_emergence_owners.md),
[`sniff_trajectory_entropy()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_entropy.md),
[`sniff_trajectory_formation()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_formation.md),
[`sniff_trajectory_hubs()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_hubs.md),
[`sniff_trajectory_self_sufficiency()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_self_sufficiency.md)
