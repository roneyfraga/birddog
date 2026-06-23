# Compare trajectory detectors by content coherence

Runs
[`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md)
on each named flow, across every content signal and the chosen
`profile`(s), and reports which detector produces the more
content-coherent partition, whether the signals agree, and the contested
nodes (those a pair of detectors place in different final groups).

## Usage

``` r
sniff_trajectory_comparison(
  flows,
  content,
  profile = "incremental",
  signals = names(content)
)
```

## Arguments

- flows:

  Named list of `birddog_flow` objects, e.g.
  `list(flow = ..., channel = ...)`.

- content:

  Named list of `(document_id, feature)` data frames (see
  [`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md)).

- profile:

  Passed to
  [`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md)
  (default `"incremental"`).

- signals:

  Which `names(content)` to score (default all).

## Value

A `sniff_trajectory_comparison` object: a list with `summary` (per
method x signal x profile x resolution), `verdict` (`best_method`,
`delta` per signal x profile x resolution), `agreement` (do all signals
pick the same `best_method`, per profile x resolution), and `contested`
(nodes the detectors place in different final groups, with each method's
`final_group` and silhouette, plus the `closer_method`; `NULL` unless
exactly two flows are given).

## See also

[`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md)

Other flow utilities:
[`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md)
