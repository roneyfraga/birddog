# Appraise the content coherence of a trajectory flow

Scores how content-coherent a
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
/
[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md)
partition is, using a silhouette over an independent content signal
(shared references or keywords) the detectors never optimized. Each
group-year node is profiled from its documents' features; node-to-node
distance is `1 - Salton cosine`; the silhouette is computed at two
cluster resolutions: `trajectory` (each backbone/tributary chain) and
`final_group` (each central + its tributaries).

## Usage

``` r
sniff_trajectory_coherence(
  flow,
  content,
  profile = "incremental",
  signals = names(content)
)
```

## Arguments

- flow:

  A `birddog_flow` (from
  [`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md)
  or
  [`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md)).

- content:

  Named list; each element a data frame with columns `document_id` and
  `feature` (long, one row per document-feature pair). The element name
  labels the signal in the output (e.g. `coupling`, `keywords`).

- profile:

  `"incremental"` (default: profile a node from the papers new to its
  year, stripping cumulative carry-over) and/or `"full"` (all of a
  node's papers).

- signals:

  Which `names(content)` to score (default all).

## Value

An `sniff_trajectory_coherence` object: a list with `nodes` (one row per
node x signal x profile x resolution) and `summary` (one row per signal
x profile x resolution, with `mean_sil`, `n_nodes`, `n_singletons`,
`n_excluded`, `coverage`).

## See also

[`sniff_trajectory_comparison()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_comparison.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)

Other flow utilities:
[`sniff_trajectory_comparison()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_comparison.md)
