# Build the global temporal edge table by bibliographic coupling

The coupling twin of
[`.build_global_edges()`](https://roneyfraga.com/birddog/reference/dot-build_global_edges.md):
consecutive-year edges weighted by the Jaccard of two nodes'
cited-reference sets (the references their documents cite) instead of
the Jaccard of their shared documents. Same filters:
`weight >= jaccard_min`, consecutive years only, top `k_out` outgoing
edges per source. Two nodes can couple without sharing a single
document.

## Usage

``` r
.build_coupling_edges(
  docs_per_group,
  references,
  min_group_size = 10,
  jaccard_min = 0.05,
  k_out = 2
)
```

## Arguments

- docs_per_group:

  Tibble with `group_id` and `document_id`.

- references:

  Data frame with `document_id` and `feature` (one cited reference per
  row).

- min_group_size:

  Minimum distinct-document count for a node (default 10).

- jaccard_min:

  Minimum Jaccard weight to keep an edge (default 0.05).

- k_out:

  Max outgoing edges kept per source node (default 2).

## Value

Tibble with `from`, `to`, `weight`, `documents` (here `documents` is the
count of shared references, the tie-break analogue of the overlap
count).
