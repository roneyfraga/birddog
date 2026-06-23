# Build the global temporal edge table from docs_per_group

Consecutive-year Jaccard between group-year nodes, reconstructed
entirely from `docs_per_group`. Applies the edge filters globally,
without an anchor group: keep `weight >= jaccard_min`, consecutive years
only, top `k_out` outgoing edges per source node.

## Usage

``` r
.build_global_edges(
  docs_per_group,
  min_group_size = 10,
  jaccard_min = 0.05,
  k_out = 2
)
```

## Arguments

- docs_per_group:

  Tibble with `group_id` and `document_id`.

- min_group_size:

  Minimum distinct-document count for a node (default 10).

- jaccard_min:

  Minimum Jaccard weight to keep an edge (default 0.05).

- k_out:

  Max outgoing edges kept per source node (default 2).

## Value

Tibble with `from`, `to`, `weight`, `documents`.
