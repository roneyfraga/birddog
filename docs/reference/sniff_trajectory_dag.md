# Build the soft cumulative-clustering trajectory DAG

Reconstructs, from `docs_per_group` alone, the full temporal DAG of
cluster-year nodes: vertices are every cluster whose distinct-document
count is at least `min_group_size` (isolated nodes included), edges are
consecutive-year Jaccard links (top `k_out` per source,
`weight >= jaccard_min`). Each node is labelled with the final group its
heaviest-successor walk reaches (`terminal_group`) and whether it is a
birth (no strong predecessor). This DAG is the first-class object the
soft trajectories and group-formation views are read from.

## Usage

``` r
sniff_trajectory_dag(
  x,
  min_group_size = 10,
  jaccard_min = 0.05,
  k_out = 2,
  similarity = c("overlap", "coupling"),
  references = NULL
)
```

## Arguments

- x:

  The membership tibble `docs_per_group` (`group_id`, `document_id`,
  `network_until`, `group`) or a
  [`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)
  object (its `$docs_per_group` is used).

- min_group_size:

  Minimum distinct-document count for a node (default 10).

- jaccard_min:

  Minimum Jaccard weight to keep an edge (default 0.05).

- k_out:

  Max outgoing edges kept per source node (default 2).

- similarity:

  How to weight consecutive-year edges: `"overlap"` (default) uses the
  Jaccard of the nodes' shared documents (the cumulative carry-over);
  `"coupling"` uses the Jaccard of their shared cited references
  (bibliographic coupling) and requires `references`. Both detectors
  ([`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
  [`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md))
  route either DAG unchanged.

- references:

  When `similarity = "coupling"`, a data frame with columns
  `document_id` and `feature` (one cited reference per row), the same
  shape as a `content` element of
  [`sniff_trajectory_coherence()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_coherence.md).
  Ignored for `"overlap"`.

## Value

A `birddog_dag` object: a classed list with `graph` (scored igraph DAG
with vertex attrs `year`, `size`, `group`, `is_birth`, `terminal_group`,
plus `doc_ids`), `nodes` (tidy tibble `name`, `year`, `group`, `size`,
`is_birth`, `terminal_group`), `edges` (`from`, `to`, `weight`,
`documents`), `births` (character vector of birth node names),
`last_year`, and the de-duplicated `docs_per_group`.

## See also

[`plot_trajectory_dag()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dag.md)

Other trajectory detection:
[`is_flow()`](https://roneyfraga.com/birddog/reference/is_flow.md),
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md),
[`sniff_trajectory_channel()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_channel.md),
[`subset.birddog_flow()`](https://roneyfraga.com/birddog/reference/subset.birddog_flow.md),
[`validate_flow()`](https://roneyfraga.com/birddog/reference/validate_flow.md)
