# Share of each cluster's papers that end up in a group's final cluster

For every year-group node in `docs_per_group`, the fraction of its
documents that also belong to `group`'s last-year cluster. Captures how
much of an earlier cluster funnels into the group as it stands in the
final year.

## Usage

``` r
.final_group_share_lookup(docs_per_group, group)
```

## Arguments

- docs_per_group:

  Membership tibble with `group_id`, `document_id`, `network_until` and
  `group`.

- group:

  Final-year group label (e.g. "c1g1").

## Value

Named numeric vector keyed by `group_id`, each in `[0, 1]`.
