# Cumulative arrival of a feeder's contributed papers

For the subset of papers that actually flow from a feeder into a target,
counts them cumulatively by the year each first joins one of the
feeder's own year-nodes. Restricting to the feeder's nodes keeps the
curve within the feeder's lifespan, instead of reaching back to a
paper's corpus first- appearance year, which under cumulative clustering
can long precede the trajectory. Monotone non-decreasing, ending at the
number of contributed papers; if their last node-year precedes the
handoff, a final point at `handoff_year` holds the full count.

## Usage

``` r
.contributed_arrival_series(doc_ids, nodes, docs_per_group, handoff_year)
```

## Arguments

- doc_ids:

  Character vector of the contributed document ids.

- nodes:

  Character vector of the feeder trajectory's node names (e.g.
  "y2018c1g16"), restricting arrival to the feeder's own clusters.

- docs_per_group:

  Membership tibble with `group_id`, `document_id`, `network_until`.

- handoff_year:

  Integer year the feeder hands off into the target.

## Value

A tibble with integer columns `year` and `size`, ordered by year.
