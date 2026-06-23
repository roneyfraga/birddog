# Document count per year-node

Document count per year-node

## Usage

``` r
.node_size_lookup(docs_per_group)
```

## Arguments

- docs_per_group:

  Membership tibble with `group_id` and `document_id`.

## Value

Named integer vector mapping node name (e.g. "y2018c1g16") to its
distinct document count.
