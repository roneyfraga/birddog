# Node -\> distinct document ids

Node -\> distinct document ids

## Usage

``` r
.node_docs(docs_per_group)
```

## Arguments

- docs_per_group:

  Membership tibble with `group_id` and `document_id`.

## Value

Named list mapping each node (`group_id`) to its distinct `document_id`
vector.
