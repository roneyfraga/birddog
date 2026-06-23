# Stack a node -\> docs list into a (node, document_id) data frame

Stack a node -\> docs list into a (node, document_id) data frame

## Usage

``` r
.stack_profiles(prof_list)
```

## Arguments

- prof_list:

  Named list mapping each node to a character vector of document ids
  (e.g. from
  [`.node_docs()`](https://roneyfraga.com/birddog/reference/dot-node_docs.md)).

## Value

A `data.frame` with columns `node` and `document_id`, one row per (node,
document); zero rows (not zero columns) when every element is empty.
