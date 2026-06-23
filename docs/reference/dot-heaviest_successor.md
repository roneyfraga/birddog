# Heaviest single successor per node

Heaviest single successor per node

## Usage

``` r
.heaviest_successor(edges)
```

## Arguments

- edges:

  Tibble from
  [`.build_global_edges()`](https://roneyfraga.com/birddog/reference/dot-build_global_edges.md)
  (`from`, `to`, `weight`, `documents`), already top-`k_out` per source.

## Value

Named character vector mapping each `from` node to its single heaviest
successor (max `weight`, tie-break max `documents`).
