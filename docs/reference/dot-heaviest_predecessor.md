# Heaviest single predecessor per node

Heaviest single predecessor per node

## Usage

``` r
.heaviest_predecessor(edges)
```

## Arguments

- edges:

  Tibble from
  [`.build_global_edges()`](https://roneyfraga.com/birddog/reference/dot-build_global_edges.md)
  (`from`, `to`, `weight`, `documents`).

## Value

Named character vector mapping each `to` node to its single heaviest
predecessor (max `weight`, tie-break max `documents`).
