# Per-node silhouette over a distance matrix and a cluster labeling

`sil = (b - a) / max(a, b)`. Singletons and single-cluster sets get 0.

## Usage

``` r
.silhouette(dmat, label)
```

## Arguments

- dmat:

  A square numeric distance matrix with node names on both axes.

- label:

  Named vector of cluster labels; names must cover `rownames(dmat)`.
  Indexed by name, then used positionally.

## Value

A tibble with columns `node`, `a`, `b`, `sil`.
