# Per-year cumulative size of a trajectory (papers)

Counts the documents in each of a trajectory's year-nodes, one (year,
size) row per year. Under cumulative clustering a node already holds
every paper up to its year, so the node size is the trajectory's
cumulative size that year; the series therefore ends at the terminal
cohort size.

## Usage

``` r
.feeder_growth_series(nodes, node_size)
```

## Arguments

- nodes:

  Character vector of node names (e.g. "y2018c1g16").

- node_size:

  Named integer vector from
  [`.node_size_lookup()`](https://roneyfraga.com/birddog/reference/dot-node_size_lookup.md).

## Value

A tibble with integer columns `year` and `size`, ordered by year.
