# Extract top trajectories from graph

Iteratively extracts the highest-scoring disjoint trajectories from the
graph.

## Usage

``` r
extract_top_trajectories(g, M = 5, min_len = 3)
```

## Arguments

- g:

  igraph object with scoring attributes

- M:

  Maximum number of trajectories to extract (default: 5)

- min_len:

  Minimum number of distinct years for valid trajectory (default: 3)

## Value

Tibble of trajectory information
