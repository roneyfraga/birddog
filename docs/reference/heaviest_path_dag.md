# Find heaviest path in directed acyclic graph

Uses dynamic programming to find the highest-scoring path in a DAG where
scores combine node quality and edge strength.

## Usage

``` r
heaviest_path_dag(g)
```

## Arguments

- g:

  igraph object with node_score and edge_score attributes

## Value

List with path nodes, edges, and total score
