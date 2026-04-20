# Score nodes and edges for trajectory detection

Computes node scores based on paper quantity and proportion tracked, and
edge scores based on similarity and document overlap.

## Usage

``` r
score_nodes_edges(g, alpha = 1, beta = 0.1)
```

## Arguments

- g:

  igraph object

- alpha:

  Weight for edge strength in scoring (default: 1)

- beta:

  Per-step persistence bonus (default: 0.1)

## Value

Modified igraph with node_score and edge_score attributes
