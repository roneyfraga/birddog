# Count unique documents along a path

Calculates the number of unique documents covered by a trajectory path,
accounting for document overlap between connected nodes.

## Usage

``` r
.count_unique_docs_on_path(g, path_nodes, path_edges)
```

## Arguments

- g:

  igraph object with document information

- path_nodes:

  Character vector of node names along the path

- path_edges:

  Edge sequence along the path

## Value

Integer count of unique documents
