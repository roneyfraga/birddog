# Attach document IDs to graph vertices

Adds document ID lists to each vertex in the graph based on the
group-document mapping.

## Usage

``` r
attach_docs_to_vertices(g, docs_tbl)
```

## Arguments

- g:

  igraph object

- docs_tbl:

  Tibble with columns `group_id` and `document_id`

## Value

Modified igraph with `doc_ids` vertex attribute
