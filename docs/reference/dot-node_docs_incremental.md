# Node -\> incremental document ids (docs new to the node's year)

`P_inc(n) = docs(n) \ union{ docs(m) : year(m) == year(n) - 1 }`.
First-year nodes keep all their documents. Strips the cumulative
carry-over so the incremental sets partition the corpus.

## Usage

``` r
.node_docs_incremental(node_docs)
```
