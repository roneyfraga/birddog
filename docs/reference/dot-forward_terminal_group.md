# Terminal group label reached by following heaviest successors forward

Terminal group label reached by following heaviest successors forward

## Usage

``` r
.forward_terminal_group(nodes, succ)
```

## Arguments

- nodes:

  Character vector of node names (e.g. `"y2000c1g1"`).

- succ:

  Named vector from
  [`.heaviest_successor()`](https://roneyfraga.com/birddog/reference/dot-heaviest_successor.md).

## Value

Character vector of terminal group labels, one per input node.
