# Does the absorption forest contain a cycle?

Does the absorption forest contain a cycle?

## Usage

``` r
.flow_has_cycle(tr)
```

## Arguments

- tr:

  A flow `trajectories` data frame (`traj_id`, `absorbed_into`).

## Value

`TRUE` if following `absorbed_into` from any trajectory revisits a node.
