# Share of a trajectory's year-steps that keep growing at the emergence rate

Staying power in the spirit of Carley et al. (2017): the fraction of a
lineage's consecutive-year steps whose local cumulative growth clears
`growth_bar` (the `emergence_growth` cut). High = the lineage stayed in
the emergent regime year after year; low = it grew once and plateaued.
`NA` for a single-node lineage (no step to score). Unlike `age`, it is
not a span: a long lineage that grew briefly scores low, a short one
that kept growing scores high.

## Usage

``` r
.traj_emergence_density(gs, growth_bar)
```
