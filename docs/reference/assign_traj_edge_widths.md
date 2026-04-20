# Assign trajectory-specific edge attributes

Computes edge-level trajectory identifiers and widths based on
cumulative paper counts along each trajectory path.

## Usage

``` r
assign_traj_edge_widths(
  g,
  tr_tbl,
  width_range = c(0.8, 6),
  use_raw_papers = FALSE
)
```

## Arguments

- g:

  igraph object

- tr_tbl:

  Tibble of trajectories with `traj_id` and `nodes` columns

- width_range:

  Numeric range for edge width scaling (default: c(0.8, 6.0))

- use_raw_papers:

  Whether to use raw paper counts (TRUE) or weighted counts (FALSE) for
  width calculation

## Value

Modified igraph with `traj_id` and `traj_width` edge attributes
