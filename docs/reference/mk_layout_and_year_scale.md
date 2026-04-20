# Create temporal layout for trajectory plotting

Generates a Sugiyama layout with nodes aligned by publication year,
providing mappings between layout coordinates and actual years.

## Usage

``` r
mk_layout_and_year_scale(g)
```

## Arguments

- g:

  igraph object with year-encoded vertex names

## Value

List with layout data and year scaling information
