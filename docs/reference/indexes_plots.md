# Create CCT or Entropy Visualization Plots

Create CCT or Entropy Visualization Plots

## Usage

``` r
indexes_plots(
  data,
  group_name,
  start_year,
  end_year,
  method = "cct",
  y_limits = NULL,
  y_limits_diff = NULL
)
```

## Arguments

- data:

  Data from calculate_cct or calculate_entropy function. Can be either:

  - A single data frame/tibble with columns: year, index, group

  - A named list where each element is a data frame with columns: year,
    index, group

- group_name:

  Specific group to visualize

- start_year:

  Starting year for x-axis

- end_year:

  Ending year for x-axis

- method:

  Character string indicating the method: "cct" or "entropy"

- y_limits:

  Optional numeric vector of length 2 (c(min, max)) to fix the y-axis
  range for the absolute values plot. If NULL (default), auto-scales per
  group.

- y_limits_diff:

  Optional numeric vector of length 2 (c(min, max)) to fix the y-axis
  range for the differences plot. If NULL (default), auto-scales per
  group.

## Value

A plotly object with combined plots
