# Calculate and Visualize Group Attributes from Scientific Networks

This function analyzes publication growth rates and other attributes for
research groups identified in scientific networks. It calculates growth
rates using exponential models, creates horizon plots for visualization,
and generates summary tables.

## Usage

``` r
sniff_groups_attributes(
  groups,
  growth_rate_period = 2010:2022,
  horizon_plot = TRUE,
  show_results = TRUE,
  assign_result = NULL
)
```

## Arguments

- groups:

  A list containing network data with publications by year and group
  information. Must include elements: `network`, `pubs_by_year`, and
  `aggregate`.

- growth_rate_period:

  Numeric vector of years to use for growth rate calculation (default:
  2010:2024).

- horizon_plot:

  Logical indicating whether to include horizon plots in the output
  table (default: TRUE).

- show_results:

  Logical indicating whether to print results to console (default:
  TRUE).

- assign_result:

  Character string specifying a variable name to assign the results to
  in the global environment (default: NULL).

## Value

A list with two components:

- `attributes_table`: A gt table showing group attributes including
  growth rates

- `regression`: A list of model summaries for each group's growth rate
  calculation

## Details

The function performs the following steps:

1.  Calculates growth rates using exponential models for each group

2.  Processes publication age and doubling time metrics

3.  Optionally creates horizon plots for each group's publication trend

4.  Generates a comprehensive summary table

## See also

Other groups (stock):
[`sniff_components()`](https://roneyfraga.com/birddog/reference/sniff_components.md),
[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md),
[`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md),
[`sniff_groups_cumulative_citations()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative_citations.md),
[`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md),
[`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md),
[`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md),
[`sniff_network()`](https://roneyfraga.com/birddog/reference/sniff_network.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming groups is output from sniff_groups()
groups_attributes <- sniff_groups_attributes(groups,
  growth_rate_period = 2010:2022,
  horizon_plot = TRUE
)

# View the results table
print(groups_attributes$attributes_table)

# Access model summaries
groups_attributes$regression
} # }
```
