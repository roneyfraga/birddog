# Calculate Entropy Based on Keywords Over Time

Computes the normalized Shannon entropy of keyword distributions from
scientific publications over a specified time range. Entropy measures
the diversity and evenness of keyword usage within research groups or
the entire network.

## Usage

``` r
sniff_entropy(
  network,
  scope = "groups",
  start_year = NULL,
  end_year = NULL,
  mode = "rolling",
  window_size = 5
)
```

## Arguments

- network:

  A network object to analyze. For `scope = "groups"`, this should be
  the output of
  [`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md).
  For `scope = "network"`, this should be a `tbl_graph` or `igraph`
  object from
  [`sniff_network()`](https://roneyfraga.com/birddog/reference/sniff_network.md).

- scope:

  Character specifying the analysis scope: "groups" for multiple groups
  or "network" for the entire network (default: "groups").

- start_year:

  Starting year for entropy calculation. If NULL, uses the minimum
  publication year found in the network data.

- end_year:

  Ending year for entropy calculation. If NULL, uses the maximum
  publication year found in the network data.

- mode:

  Character specifying the temporal mode for entropy calculation:

  "annual"

  :   Uses only publications from each specific year (default).

  "cumulative"

  :   Uses all publications from the start up to each year.

  "rolling"

  :   Uses a sliding window of `window_size` years ending at each year.

- window_size:

  Integer specifying the rolling window size in years (default: 5). Only
  used when `mode = "rolling"`.

## Value

A list with three components:

- data:

  A tibble containing entropy values for each group and year

- plots:

  A list of plotly objects visualizing entropy trends for each group

- years_range:

  A vector with the start_year and end_year used in calculations

## Details

The function calculates the normalized Shannon entropy (Pielou's
evenness index) based on Shannon's information theory (Shannon, 1948).
The temporal scope of keyword data depends on the `mode` parameter:

- **annual**: entropy from keywords published in each specific year.
  Values tend to be high (near 1) since within-year distributions are
  typically even.

- **cumulative**: entropy from all keywords published up to each year.
  Shows long-term trends in thematic concentration as the keyword pool
  grows.

- **rolling**: entropy from a sliding window of recent years. Balances
  sensitivity to recent shifts with enough data for stable estimates.

The normalized entropy (Pielou's J') is calculated as: \$\$J' =
\frac{H}{H\_{max}} = \frac{-\sum\_{i=1}^{n} p_i \log_2 p_i}{\log_2
n}\$\$ where \\p_i\\ is the relative frequency of keyword \\i\\, \\n\\
is the number of unique keywords, and \\H\_{max} = \log_2 n\\ is the
maximum possible entropy for \\n\\ categories.

Entropy values range from 0 to 1, where:

- 0 indicates minimal diversity (one dominant keyword)

- 1 indicates maximal diversity (all keywords equally frequent)

A sudden increase in entropy may signal the emergence of new research
topics, while a decrease suggests thematic convergence.

## References

Shannon, C. E. (1948). A mathematical theory of communication. *Bell
System Technical Journal*, 27(3), 379-423.
[doi:10.1002/j.1538-7305.1948.tb01338.x](https://doi.org/10.1002/j.1538-7305.1948.tb01338.x)

Pielou, E. C. (1966). The measurement of diversity in different types of
biological collections. *Journal of Theoretical Biology*, 13, 131-144.

## See also

[`sniff_groups`](https://roneyfraga.com/birddog/reference/sniff_groups.md),
[`sniff_network`](https://roneyfraga.com/birddog/reference/sniff_network.md),
[`indexes_plots`](https://roneyfraga.com/birddog/reference/indexes_plots.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Rolling window (default: 5 years)
entropy_results <- sniff_entropy(groups_data, scope = "groups")

# Cumulative mode
entropy_results <- sniff_entropy(groups_data, mode = "cumulative")

# Annual mode
entropy_results <- sniff_entropy(groups_data, mode = "annual")

# Rolling window with custom size
entropy_results <- sniff_entropy(groups_data, mode = "rolling", window_size = 3)

# Access results
entropy_data <- entropy_results$data
entropy_plots <- entropy_results$plots
} # }
```
