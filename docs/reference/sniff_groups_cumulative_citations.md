# Calculate Cumulative Citations by Group and Year

This function calculates cumulative citations for papers within research
groups, tracking how citations accumulate over time for highly cited
papers.

## Usage

``` r
sniff_groups_cumulative_citations(groups, min_citations = 5)
```

## Arguments

- groups:

  A list containing network data with the following components:

  - network: A tidygraph network object

  - pubs_by_year: Publication counts by year

  - aggregate: Aggregate network statistics

- min_citations:

  Minimum number of citations for a paper to be included in analysis
  (default: 10).

## Value

A named list (by research group) where each element contains a tibble
with:

- `group`: Research group identifier

- `SR`: Paper identifier

- `TC`: Total citations

- `PY`: Publication year

- `Ki`: Total network citations

- `citations_by_year`: A tibble with annual citation counts (PY: year,
  citations: count)

- `growth_power`: Growth power score (0-100)

- `growth_consistency`: Percentage of years with citations

- `peak_momentum`: Highest 3-year rolling average citation count

- `early_impact`: Citations in first 5 years

- `recent_momentum`: Citations in last 3 years

- `acceleration_factor`: Ratio of late to early citations

## Details

For each research group, the function:

- Identifies papers with citations above the threshold

- Tracks citations to these papers year by year

- Calculates cumulative citation patterns

- Computes various growth metrics for citation analysis

Works with both Web of Science (WOS) and OpenAlex data formats.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming groups is output from sniff_groups()
# Calculate cumulative citations
groups_cumulative_citations <- sniff_groups_cumulative_citations(groups, min_citations = 5)
# View results for first group
head(groups_cumulative_citations[[1]])
} # }
```
