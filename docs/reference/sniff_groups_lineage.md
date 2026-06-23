# Per-group lineage data

This function analyzes the evolution of document groups over time to
detect technological trajectories and scientific emergence patterns. It
computes similarity measures between groups across time periods and
tracks their attributes.

## Usage

``` r
sniff_groups_lineage(
  groups_cumulative,
  min_group_size = 10,
  top_n_keywords = 3
)
```

## Arguments

- groups_cumulative:

  A list of cumulative group data over time, typically produced by other
  functions in the birddog package. Each element should contain network,
  documents, and groups data.

- min_group_size:

  Minimum number of documents required for a group to be considered
  (default: 10). Smaller groups will be filtered out.

- top_n_keywords:

  Number of top keywords to consider when analyzing group
  characteristics (default: 3).

## Value

A list with three components:

- groups_attributes: A list of data frames containing attributes for
  each tracked group

- groups_similarity: A list of data frames containing Jaccard similarity
  measures between groups across time periods

- docs_per_group: A data frame containing document IDs for all groups
  across time periods

## Details

Prepares lineage data (`docs_per_group`, `groups_similarity`,
`groups_attributes`); detects no trajectories – see
[`sniff_trajectory_braid()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_braid.md).

## See also

Other groups (stock):
[`sniff_components()`](https://roneyfraga.com/birddog/reference/sniff_components.md),
[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md),
[`sniff_groups_attributes()`](https://roneyfraga.com/birddog/reference/sniff_groups_attributes.md),
[`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md),
[`sniff_groups_cumulative_citations()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative_citations.md),
[`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md),
[`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md),
[`sniff_network()`](https://roneyfraga.com/birddog/reference/sniff_network.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have cumulative group data:
trajectories <- sniff_groups_lineage(groups_cumulative, min_group_size = 15)
} # }
```
