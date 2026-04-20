# Detect Technological Trajectories from Grouped Documents

This function analyzes the evolution of document groups over time to
detect technological trajectories and scientific emergence patterns. It
computes similarity measures between groups across time periods and
tracks their attributes.

## Usage

``` r
sniff_groups_trajectories(
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

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have cumulative group data:
trajectories <- sniff_groups_trajectories(groups_cumulative, min_group_size = 15)
} # }
```
