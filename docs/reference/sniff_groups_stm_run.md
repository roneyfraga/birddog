# Run Structural Topic Modeling Analysis

Performs structural topic modeling on prepared text data and returns
topic proportions and top documents for each topic.

## Usage

``` r
sniff_groups_stm_run(groups_stm_prepare, k_topics = 12, n_top_documents = 50)
```

## Arguments

- groups_stm_prepare:

  A prepared STM object from
  [`sniff_groups_stm_prepare()`](https://roneyfraga.com/birddog/reference/sniff_groups_stm_prepare.md)

- k_topics:

  Number of topics to model (default: 12)

- n_top_documents:

  Number of top documents to each topic (default: 50)

## Value

A list containing:

- topic_proportion2: Data frame with topic proportions and top terms

- tab_top_documents: Data frame of top documents for each topic

## Details

This function:

- Fits an STM model with specified number of topics

- Identifies top terms for each topic

- Calculates topic proportions

- Identifies top documents for each topic

## Examples

``` r
if (FALSE) { # \dontrun{
# Prepare data first
stm_data <- sniff_groups_stm_prepare(network_data)

# Run topic modeling
stm_results <- sniff_groups_stm_run(stm_data, k_topics = 15)

# Access results
stm_results$topic_proportion2  # Topic proportions and terms
stm_results$tab_top_documents  # Top documents per topic
} # }
```
