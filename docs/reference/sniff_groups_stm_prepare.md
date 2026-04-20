# Prepare Text Data and Analyze Topic Models

Processes text data for structural topic modeling and performs topic
number selection analysis, returning both the processed data and
diagnostic plots.

## Usage

``` r
sniff_groups_stm_prepare(
  groups,
  group_to_stm = "g01",
  search_topics = c(5:40, 45, 50, 55, 60),
  seed = 1234,
  cores = 1
)
```

## Arguments

- groups:

  A list containing network data with a 'network' component

- group_to_stm:

  Character string specifying which research group to process (default:
  'g01')

- search_topics:

  Numeric vector of topic numbers to evaluate (default: c(5:40, 45, 50,
  55, 60))

- seed:

  Random seed for reproducibility (default: 1234)

- cores:

  Number of CPU cores to use (default: 1)

## Value

A list containing:

- result: The searchK results object

- plots: A list containing two ggplot objects (p1: metrics by K, p2:
  exclusivity vs coherence)

- df_prep: Output from stm::textProcessor

- df_doc: Output from stm::prepDocuments

- df: Original filtered data

## Examples

``` r
if (FALSE) { # \dontrun{
output <- sniff_groups_stm_prepare(network_data)
output$plots$p1 # View first plot
output$result # Access search results
} # }
```
