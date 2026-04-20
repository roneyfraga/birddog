# Fetch Publication Years for OpenAlex IDs

Internal function to retrieve publication years for OpenAlex work IDs
using the OpenAlex API. Processes data in batches to avoid API rate
limits.

## Usage

``` r
fetch_publication_years(oa_data, batch_size = 100)
```

## Arguments

- oa_data:

  A tibble containing OpenAlex IDs in a column named "CR"

- batch_size:

  Number of IDs to process per API call (default: 100)

## Value

A tibble with added CR_PY column containing publication years
