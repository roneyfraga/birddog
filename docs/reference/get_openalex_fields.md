# Get Fields from OpenAlex for Work IDs

Retrieves specified fields for OpenAlex work IDs using the OpenAlex API.
Processes data in batches to avoid API rate limits.

## Usage

``` r
get_openalex_fields(
  openalex_ids,
  variables = "publication_year",
  batch_size = 50,
  save_dir = NULL
)
```

## Arguments

- openalex_ids:

  Character vector of OpenAlex work IDs (format: "W1234567890") or a
  data frame/tibble containing a column named "CR" with OpenAlex IDs.
  IDs can be semicolon-separated strings which will be split
  automatically.

- variables:

  Character vector of variable names to fetch from OpenAlex. Options
  include: "publication_year", "doi", "type", "source_display_name", or
  any valid OpenAlex work field. Default is "publication_year".

- batch_size:

  Number of IDs to process per API call (default: 50). Smaller batches
  help avoid API rate limits.

- save_dir:

  Optional path to directory where intermediate results should be saved
  as RDS files. If NULL (default), no saving occurs. Directory will be
  created if it doesn't exist.

## Value

A tibble with the following columns:

- `id`: The OpenAlex work ID

- One column for each requested variable (e.g., "publication_year",
  "doi", "type")

Rows without valid OpenAlex IDs or where API calls fail will have NA
values.

## Details

This function:

1.  Accepts either a character vector of IDs or a data frame with a "CR"
    column

2.  Splits semicolon-separated ID strings into individual IDs

3.  Validates IDs against the pattern "^W\d+\$"

4.  Fetches specified variables from OpenAlex API in batches

5.  Optionally saves each batch to disk as it's processed

6.  Handles API errors gracefully with informative messages

7.  Includes delays between batches to respect API rate limits

## Note

The OpenAlex API has rate limits. This function implements:

- Batch processing to reduce number of API calls

- 0.5 second delays between batches

- Error handling for failed API requests

- Progress messages to track execution

- Optional disk saving for data persistence

If you encounter rate limiting errors, consider reducing batch_size or
implementing longer delays.

## Examples

``` r
if (FALSE) { # \dontrun{
# From a character vector
ids <- c("W2261389918", "W1548650423", "W1504492735")
result <- get_openalex_fields(ids)

# Fetch multiple variables
result <- get_openalex_fields(
  ids,
  variables = c("publication_year", "doi", "type")
)

# From a data frame with CR column
oa_data <- data.frame(CR = c("W123;W456", "W789"))
result <- get_openalex_fields(oa_data)

# Save intermediate results while downloading
result <- get_openalex_fields(
  ids,
  variables = c("publication_year", "source_display_name"),
  save_dir = tempdir()
)
} # }
```
