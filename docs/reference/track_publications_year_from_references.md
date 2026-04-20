# Track Publication Years from References

Processes citation data from network objects to extract and fetch
publication years of cited references. Handles both Web of Science and
OpenAlex data sources automatically.

## Usage

``` r
track_publications_year_from_references(
  network,
  cr_column = "CR",
  py_column = "PY"
)
```

## Arguments

- network:

  A `tbl_graph` object from tidygraph containing network data

- cr_column:

  Name of the column containing citation references (default: "CR")

- py_column:

  Name of the column containing publication years (default: "PY")

## Value

A `tbl_graph` object with added `CR_PY` column containing publication
years of cited references, separated by pipes ("\|") for multiple
references

## Examples

``` r
if (FALSE) { # \dontrun{
# Process Web of Science network data
network_with_years <- track_publications_year_from_references(network_wos)

# Process OpenAlex network data
network_with_years <- track_publications_year_from_references(network_oa)

# Specify custom column names
network_with_years <- track_publications_year_from_references(
  network, 
  cr_column = "references", 
  py_column = "year"
)
} # }
```
