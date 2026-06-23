# Create Citation Networks from Bibliographic Data

Constructs different types of citation networks from bibliographic data
imported from Web of Science or OpenAlex using `birddog's` reading
functions.

## Usage

``` r
sniff_network(dataframe, type = "direct citation", external_references = FALSE)
```

## Arguments

- dataframe:

  A data frame imported via
  [`read_openalex()`](https://roneyfraga.com/birddog/reference/read_openalex.md)
  or
  [`read_wos()`](https://roneyfraga.com/birddog/reference/read_wos.md)

- type:

  Type of network to create. One of:

  - "direct citation": Direct citation links between documents

  - "bibliographic coupling": Documents linked by shared references

- external_references:

  Logical indicating whether to include external references (references
  not in the original dataset) as nodes in the network

## Value

A `tbl_graph` object from the tidygraph package representing the
citation network. Node attributes include bibliographic information from
the input data.

## See also

Other groups (stock):
[`sniff_components()`](https://roneyfraga.com/birddog/reference/sniff_components.md),
[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md),
[`sniff_groups_attributes()`](https://roneyfraga.com/birddog/reference/sniff_groups_attributes.md),
[`sniff_groups_cumulative()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative.md),
[`sniff_groups_cumulative_citations()`](https://roneyfraga.com/birddog/reference/sniff_groups_cumulative_citations.md),
[`sniff_groups_hubs()`](https://roneyfraga.com/birddog/reference/sniff_groups_hubs.md),
[`sniff_groups_influence()`](https://roneyfraga.com/birddog/reference/sniff_groups_influence.md),
[`sniff_groups_lineage()`](https://roneyfraga.com/birddog/reference/sniff_groups_lineage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Using OpenAlex data
oa_data <- read_openalex("works.csv", format = "csv")
net <- sniff_network(oa_data, type = "direct citation")

# Using WoS data
wos_data <- read_wos("savedrecs.txt")
net <- sniff_network(wos_data, type = "bibliographic coupling", external_references = TRUE)
} # }
```
