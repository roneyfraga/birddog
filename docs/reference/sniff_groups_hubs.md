# Identify Hub Papers in Research Groups

This function analyzes citation networks to identify hub papers within
research groups based on their citation patterns. It calculates several
metrics (Zi, Pi) to classify papers into different hub categories.

## Usage

``` r
sniff_groups_hubs(groups, min_citations = 1)
```

## Arguments

- groups:

  A list containing network data with the following components:

  - network: A tidygraph network object

  - pubs_by_year: Publication counts by year

  - aggregate: Aggregate network statistics

- min_citations:

  Minimum number of citations for a paper to be considered (default: 1)

## Value

A tibble containing:

- group: Research group identifier

- SR: Paper identifier

- TC: Total citations

- Ki: Total citations from all groups

- ki: Citations from within the same group

- Zi: Standardized within-group citation score

- Pi: Citation diversity index

- zone: Hub classification ("noHub", "R5", "R6", "R7")

## Details

The function classifies papers into hub categories based on:

- R5: Knowledge hubs (Zi \>= 2.5 and Pi \<= 0.3)

- R6: Bridging hubs (Zi \>= 2.5 and 0.3 \< Pi \<= 0.75)

- R7: Boundary-spanning hubs (Zi \>= 2.5 and Pi \> 0.75)

## Examples

``` r
if (FALSE) { # \dontrun{

# Assuming 'groups' is output from sniff_groups()

# Identify hub papers
hubs <- sniff_groups_hubs(groups, min_citations = 5)

# View results
head(hubs)
} # }
```
