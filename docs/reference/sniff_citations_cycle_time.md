# Calculate Citation Cycle Time (CCT) indicator

Calculates the Citation Cycle Time (CCT) to measure the pace of
scientific or technological progress in a publication network. Based on
Kayal (1999), the indicator measures the median age of cited
publications, where lower values indicate faster knowledge replacement
cycles.

## Usage

``` r
sniff_citations_cycle_time(
  network,
  scope = "groups",
  start_year = NULL,
  end_year = NULL,
  tracked_cr_py = NULL,
  batch_size = 50,
  min_papers_per_year = 3,
  rolling_window = NULL,
  normalize = "none",
  dispersion = FALSE,
  citation_scope = "global"
)
```

## Arguments

- network:

  Required. Network object containing publication data. For
  `scope = "groups"`: object returned by
  [`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md).
  For `scope = "network"`: network object (`tbl_graph` or `igraph`).

- scope:

  Analysis scope. Either `"groups"` (default) for separate group
  analysis or `"network"` for complete network analysis.

- start_year, end_year:

  Start and end years for temporal analysis. If not specified, uses
  minimum and maximum years found in the data.

- tracked_cr_py:

  Pre-processed citation year data (optional). A tibble with columns
  `CR` (OpenAlex work ID) and `CR_PY` (publication year). If provided,
  skips fetching data from OpenAlex API. Useful for avoiding repeated
  API calls.

- batch_size:

  For OpenAlex data: number of IDs to process per API call (default:
  50). Smaller batches help avoid API rate limits, larger batches
  process data faster but may trigger rate limiting.

- min_papers_per_year:

  Minimum number of papers required in a given year to compute CCT.
  Years with fewer papers are reported as NA (default: 3).

- rolling_window:

  Optional integer for rolling window smoothing. If provided, CCT values
  are smoothed using a centered moving average of the specified width
  (e.g., 3 for a 3-year window). Default is NULL (no smoothing).

- normalize:

  Normalization mode. Either `"none"` (default) for raw CCT values in
  years, or `"domain"` to divide each group's annual CCT by the per-year
  network median across all groups. With `normalize = "domain"`, values
  around 1 indicate average cycle time for the domain; values below 1 =
  short-cycle relative to the domain; values above 1 = long-cycle. Only
  applies when `scope = "groups"`.

- dispersion:

  Logical. If `TRUE`, the returned data tibble includes `p25` and `p75`
  columns (25th and 75th percentiles of per-paper citation-age medians),
  useful for detecting bimodal citation-age distributions and wide
  within-group heterogeneity. Default is `FALSE`.

- citation_scope:

  Either `"global"` (default) to count all references, or `"local"` to
  restrict references to those that are also nodes in the birddog
  network (i.e., intra-corpus citations). Local CCT captures the renewal
  pace of the domain's own knowledge stock and is closer to Lee's (2024)
  sector-level TCT. Currently supported for OpenAlex data only; Web of
  Science data falls back to `"global"` with a warning.

## Value

A list with the following components:

- data:

  Tibble with CCT data containing columns: group, year, index. When
  `dispersion = TRUE`, also includes `p25` and `p75` columns with the
  25th and 75th percentiles of per-paper citation-age medians.

- plots:

  Named list of plotly objects showing temporal evolution of CCT for
  each group. Each plot shows both absolute CCT values and
  year-over-year differences.

- years_range:

  Named vector with start_year and end_year used in the analysis

- tracked_cr_py:

  Citation year data with columns CR and CR_PY. Can be saved and reused
  in subsequent analyses to avoid repeated API calls.

## Details

The Citation Cycle Time (CCT) is calculated following Kayal (1999):

1.  Extract citation IDs from the network's CR column

2.  Fetch publication years for cited works from OpenAlex API using
    [`get_openalex_fields()`](https://roneyfraga.com/birddog/reference/get_openalex_fields.md)

3.  For each publication, calculate the age of each cited reference
    (PY - CR_PY)

4.  Calculate the median citation age per publication

5.  For each year, calculate the median of per-publication medians
    across all publications in that year (annual mode)

Lower CCT values indicate that publications are citing more recent work,
suggesting a faster pace of knowledge replacement. A sudden drop in CCT
within a group signals potential scientific emergence.

The function automatically handles:

- Splitting semicolon-separated citation IDs

- Batch processing of OpenAlex API requests

- Filtering invalid citations (where cited work was published after
  citing work)

- Skipping years with too few papers (`min_papers_per_year`)

- Optional rolling window smoothing for noisy time series

- Creating temporal plots for each group

## References

Kayal AA, Waters RC. An empirical evaluation of the technology cycle
time indicator as a measure of the pace of technological progress in
superconductor technology. IEEE Transactions on Engineering Management.
1999;46(2):127-31.
[doi:10.1109/17.759138](https://doi.org/10.1109/17.759138)

## See also

[`sniff_groups()`](https://roneyfraga.com/birddog/reference/sniff_groups.md),
[`get_openalex_fields()`](https://roneyfraga.com/birddog/reference/get_openalex_fields.md),
[`indexes_plots()`](https://roneyfraga.com/birddog/reference/indexes_plots.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Group analysis
results <- sniff_citations_cycle_time(network_groups, scope = "groups")

# Network analysis
results_network <- sniff_citations_cycle_time(complete_network, scope = "network")

# With rolling window smoothing
results_smooth <- sniff_citations_cycle_time(
  network_groups,
  scope = "groups",
  rolling_window = 3
)

# Domain-normalized CCT (values around 1; <1 = short-cycle relative to domain)
results_norm <- sniff_citations_cycle_time(
  network_groups,
  scope = "groups",
  normalize = "domain"
)

# With dispersion (adds p25 and p75 columns to detect bimodal distributions)
results_disp <- sniff_citations_cycle_time(
  network_groups,
  scope = "groups",
  dispersion = TRUE
)

# Local citations only (intra-corpus references)
results_local <- sniff_citations_cycle_time(
  network_groups,
  scope = "groups",
  citation_scope = "local"
)

# Accessing results
cct_data <- results$data
plots <- results$plots
plots$c1g1  # View plot for specific group

# Reuse citation data to avoid repeated API calls
saved_citations <- results$tracked_cr_py
results2 <- sniff_citations_cycle_time(
  network_groups,
  tracked_cr_py = saved_citations
)
} # }
```
