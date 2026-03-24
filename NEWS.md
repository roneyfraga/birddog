# birddog (development version)

## New features

* `mixed_sort()`: exported for external use, with improved documentation.

# birddog 1.0.2

## New features

* `sniff_key_route()`: support for multiple key-routes and natural group ordering.
* `sniff_key_route()`: new `compact_gaps` and `direction` arguments.
* `sniff_entropy()`: new `mode` and `window_size` arguments.
* `get_openalex_fields()`: cache saved batches to skip duplicate API downloads.

## Improvements

* Optimize SPC and key-route search with pre-built adjacency lists.
* Pre-render horizon plots to reduce gt object size in `sniff_groups_attributes()`.
* Improve key-route plot layout, axis labels, and data output.
* Improve horizon plot x-axis: vertical year labels, removed title.
* Use 95th percentile for entropy diff plot y-axis range.
* Swap diff colors in index plots: red for negative, blue for positive.
* Add `y_limits_diff` parameter to `indexes_plots()`.

## Bug fixes

* Fix y-axis scale across entropy plots for group comparison.
* Fix dplyr variable collision in `sniff_key_route()` group filter.
* Fix `rlang` `.env` import to resolve R CMD check NOTE.
* Fix plot issues: diff colors and same-year node spreading.

# birddog 1.0.0

* Initial CRAN release.
* Rewrite of `sniff_key_route()`, `sniff_citations_cycle_time()`, and `sniff_entropy()`.
* Added `testthat` test suite with 102 tests across 16 files.
* Fix BibTeX parser for Windows CRLF line endings.
* Fix CRAN check issues: Unicode chars, `\dontrun` examples, DOI URLs.
* Replace `\dontrun` with `\donttest` in all examples.
* Add error guards to vignettes to prevent crashes.

# birddog 0.1.0

* Initial development version.
* Added `sniff_key_route()` function for main path analysis.
* Includes visualization capabilities.
* Implements methodology from Liu & Lu (2012).
