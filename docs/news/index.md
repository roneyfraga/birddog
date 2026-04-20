# Changelog

## birddog 1.0.5

### Bug fixes

- [`sniff_groups_terms()`](https://roneyfraga.com/birddog/reference/sniff_groups_terms.md):
  replace base [`tolower()`](https://rdrr.io/r/base/chartr.html) with
  [`stringr::str_to_lower()`](https://stringr.tidyverse.org/reference/case.html)
  to avoid locale-dependent failures on non-ASCII UTF-8 text.
- [`sniff_groups_terms()`](https://roneyfraga.com/birddog/reference/sniff_groups_terms.md):
  fix `tryCatch` scoping bug where error-handler assignments to
  `stats[[i]]` were lost, causing `purrr::map_int(stats, nrow)` to abort
  the whole run on a single failing group. Skipped groups are now
  dropped cleanly via
  [`purrr::compact()`](https://purrr.tidyverse.org/reference/keep.html).

## birddog 1.0.4

CRAN release: 2026-04-04

### New features

- [`mixed_sort()`](https://roneyfraga.com/birddog/reference/mixed_sort.md):
  exported for external use, with improved documentation.
- [`plot_group_trajectories_lines_2d()`](https://roneyfraga.com/birddog/reference/plot_group_trajectories_lines_2d.md):
  new `width_by_traj_size` parameter to scale line width by trajectory
  size (default TRUE).
- [`plot_group_trajectories_lines_3d()`](https://roneyfraga.com/birddog/reference/plot_group_trajectories_lines_3d.md):
  new `width_by_traj_size` parameter to scale line width by trajectory
  size (default TRUE).

### Improvements

- Rewrite vignette with biogas dataset and gallery-with-code format.
- Fix README: wrong package name and CRAN URL, add Features section.

## birddog 1.0.2

### New features

- [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md):
  support for multiple key-routes and natural group ordering.
- [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md):
  new `compact_gaps` and `direction` arguments.
- [`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md):
  new `mode` and `window_size` arguments.
- [`get_openalex_fields()`](https://roneyfraga.com/birddog/reference/get_openalex_fields.md):
  cache saved batches to skip duplicate API downloads.

### Improvements

- Optimize SPC and key-route search with pre-built adjacency lists.
- Pre-render horizon plots to reduce gt object size in
  [`sniff_groups_attributes()`](https://roneyfraga.com/birddog/reference/sniff_groups_attributes.md).
- Improve key-route plot layout, axis labels, and data output.
- Improve horizon plot x-axis: vertical year labels, removed title.
- Use 95th percentile for entropy diff plot y-axis range.
- Swap diff colors in index plots: red for negative, blue for positive.
- Add `y_limits_diff` parameter to
  [`indexes_plots()`](https://roneyfraga.com/birddog/reference/indexes_plots.md).

### Bug fixes

- Fix y-axis scale across entropy plots for group comparison.
- Fix dplyr variable collision in
  [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md)
  group filter.
- Fix `rlang` `.env` import to resolve R CMD check NOTE.
- Fix plot issues: diff colors and same-year node spreading.

## birddog 1.0.0

CRAN release: 2026-02-19

- Initial CRAN release.
- Rewrite of
  [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md),
  [`sniff_citations_cycle_time()`](https://roneyfraga.com/birddog/reference/sniff_citations_cycle_time.md),
  and
  [`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md).
- Added `testthat` test suite with 102 tests across 16 files.
- Fix BibTeX parser for Windows CRLF line endings.
- Fix CRAN check issues: Unicode chars, `\dontrun` examples, DOI URLs.
- Replace `\dontrun` with `\donttest` in all examples.
- Add error guards to vignettes to prevent crashes.

## birddog 0.1.0

- Initial development version.
- Added
  [`sniff_key_route()`](https://roneyfraga.com/birddog/reference/sniff_key_route.md)
  function for main path analysis.
- Includes visualization capabilities.
- Implements methodology from Liu & Lu (2012).
