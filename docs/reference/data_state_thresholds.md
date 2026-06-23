# Data-driven, outlier-robust dynamic-state thresholds

Derives a
[`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)-shaped
list from an observed
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
table, so the cut points adapt to the dataset instead of fixed
constants. The growth cuts are placed `k` robust deviations (median
absolute deviation) from the median, which resists outliers such as a
single fast-growing young core; the bounded metrics use their median as
the neutral split, and `dormancy_share` keeps the absolute majority rule
(its distribution is usually too concentrated at zero to standardize).

## Usage

``` r
data_state_thresholds(dyn, k = 1)
```

## Arguments

- dyn:

  A
  [`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)
  tibble; only its metric columns are read, so the classification it
  currently carries is irrelevant.

- k:

  Number of robust deviations (scaled MAD) above/below the median for
  the growth cuts (default 1): `emergence_growth = median + k*MAD` and
  `decline_growth = median - k*MAD` over the living cores'
  `growth_rate`. Larger `k` widens the maturity band (fewer emergence /
  dormancy calls).

## Value

A list shaped like
[`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md):
`emergence_growth`, `decline_growth` (robust, from the centrals'
growth), `emergence_novelty` (median novelty of the centrals),
`convergence_entropy` (median destination entropy of the absorbed), and
`dormancy_share` (the default majority rule). Any quantity that cannot
be derived (too few points, zero spread) falls back to its
[`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md)
value.

## Details

Use it as a drop-in for the `thresholds` argument; the metrics are
threshold-independent, so one pass is enough to derive and re-classify:
`dyn <- sniff_trajectory_dynamics(flow)`,
`th <- data_state_thresholds(dyn)`,
`dyn <- sniff_trajectory_dynamics(flow, thresholds = th)`.

## See also

[`fixed_state_thresholds()`](https://roneyfraga.com/birddog/reference/fixed_state_thresholds.md),
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md),
[`plot_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/plot_trajectory_dynamics.md)
