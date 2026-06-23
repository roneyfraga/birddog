# Fixed-constant thresholds for dynamic-state classification

The fixed knobs for
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md).
These are **not** the applied default: by default the cut points are
derived from the data via
[`data_state_thresholds()`](https://roneyfraga.com/birddog/reference/data_state_thresholds.md),
and these constants serve as the explicit fixed-constant option (pass
this list as `thresholds`) and as the fallback for any quantity that
cannot be derived. The classification follows two axes (see
[`sniff_trajectory_dynamics()`](https://roneyfraga.com/birddog/reference/sniff_trajectory_dynamics.md)):
a life-cycle **phase** for living (central) trajectories and a terminal
**fate** for declining (absorbed) ones.

## Usage

``` r
fixed_state_thresholds()
```

## Value

A list: `emergence_growth` (min annualized `growth_rate` for a central
to count as emergence; default 0.15), `emergence_novelty` (min `novelty`
for emergence; default 0.30), `decline_growth` (a central at or below
this `growth_rate` is dormant by stall; default -0.05),
`convergence_entropy` (an absorbed trajectory whose normalized
destination entropy is below this converged, else diverged; default
0.5), `dormancy_share` (an absorbed trajectory whose terminal cohort
drops at least this share is dormant by death; default 0.5).
