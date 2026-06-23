# Exponential growth of a trajectory's recent size curve (pillar: growth)

Annualized growth rate from a log-linear fit of size on year over the
last `window` + 1 curve points (the trajectory's recent momentum, in the
spirit of the field-level exponential model y(t) = b0 e^(b1 (t - t0))).
Returns `growth_rate` = exp(b1) - 1 and `doubling_time` = ln(2) / b1 (NA
unless growing).

## Usage

``` r
.traj_growth(gs, window = 3)
```
