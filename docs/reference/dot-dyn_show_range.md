# Axis range that always shows every state region

Spans the data, the threshold cuts, and (for a `[0,1]` share axis) the
unit interval, with padding so the outer state bands keep a visible
height even when the points cluster.

## Usage

``` r
.dyn_show_range(vals, cuts, bound01 = FALSE, pad_frac = 0.1, min_pad = 0.04)
```
