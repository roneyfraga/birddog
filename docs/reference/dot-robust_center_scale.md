# Robust centre and scale (median, scaled MAD) with zero-spread fallbacks

The scale is the scaled MAD (consistent with a standard deviation under
normality), falling back to `IQR / 1.349` then `sd` when the MAD is
zero, so a single outlier cannot inflate it. Returns `c(centre, scale)`.

## Usage

``` r
.robust_center_scale(x)
```
