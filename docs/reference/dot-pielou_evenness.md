# Pielou's evenness (normalized Shannon entropy) of a vector of categories

`J' = H / log2(n)` over the category frequencies; `0` for a single
category, `NA` for none. Matches the normalization of
[`sniff_entropy()`](https://roneyfraga.com/birddog/reference/sniff_entropy.md).

## Usage

``` r
.pielou_evenness(x)
```
