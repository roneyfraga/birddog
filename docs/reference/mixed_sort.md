# Natural sort for alphanumeric strings

Sort character vectors containing embedded numbers in natural numeric
order rather than lexicographic order. For example, `"c1g2"` comes
before `"c1g10"`, which standard
[`sort()`](https://rdrr.io/r/base/sort.html) would not guarantee.

## Usage

``` r
mixed_sort(x)
```

## Arguments

- x:

  Character vector to sort.

## Value

A character vector sorted in natural numeric order.

## Details

The function extracts all numeric segments from each string using
[`stringr::str_extract_all()`](https://stringr.tidyverse.org/reference/str_extract.html)
and sorts by them sequentially (first number, then second). Designed for
two-segment identifiers such as `"c1g1"`, `"c2g10"`, etc.

## See also

[`sort()`](https://rdrr.io/r/base/sort.html) for standard lexicographic
sorting.

## Examples

``` r
mixed_sort(c("c1g10", "c1g2", "c1g1", "c1g9"))
#> [1] "c1g1"  "c1g2"  "c1g9"  "c1g10"

mixed_sort(c("group12", "group1", "group3"))
#> [1] "group1"  "group3"  "group12"
```
