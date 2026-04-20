# Extract representative keywords from grouped nodes

This function processes nodes grouped in a network (typically by
community detection), and extracts the most frequent and the most
distinctive keywords (using TF-IDF) from a descriptor field such as
keywords or subject terms.

## Usage

``` r
sniff_groups_keywords(net_groups, n_terms = 15, min_freq = 1, sep = ";")
```

## Arguments

- net_groups:

  A list containing a `network` component of class `tbl_graph`, where
  each node has at least two attributes: `group` and `DE`.

- n_terms:

  Integer. The number of top terms to return per group, both by
  frequency and by TF-IDF. Default is 15.

- min_freq:

  Integer. Minimum frequency a term must have in a group to be
  considered. Default is 2.

- sep:

  Character. Separator used in the `DE` field to split multiple terms.
  Default is `";"`.

## Value

A tibble with one row per group, containing two columns:

- `term_freq`: the most frequent terms (with raw frequency).

- `term_tfidf`: the most distinctive terms (with TF-IDF scores).

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'groups' is output from sniff_groups()
groups_keywords <- sniff_groups_keywords(groups)
} # }
```
