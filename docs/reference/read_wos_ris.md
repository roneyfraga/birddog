# Read Web of Science RIS files

Read Web of Science RIS files

## Usage

``` r
read_wos_ris(file, normalized_names = TRUE)
```

## Arguments

- file:

  Character scalar or vector. Path(s) to a WoS export file, or a single
  URL (`http://` or `https://`) pointing to a WoS export.

- normalized_names:

  Logical. If `TRUE` (default), use standardized column names when
  possible; if `FALSE`, keep original WoS field tags.
