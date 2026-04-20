# Read Web of Science exported files

Parse Web of Science (WoS) export files in multiple formats and return a
tidy table. The function automatically dispatches to a specialized
parser based on the `format` argument and can also **download from a
URL** if `file` points to an `http://` or `https://` resource.

## Usage

``` r
read_wos(file, format = "bib", normalized_names = TRUE)
```

## Arguments

- file:

  Character scalar or vector. Path(s) to a WoS export file, or a single
  URL (`http://` or `https://`) pointing to a WoS export.

- format:

  Character scalar. Export format; one of `"bib"`, `"ris"`,
  `"txt-plain-text"`, or `"txt-tab-delimited"`.

- normalized_names:

  Logical. If `TRUE` (default), use standardized column names when
  possible; if `FALSE`, keep original WoS field tags.

## Value

A tibble with the parsed WoS records. See **Details** for notes on
added/coerced columns (`DI2`, `PY`, `DB`) and column ordering.

## Details

- `file` may be a single path/URL or a **vector of paths**; multiple
  files will be combined row-wise when applicable.

- When `file` is a URL, the file is downloaded to a temporary path
  before parsing (a progress message is printed).

- If `normalized_names = TRUE`, selected WoS tags are mapped to
  standardized names (e.g., `AU` -\> `author`, `TI` -\> `title`, `PY`
  -\> `year`, `DI` -\> `doi`, `DE` -\> `keywords`, `SR` -\> `unique_id`,
  etc.; the exact mapping depends on the format). Otherwise, original
  field tags are preserved.

- The output includes:

  - `DI2`: an uppercase, punctuation-stripped variant of `DI` (if
    present),

  - `PY`: coerced to numeric (when present),

  - `DB`: a provenance flag indicating the source/format and whether
    names were normalized.

- Columns with ALL-CAPS tags (e.g., `AU`, `TI`, `PY`) are placed first,
  followed by other columns, and `DI2` is relocated just after `DI`.

## Supported formats

- `"bib"` — BibTeX export

- `"ris"` — RIS export

- `"txt-plain-text"` — Plain-text export

- `"txt-tab-delimited"` — Tab-delimited export

## See also

Internal parsers used by this function:
[`read_wos_bib`](https://roneyfraga.com/birddog/reference/read_wos_bib.md),
[`read_wos_ris`](https://roneyfraga.com/birddog/reference/read_wos_ris.md),
[`read_wos_plain`](https://roneyfraga.com/birddog/reference/read_wos_plain.md),
[`read_wos_tab`](https://roneyfraga.com/birddog/reference/read_wos_tab.md).

## Examples

``` r
bib_file <- system.file("extdata", "sample_wos.bib", package = "birddog")
M <- read_wos(bib_file, format = "bib", normalized_names = TRUE)
head(M)
#> # A tibble: 3 × 17
#>   AU     TI    SO       PY DI    DI2   TC    SR    DE    ID    AB    DT    CR   
#>   <chr>  <chr> <chr> <dbl> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 Silva… Adva… JOUR…  2020 10.1… 1012… 12    WOS:… cita… BIBL… This… Arti… John…
#> 2 Ferre… Comm… RESE…  2021 10.1… 1012… 8     WOS:… pate… PATE… We a… Arti… Silv…
#> 3 Olive… Mapp… JOUR…  2023 10.1… 1012… 3     WOS:… topi… STRU… This… Arti… Silv…
#> # ℹ 4 more variables: DB <chr>, volume <chr>, number <chr>, pages <chr>

if (FALSE) { # \dontrun{
# load data from a URL
M <- read_wos("https://example.com/savedrecs.bib", format = "bib")
} # }
```
