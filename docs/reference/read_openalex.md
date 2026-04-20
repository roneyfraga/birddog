# Read and Process OpenAlex data

Parse datasets exported from **OpenAlex** in two ways: (1) a CSV file
exported in the browser, or (2) a data frame obtained via the
`{openalexR}` API helpers. The function standardizes fields to common
bibliographic tags (e.g., `AU`, `SO`, `CR`, `PY`, `DI`) and returns a
tidy tibble.

## Usage

``` r
read_openalex(file, format = "csv")
```

## Arguments

- file:

  For `format = "csv"`, a character string with a local path or an
  HTTP(S) URL to a CSV export. For `format = "api"`, a data frame
  produced by `{openalexR}` for the **works** entity.

- format:

  Either `"csv"` (CSV export) or `"api"` (data frame from
  `{openalexR}`).

## Value

A tibble with standardized bibliographic columns. Typical output
includes: `id_short`, `AU`, `DI`, `CR`, `SO`, `DT`, `DE`, `AB`, `C1`,
`TC`, `SC`, `SR`, `PY`, and `DB` (source flag: `"openalex_csv"` or
`"openalex_api"`). See **Details**.

## Details

**CSV mode (`format = "csv"`):**

- If `file` is a URL, it is downloaded to a temporary file before
  parsing (a progress message is printed).

- Selected fields are mapped to standardized tags: `id_short` (short
  OpenAlex ID), `SR` (= `id_short`), `PY` (= `publication_year`), `TI`
  (= `title`), `DI` (= `doi`), `DT` (= `type`), `DE` (=
  `keywords.display_name`), `AB` (= `abstract`), `AU` (=
  `authorships.author.display_name`), `SO` (=
  `locations.source.display_name`), `C1` (= `authorships.countries`),
  `TC` (= `cited_by_count`), `SC` (=
  `primary_topic.field.display_name`), `CR` (= `referenced_works`, with
  the `https://openalex.org/` prefix stripped), and
  `DB = "openalex_csv"`.

- `PY` is coerced to numeric; a helper column `DI2` (uppercase,
  punctuation-stripped variant of `DI`) is added; columns with all-caps
  tags are placed first and `DI2` is relocated after `DI`.

**API mode (`format = "api"`):**

- `file` must be a data frame containing at least column `id`; typically
  this is returned by
  [`openalexR::oa_request()`](https://docs.ropensci.org/openalexR/reference/oa_request.html) +
  [`openalexR::oa2df()`](https://docs.ropensci.org/openalexR/reference/oa2df.html)
  or similar.

- Records are filtered to `type %in% c("article","review")` and
  deduplicated by `id`.

- The function derives:

  - `id_short` (= `id` without the `https://openalex.org/` prefix) and
    `SR` (= `id_short`);

  - `CR`: concatenated short IDs from `referenced_works`
    (semicolon-separated);

  - `DE`: concatenated keyword names (lower case) from `keywords`;

  - `AU`: concatenated author names (upper case) from `authorships`;

  - plus core fields `PY` (= `publication_year`), `TC` (=
    `cited_by_count`), `TI` (= `title`), `AB` (= `abstract`), `DI` (=
    `doi`), and `DB = "openalex_api"`.

- The result keeps one row per `id` and may include original columns
  from the input (via a right join), after constructing the standardized
  fields above.

## Supported inputs

- `format = "csv"` — a local path or an HTTP(S) URL to an OpenAlex
  **CSV** export.

- `format = "api"` — a **data frame** produced by `{openalexR}` for the
  **works** entity (with the usual OpenAlex columns, including
  list-columns such as `keywords`, `authorships`, and
  `referenced_works`).

## See also

OpenAlex R client:
[`oa_request`](https://docs.ropensci.org/openalexR/reference/oa_request.html),
[`oa2df`](https://docs.ropensci.org/openalexR/reference/oa2df.html).
Importers for Web of Science:
[`read_wos`](https://roneyfraga.com/birddog/reference/read_wos.md).

## Examples

``` r
if (FALSE) { # \dontrun{
## CSV export (local path)
x <- read_openalex("openalex-works.csv", format = "csv")

## Using the API with openalexR
library(openalexR)
url_api <- "https://api.openalex.org/works?page=1&filter=primary_location.source.id:s121026525"
df_api  <- openalexR::oa_request(query_url = url_api) |>
  openalexR::oa2df(entity = "works")
y <- read_openalex(df_api, format = "api")
} # }
```
