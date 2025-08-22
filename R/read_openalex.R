#' Read and Process OpenAlex data
#'
#' Parse datasets exported from **OpenAlex** in two ways:
#' (1) a CSV file exported in the browser, or
#' (2) a data frame obtained via the `{openalexR}` API helpers.
#' The function standardizes fields to common bibliographic tags (e.g., `AU`,
#' `SO`, `CR`, `PY`, `DI`) and returns a tidy tibble.
#'
#' @section Supported inputs:
#' * `format = "csv"` — a local path or an HTTP(S) URL to an OpenAlex **CSV** export.
#' * `format = "api"` — a **data frame** produced by `{openalexR}` for the
#'   **works** entity (with the usual OpenAlex columns, including list-columns
#'   such as `keywords`, `authorships`, and `referenced_works`).
#'
#' @details
#' **CSV mode (`format = "csv"`):**
#' - If `file` is a URL, it is downloaded to a temporary file before parsing
#'   (a progress message is printed).
#' - Selected fields are mapped to standardized tags:
#'   `id_short` (short OpenAlex ID), `SR` (= `id_short`), `PY` (= `publication_year`),
#'   `TI` (= `title`), `DI` (= `doi`), `DT` (= `type`), `DE` (= `keywords.display_name`),
#'   `AB` (= `abstract`), `AU` (= `authorships.author.display_name`),
#'   `SO` (= `locations.source.display_name`),
#'   `C1` (= `authorships.countries`), `TC` (= `cited_by_count`),
#'   `SC` (= `primary_topic.field.display_name`), `CR` (= `referenced_works`,
#'   with the `https://openalex.org/` prefix stripped),
#'   and `DB = "openalex_csv"`.
#' - `PY` is coerced to numeric; a helper column `DI2` (uppercase, punctuation-stripped
#'   variant of `DI`) is added; columns with all-caps tags are placed first and
#'   `DI2` is relocated after `DI`.
#'
#' **API mode (`format = "api"`):**
#' - `file` must be a data frame containing at least column `id`; typically this
#'   is returned by `openalexR::oa_request()` + `openalexR::oa2df()` or similar.
#' - Records are filtered to `type %in% c("article","review")` and deduplicated by `id`.
#' - The function derives:
#'   * `id_short` (= `id` without the `https://openalex.org/` prefix) and `SR` (= `id_short`);
#'   * `CR`: concatenated short IDs from `referenced_works` (semicolon-separated);
#'   * `DE`: concatenated keyword names (lower case) from `keywords`;
#'   * `AU`: concatenated author names (upper case) from `authorships`;
#'   * plus core fields `PY` (= `publication_year`), `TC` (= `cited_by_count`),
#'     `TI` (= `title`), `AB` (= `abstract`), `DI` (= `doi`),
#'     and `DB = "openalex_api"`.
#' - The result keeps one row per `id` and may include original columns from the
#'   input (via a right join), after constructing the standardized fields above.
#'
#' @param file For `format = "csv"`, a character string with a local path or an
#'   HTTP(S) URL to a CSV export. For `format = "api"`, a data frame produced by
#'   `{openalexR}` for the **works** entity.
#' @param format Either `"csv"` (CSV export) or `"api"` (data frame from `{openalexR}`).
#'
#' @return A tibble with standardized bibliographic columns. Typical output includes:
#' `id_short`, `AU`, `DI`, `CR`, `SO`, `DT`, `DE`, `AB`, `C1`, `TC`, `SC`, `SR`,
#' `PY`, and `DB` (source flag: `"openalex_csv"` or `"openalex_api"`). See **Details**.
#'
#' @seealso
#' OpenAlex R client: \code{\link[openalexR]{oa_request}}, \code{\link[openalexR]{oa2df}}.  
#' Importers for Web of Science: \code{\link{read_wos}}.
#'
#' @examples
#' \dontrun{
#' ## CSV export (local path)
#' x <- read_openalex("~/Downloads/openalex-works.csv", format = "csv")
#'
#' ## CSV export (URL)
#' x <- read_openalex("http://yoursite/openalex-works-2025-05-28T23-12-11.csv", format = "csv")
#'
#' ## Using the API with openalexR
#' # install.packages("openalexR")
#' library(openalexR)
#' url_api <- "https://api.openalex.org/works?page=1&filter=primary_location.source.id:s121026525"
#' df_api  <- openalexR::oa_request(query_url = url_api) |>
#'   openalexR::oa2df(entity = "works")
#' y <- read_openalex(df_api, format = "api")
#'
#' }
#'
#' @export
#' @importFrom rlang .data
read_openalex <- function(file, format = "csv") {
  # Validate format argument
  allowed_formats <- c("csv", "api")

  if (!format %in% allowed_formats) {
    stop("`format` must be either 'csv' or 'api'", call. = FALSE)
  }
  
  if (format == "csv") {

    is_url <- grepl("^http?://", file)

    # Validate file input for CSV format
    if (!is.character(file) || !is_url) {
      stop("For CSV format, `file` must be a character string specifying the file path or a url address", call. = FALSE)
    }
 
    if (is_url) { # from http*
      temp_file <- tempfile(fileext = ".csv")

      tryCatch({
        utils::download.file(file, temp_file, quiet = FALSE)
        message("Downloading data from URL: ", file)
      }, error = function(e) {
          stop("Error downloading file from URL: ", e$message, call. = FALSE)
        })

      res <- readr::read_csv(temp_file, show_col_types = FALSE)

      unlink(temp_file)

    } else { # local file

      if (!file.exists(file)) {
        stop("File not found at: ", file, call. = FALSE)
      }

      res <- readr::read_csv(file, show_col_types = FALSE)

    }

    # Process CSV data
    tryCatch({
      
      # Standardize column names
      res |>
        dplyr::mutate(
          id_short = gsub("https://openalex.org/", "", .data$id),
          CR = gsub("https://openalex.org/", "", .data$referenced_works),
          AU = .data$authorships.author.display_name,
          SO = .data$locations.source.display_name,
          SR = .data$id_short,
          PY = .data$publication_year,
          TI = .data$title,
          DI = .data$doi,
          DT = .data$type,
          DE = .data$keywords.display_name,
          AB = .data$abstract,
          C1 = .data$authorships.countries,
          TC = .data$cited_by_count,
          SC = .data$primary_topic.field.display_name,
          DB = paste('openalex', format, sep = '_')
        ) |>
        dplyr::relocate(.data$id_short, .data$TI, .data$AU, .data$PY, .data$DI, .data$CR, .data$SO, .data$DT, .data$DE, .data$AB, .data$C1, .data$TC, .data$SC, .data$SR, .data$DB) |>
        dplyr::mutate(PY = as.numeric(.data$PY)) ->
        res
      
      return(res)
    }, error = function(e) {
      stop("Error reading CSV file: ", e$message, call. = FALSE)
    })
  }
  
  if (format == "api") {
    # Validate file input for API format
    if (!inherits(file, "data.frame")) {
      stop("For API format, `file` must be a data frame from openalexR", call. = FALSE)
    }
    if (!"id" %in% names(file)) {
      stop("Input data frame must contain 'id' column", call. = FALSE)
    }
    
    tryCatch({
      # Process API data
      file |>
        dplyr::filter(.data$type %in% c('article', 'review')) |>
        dplyr::distinct(.data$id, .keep_all = TRUE) |>
        dplyr::mutate(
          id_short = gsub('https://openalex.org/', '', .data$id),
          SR = .data$id_short
        ) |>
        dplyr::relocate(.data$id_short) ->
        papers

      # Process references
      papers |>
        dplyr::mutate(referenced_works_short = purrr::map(.data$referenced_works, ~gsub('https://openalex.org/', '', .x))) |>
        dplyr::select(.data$id_short, .data$referenced_works_short) |>
        tidyr::unnest_longer(col = 'referenced_works_short') |>
        dplyr::filter(!is.na(.data$referenced_works_short)) |>
        dplyr::group_by(.data$id_short) |>
        dplyr::summarise(CR = paste(.data$referenced_works_short, collapse = ';')) ->
        papers_ref
      
      # Process keywords
      tibble::tibble(
        id = papers$id,
        keywords_inline = purrr::map_chr(papers$keywords, ~paste(purrr::pluck(.x, 'display_name'), collapse = ';') |> tolower())
      ) |>
        dplyr::rename(DE = .data$keywords_inline) ->
        papers_keyw
      
      # Process authors
      tibble::tibble(
        id = papers$id,
        AU = purrr::map_chr(papers$authorships, ~paste(purrr::pluck(.x, 'display_name'), collapse = ';') |> toupper())
      ) ->
        papers_auth
      
      # Combine all processed data
      papers |>
        dplyr::mutate(
          PY = .data$publication_year,
          TC = .data$cited_by_count,
          TI = .data$title,
          AB = .data$abstract,
          DI = .data$doi
        ) |>
        dplyr::select(.data$id, .data$id_short, .data$SR, .data$PY, .data$TC, .data$TI, .data$DI, .data$AB) |>
        dplyr::left_join(papers_ref |> dplyr::rename(SR = .data$id_short), by = 'SR') |>
        dplyr::left_join(papers_keyw, by = 'id') |>
        dplyr::left_join(papers_auth, by = 'id') |>
        dplyr::mutate(DB = paste('openalex', format, sep = '_')) |>
        dplyr::relocate(.data$DB, .after = .data$AU) |>
        dplyr::right_join(file, by = 'id') |>
        dplyr::distinct(.data$id, .keep_all = TRUE) ->
        papers_full
      
      return(papers_full)
    }, error = function(e) {
      stop("Error processing API data: ", e$message, call. = FALSE)
    })
  }
}

