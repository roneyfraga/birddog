#' Read Web of Science exported files
#'
#' Parse Web of Science (WoS) export files in multiple formats and return a
#' tidy table. The function automatically dispatches to a specialized parser
#' based on the `format` argument and can also **download from a URL** if
#' `file` points to an `http://` or `https://` resource.
#'
#' @section Supported formats:
#' * `"bib"` — BibTeX export
#' * `"ris"` — RIS export
#' * `"txt-plain-text"` — Plain-text export
#' * `"txt-tab-delimited"` — Tab-delimited export
#'
#' @details
#' - `file` may be a single path/URL or a **vector of paths**; multiple files
#'   will be combined row-wise when applicable.
#' - When `file` is a URL, the file is downloaded to a temporary path before
#'   parsing (a progress message is printed).
#' - If `normalized_names = TRUE`, selected WoS tags are mapped to standardized
#'   names (e.g., `AU` → `author`, `TI` → `title`, `PY` → `year`, `DI` → `doi`,
#'   `DE` → `keywords`, `SR` → `unique_id`, etc.; the exact mapping depends on
#'   the format). Otherwise, original field tags are preserved.
#' - The output includes:
#'   - `DI2`: an uppercase, punctuation-stripped variant of `DI` (if present),
#'   - `PY`: coerced to numeric (when present),
#'   - `DB`: a provenance flag indicating the source/format and whether names
#'     were normalized.
#' - Columns with ALL-CAPS tags (e.g., `AU`, `TI`, `PY`) are placed first,
#'   followed by other columns, and `DI2` is relocated just after `DI`.
#'
#' @param file Character scalar or vector. Path(s) to a WoS export file, or a
#'   single URL (`http://` or `https://`) pointing to a WoS export.
#' @param format Character scalar. Export format; one of
#'   `"bib"`, `"ris"`, `"txt-plain-text"`, or `"txt-tab-delimited"`.
#' @param normalized_names Logical. If `TRUE` (default), use standardized column
#'   names when possible; if `FALSE`, keep original WoS field tags.
#'
#' @return A tibble with the parsed WoS records. See **Details** for notes on
#'   added/coerced columns (`DI2`, `PY`, `DB`) and column ordering.
#'
#' @seealso
#' Internal parsers used by this function:
#' \code{\link{read_wos_bib}}, \code{\link{read_wos_ris}},
#' \code{\link{read_wos_plain}}, \code{\link{read_wos_tab}}.
#'
#' @examples
#' \dontrun{
#'
#' # load data from websites
#' # M <- birddog::read_wos('http://yoursite/wos-savedrecs-plain-text.txt', format = "txt-plain-text")
#'
#'  # load from local files
#'   M <- read_wos('~/Downloads/savedrecs.bib', format = "bib", normalized_names = TRUE)
#'
#' }
#'
#' @export
#' @importFrom rlang .data
read_wos <- function(file, format = "bib", normalized_names = TRUE) {
  # Validate inputs
  allowed_formats <- c("bib", "ris", "txt-plain-text", "txt-tab-delimited")
  format <- rlang::arg_match(format, allowed_formats)

  # file <- "inst/extdata/skg-example-data/wos-technological-trajectories/savedrecs-tab-delimited.txt"
  # file <- "inst/extdata/skg-example-data/wos-technological-trajectories/savedrecs.ris"
  # file <- "inst/extdata/skg-example-data/wos-technological-trajectories/savedrecs.bib"

  is_url <- grepl("^http?://", file)

  if (is_url) {
    temp_file <- tempfile(fileext = paste0(".", stringr::str_sub(format, 1, 3)))

    tryCatch(
      {
        utils::download.file(file, temp_file, quiet = FALSE)
        message("Downloading data from URL: ", file)
        file <- temp_file
      },
      error = function(e) {
        stop("Error downloading file from URL: ", e$message, call. = FALSE)
      }
    )
  }

  if (!file.exists(file)) {
    missing_files <- file[!file.exists(file)]
    cli::cli_abort("File(s) not found: {missing_files}")
  }

  # Dispatch to appropriate parser
  switch(format,
    "bib" = read_wos_bib(file, normalized_names),
    "ris" = read_wos_ris(file, normalized_names),
    "txt-plain-text" = read_wos_plain(file, normalized_names),
    "txt-tab-delimited" = read_wos_tab(file, normalized_names)
  ) |>
    dplyr::mutate(DI2 = toupper(gsub("[[:punct:]]", "", .data$DI))) |>
    dplyr::mutate(PY = as.numeric(.data$PY)) ->
    res

  uppercase_cols <- names(res)[grepl("^[A-Z]{2,3}$", names(res))]
  other_cols <- setdiff(names(res), uppercase_cols)

  res |>
    dplyr::select(dplyr::all_of(uppercase_cols), dplyr::all_of(other_cols)) |>
    dplyr::relocate(.data$DI2, .after = .data$DI) ->
    res

  return(res)
}

#' Read Web of Science BibTeX files
#' @inheritParams read_wos
#' @keywords internal
read_wos_bib <- function(file, normalized_names = TRUE) {
  if (length(file) > 1) {
    file |>
      purrr::map(\(x) readr::read_file(x)) |>
      purrr::flatten() ->
      lines
  } else {
    lines <- readr::read_file(file)
  }

  lines |>
    {
      \(x) strsplit(x, "\n\n")[[1]]
    }() |>
    {
      \(x) strsplit(x, "\n")
    }() |>
    purrr::map(\(x) gsub("\\s+", " ", x)) ->
    mylist

  mylist |>
    purrr::map(\(x) stringr::str_extract(x, "^[\\w\\-]+(?=\\s*=\\s*\\{)")) |>
    purrr::map(\(x) x[!is.na(x)]) |>
    purrr::map(\(x) stringr::str_c("^(", stringr::str_c(x, collapse = "|"), ")\\s?=")) ->
    tags_query

  purrr::map2(mylist, tags_query, \(x, y) split(x, cumsum(stringr::str_detect(x, y)))) |>
    purrr::map(\(x) bib_splited_to_tibble(x)) |>
    dplyr::bind_rows() |>
    janitor::clean_names() |>
    dplyr::mutate(DB = paste("wos", "bib", sep = "_")) ->
    result

  if (normalized_names == FALSE) {
    return(result)
  } else {
    normalize_column_names(result, "bib") |>
      dplyr::mutate(DB = paste(.data$DB, "normalized_names", sep = "_")) ->
      result

    return(result)
  }
}

#' Read Web of Science RIS files
#' @inheritParams read_wos
#' @keywords internal
read_wos_ris <- function(file, normalized_names = TRUE) {
  # Read file contents
  if (length(file) > 1) {
    purrr::map_chr(file, ~ readr::read_file(.x))
  } else {
    {
      readr::read_file(file)
    } ->
      content
  }

  # Split into individual records
  stringr::str_split(content, "\nER\\s*-\\s*\n")[[1]] |>
    purrr::discard(~ .x == "") ->
    records

  # Parse each record
  purrr::map_dfr(records, function(record) {
    # Split into fields
    fields <- stringr::str_split(record, "\n(?=[A-Z0-9]{2}\\s+-)", simplify = FALSE)[[1]]

    # Extract field tags and values
    purrr::map_dfr(fields, function(field) {
      if (nchar(field) < 2) {
        return(tibble::tibble())
      }

      tag <- stringr::str_sub(field, 1, 2)
      value <- stringr::str_sub(field, 6) |> # Skip tag and " - " separator
        stringr::str_replace_all("\n", " ") |>
        stringr::str_squish()

      tibble::tibble(tag = tag, value = value)
    }) |>
      dplyr::filter(!is.na(.data$tag), .data$tag != "") |>
      dplyr::group_by(.data$tag) |>
      dplyr::summarize(value = paste(.data$value, collapse = "; "), .groups = "drop") |>
      tidyr::pivot_wider(names_from = "tag", values_from = "value")
  }) |>
    dplyr::mutate(DB = "wos_ris") ->
    result

  # Handle special fields
  if ("TC" %in% names(result)) {
    result <- dplyr::mutate(result, TC = as.numeric(.data$TC))
  }
  if ("PY" %in% names(result)) {
    result <- dplyr::mutate(result, PY = as.numeric(.data$PY))
  }

  if (normalized_names) {
    result <- normalize_column_names(result, "ris")
  }

  if ("DO" %in% names(result)) {
    result <- dplyr::rename(result, DI = .data$DO)
  }

  if ("WE" %in% names(result)) {
    result <- dplyr::mutate(result, SR = stringr::str_extract(.data$WE, "WOS:\\d+"))
  }

  return(result)
}

#' Read Web of Science tab-delimited files
#' @inheritParams read_wos
#' @keywords internal
read_wos_tab <- function(file, normalized_names = TRUE) {
  purrr::map_dfr(file, ~ readr::read_tsv(.x, show_col_types = FALSE)) |>
    dplyr::mutate(SR = .data$UT, DB = "wos_tab") ->
    result

  if (normalized_names) {
    result <- normalize_column_names(result, "tab")
  }

  return(result)
}

#' Read Web of Science plain text files
#' @inheritParams read_wos
#' @keywords internal
read_wos_plain <- function(file, normalized_names = TRUE) {
  lines <- read_lines_multiple(file)

  records <- split_wos_records(lines)
  result <- purrr::map_dfr(records, parse_plain_record) |>
    dplyr::mutate(SR = .data$UT, DB = "wos_plain")

  if (normalized_names) {
    result <- normalize_column_names(result, "plain")
  }

  return(result)
}

# ----------------------
# HELPER FUNCTIONS
# ----------------------

#' Normalize column names across formats
#' @keywords internal
bib_splited_to_tibble <- function(bib_splited_by_field) {
  ff <- campo <- NULL

  for (i in seq_along(bib_splited_by_field)) {
    cr_or_aff <- any(stringr::str_detect(bib_splited_by_field[[i]], "^(Affiliation|Cited-References)\\s?="))

    if (cr_or_aff == FALSE) {
      ff[[i]] <- paste(bib_splited_by_field[[i]], collapse = "")
    } else {
      ff[[i]] <- paste(bib_splited_by_field[[i]], collapse = ";")
      ff[[i]] <- gsub(".;", ";", ff[[i]])
      ff[[i]] <- gsub("(DOI \\{\\[\\}[^,]+), DOI [^]]+", "\\1", ff[[i]]) # doi remove duplicates
      ff[[i]] <- gsub("DOI \\{\\[\\}([^]]+)\\]", "DOI \\1", ff[[i]]) # doi remove duplicates
      ff[[i]] <- gsub("DOI DOI", "DOI", ff[[i]]) # doi remove duplicates
      ff[[i]] <- gsub("\\{\\[\\}", "", ff[[i]])
      ff[[i]] <- gsub("\\]\\.", "\\.", ff[[i]])
      ff[[i]] <- gsub(".\\},$", "\\}", ff[[i]])
    }
    campo[[i]] <- stringr::str_extract(ff[[i]], "^[\\w\\-]+(?=\\s*=\\s*\\{)")
    ff[[i]] <- stringr::str_extract(ff[[i]], "(?<=\\{).*?(?=\\})")
    ff[[i]] <- stringr::str_squish(ff[[i]])
  }

  names(ff) <- campo
  ff[[1]] <- NULL
  tibble::as_tibble(ff)
}

#' Normalize column names across formats
#' @keywords internal
normalize_column_names <- function(data, format) {
  name_mapping <- switch(format,
    "bib" = c(
      AU = "author", TI = "title", PY = "year",
      DI = "doi", DE = "keywords", ID = "keywords_plus",
      SC = "research_areas", AB = "abstract", C1 = "affiliation",
      DT = "type", CR = "cited_references", SO = "journal",
      TC = "times_cited", JI = "journal_iso", SR = "unique_id"
    ),
    "plain" = ,
    "tab" = c(
      AU = "author", TI = "title", PY = "year", DI = "doi",
      DE = "keywords", ID = "keywords_plus", SC = "research_areas",
      AB = "abstract", C1 = "affiliation", DT = "type",
      CR = "cited_references", SO = "journal", TC = "times_cited",
      JI = "journal_iso", SR = "unique_id"
    ),
    "ris" = c(
      DO = "doi", KW = "keywords", AD = "affiliation",
      T2 = "journal", AN = "unique_id"
    )
  )

  data |>
    dplyr::rename(dplyr::any_of(name_mapping)) |>
    dplyr::mutate(DB = paste(.data$DB, "normalized", sep = "_"))
}

#' Read lines from single or multiple files
#' @keywords internal
read_lines_multiple <- function(file) {
  if (length(file) > 1) {
    purrr::map(file, ~ readLines(.x, encoding = "UTF-8")) |>
      purrr::flatten_chr()
  } else {
    readLines(file, encoding = "UTF-8")
  }
}

#' Split WOS plain text into individual records
#' @keywords internal
split_wos_records <- function(lines) {
  record_breaks <- which(lines == "ER")
  record_starts <- c(1, record_breaks + 1)[-length(record_breaks) - 1]
  purrr::map2(record_starts, record_breaks, ~ lines[.x:.y])
}

#' Parse individual plain text record
#' @keywords internal
parse_plain_record <- function(lines) {
  is_tag <- stringr::str_detect(lines, "^[A-Z0-9]{2} ")
  idx <- which(is_tag)
  end_idx <- c(utils::tail(idx, -1) - 1, length(lines))

  purrr::map2(idx, end_idx, ~ {
    tag <- stringr::str_sub(lines[.x], 1, 2)
    values <- stringr::str_sub(lines[.x:.y], 4) |> stringr::str_squish()
    sep <- if (tag == "CR") "; " else " "
    tibble::tibble(tag = tag, value = paste(values, collapse = sep))
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!.data$tag %in% c("FN", "VR")) |>
    dplyr::group_by(.data$tag) |>
    dplyr::summarise(value = paste(.data$value, collapse = "; "), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "tag", values_from = "value")
}
