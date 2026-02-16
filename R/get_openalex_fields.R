#' Get Fields from OpenAlex for Work IDs
#'
#' Retrieves specified fields for OpenAlex work IDs using the OpenAlex API.
#' Processes data in batches to avoid API rate limits.
#'
#' @param openalex_ids Character vector of OpenAlex work IDs (format: "W1234567890")
#'   or a data frame/tibble containing a column named "CR" with OpenAlex IDs.
#'   IDs can be semicolon-separated strings which will be split automatically.
#' @param variables Character vector of variable names to fetch from OpenAlex.
#'   Options include: "publication_year", "doi", "type", "source_display_name",
#'   or any valid OpenAlex work field. Default is "publication_year".
#' @param batch_size Number of IDs to process per API call (default: 50).
#'   Smaller batches help avoid API rate limits.
#' @param save_dir Optional path to directory where intermediate results should
#'   be saved as RDS files. If NULL (default), no saving occurs. Directory will
#'   be created if it doesn't exist.
#'
#' @return A tibble with the following columns:
#' \itemize{
#'   \item \code{id}: The OpenAlex work ID
#'   \item One column for each requested variable (e.g., "publication_year", "doi", "type")
#' }
#' Rows without valid OpenAlex IDs or where API calls fail will have NA values.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Accepts either a character vector of IDs or a data frame with a "CR" column
#'   \item Splits semicolon-separated ID strings into individual IDs
#'   \item Validates IDs against the pattern "^W\\d+$"
#'   \item Fetches specified variables from OpenAlex API in batches
#'   \item Optionally saves each batch to disk as it's processed
#'   \item Handles API errors gracefully with informative messages
#'   \item Includes delays between batches to respect API rate limits
#' }
#'
#' @note
#' The OpenAlex API has rate limits. This function implements:
#' \itemize{
#'   \item Batch processing to reduce number of API calls
#'   \item 0.5 second delays between batches
#'   \item Error handling for failed API requests
#'   \item Progress messages to track execution
#'   \item Optional disk saving for data persistence
#' }
#' If you encounter rate limiting errors, consider reducing batch_size or
#' implementing longer delays.
#'
#' @examples
#' \donttest{
#' # From a character vector
#' ids <- c("W2261389918", "W1548650423", "W1504492735")
#' result <- get_openalex_fields(ids)
#'
#' # Fetch multiple variables
#' result <- get_openalex_fields(
#'   ids,
#'   variables = c("publication_year", "doi", "type")
#' )
#'
#' # From a data frame with CR column
#' oa_data <- data.frame(CR = c("W123;W456", "W789"))
#' result <- get_openalex_fields(oa_data)
#'
#' # Save intermediate results while downloading
#' result <- get_openalex_fields(
#'   ids,
#'   variables = c("publication_year", "source_display_name"),
#'   save_dir = tempdir()
#' )
#' }
#'
#' @importFrom openalexR oa_fetch
#' @importFrom dplyr mutate bind_rows rename_with
#' @importFrom tibble as_tibble tibble
#' @importFrom stringr str_trim str_split
#' @importFrom utils head
#' @export
get_openalex_fields <- function(
  openalex_ids,
  variables = "publication_year",
  batch_size = 50,
  save_dir = NULL) {
  # Input validation and processing
  if (is.data.frame(openalex_ids)) {
    if (!"CR" %in% names(openalex_ids)) {
      stop("Data frame must contain a column named 'CR'", call. = FALSE)
    }
    ids_vector <- openalex_ids$CR
  } else if (is.character(openalex_ids)) {
    ids_vector <- openalex_ids
  } else {
    stop("openalex_ids must be a character vector or data frame with 'CR' column", call. = FALSE)
  }

  if (!is.character(variables) || length(variables) == 0) {
    stop("variables must be a non-empty character vector", call. = FALSE)
  }

  # Create save directory if specified
  if (!is.null(save_dir)) {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
      message("Created directory: ", save_dir)
    }
  }

  # Split semicolon-separated IDs and clean
  ids_vector |>
    stringr::str_split(";") |>
    unlist() |>
    stringr::str_trim() ->
    all_ids

  # Remove empty strings and NAs
  all_ids <- all_ids[all_ids != "" & !is.na(all_ids)]

  message("Processing ", length(all_ids), " OpenAlex IDs")

  # Get unique OpenAlex IDs
  ids <- unique(all_ids)

  # Extract valid OpenAlex IDs (format: W1234567890)
  valid_ids <- grep("^W\\d+$", ids, value = TRUE)

  if (length(valid_ids) == 0) {
    message("No valid OpenAlex IDs found in format 'W1234567890'")
    message("Sample of ID values: ", paste(head(ids, 5), collapse = ", "))

    # Create empty result with requested variables
    result <- tibble::tibble(id = character())
    for (var in variables) {
      result[[var]] <- logical()
    }
    return(result)
  }

  message(
    "Fetching ", paste(variables, collapse = ", "), " for ",
    length(valid_ids), " unique work IDs using openalexR package"
  )

  # Process in batches
  all_data <- list()

  for (i in seq(1, length(valid_ids), by = batch_size)) {
    batch_ids <- valid_ids[i:min(i + batch_size - 1, length(valid_ids))]
    batch_num <- ceiling(i / batch_size)
    total_batches <- ceiling(length(valid_ids) / batch_size)

    message("Batch ", batch_num, "/", total_batches, " (", length(batch_ids), " IDs)")

    tryCatch(
      {
        batch_data <- openalexR::oa_fetch(
          entity = "works",
          identifier = batch_ids,
          options = list(select = c("id", variables)),
          verbose = FALSE
        )

        if (!is.null(batch_data) && nrow(batch_data) > 0) {
          # Create tibble with requested variables
          batch_df <- tibble::tibble(id = batch_data$id)

          for (var in variables) {
            if (var %in% names(batch_data)) {
              batch_df[[var]] <- batch_data[[var]]
            } else {
              batch_df[[var]] <- NA
              message("  Warning: Variable '", var, "' not found in API response")
            }
          }

          all_data[[length(all_data) + 1]] <- batch_df
          message("  Retrieved ", nrow(batch_data), " records")

          # Save batch if directory specified
          if (!is.null(save_dir)) {
            batch_file <- file.path(save_dir, paste0("batch_", batch_num, ".rds"))
            saveRDS(batch_df, batch_file)
            message("  Saved to: ", batch_file)
          }
        } else {
          message("  No data returned for this batch")
        }
      },
      error = function(e) {
        message("  Error in batch: ", e$message)
        # Create empty result for failed batch
        batch_df <- tibble::tibble(id = batch_ids)
        for (var in variables) {
          batch_df[[var]] <- NA
        }
        all_data[[length(all_data) + 1]] <- batch_df
      }
    )

    # Delay to be API-friendly
    Sys.sleep(0.5)
  }

  if (length(all_data) == 0) {
    message("No data retrieved from OpenAlex")
    result <- tibble::tibble(id = valid_ids)
    for (var in variables) {
      result[[var]] <- NA
    }
    return(result)
  }

  # Combine results
  result <- dplyr::bind_rows(all_data) |>
    dplyr::mutate(id = gsub("https://openalex.org/", "", .data$id)) |>
    tibble::as_tibble()

  message("Successfully retrieved data for ", nrow(result), " references")

  # Report statistics for each variable
  for (var in variables) {
    n_valid <- sum(!is.na(result[[var]]))
    message("  ", var, ": ", n_valid, " valid values out of ", nrow(result))
  }

  return(result)
}
