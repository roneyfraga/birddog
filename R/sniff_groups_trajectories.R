#' Detect Technological Trajectories from Grouped Documents
#'
#' This function analyzes the evolution of document groups over time to detect technological trajectories
#' and scientific emergence patterns. It computes similarity measures between groups across time periods
#' and tracks their attributes.
#'
#' @param groups_cumulative A list of cumulative group data over time, typically produced by
#'        other functions in the birddog package. Each element should contain network, documents,
#'        and groups data.
#' @param min_group_size Minimum number of documents required for a group to be considered
#'        (default: 10). Smaller groups will be filtered out.
#' @param top_n_keywords Number of top keywords to consider when analyzing group characteristics
#'        (default: 3).
#'
#' @return A list with two components:
#' \itemize{
#'   \item groups_attributes: A list of data frames containing attributes for each tracked group
#'   \item groups_similarity: A list of data frames containing Jaccard similarity measures between groups across time periods
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming you have cumulative group data:
#' trajectories <- sniff_groups_trajectories(groups_cumulative, min_group_size = 15)
#' }
#'
#' @export
sniff_groups_trajectories <- function(groups_cumulative, min_group_size = 10, top_n_keywords = 3) {
  # Input validation
  if (!is.list(groups_cumulative) || length(groups_cumulative) == 0) {
    stop("groups_cumulative must be a non-empty list")
  }
  if (!all(c("network", "documents", "groups") %in% names(groups_cumulative[[1]]))) {
    stop("Each element of groups_cumulative must contain 'network', 'documents', and 'groups' components")
  }
  if (!is.numeric(min_group_size) || min_group_size <= 0) {
    stop("min_group_size must be a positive number")
  }
  if (!is.numeric(top_n_keywords) || top_n_keywords <= 0) {
    stop("top_n_keywords must be a positive integer")
  }

  tryCatch({
    # Extract networks from each time period
    net4 <- purrr::map(groups_cumulative, \(x) x$network)

    # Get the last year from the data
    last_year_position <- length(groups_cumulative)
    last_year <- as.numeric(gsub("^.*_", "", (names(groups_cumulative)[last_year_position])))

    # Get all groups from the last year
    grupos <- groups_cumulative[[last_year_position]][["groups"]][["group"]]

    # Initialize output lists
    groups_similarity <- stats::setNames(vector(mode = "list", length = length(grupos)), grupos)
    groups_attributes <- stats::setNames(vector(mode = "list", length = length(grupos)), grupos)

    # Process each group
    for (i in seq_along(grupos)) {
      message("Processing group: ", grupos[[i]])

      # Get documents in the current group
      tracked_documents <- groups_cumulative[[last_year_position]]$documents |>
        dplyr::filter(group == grupos[i]) |>
        dplyr::pull(name)

      # Track documents over time
      tr_docs <- sniff_groups_cumulative_attributes(
        groups_cumulative,
        min_group_size = min_group_size,
        top_n_keywords = top_n_keywords,
        group_to_track = grupos[[i]],
        attributes = "documents"
      ) |> purrr::keep(\(x) nrow(x) > 1)

      # Track groups over time
      tr_groups <- sniff_groups_cumulative_attributes(
        groups_cumulative,
        min_group_size = min_group_size,
        top_n_keywords = top_n_keywords,
        group_to_track = grupos[[i]],
        attributes = "groups"
      ) |> purrr::keep(\(x) nrow(x) > 1)

      # Prepare tracked group data
      tracked_group <- groups_cumulative[[last_year_position]]$documents |>
        dplyr::filter(group == grupos[i]) |>
        dplyr::mutate(
          network_until = last_year,
          tracked_document = 1
        )

      # Calculate Jaccard similarities between time periods
      jaccard_weight <- vector(mode = "list", length = length(tr_docs))

      for (k in seq_along(utils::head(names(tr_docs), -1))) {
        message("Calculating similarities between ", names(tr_docs)[k], " and ", names(tr_docs)[k+1])

        # Split documents by group for time periods k and k+1
        groups_t1 <- split(tr_docs[[k]], f = tr_docs[[k]]$group)
        groups_t2 <- split(tr_docs[[k + 1]], f = tr_docs[[k + 1]]$group)

        t1 <- tr_docs[[k]]$network_until[1]
        t2 <- tr_docs[[k + 1]]$network_until[1]

        # Calculate Jaccard similarity and document overlap
        similarity_weight <- purrr::map(
          groups_t1,
          \(x) purrr::map(groups_t2, \(y) jaccard(x$name, y$name))
        ) |> purrr::list_flatten() |> purrr::keep(\(x) x > 0)

        documents_weigth <- purrr::map(
          groups_t1,
          \(x) purrr::map(groups_t2, \(y) sum(x$name %in% y$name))
        ) |> purrr::list_flatten() |> purrr::keep(\(x) x > 0)

        # Prepare edge data for network
        a <- strsplit(names(similarity_weight), "_") |> purrr::list_flatten()
        arestas <- purrr::map(seq_along(a), function(l) {
          tibble::tibble(
            from = paste0("y", t1, a[[l]][2]),
            to = paste0("y", t2, a[[l]][4]),
            weight = similarity_weight[[l]],
            documents = documents_weigth[[l]]
          )
        })

        jaccard_weight[[k]] <- dplyr::bind_rows(arestas)
      }

      groups_similarity[[i]] <- dplyr::bind_rows(jaccard_weight)

      # Prepare network attributes
      dados <- tr_groups |>
        purrr::map(\(x) x |> dplyr::mutate(id = paste0("y", network_until, gsub("^.*_", "", group)))) |>
        dplyr::bind_rows() |>
        dplyr::mutate(PY.sd = network_until - average_age_group) |>
        dplyr::filter(id %in% unique(c(groups_similarity[[i]]$from, groups_similarity[[i]]$to)))

      group_tracked_complete <- groups_cumulative[[last_year_position]]$groups |>
        dplyr::rename(average_age_group = average_age) |>
        dplyr::filter(group == grupos[i]) |>
        dplyr::mutate(
          network_until = max(dados$network_until, na.rm = TRUE) + 1,
          tracked_documents = quantity_papers,
          prop_tracked_intra_group = 1,
          prop_tracked_documents = 1,
          PY.sd = network_until - average_age_group,
          id = grupos[i]
        )

      groups_attributes[[i]] <- dplyr::bind_rows(group_tracked_complete, dados)
    }

    return(list(groups_attributes = groups_attributes, groups_similarity = groups_similarity))

  }, error = function(e) {
    stop("Error in sniff_groups_trajectories: ", e$message)
  })
}

#' Calculate Jaccard Similarity Between Two Vectors
#'
#' @param a First vector
#' @param b Second vector
#' @return Jaccard similarity coefficient (between 0 and 1)
#' @keywords internal
jaccard <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  intersection <- length(intersect(a, b))
  union <- length(a) + length(b) - intersection
  if (union == 0) return(0)
  intersection / union
}

#' Extract attributes from cumulative groups
#'
#' @keywords internal
sniff_groups_cumulative_attributes <- function(cummulative_network,
                                       min_group_size = 10,
                                       top_n_keywords = 3,
                                       group_to_track = "component1_g01",
                                       attributes = "groups") {

  # Validate inputs
  if (!is.list(cummulative_network) || !all(purrr::map_lgl(cummulative_network, ~ all(c("groups", "documents", "network") %in% names(.x))))) {
    stop("Input must be output from sniff_cumulative_groups()", call. = FALSE)
  }

  if (!is.numeric(min_group_size) || min_group_size < 1) {
    stop("min_group_size must be a positive integer", call. = FALSE)
  }

  if (!is.numeric(top_n_keywords) || top_n_keywords < 1) {
    stop("top_n_keywords must be a positive integer", call. = FALSE)
  }

  if (!is.character(group_to_track) || length(group_to_track) != 1) {
    stop("group_to_track must be a single character value", call. = FALSE)
  }

  valid_attributes <- c("groups", "documents")
  if (!attributes %in% valid_attributes) {
    stop("attributes must be one of: ", paste(valid_attributes, collapse = ", "), call. = FALSE)
  }

  tryCatch({
    # Get documents from the specified group in the final time period
    cummulative_network[[length(cummulative_network)]]$documents |>
      dplyr::filter(.data$group == group_to_track) |>
      dplyr::pull(.data$name) ->
      documents_to_track

    if (length(documents_to_track) == 0) {
      warning("No documents found in specified group: ", group_to_track, call. = FALSE)
    }

    # Process based on requested attributes
    if (attributes == "groups") {
      res <- purrr::map(cummulative_network, function(time_period) {
        # Filter groups by size
        groups_1 <- time_period$groups |>
          dplyr::filter(.data$quantity_papers >= min_group_size)

        # Process documents with tracking flag
        documents_1 <- time_period$documents |>
          dplyr::filter(.data$group %in% groups_1$group) |>
          dplyr::mutate(
            tracked_document = as.integer(.data$name %in% documents_to_track)
          )

        # Extract keywords from tracked documents
        groups_2 <- documents_1 |>
          dplyr::filter(.data$tracked_document == 1, !is.na(.data$DE)) |>
          tidyr::separate_rows(.data$DE, sep = ";") |>
          dplyr::mutate(DE = stringr::str_trim(.data$DE)) |>
          dplyr::group_by(.data$group, .data$DE) |>
          dplyr::summarise(qtde = dplyr::n(), .groups = "drop") |>
          dplyr::arrange(.data$group, dplyr::desc(.data$qtde)) |>
          dplyr::filter(.data$qtde > 1, !is.na(.data$DE)) |>
          dplyr::group_by(.data$group) |>
          dplyr::slice_head(n = top_n_keywords) |>
          dplyr::mutate(keywords_freq = paste0(.data$DE, " (", .data$qtde, ")")) |>
          dplyr::summarise(
            keywords_tracked_documents = paste(.data$keywords_freq, collapse = ";"),
            .groups = "drop"
          )

        # Calculate average age of tracked documents
        avg_age_tracked <- documents_1 |>
          dplyr::filter(.data$tracked_document == 1) |>
          dplyr::group_by(.data$group) |>
          dplyr::summarise(
            average_age_tracked_documents = mean(.data$network_until, na.rm = TRUE),
            .groups = "drop"
          )

        # Combine all metrics
        documents_1 |>
          dplyr::group_by(.data$group) |>
          dplyr::summarise(
            tracked_documents = sum(.data$tracked_document),
            prop_tracked_documents = sum(.data$tracked_document) / max(1, length(documents_to_track)),
            prop_tracked_intra_group = sum(.data$tracked_document) / dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::right_join(groups_1, by = "group") |>
          dplyr::left_join(groups_2, by = "group") |>
          dplyr::left_join(avg_age_tracked, by = "group") |>
          dplyr::rename(average_age_group = .data$average_age) |>
          dplyr::relocate(
            .data$network_until,
            .data$prop_tracked_documents,
            .data$prop_tracked_intra_group,
            .data$quantity_papers,
            .data$average_age_group,
            .data$average_age_tracked_documents
          )
      })
    } else { # attributes = "documents"
      res <- purrr::map(cummulative_network, function(time_period) {
        # Filter groups by size
        groups_1 <- time_period$groups |>
          dplyr::filter(.data$quantity_papers >= min_group_size)

        # Flag tracked documents
        time_period$documents |>
          dplyr::filter(.data$group %in% groups_1$group) |>
          dplyr::mutate(
            tracked_document = as.integer(.data$name %in% documents_to_track)
          )
      })
    }

    names(res) <- names(cummulative_network)
    return(res)
  }, error = function(e) {
    stop("Error analyzing cumulative attributes: ", e$message, call. = FALSE)
  })
}
