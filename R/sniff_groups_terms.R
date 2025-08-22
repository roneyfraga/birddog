#' Extract and Analyze Key Terms from Research Groups
#'
#' Identifies and extracts key terms from titles and abstracts of publications within different
#' research groups using natural language processing techniques, and computes term statistics
#' including TF-IDF scores.
#'
#' @param net_groups A list containing network data with publication information.
#'        Must include elements: `network` (with vertex attributes 'group', 'TI', 'AB'),
#'        `pubs_by_year`, and `aggregate`.
#' @param algorithm Term extraction algorithm to use. Options are:
#'        \itemize{
#'          \item "rake" - Rapid Automatic Keyword Extraction (default)
#'          \item "pointwise" - Pointwise Mutual Information
#'          \item "phrase" - Phrase pattern matching
#'        }
#' @param phrase_pattern Regular expression pattern for phrase extraction when
#'        algorithm = "phrase" (default: "(A|N)*N(P+D*(A|N)*N)*")
#' @param n_cores Number of CPU cores to use for parallel processing (default: 1)
#' @param show_progress Logical indicating whether to show progress bar (default: TRUE)
#' @param n_terms Number of top terms to return in summary table (default: 15)
#' @param min_freq Minimum frequency threshold for terms (default: 2)
#' @param digits Number of decimal places to round numerical values (default: 4)
#'
#' @return A list with two components:
#' \itemize{
#'   \item `terms_by_group`: A named list (by group) of data frames containing extracted terms with statistics
#'   \item `terms_table`: A summary tibble with top terms by frequency and TF-IDF for each group
#' }
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Validates input structure and parameters
#'   \item Loads the UDPipe language model from package extdata
#'   \item Processes text data (titles and abstracts) for each group
#'   \item Applies the selected term extraction algorithm (RAKE, PMI, or phrase patterns)
#'   \item Computes term frequencies and TF-IDF scores
#'   \item Returns ranked terms for each research group with comprehensive statistics
#' }
#'
#' The function uses UDPipe for tokenization, lemmatization and POS tagging before term extraction.
#' For phrase extraction, the default pattern finds noun phrases.
#'
#' @examples
#' \dontrun{
#' # Assuming groups is output from sniff_groups()
#' terms <- sniff_groups_terms(groups, algorithm = "rake")
#'
#' # View terms for first group
#' head(terms$terms_by_group[[1]])
#'
#' # View summary table
#' print(terms$terms_table)
#'
#' # Customized extraction
#' net_groups_terms <- sniff_groups_terms(net_groups,
#'   algorithm = "phrase",
#'   n_terms = 10,
#'   min_freq = 3,
#'   n_cores = 4
#' )
#' }
#'
#' @importFrom tidygraph activate
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr select filter group_by summarise mutate arrange desc rename top_n ungroup
#' @importFrom purrr map map_dfr
#' @importFrom stats setNames
#' @importFrom igraph vertex_attr
#' @export
sniff_groups_terms <- function(net_groups,
                               algorithm = "rake",
                               phrase_pattern = "(A|N)*N(P+D*(A|N)*N)*",
                               n_cores = 1,
                               show_progress = TRUE,
                               n_terms = 15,
                               min_freq = 2,
                               digits = 4) {
  # Validate inputs
  if (!is.list(net_groups)) {
    stop("net_groups must be a list containing network data", call. = FALSE)
  }

  required_components <- c("network", "pubs_by_year", "aggregate")
  missing_components <- setdiff(required_components, names(net_groups))
  if (length(missing_components) > 0) {
    stop("net_groups must contain: ", paste(required_components, collapse = ", "), call. = FALSE)
  }

  required_vertex_attrs <- c("group", "TI", "AB")
  missing_attrs <- setdiff(required_vertex_attrs, igraph::vertex_attr_names(net_groups$network))
  if (length(missing_attrs) > 0) {
    stop("network must contain vertex attributes: ", paste(required_vertex_attrs, collapse = ", "), call. = FALSE)
  }

  valid_algorithms <- c("rake", "pointwise", "phrase")
  if (!algorithm %in% valid_algorithms) {
    stop("algorithm must be one of: ", paste(valid_algorithms, collapse = ", "), call. = FALSE)
  }

  if (!is.numeric(n_cores) || n_cores < 1) {
    stop("n_cores must be a number greater than 0", call. = FALSE)
  }

  ud_model <- tryCatch(
    load_udpipe_model(model_name = "english", model_dir = "."),
    error = function(e) {
      stop("Failed to load language model: ", e$message, call. = FALSE)
    }
  )

  # Get unique groups
  grupos <- sort(unique(igraph::vertex_attr(net_groups$network, "group")))
  if (length(grupos) == 0) {
    stop("No groups found in network", call. = FALSE)
  }

  # Initialize results
  stats <- setNames(vector("list", length(grupos)), grupos)

  # Process each group
  for (i in seq_along(grupos)) {
    tryCatch(
      {
        # Prepare text data
        text_data <- net_groups$network |>
          tidygraph::activate(nodes) |>
          tibble::as_tibble() |>
          dplyr::select(name, group, TI, AB) |>
          dplyr::filter(group == grupos[i]) |>
          dplyr::group_by(group) |>
          dplyr::summarise(
            text = paste(stats::na.omit(c(TI, AB)), collapse = ". "),
            .groups = "drop"
          ) |>
          dplyr::mutate(text = tolower(text))

        if (nchar(text_data$text) == 0) {
          warning("No text data available for group: ", grupos[i])
          next
        }

        # Annotate text
        if (n_cores == 1) {
          x <- udpipe::udpipe_annotate(
            ud_model,
            x = text_data$text,
            doc_id = text_data$group,
            trace = show_progress
          )
        } else {
          x <- udpipe::udpipe(
            text_data,
            ud_model,
            parallel.cores = n_cores
          )
        }

        x <- as.data.frame(x)

        # Extract terms based on algorithm
        stats[[i]] <- switch(algorithm,
          "rake" = {
            udpipe::keywords_rake(
              x = x,
              term = "lemma",
              group = "doc_id",
              relevant = x$upos %in% c("NOUN", "ADJ")
            ) |>
              dplyr::mutate(arranged_by = .data$rake)
          },
          "pointwise" = {
            x$word <- tolower(x$token)
            udpipe::keywords_collocation(
              x = x,
              term = "word",
              group = "doc_id"
            ) |>
              dplyr::mutate(arranged_by = .data$freq)
          },
          "phrase" = {
            x$phrase_tag <- udpipe::as_phrasemachine(x$upos, type = "upos")
            udpipe::keywords_phrases(
              x = x$phrase_tag,
              term = tolower(x$token),
              pattern = phrase_pattern,
              is_regex = TRUE,
              detailed = FALSE
            ) |>
              dplyr::filter(.data$ngram > 1 & .data$freq > min_freq) |>
              dplyr::mutate(arranged_by = .data$freq)
          }
        )

        # Convert to tibble and arrange
        if (!is.null(stats[[i]])) {
          stats[[i]] <- stats[[i]] |>
            tibble::as_tibble() |>
            dplyr::arrange(dplyr::desc(.data$arranged_by))
        }
      },
      error = function(e) {
        warning("Failed to process group ", grupos[i], ": ", e$message)
        stats[[i]] <- tibble::tibble(
          keyword = character(),
          ngram = integer(),
          freq = numeric(),
          arranged_by = numeric()
        )
      }
    )
  }

  # Remove empty groups
  stats <- stats[purrr::map_int(stats, nrow) > 0]

  if (length(stats) == 0) {
    warning("No terms could be extracted from any group")
    return(list(
      terms_by_group = stats,
      terms_table = tibble::tibble(
        group = character(),
        term_freq = character(),
        term_tfidf = character()
      )
    ))
  }

  # Compute TF-IDF and create summary table
  terms_table <- tryCatch(
    {
      # Prepare term frequency data
      term_freq <- purrr::map_dfr(stats, ~.x, .id = "group") |>
        dplyr::select(.data$group, .data$keyword, freq = .data$arranged_by) |>
        dplyr::filter(!is.na(.data$group), !is.na(.data$keyword)) |>
        dplyr::mutate(keyword = tolower(.data$keyword))

      # Calculate TF-IDF
      tfidf_data <- term_freq |>
        dplyr::group_by(.data$group) |>
        dplyr::mutate(total = sum(.data$freq)) |>
        dplyr::ungroup() |>
        tidytext::bind_tf_idf(.data$keyword, .data$group, .data$freq)

      # Create formatted output
      freq_terms <- tfidf_data |>
        dplyr::group_by(.data$group) |>
        dplyr::arrange(dplyr::desc(.data$freq)) |>
        dplyr::slice_head(n = n_terms) |>
        dplyr::filter(.data$freq >= min_freq) |>
        dplyr::mutate(
          term_freq = sprintf("%s (%d)", .data$keyword, round(.data$freq))
        ) |>
        dplyr::summarise(
          term_freq = paste(.data$term_freq, collapse = "; "),
          .groups = "drop"
        )

      tfidf_terms <- tfidf_data |>
        dplyr::group_by(.data$group) |>
        dplyr::arrange(dplyr::desc(.data$tf_idf)) |>
        dplyr::slice_head(n = n_terms) |>
        dplyr::filter(.data$freq >= min_freq) |>
        dplyr::mutate(
          term_tfidf = sprintf(
            "%s [%.*f] (%d)",
            .data$keyword,
            digits,
            .data$tf_idf,
            round(.data$freq)
          )
        ) |>
        dplyr::summarise(
          term_tfidf = paste(.data$term_tfidf, collapse = "; "),
          .groups = "drop"
        )

      dplyr::full_join(freq_terms, tfidf_terms, by = "group")
    },
    error = function(e) {
      warning("Failed to compute TF-IDF: ", e$message)
      tibble::tibble(
        group = names(stats),
        term_freq = NA_character_,
        term_tfidf = NA_character_
      )
    }
  )

  # Return results
  list(
    terms_by_group = stats,
    terms_table = terms_table
  )
}

#' Load UDPipe model with on-demand downloading
#'
#' @param model_name Name of the model to load (default: "english")
#' @param model_dir Directory where models are stored (default: current directory)
#' @return A UDPipe model object
#' @keywords internal
load_udpipe_model <- function(model_name = "english", model_dir = ".") {
  
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
    message("Created model directory: ", model_dir)
  }
  
  pattern <- paste0("^", model_name, ".*\\.udpipe$")
  udpipe_files <- list.files(path = model_dir, pattern = pattern, full.names = TRUE)
  
  # Se encontrou arquivo(s), carregar o primeiro
  if (length(udpipe_files) > 0) {
    message("Loading existing model: ", basename(udpipe_files[1]))
    udpipe::udpipe_load_model(udpipe_files[1])
  }
  
  message("Model not found locally. Downloading UDPipe model for '", model_name, "'...")
  
  tryCatch({
    # Baixar o modelo
    model_info <- udpipe::udpipe_download_model(
      language = model_name,
      model_dir = model_dir
    )
    
    # Carregar o modelo baixado
    message("Successfully downloaded and loading model: ", basename(model_info$file_model))
    udpipe::udpipe_load_model(model_info$file_model)
    
  }, error = function(e) {
    stop("Failed to download or load UDPipe model: ", e$message)
  })
}

