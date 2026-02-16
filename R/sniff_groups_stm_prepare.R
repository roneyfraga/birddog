#' Prepare Text Data and Analyze Topic Models
#'
#' Processes text data for structural topic modeling and performs topic number selection analysis,
#' returning both the processed data and diagnostic plots.
#'
#' @param groups A list containing network data with a 'network' component
#' @param group_to_stm Character string specifying which research group to process (default: 'g01')
#' @param search_topics Numeric vector of topic numbers to evaluate (default: c(5:40, 45, 50, 55, 60))
#' @param seed Random seed for reproducibility (default: 1234)
#' @param cores Number of CPU cores to use (default: 1)
#'
#' @return A list containing:
#' \itemize{
#'   \item{result: The searchK results object}
#'   \item{plots: A list containing two ggplot objects (p1: metrics by K, p2: exclusivity vs coherence)}
#'   \item{df_prep: Output from stm::textProcessor}
#'   \item{df_doc: Output from stm::prepDocuments}
#'   \item{df: Original filtered data}
#' }
#'
#' @examples
#' \dontrun{
#' output <- sniff_groups_stm_prepare(network_data)
#' output$plots$p1 # View first plot
#' output$result # Access search results
#' }
#'
#' @importFrom dplyr filter select mutate vars
#' @importFrom ggplot2 ggplot aes geom_point geom_line guides facet_wrap scale_x_continuous theme_bw labs theme_classic
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dbl
#' @export
sniff_groups_stm_prepare <- function(groups,
                                     group_to_stm = "g01",
                                     search_topics = c(5:40, 45, 50, 55, 60),
                                     seed = 1234,
                                     cores = 1) {
  # Input validation
  if (!is.list(groups)) {
    stop("groups must be a list containing network data", call. = FALSE)
  }

  if (!"network" %in% names(groups)) {
    stop("groups must contain a 'network' component", call. = FALSE)
  }

  if (!is.character(group_to_stm) || length(group_to_stm) != 1) {
    stop("group_to_stm must be a single character string", call. = FALSE)
  }

  # Extract and validate network data
  net_data <- tryCatch(
    {
      groups$network |>
        tidygraph::activate(nodes) |>
        tibble::as_tibble()
    },
    error = function(e) {
      stop("Failed to extract network data: ", e$message, call. = FALSE)
    }
  )

  # Check for required columns
  required_cols <- c("group", "TI", "AB")
  missing_cols <- setdiff(required_cols, colnames(net_data))
  if (length(missing_cols) > 0) {
    stop("Network data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Check if specified group exists
  if (!group_to_stm %in% net_data$group) {
    stop("Group '", group_to_stm, "' not found in network data", call. = FALSE)
  }

  if (!is.numeric(search_topics) || any(search_topics < 2)) {
    stop("search_topics must be a numeric vector with all values >= 2", call. = FALSE
    )
  }

  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)) {
    stop("seed must be a single finite numeric value", call. = FALSE)
  }

  if (!is.numeric(cores) || length(cores) != 1 || cores < 1) {
    stop("cores must be a single integer >= 1", call. = FALSE)
  }

  # Process text data
  net_data |>
    dplyr::filter(group == group_to_stm) |>
    dplyr::mutate(
      texto = tolower(paste(
        ifelse(is.na(TI), "", TI),
        ifelse(is.na(AB), "", AB),
        sep = ". "
      )),
      texto = ifelse(texto == ". ", NA_character_, texto)
    ) |>
    dplyr::select(name, group, TI, DI, texto) ->
    pat

  # Remove documents with no text
  pat <- pat[!is.na(pat$texto) & nchar(pat$texto) > 0, ]

  if (nrow(pat) == 0) {
    stop("No documents with valid text found for group '", group_to_stm, "'",
      call. = FALSE
    )
  }

  # Text processing with error handling
  pat_prep <- tryCatch(
    {
      stm::textProcessor(
        documents = pat$texto,
        metadata = pat,
        lowercase = TRUE,
        removestopwords = TRUE,
        removenumbers = TRUE,
        removepunctuation = TRUE,
        stem = TRUE,
        wordLengths = c(3, Inf),
        sparselevel = 1,
        language = "en",
        verbose = FALSE,
        onlycharacter = TRUE,
        striphtml = FALSE,
        customstopwords = NULL,
        v1 = FALSE
      )
    },
    error = function(e) {
      stop("Text processing failed: ", e$message, call. = FALSE)
    }
  )

  # Document preparation with error handling
  pat_doc <- tryCatch(
    {
      stm::prepDocuments(
        documents = pat_prep$documents,
        vocab = pat_prep$vocab,
        meta = pat_prep$meta,
        lower.thresh = 5,
        verbose = FALSE
      )
    },
    error = function(e) {
      stop("Document preparation failed: ", e$message, call. = FALSE)
    }
  )

  # Validate STM document structure
  if (!all(c("documents", "vocab", "meta") %in% names(pat_doc))) {
    stop("df_doc component is malformed - should contain documents, vocab, and meta", call. = FALSE)
  }

  # Check for empty documents
  if (length(pat_doc$documents) == 0) {
    stop("No documents found in prepared STM data", call. = FALSE)
  }

  # Remove duplicates and sort topic numbers
  search_topics <- unique(sort(search_topics))

  # Run topic search with error handling
  result <- tryCatch(
    {
      stm::searchK(
        documents = pat_doc$documents,
        vocab = pat_doc$vocab,
        K = search_topics,
        N = floor(length(pat_doc$documents) * 0.5),
        proportion = 0.5,
        heldout.seed = seed,
        M = 10,
        cores = cores,
        max.em.its = 75,
        data = pat_doc$meta,
        init.type = "Spectral",
        verbose = FALSE
      )
    },
    error = function(e) {
      stop("Topic search failed: ", e$message, call. = FALSE)
    }
  )

  # Add metadata about the search
  result$search_parameters <- list(
    search_date = Sys.time(),
    document_count = length(pat_doc$documents),
    vocab_size = length(pat_doc$vocab),
    group = unique(pat$group)
  )

  # Generate plot 1: Metrics by K
  result$results |>
    tidyr::pivot_longer(cols = -K, names_to = "metric", values_to = "value") |>
    dplyr::filter(metric %in% c("lbound", "exclus", "residual", "semcoh")) |>
    dplyr::mutate(
      value = purrr::map_dbl(value, 1),
      K = purrr::map_dbl(K, 1)
    ) ->
    K_metrics

  ggplot2::ggplot(K_metrics, ggplot2::aes(x = K, y = value, color = metric)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::guides(color = "none") +
    ggplot2::facet_wrap(dplyr::vars(metric), scales = "free") +
    ggplot2::scale_x_continuous(breaks = c(3, 5, 10, 15, 20, 30, 40, 50, 60)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      y = NULL,
      title = "Topic Model Diagnostics by Number of Topics",
      subtitle = paste("Group:", group_to_stm)
    ) ->
    p1

  # Generate plot 2: Exclusivity vs Coherence
  data.frame(
    K = unlist(result$results$K),
    semcoh = unlist(result$results$semcoh),
    exclus = unlist(result$results$exclus)
  ) ->
    res

  ggplot2::ggplot(res, ggplot2::aes(x = semcoh, y = exclus)) +
    ggplot2::geom_point(shape = 21, size = 3) +
    ggplot2::geom_line() +
    ggrepel::geom_text_repel(ggplot2::aes(label = K), size = 3) +
    ggplot2::labs(
      x = "Semantic Coherence",
      y = "Exclusivity",
      title = "Exclusivity vs. Semantic Coherence",
      subtitle = paste("Group:", group_to_stm, "| Numbers indicate topic count (K)")
    ) +
    ggplot2::theme_classic() ->
    p2

  # Return comprehensive output
  list(
    result = result,
    plots = list(metrics_by_k = p1, exclusivity_vs_coherence = p2),
    data = list(df = pat, df_prep = pat_prep, df_doc = pat_doc),
    parameters = list(search_topics = search_topics, seed = seed, cores = cores)
  )
}
