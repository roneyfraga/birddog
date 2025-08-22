#' Run Structural Topic Modeling Analysis
#'
#' Performs structural topic modeling on prepared text data and returns topic proportions
#' and top documents for each topic.
#'
#' @param groups_stm_prepare A prepared STM object from \code{sniff_groups_stm_prepare()}
#' @param k_topics Number of topics to model (default: 12)
#' @param n_top_documents Number of top documents to each topic (default: 50)
#'
#' @return A list containing:
#' \itemize{
#'   \item{topic_proportion2: Data frame with topic proportions and top terms}
#'   \item{tab_top_documents: Data frame of top documents for each topic}
#' }
#'
#' @details
#' This function:
#' \itemize{
#'   \item{Fits an STM model with specified number of topics}
#'   \item{Identifies top terms for each topic}
#'   \item{Calculates topic proportions}
#'   \item{Identifies top documents for each topic}
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare data first
#' stm_data <- sniff_groups_stm_prepare(network_data)
#'
#' # Run topic modeling
#' stm_results <- sniff_groups_stm_run(stm_data, k_topics = 15)
#'
#' # Access results
#' stm_results$topic_proportion2  # Topic proportions and terms
#' stm_results$tab_top_documents  # Top documents per topic
#' }
#'
#' @importFrom dplyr arrange group_by top_n select summarise mutate ungroup slice_head left_join desc distinct rename
#' @importFrom purrr map
#' @export
sniff_groups_stm_run <- function(groups_stm_prepare, k_topics = 12, n_top_documents = 50) {
  # Input validation
  if (!is.list(groups_stm_prepare)) {
    stop("groups_stm_prepare must be a list from sniff_groups_stm_prepare()", call. = FALSE)
  }

  required_components <- c("result", "plots", "data", "parameters")
  missing_components <- setdiff(required_components, names(groups_stm_prepare))
  if (length(missing_components) > 0) {
    stop("groups_stm_prepare is missing required components: ", paste(missing_components, collapse = ", "), call. = FALSE)
  }

  if (!is.numeric(k_topics) || length(k_topics) != 1 || k_topics < 1) {
    stop("k_topics must be a single integer >= 1", call. = FALSE)
  }

  # Extract prepared data
  pat <- groups_stm_prepare$data

  # Fit STM model with error handling
  pat_stm_fit <- tryCatch({
    stm::stm(documents = pat$df_doc$documents, vocab = pat$df_doc$vocab, K = k_topics, data = pat$df_doc$meta, verbose = FALSE)
  }, error = function(e) {
    stop("STM model fitting failed: ", e$message, call. = FALSE)
  })

  # Get top terms for each topic
  top_terms <- tryCatch({
    tidytext::tidy(pat_stm_fit) |>
      dplyr::arrange(beta) |>
      dplyr::group_by(topic) |>
      dplyr::top_n(10, beta) |>
      dplyr::arrange(-beta) |>
      dplyr::select(topic, term) |>
      dplyr::summarise(terms = list(term)) |>
      dplyr::mutate(terms = purrr::map(terms, paste, collapse = ", ")) |>
      tidyr::unnest(cols = c(terms))
  }, error = function(e) {
      stop("Failed to extract top terms: ", e$message, call. = FALSE)
    })

  # Calculate topic proportions
  n_documents <- dim(pat_stm_fit$theta)[1]
  
  topic_proportion <- tryCatch({
    tidytext::tidy(pat_stm_fit, matrix = "gamma") |>
      dplyr::group_by(document) |>
      dplyr::arrange(dplyr::desc(gamma)) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::group_by(topic) |>
      dplyr::summarise(topic_proportion = n() / n_documents)
  }, error = function(e) {
    stop("Failed to calculate topic proportions: ", e$message, call. = FALSE)
  })

  # Create final topic proportion table
  topic_proportion2 <- topic_proportion |>
    dplyr::left_join(top_terms, by = "topic") |>
    dplyr::mutate(topic = paste0("Topic ", topic)) |>
    dplyr::arrange(dplyr::desc(topic_proportion))

  # Get top documents for each topic
  gamma_documents <- tryCatch({
    tidytext::tidy(pat_stm_fit, matrix = "theta", 
                  document_names = pat$df_prep$meta$name) |>
      dplyr::left_join(pat$df |> dplyr::rename(document = name), 
                      by = join_by(document)) |>
      dplyr::distinct(.keep_all = TRUE) |>
      dplyr::arrange(document, topic) |>
      dplyr::group_by(topic) |>
      dplyr::arrange(dplyr::desc(gamma)) |>
      dplyr::slice_head(n = n_top_documents) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        topic = paste("topic", topic, sep = "_"),
        gamma = round(.data$gamma, digits = 3)
      ) |>
      dplyr::select(document, topic, gamma, DI, title = TI)
  }, error = function(e) {
    stop("Failed to identify top documents: ", e$message, call. = FALSE)
  })

  # Return results in a structured list
  list(
    topic_proportion = topic_proportion2,
    top_documents = gamma_documents
  )
}
