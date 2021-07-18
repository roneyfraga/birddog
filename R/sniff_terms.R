
#' @title sniff_terms
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param groups PARAM_DESCRIPTION
#' @param text PARAM_DESCRIPTION
#' @param algorithm PARAM_DESCRIPTION, Default: 'RAKE'
#' @param phrase_pattern PARAM_DESCRIPTION, Default: '(A|N)*N(P+D*(A|N)*N)*'
#' @param n_cores PARAM_DESCRIPTION, Default: 1
#' @param show_progress PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if (interactive()) {
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{nse-defuse}}
#'  \code{\link[dplyr]{rename}}
#'  \code{\link[udpipe]{udpipe_load_model}},\code{\link[udpipe]{udpipe_annotate}},\code{\link[udpipe]{udpipe}},\code{\link[udpipe]{keywords_rake}},\code{\link[udpipe]{keywords_collocation}},\code{\link[udpipe]{as_phrasemachine}},\code{\link[udpipe]{keywords_phrases}}
#' @rdname sniff_terms
#' @export 
#' @importFrom rlang enquo
#' @importFrom dplyr rename
#' @importFrom udpipe udpipe_load_model udpipe_annotate udpipe keywords_rake keywords_collocation as_phrasemachine keywords_phrases
#' @importFrom rlang .data
sniff_terms <- function(data, 
                      groups, 
                      text, 
                      algorithm = 'RAKE', 
                      phrase_pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                      n_cores = 1,
                      show_progress = TRUE) {

    groups <- rlang::enquo(groups)
    text <- rlang::enquo(text)
    data <- dplyr::rename(data, doc_id = !!groups, text = !!text)

    ngram <- freq <- NULL

    # load model
    arquivo <- system.file('extdata/english-ewt-ud-2.5-191206.udpipe', package = 'caiporar')
    ud_model <- udpipe::udpipe_load_model(file = arquivo)

    # n_cores number > 0
    if (any(n_cores < 1 | !is.numeric(n_cores))) stop('"n_cores" must be a number higher than 0.')

    if (n_cores == 1) {

        x <- udpipe::udpipe_annotate(ud_model, x = data[['text']], doc_id = data[['doc_id']], trace = show_progress)

    } else {

        x <- udpipe::udpipe(data, ud_model, parallel.cores = n_cores) 

    }

    x <- as.data.frame(x)

    if (algorithm == 'RAKE') {
        stats <- udpipe::keywords_rake(x = x, term = "lemma", group = "doc_id", relevant = x$upos %in% c("NOUN", "ADJ"))
    }

    if (algorithm == 'pointwise') {
        x$word <- tolower(x$token)
        stats <- udpipe::keywords_collocation(x = x, term = "word", group = "doc_id")
    }

    if (algorithm == 'phrase') {
        x$phrase_tag <- udpipe::as_phrasemachine(x$upos, type = "upos")
        stats <- udpipe::keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                                          pattern = phrase_pattern, 
                                          is_regex = TRUE, detailed = FALSE)
        stats <- subset(stats, ngram > 1 & freq > 2)
    }

    stats
}
