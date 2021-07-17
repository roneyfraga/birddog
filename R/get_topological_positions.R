
#' @title get_topological_positions
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @param group PARAM_DESCRIPTION
#' @param PY PARAM_DESCRIPTION, Default: PY
#' @param TI PARAM_DESCRIPTION, Default: TI
#' @param CR PARAM_DESCRIPTION, Default: CR
#' @param min.citations PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{nse-defuse}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{rename}},\code{\link[dplyr]{mutate}},
#' \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},
#' \code{\link[dplyr]{filter}},\code{\link[dplyr]{arrange}},
#' \code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{bind}},
#' \code{\link[dplyr]{desc}}
#'  \code{\link[stringr]{str_locate}}
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[stats]{sd}}
#' @rdname get_topological_positions
#' @export
#' @importFrom rlang enquo
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename mutate select starts_with filter arrange right_join bind_rows left_join desc
#' @importFrom stringr str_locate_all
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' @importFrom rlang .data
get_topological_positions <- function(data, id, group, PY = PY, 
                                      TI = TI, CR = CR, 
                                      min.citations = 1) {

    SR <- rlang::enquo(id)
    group <- rlang::enquo(group)
    PY <- rlang::enquo(PY)
    TI <- rlang::enquo(TI)
    CR <- rlang::enquo(CR)

    M <- data

    M |>
        tibble::as_tibble() |>
        dplyr::rename(group = !!group, PY = !!PY, TI = !!TI, CR = !!CR) |> 
        dplyr::mutate(TI = ifelse(is.na(.data$TI), 'noTitle', .data$TI)) ->
        M

    M$nCITING <- 1:nrow(M)
    papers <- M$nCITING[M$TC >= min.citations]
    TIpost <- paste(gsub("[[:punct:]]", "", M$TI[papers]), " ", M$PY[papers], " ", sep = "")

    M |> 
        dplyr::select(.data$group, .data$CR) |>  
        {\(x) split(x, x$group)}() ->
        Ml

    for (i in seq_along(Ml)) {

        cat('group', i, '\n')

        CR <- gsub("[[:punct:]]", "", Ml[[i]]$CR)
        CR <- paste(CR, collapse = " ")

        L <- stringr::str_locate_all(CR, TIpost)

        LCS <- lengths(L) / 2

        M[, paste0('cited_from_', names(Ml)[[i]])] <- 0
        M[papers, paste0('cited_from_', names(Ml)[[i]])] <- LCS

    }

    M$nCITING <- NULL
    M$CR <- NULL
    M$PY <- NULL
    M$TI <- NULL

    # all groups citation
    # Ki
    M |> 
        dplyr::select(starts_with('cited_from')) |> 
        rowSums() -> 
        M$Ki

    # inside group citations 
    # ki
    M |> 
        dplyr::select(.data$SR, .data$group, dplyr::starts_with('cited_from')) |> 
        tidyr::pivot_longer(! c(.data$SR, .data$group), names_to = "cited_from", values_to = "ki") |>
        dplyr::mutate(cited_from = gsub('cited_from_', '', .data$cited_from)) |> 
        dplyr::filter(group == .data$cited_from) |> 
        dplyr::select(- .data$cited_from) |>
        dplyr::arrange(.data$SR) |> 
        dplyr::right_join(M) |>
        dplyr::select(.data$SR, .data$group, .data$TC, .data$Ki, .data$ki, dplyr::starts_with('cited_from')) |> 
        dplyr::arrange(group, desc(.data$Ki)) ->
        M

    # calculo dentro de cada grupo
    # Zi
    Ml2 <- split(M, M[, 'group']) 

    lapply(Ml2, function(x) { 
               x |> 
                   dplyr::mutate(Zi = (.data$ki - mean(.data$ki)) / stats::sd(.data$ki)) |> 
                   dplyr::arrange(desc(.data$Zi)) 
    }) |> 
    dplyr::bind_rows() ->
    M

    # Pi
    M |> 
        dplyr::select(dplyr::starts_with('cited_from'), .data$Ki, .data$SR) -> 
        tt
    
    nm <- names(tt |> dplyr::select(dplyr::starts_with('cited_from')))
    
    for (i in seq_along(nm)) {
        tt[, nm[[i]]] <- (M[, nm[[i]]] / M$Ki)^2
    }
    
    tt |> 
        dplyr::select(dplyr::starts_with('cited_from')) |> 
        {\(acima)(1 - rowSums(acima))}() ->
        tt$Pi
    
    M |> 
        dplyr::left_join(tt |> dplyr::select(.data$SR, .data$Pi)) |>
        dplyr::arrange(.data$group, dplyr::desc(.data$Ki)) |>
        dplyr::select(.data$group, .data$SR, .data$TC, .data$Ki, 
                      .data$ki, .data$Zi, .data$Pi, - dplyr::starts_with('cited_from')) -> 
        M

    M$zone <- 'noHub'
    M[M$Zi >= 2.5 & M$Pi <= 0.3, 'zone'] <- 'R5'
    M[M$Zi >= 2.5 & M$Pi > 0.30 & M$Pi <= 0.75, 'zone'] <- 'R6'
    M[M$Zi >= 2.5 & M$Pi >= 0.75, 'zone'] <- 'R7'
    
    return(M)
}
