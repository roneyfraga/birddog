
#' @title Binary Probability Matrix
#' @description Create a matrix with n columns filled with zeros and ones, as the probabilities of occurrences.
#' @param nVar number of variables, Default: 3
#' @param asList return as a list of matrix or one matrix, Default: TRUE 
#' @return list of matrix or matrix
#' @examples 
#' @examples 
#' if(interactive()){
#'  binaryPropMatrix(nvar=4)
#'  binaryPropMatrix(nvar=4, asList=F)
#'  }
#' @rdname binaryProbMatrix
#' @export 

binaryProbMatrix <- function(nVar=3, asList=TRUE){

    stopifnot(is.numeric(nVar), length(nVar) == 1)
    stopifnot(is.logical(asList), length(asList) == 1)

    lst <- lapply(numeric(nVar), function(x) c(0, 1))
    m <- as.matrix(expand.grid(lst))

    # order rows
    ordenar <- data.frame(somaLinha=rowSums(m),id=1:nrow(m))
    ordenar <- ordenar[ order(ordenar$somaLinha),]
    m <- m[ordenar$id,]

    if(asList==TRUE){

        # find combinations
        # comb(5,2)     # ex.: 5 numbers 2 combinations
        comb = function(n, x) {
            factorial(n) / factorial(n-x) / factorial(x)
        }

        cb <- NULL

        for(i in seq_along(m[1,])){
            cb[[i]] <- comb(ncol(m),i)
        }

        cb <- unlist(cb)
        cb <- c(cb[length(cb)],cb)

        res <- NULL

        for(i in seq_along(cb)){

            if(i==1){

                res[[1]] <- m[1,]

            }else{

                final <- sum(cb[1:i])
                inicio <- (sum(cb[1:i])+1)-cb[i]

                res[[i]] <- m[ inicio:final , ]

            }
        }
    } else { res <- m }

    return(res)

}
