
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param inputPath PARAM_DESCRIPTION, Default: '/home/roney/Desktop/gallery/2019_MartinaFlorence/'
#' @param pattern PARAM_DESCRIPTION, Default: '*.jpg'
#' @param rows PARAM_DESCRIPTION, Default: 1
#' @param outputPath PARAM_DESCRIPTION, Default: '../../gallery/2020_MartinaFlorence/'
#' @param output PARAM_DESCRIPTION, Default: '~/output.html'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname img2html
#' @export 
img2html <- function(inputPath = "/home/roney/Desktop/gallery/2019_MartinaFlorence/", pattern = '*.jpg', rows=1, 
                     outputPath = "../../gallery/2020_MartinaFlorence/", output='~/output.html'){

    img <- list.files(path=inputPath,pattern=pattern)
    full <- paste0(outputPath,img)

    a <- NULL

    if(rows==1){

        for(i in seq_along(full)){

            a[[i]] <- paste0( '
                             <center>
                             <div class="row"> 
                             <div class="col-md-12"> 
                             <a class="lightbox" href="',full[[i]],'">
                             <img src=" ',full[[i]],'">
                             </a>
                             </div>
                             </div>
                             </center>') 

        }
    }
    if(rows==2){

        # dividir o vetor em lista, sendo estas em blocos de no máximo dois arquivos
        full_ls <- split(full, ceiling(seq_along(full)/2))

        for(i in seq_along(full_ls)){

            if(length(full_ls[[i]])==2){

                a[[i]] <- paste0('
                                 <center>
                                 <div class="row">
                                 <div class="col-md-6 col-sm-6 col-xs-12">
                                 <a class="lightbox" href="',full_ls[[i]][[1]],'">
                                 <img src="',full_ls[[i]][[1]],'">
                                 </a>
                                 </div>
                                 </center>
                                 <center>
                                 <div class="col-md-6 col-sm-6 col-xs-12">
                                 <a class="lightbox" href="',full_ls[[i]][[2]],'">
                                 <img src="',full_ls[[i]][[2]],'">
                                 </a>
                                 </div>
                                 </div>
                                 </center>')
            }else{
                a[[i]] <- paste0('
                                 <center>
                                 <div class="row">
                                 <div class="col-md-6 col-sm-6 col-xs-12">
                                 <a class="lightbox" href="',full_ls[[i]][[1]],'">
                                 <img src="',full_ls[[i]][[1]],'">
                                 </a>
                                 </div>
                                 </center>
                                 <center>
                                 <div class="col-md-6 col-sm-6 col-xs-12">
                                 <a class="lightbox" href="',full_ls[[i]][[1]],'">
                                 <img src="',full_ls[[i]][[1]],'">
                                 </a>
                                 </div>
                                 </div>
                                 </center>')
            }
        }
    }

    fileConn <- file(output)
    writeLines(unlist(a), fileConn)
    close(fileConn)
}

