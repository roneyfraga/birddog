#' @title get_components
#' @description FUNCTION_DESCRIPTION
#' @param net PARAM_DESCRIPTION
#' @param name_pad_width PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if (interactive()) {
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[igraph]{component_distribution}},\code{\link[igraph]{V}},\code{\link[igraph]{as_data_frame}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{context}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{rename}}
#'  \code{\link[stringr]{str_pad}}
#'  \code{\link[tidygraph]{as_tbl_graph.data.frame}},\code{\link[tidygraph]{activate}}
#' @rdname get_components
#' @export 
#' @importFrom igraph clusters V get.data.frame
#' @importFrom tibble as_tibble
#' @importFrom dplyr select group_by summarise n arrange desc mutate left_join rename
#' @importFrom stringr str_pad
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom rlang .data 
get_components <- function(net, name_pad_width = 2) {

    cfg <- igraph::clusters(net)

    igraph::V(net)$component <- cfg$membership

    igraph::get.data.frame(net, what = "vertices") |> 
        tibble::as_tibble() |> 
        dplyr::select(.data$PY, .data$component) |> 
        dplyr::group_by(.data$component) |> 
        dplyr::summarise(qtde_publi = dplyr::n(), idade_media = mean(.data$PY, na.rm = TRUE)) |> 
        dplyr::arrange(dplyr::desc(.data$qtde_publi)) |> 
        dplyr::mutate(componente_name = 
                      paste('component', stringr::str_pad(1:dplyr::n(), 
                                                          width = name_pad_width, 
                                                          pad = '0', side = 'left'), 
                            sep = '')) -> 
        componentes

    componentes |> 
        dplyr::select(.data$component, .data$componente_name) ->
        a

    net |> 
        tidygraph::as_tbl_graph() |>
        tidygraph::activate('nodes') |>
        dplyr::left_join(a) |>
        dplyr::select(-.data$component) |>
        dplyr::rename(component = .data$componente_name) ->
        net

    componentes |> 
        dplyr::select(component = .data$componente_name, 
                      quantity_publications = .data$qtde_publi, 
                      average_age = .data$idade_media) ->
        componentes 

    list(components = componentes, network = net)
}
