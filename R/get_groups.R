
#' @title get_groups
#' @description FUNCTION_DESCRIPTION
#' @param net PARAM_DESCRIPTION
#' @param min_group_size PARAM_DESCRIPTION, Default: 10
#' @param keep_component PARAM_DESCRIPTION, Default: c("component01", "component02")
#' @param cluster_component PARAM_DESCRIPTION, Default: 'component01'
#' @param algorithm PARAM_DESCRIPTION, Default: 'louvain'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if (interactive()) {
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[igraph]{cluster_louvain}},\code{\link[igraph]{cluster_walktrap}},
#'  \code{\link[igraph]{cluster_edge_betweenness}},\code{\link[igraph]{cluster_fast_greedy}},
#'  \code{\link[igraph]{as_data_frame}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{context}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{bind}}
#'  \code{\link[stringr]{str_pad}}
#'  \code{\link[tidygraph]{as_tbl_graph.data.frame}},\code{\link[tidygraph]{activate}}
#' @rdname get_groups
#' @export 
#' @importFrom igraph cluster_louvain cluster_walktrap cluster_edge_betweenness cluster_fast_greedy get.data.frame
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by summarise n arrange desc mutate select ungroup filter right_join bind_rows left_join
#' @importFrom stringr str_pad
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom rlang .data
get_groups <- function(net, 
                       min_group_size = 10, 
                       keep_component = c('component01', 'component02'),
                       cluster_component = 'component01', 
                       algorithm = 'louvain') {  

    component <- quantity_papers <- group <- group_new  <- NULL

    net |>
        tidygraph::as_tbl_graph() |>
        tidygraph::activate('nodes') |>
        dplyr::filter(component %in% keep_component) ->
        net

    comp <- lapply(keep_component, function(x) {
                        net |>
                            tidygraph::as_tbl_graph() |>
                            tidygraph::activate('nodes') |>
                            dplyr::filter(component %in% x)
                       })
    names(comp) <- keep_component

    no_cluster <- keep_component[! (keep_component %in% cluster_component)]
    no_cluster <- comp[no_cluster]

    to_cluster <- keep_component[(keep_component %in% cluster_component)]
    to_cluster <- comp[to_cluster]

    res <- vector(mode = "list", length = length(to_cluster))
    res2 <- vector(mode = "list", length = length(to_cluster))
    res3 <- list()

    for (i in seq_along(to_cluster)) {

        if (algorithm == 'louvain') {
            eb <- igraph::cluster_louvain(to_cluster[[i]])
        }

        if (algorithm == 'walktrap') {
            eb <- igraph::cluster_walktrap(to_cluster[[i]])
        }

        if (algorithm == 'edge_betweenness') {
            eb <- igraph::cluster_edge_betweenness(to_cluster[[i]])
        }

        if (algorithm == 'fast_greedy') {
            eb <- igraph::cluster_fast_greedy(to_cluster[[i]])
        }

        igraph::V(to_cluster[[i]])$group <- eb$membership

        igraph::get.data.frame(to_cluster[[i]], what = "vertices") |> 
        tibble::as_tibble() |> 
        dplyr::group_by(.data$group) |> 
        dplyr::summarise(quantity_papers = dplyr::n(), average_age = mean(.data$PY, na.rm = TRUE), component = unique(.data$component)) |> 
        dplyr::arrange(component, dplyr::desc(quantity_papers)) |> 
        dplyr::mutate(group_new = paste(.data$component, '_', 'g', stringr::str_pad(1:dplyr::n(), width = 2, pad = '0', side = 'left'), sep = '')) |> 
        dplyr::select(.data$group, .data$group_new, .data$quantity_papers, .data$average_age, .data$group) |> 
        dplyr::ungroup()|> 
        dplyr::filter(.data$quantity_papers >= min_group_size) -> 
        res[[i]]

        res[[i]] |> 
        dplyr::select(group, group_new)|> 
        dplyr::right_join(igraph::get.data.frame(to_cluster[[i]], what = "vertices")) |>
        dplyr::select(.data$SR, group = .data$group_new) -> 
        res2[[i]]

        res[[i]] |> 
        dplyr::select(group = .data$group_new, .data$quantity_papers, .data$average_age) -> 
        res[[i]]

        res3[[i]] <- list(aggregate = res[[i]], ids = res2[[i]])

    }

    nc <- vector(mode = "list", length = length(no_cluster))
    nc2 <- vector(mode = "list", length = length(no_cluster))
    nc3 <- list() 

    for (i in seq_along(no_cluster)) {

        igraph::get.data.frame(no_cluster[[i]], what = "vertices") |> 
        tibble::as_tibble() |> 
        dplyr::group_by(.data$component) |> 
        dplyr::summarise(quantity_papers = dplyr::n(), average_age = mean(.data$PY, na.rm = TRUE), component = unique(.data$component)) |> 
        dplyr::arrange(.data$component, dplyr::desc(.data$quantity_papers)) |> 
        dplyr::select(group = .data$component, .data$quantity_papers, .data$average_age) -> 
        nc[[i]]

        igraph::get.data.frame(no_cluster[[i]], what = "vertices")|> 
        tibble::as_tibble() |> 
        dplyr::select(.data$SR, group = .data$component) -> 
        nc2[[i]]

        nc3[[i]] <- list('aggregate' = nc[[i]], 'ids' = nc2[[i]])

    }

    g <- c(res3, nc3)

    lapply(g, function(x) x$ids) |> 
    dplyr::bind_rows() -> 
    ids

    lapply(g, function(x) x$aggregate) |> 
    dplyr::bind_rows() -> 
    aggregate

    tidygraph::as_tbl_graph(net) |>
    tidygraph::activate('nodes') |>
    dplyr::left_join(ids) ->
    net

    tidygraph::as_tbl_graph(net) |>
    tidygraph::activate('nodes') |>
    dplyr::left_join(aggregate) ->
    net

    list('aggregate' = aggregate, 'network' = net)
}
