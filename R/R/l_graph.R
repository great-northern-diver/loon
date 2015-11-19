#' @export
l_graph <- function(nodes, ...) {
    UseMethod("l_graph")
}

#' @export l_graph.graph
l_graph.graph <- function(nodes, ...) {
    l_graph.loongraph(as.loongraph(nodes), ...)
}

#' @export l_graph.loongraph
l_graph.loongraph <- function(nodes,...) {
    graph <- nodes
    l_graph.default(nodes=graph$nodes, from=graph$from, to=graph$to,
                    isDirected=graph$isDirected, ...)
}


#' @export l_graph.default
l_graph.default <- function(nodes="", from="", to="",  parent=NULL, ...) {

    loonPlotFactory('::loon::graph', 'graph', 'loon graph', parent,
                    nodes=nodes, from=from, to=to, ...)
    
}

