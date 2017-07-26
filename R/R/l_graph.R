#' @title Generic funtction to create an interactive graph display
#'   
#' @description Interactive graphs in loon are currently most often used for 
#'   navigation graphs.
#'   
#' @param nodes object for method dispatch
#' @param ... arguments passed on to methods
#'   
#' @return graph handle
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section graph
#' @template see_l_help
#' 
#' @seealso \code{\link{l_graph.graph}}, \code{\link{l_graph.loongraph}},
#'   \code{l_graph.default}
#'   
#' @export
#' 
l_graph <- function(nodes = NULL, ...) {
    UseMethod("l_graph", nodes)
}

#' @title Create a graph display based on a graph object
#'   
#' @description Graph objects are defined in the graph \R package.
#' 
#' @param nodes a graph object created with the functions in the \code{graph} \R
#'   package.
#' @param ... arguments to modify the graph display state
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section graph
#' @template see_l_help
#'   
#' @return graph handle
#'   
#' @seealso \code{\link{l_graph}}, \code{\link{l_info_states}},
#'   \code{\link{l_graph.loongraph}}
#'   
#' @export 
#' @export l_graph.graph
l_graph.graph <- function(nodes, ...) {
    l_graph.loongraph(as.loongraph(nodes), ...)
}


#' @title Create a graph display based on a loongraph object
#'   
#' @description Loongraphs can be created with the \code{\link{loongraph}}
#'   function.
#'   
#' @inheritParams l_graph.graph
#' @param nodes a loongraph object created with the \code{\link{loongraph}} 
#'   function.
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section graph
#' @template see_l_help
#'   
#' @return graph handle
#'   
#' @seealso \code{\link{loongraph}}, \code{\link{l_graph}}, 
#'   \code{\link{l_info_states}}, \code{\link{l_graph.graph}}
#' 
#' @export
#' @export l_graph.loongraph
l_graph.loongraph <- function(nodes,...) {
    graph <- nodes
    l_graph.default(nodes=graph$nodes, from=graph$from, to=graph$to,
                    isDirected=graph$isDirected, ...)
}


#' @title Create a graph display based on node names and from-to edges list
#'   
#' @description This default method uses the loongraph display states as
#'   arguments to create a graph display.
#'   
#' @inheritParams l_graph.graph
#' @param nodes vector with nodenames
#' @param from vector with node names of the from-to pairs for edges
#' @param to vector with node names of the from-to pairs for edges
#' @param parent parent widget of graph display
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section graph
#' @template see_l_help
#'   
#' @return graph handle
#'   
#' @seealso \code{\link{loongraph}}, \code{\link{l_graph}}, 
#'   \code{\link{l_info_states}}, \code{\link{l_graph.graph}}
#'   
#' @export
#' @export l_graph.default
l_graph.default <- function(nodes="", from="", to="",  parent=NULL, ...) {

    loonPlotFactory('::loon::graph', 'graph', 'loon graph', parent,
                    nodes=nodes, from=from, to=to, ...)
    
}

