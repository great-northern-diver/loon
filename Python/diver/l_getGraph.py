
# #' @title Extract a loongraph or graph object from loon's graph display
# #'   
# #' @description The graph display represents a graph with the \code{nodes}, 
# #'   \code{from}, \code{to}, and \code{isDirected} plot states. This function 
# #'   creates a loongraph or a graph object using these states.
# #'   
# #' @param widget a graph widget handle
# #' @param asloongraph boolean, if TRUE then the function returns a loongraph
# #'   object, otherwise the function returns a graph object defined in the graph
# #'   \R package.
# #' 
# #' @return a loongraph or a graph object
# #' 
# #' @seealso \code{\link{l_graph}}, \code{\link{loongraph}}
# #'   
# #' @export

# from .graphutils import loongraph
# from .l_cget import l_cget
# def l_getGraph(widget, asloongraph=True):
    
#     graph = loongraph(nodes = l_cget(widget, 'nodes'),
#                       f = l_cget(widget, 'from'),
#                       to = l_cget(widget, 'to'),
#                       isDirected = l_cget(widget, 'isDirected'))

#     return graph
    