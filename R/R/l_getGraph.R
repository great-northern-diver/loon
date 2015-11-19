
#' @export
l_getGraph <- function(widget, asloongraph=TRUE) {
    
    graph <- loongraph(nodes = l_cget(widget, 'nodes'),
                       from = l_cget(widget, 'from'),
                       to = l_cget(widget, 'to'),
                       isDirected = l_cget(widget, 'isDirected'))
    
    if(!asloongraph) {
        graph <- as.graph(graph)
    }
    
}